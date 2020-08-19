// Copyright 2020 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"image/color"
	"io"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"strconv"
	"sync"
	"sync/atomic"
	"time"

	"golang.org/x/sys/unix"
)

// Since the Event that triggered the error handler is also embedded, error handlers have information about the position of the module and can choose to display more contextual messages than a simple bar across the entire screen.
type barErrorEvent struct {
	Error error
	barEvent
}

// baristaSwayEvent instances are received from i3bar on stdin.
type baristaSwayEvent struct {
	barEvent
	Name string `json:"name"`
}

// baristaSwayBar is the "bar" instance that handles events and streams output.
type baristaSwayBar struct {
	sync.Mutex
	// The list of modules that make up this bar.
	modules   []barModule
	moduleSet *coreModuleSet
	// A map of previously set click handlers for each segment.
	clickHandlers map[string]func(barEvent)
	// The function to call when an error segment is right-clicked.
	errorHandler func(barErrorEvent)
	// The channel that receives a signal on module updates.
	update chan struct{}
	// The channel that aggregates all events from i3.
	events chan baristaSwayEvent
	// The Reader to read events from (e.g. stdin)
	reader io.Reader
	// The Writer to write bar output to (e.g. stdout)
	writer io.Writer
	// A json encoder set to write to the output stream.
	encoder *json.Encoder
	// Flipped when Run() is called, to prevent issues with modules
	// being added after the bar has been started.
	started bool
	// Suppress pause/resume signal handling to workaround potential
	// weirdness with signals.
	suppressSignals bool
	// Keeps track of whether the bar is currently paused, and
	// whether it needs to be refreshed on resume.
	paused          bool
	refreshOnResume bool
	// For testing, output the associated error in the json as well.
	// This allows output tester to accurately check for errors.
	includeErrorsInOutput bool
}

// maybeUpdate signals the update channel unless already signalled.
func (b *baristaSwayBar) maybeUpdate() {
	select {
	case b.update <- struct{}{}:
	default:
		// Since b.update has a buffer of 1, a failure to send to it
		// implies that an update is already queued. Since refresh
		// is only be called after individual modules' lastOutput is
		// set, when the previous update is consumed, each module will
		// already have the latest output.
	}
}

// refresh requests an update of the bar's output.
func (b *baristaSwayBar) refresh() {
	b.Lock()
	defer b.Unlock()
	// If paused, defer the refresh until the bar resumes.
	if b.paused {
		b.refreshOnResume = true
		return
	}
	b.maybeUpdate()
}

// readEvents parses the infinite stream of events received from i3.
func (b *baristaSwayBar) readEvents() error {
	decoder := json.NewDecoder(b.reader)
	// Consume opening '['
	_, err := decoder.Token()
	if err != nil {
		return err
	}
	for decoder.More() {
		var event baristaSwayEvent
		err = decoder.Decode(&event)
		if err != nil {
			return err
		}
		b.events <- event
	}
	return errors.New("stdin exhausted")
}

// Pause timing.
func timingPause() {
	timingMu.Lock()
	defer timingMu.Unlock()
	timingPaused = true
}

// pause instructs all pausable modules to suspend processing.
func (b *baristaSwayBar) pause() {
	b.Lock()
	defer b.Unlock()
	if b.paused {
		return
	}
	b.paused = true
	timingPause()
}

// Resume timing.
func timingResume() {
	timingMu.Lock()
	defer timingMu.Unlock()
	timingPaused = false
	for _, ch := range timingWaiters {
		close(ch)
	}
	timingWaiters = nil
}

// resume instructs all pausable modules to continue processing.
func (b *baristaSwayBar) resume() {
	b.Lock()
	defer b.Unlock()
	if !b.paused {
		return
	}
	b.paused = false
	timingResume()
	if b.refreshOnResume {
		b.refreshOnResume = false
		b.maybeUpdate()
	}
}

// sa* (Segment Attribute) consts are used as bitwise flags in attrSet
// to indicate which attributes are set (and so should be serialised).
const (
	saShortText int = 1 << iota
	saUrgent
	saSeparator
	saPadding
)

// IsUrgent returns true if this segment is marked urgent.
// The second value indicates whether it was explicitly set.
func (s *barSegment) IsUrgent() (bool, bool) {
	return s.urgent, s.attrSet&saUrgent != 0
}

// GetPadding returns the padding at the end of this segment.
// The second value indicates whether it was explicitly set.
// This maps to "separator_block_width" in i3.
func (s *barSegment) GetPadding() (int, bool) {
	if s.attrSet&saPadding != 0 {
		return s.padding, true
	}
	// Default padding is 9px.
	return 9, false
}

// GetMinWidth returns the minimum width of this segment.
// The returned value will either be an int or a string, based
// on how it was originally set.
// The second value indicates whether it was explicitly set.
func (s *barSegment) GetMinWidth() (interface{}, bool) {
	return s.minWidth, s.minWidth != nil
}

// Align sets the text alignment within the segment.
func (s *barSegment) Align(align barTextAlignment) *barSegment {
	s.align = align
	return s
}

// GetAlignment returns the text alignment of this segment.
// The second value indicates whether it was explicitly set.
func (s *barSegment) GetAlignment() (barTextAlignment, bool) {
	return s.align, s.align != ""
}

// HasSeparator returns true if the segment has a separator.
// The second value indicates whether it was explicitly set.
func (s *barSegment) HasSeparator() (bool, bool) {
	if s.attrSet&saSeparator != 0 {
		return s.separator, true
	}
	// Default value for separator is true in i3.
	return true, false
}

// GetBorder returns the border color of this segment.
// The second value indicates whether it was explicitly set.
func (s *barSegment) GetBorder() (color.Color, bool) {
	return s.border, s.border != nil
}

// GetBackground returns the background color of this segment.
// The second value indicates whether it was explicitly set.
func (s *barSegment) GetBackground() (color.Color, bool) {
	return s.background, s.background != nil
}

// GetColor returns the foreground color of this segment.
// The second value indicates whether it was explicitly set.
func (s *barSegment) GetColor() (color.Color, bool) {
	return s.color, s.color != nil
}

// GetShortText returns the short text of this segment.
// The second value indicates whether it was explicitly set.
func (s *barSegment) GetShortText() (string, bool) {
	return s.shortText, s.attrSet&saShortText != 0
}

// Content returns the text content of the segment, and whether or not
// it is using pango markup.
func (s *barSegment) Content() (text string, isPango bool) {
	return s.text, s.pango
}

func colorString(c color.Color) string {
	// XXX cful, _ := colorful.MakeColor(c)
	// return cful.Hex()
	return "#affe69"
}

// baristaSwayMap serialises the attributes of the barSegment in
// the format used by swaybar.
func baristaSwayMap(s *barSegment) map[string]interface{} {
	i3map := make(map[string]interface{})
	txt, pango := s.Content()
	i3map["full_text"] = txt
	if shortText, ok := s.GetShortText(); ok {
		i3map["short_text"] = shortText
	}
	if color, ok := s.GetColor(); ok {
		i3map["color"] = colorString(color)
	}
	if background, ok := s.GetBackground(); ok {
		i3map["background"] = colorString(background)
	}
	if border, ok := s.GetBorder(); ok {
		i3map["border"] = colorString(border)
	}
	if minWidth, ok := s.GetMinWidth(); ok {
		i3map["min_width"] = minWidth
	}
	if align, ok := s.GetAlignment(); ok {
		i3map["align"] = align
	}
	if urgent, ok := s.IsUrgent(); ok {
		i3map["urgent"] = urgent
	}
	if separator, ok := s.HasSeparator(); ok {
		i3map["separator"] = separator
	}
	if padding, ok := s.GetPadding(); ok {
		i3map["separator_block_width"] = padding
	}
	if pango {
		i3map["markup"] = "pango"
	} else {
		i3map["markup"] = "none"
	}
	return i3map
}

// LastOutputs returns the last output from all modules in order. The returned
// slice will have exactly Len() elements, and if a module has not yet updated
// an empty output will be placed in its position.
func (m *coreModuleSet) LastOutputs() []barSegments {
	m.outputsMu.RLock()
	defer m.outputsMu.RUnlock()
	cp := make([]barSegments, len(m.outputs))
	copy(cp, m.outputs)
	return cp
}

// HasClick returns whether this segment has a click handler defined.
// Modules can use this check to assign default handlers to segments
// where the user has not already assigned a click handler.
func (s *barSegment) HasClick() bool {
	return s.onClick != nil
}

// print outputs the entire bar, using the last output for each module.
func (b *baristaSwayBar) print() error {
	// Store the set of click handlers for any segments that can handle clicks.
	// When i3bar sends us the click event, it will include an identifier that
	// we can use to look up the function to call.
	b.clickHandlers = map[string]func(barEvent){}
	// i3bar requires the entire bar to be printed at once, so we just take the
	// last cached value for each module and construct the current bar.
	output := make([]map[string]interface{}, 0)
	for _, segments := range b.moduleSet.LastOutputs() {
		for _, segment := range segments {
			out := baristaSwayMap(segment)
			var clickHandler func(barEvent)
			if err := segment.GetError(); err != nil {
				// because go.
				segment := segment
				clickHandler = func(e barEvent) {
					if e.Button == buttonRight {
						b.errorHandler(barErrorEvent{Error: err, barEvent: e})
					} else {
						segment.Click(e)
					}
				}
			} else if segment.HasClick() {
				clickHandler = segment.Click
			}
			if clickHandler != nil {
				name := strconv.Itoa(len(b.clickHandlers))
				out["name"] = name
				b.clickHandlers[name] = clickHandler
			}
			output = append(output, out)
		}
	}
	if err := b.encoder.Encode(output); err != nil {
		return err
	}
	_, err := io.WriteString(b.writer, ",\n")
	return err
}

// baristaSwayHeader is sent at the beginning of output.
type baristaSwayHeader struct {
	Version     int  `json:"version"`
	StopSignal  int  `json:"stop_signal,omitempty"`
	ContSignal  int  `json:"cont_signal,omitempty"`
	ClickEvents bool `json:"click_events"`
}

// Module represents a bar.Module wrapped with core barista functionality.
// It is used as a building block for the main bar, modules that manipulate
// other modules (group, reformat), and for writing tests.
// It handles restarting the wrapped module on a left/right/middle click,
// as well as providing an option to "replay" the last output from the module.
// It also provides timed output functionality.
type coreModule struct {
	original  barModule
	replayCh  <-chan struct{}
	replayFn  func()
	restartCh <-chan struct{}
	restartFn func()
}

func coreToSegments(o barOutput) barSegments {
	if o == nil {
		return nil
	}
	return o.barSegments()
}

// addRestartHandlers replaces all click handlers with a function
// that restarts the module. This is used on the last output of
// the wrapped module after the original finishes.
func coreAddRestartHandlers(o barOutput, restartFn func()) barSegments {
	in := coreToSegments(o)
	var out barSegments
	for _, s := range in {
		out = append(out, s.Clone().OnClick(func(e barEvent) {
			if coreIsRestartableClick(e) {
				restartFn()
			}
		}))
	}
	return out
}

// barRefresherModule extends module with a Refresh() method that forces a refresh
// of the data being displayed (e.g. a fresh HTTP request or file read).
// core.Module will add middle-click to refresh for modules that implement it.
type barRefresherModule interface {
	barModule
	Refresh()
}

// coreTimedSink is a wrapper around bar.Sink that supports timed output. It takes
// a single bar.TimedOutput and unrolls it into multiple calls to the underlying
// sink, automatically resetting future calls on new output.
type coreTimedSink struct {
	barSink
	*timingScheduler
	refreshFn func()

	mu          sync.Mutex
	out         barTimedOutput
	refreshable bool
}

var (
	// A set of channels to be closed by timing.Resume.
	// This allows schedulers to wait for resume, without
	// requiring a reference to each created scheduler.
	timingWaiters []chan struct{}
	timingPaused  = false
	timingMu      sync.Mutex
)

// await executes the given function when the bar is running.
// If the bar is paused, it waits for the bar to resume.
func timingAwait(fn func()) {
	timingMu.Lock()
	if !timingPaused {
		timingMu.Unlock()
		fn()
		return
	}
	ch := make(chan struct{})
	timingWaiters = append(timingWaiters, ch)
	timingMu.Unlock()
	go func() {
		<-ch
		fn()
	}()
}

func (s *timingScheduler) timingMaybeTrigger() {
	if !atomic.CompareAndSwapInt32(&s.waiting, 0, 1) {
		return
	}
	timingAwait(func() {
		if atomic.CompareAndSwapInt32(&s.waiting, 1, 0) {
			s.notifyFn()
		}
	})
}

// At sets the scheduler to trigger a specific time.
// This will replace any pending triggers.
func (s *timingScheduler) At(when time.Time) *timingScheduler {
	s.schedulerImpl.At(when, s.timingMaybeTrigger)
	return s
}

// isRestartableClick checks whether a click event should restart the
// wrapped module. A left/right/middle click will restart the module.
func coreIsRestartableClick(e barEvent) bool {
	return e.Button == buttonLeft ||
		e.Button == buttonRight ||
		e.Button == buttonMiddle
}

// Click calls a previously set click handler with the given Event.
func (s *barSegment) Click(e barEvent) {
	if s.onClick != nil {
		s.onClick(e)
	}
}

// OnClick sets a function to be called when the segment is clicked.
// A nil function is treated as equivalent to func(Event) {}, which
// means CanClick() will return true, but Click(Event) will do nothing.
// Nil can therefore be used to prevent module-level default handlers
// from being attached to a segment.
func (s *barSegment) OnClick(fn func(barEvent)) *barSegment {
	if fn == nil {
		fn = func(barEvent) {}
	}
	s.onClick = fn
	return s
}

// Clone makes a copy of the Segment that can be modified
// without the changes being reflected in the original.
func (s *barSegment) Clone() *barSegment {
	copied := &barSegment{}
	*copied = *s
	return copied
}

// GetError returns any error associated with this segment
// or nil if no error is associated with this segment.
func (s *barSegment) GetError() error {
	return s.err
}

// Segments implements bar.Output for a single Segment.
func (s *barSegment) barSegments() []*barSegment {
	return []*barSegment{s}
}

// Segments returns the list of segments as a bar.Output.
func (s barSegments) barSegments() []*barSegment {
	return s
}

// coreAddRefreshHandlers adds middle-click refresh to the output.
func coreAddRefreshHandlers(o barOutput, refreshFn func()) barSegments {
	in := coreToSegments(o)
	if refreshFn == nil {
		return in
	}
	var out barSegments
	for _, s := range in {
		handleClick := s.Click
		hasError := s.GetError() != nil
		out = append(out, s.Clone().OnClick(func(e barEvent) {
			switch {
			case e.Button == buttonMiddle:
				refreshFn()
			case hasError && coreIsRestartableClick(e):
				refreshFn()
			default:
				handleClick(e)
			}
		}))
	}
	return out
}

// Output updates the module's output on the bar.
func (s barSink) Output(o barOutput) {
	s(o)
}

func (t *coreTimedSink) renderLocked() {
	if t.out == nil {
		return
	}
	var o barOutput = t.out
	if next := t.out.NextRefresh(); !next.IsZero() {
		t.At(next)
	} else {
		t.Stop()
		t.out = nil
	}
	if t.refreshable {
		o = coreAddRefreshHandlers(o, t.refreshFn)
	}
	t.barSink.Output(o)
}

func (t *coreTimedSink) render() {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.renderLocked()
}

// Tick waits until the next tick of the scheduler.
// Equivalent to <-scheduler.C, but returns true to allow for sch.Tick() { ... }
func (s *timingScheduler) Tick() bool {
	<-s.C
	return true
}

func (t *coreTimedSink) runLoop() {
	for t.Tick() {
		t.render()
	}
}

// barTimedOutput extends bar.Output with a hint that indicates the next time that
// the output segments will be different. This can be used, for example, to show
// elapsed duration since a fixed point in time.
type barTimedOutput interface {
	barOutput
	NextRefresh() time.Time
}

type coreStaticTimedOutput struct {
	barOutput
}

func (s coreStaticTimedOutput) NextRefresh() time.Time {
	return time.Time{}
}

func (t *coreTimedSink) Output(o barOutput, refreshable bool) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.refreshable = refreshable
	var ok bool
	t.out, ok = o.(barTimedOutput)
	if !ok {
		t.out = coreStaticTimedOutput{o}
	}
	t.renderLocked()
}

type timingSchedulerImpl interface {
	At(time.Time, func())
	After(time.Duration, func())
	Every(time.Duration, func())
	EveryAlign(time.Duration, time.Duration, func())
	Stop()
	Close()
}

func (s *timingTimeScheduler) stop() {
	if s.timer != nil {
		s.timer.Stop()
		s.timer = nil
	}
	if s.ticker != nil {
		s.ticker.Stop()
		s.ticker = nil
	}
	if s.quitter != nil {
		close(s.quitter)
		s.quitter = nil
	}
}

// After implements the schedulerImpl interface.
func (s *timingTimeScheduler) After(delay time.Duration, f func()) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stop()
	s.timer = time.AfterFunc(delay, f)
}

// At implements the schedulerImpl interface.
func (s *timingTimeScheduler) At(when time.Time, f func()) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stop()
	s.timer = time.AfterFunc(when.Sub(time.Now()), f)
}

// Close implements the schedulerImpl interface.
func (s *timingTimeScheduler) Close() {
	s.Stop()
}

// Stop implements the schedulerImpl interface.
func (s *timingTimeScheduler) Stop() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stop()
}

// Every implements the schedulerImpl interface.
func (s *timingTimeScheduler) Every(interval time.Duration, f func()) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stop()
	s.quitter = make(chan struct{})
	s.ticker = time.NewTicker(interval)
	go func() {
		s.mu.Lock()
		ticker := s.ticker
		quitter := s.quitter
		s.mu.Unlock()
		if ticker == nil || quitter == nil {
			// Scheduler stopped before goroutine was started.
			return
		}
		for {
			select {
			case <-ticker.C:
				f()
			case <-quitter:
				return
			}
		}
	}()
}

// timingNextAlignedExpiration calculates the next expiration time.
// The expiration is first rounded to interval granularity, and then offset is added.
// For example, given interval 1h, and offset 15m, the expirations will happen at
// :15 of every hour regardless of the initial time.
func timingNextAlignedExpiration(initial time.Time, interval time.Duration, offset time.Duration) time.Time {
	next := initial.Truncate(interval).Add(offset)
	if !next.After(initial) {
		next = next.Add(interval)
	}
	if !next.After(initial) {
		panic("nextAlignedExpiration: bug: !next.After(initial)")
	}
	return next
}

// EveryAlign implements the schedulerImpl interface.
func (s *timingTimeScheduler) EveryAlign(interval time.Duration, offset time.Duration, f func()) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stop()
	quitter := make(chan struct{})
	s.quitter = quitter
	go func() {
		var timer *time.Timer
		for {
			now := time.Now()
			next := timingNextAlignedExpiration(now, interval, offset)
			delay := next.Sub(now)
			if timer == nil {
				timer = time.NewTimer(delay)
				defer timer.Stop()
			} else {
				timer.Reset(delay)
			}
			select {
			case <-timer.C:
				f()
			case <-quitter:
				return
			}
		}
	}()
}

// Scheduler represents a trigger that can be repeating or one-off, and
// is intrinsically tied to the running bar. This means that if the trigger
// condition occurs while the bar is paused, it will not fire until the bar
// is next resumed, making it ideal for scheduling work that should only be
// performed while the bar is active.
type timingScheduler struct {
	// A channel that receives an empty struct for each tick of the scheduler.
	C <-chan struct{}

	notifyFn func()
	waiting  int32 // basically bool, but we need atomics.

	schedulerImpl timingSchedulerImpl
}

var _ timingSchedulerImpl = &timingTimeScheduler{}

// timingTimeScheduler is a scheduler backed by "time" package.
type timingTimeScheduler struct {
	mu      sync.Mutex
	timer   *time.Timer
	ticker  *time.Ticker
	quitter chan struct{}
}

func timingMyNewScheduler(impl timingSchedulerImpl) *timingScheduler {
	s := new(timingScheduler)
	s.schedulerImpl = impl
	s.notifyFn, s.C = notifierNew()
	return s
}

// NewScheduler creates a new scheduler.
//
// The scheduler is backed by "time" package. Its "At" implementation
// is unreliable, as it's unable to take system suspend and time adjustments
// into account.
func timingNewScheduler() *timingScheduler {
	return timingMyNewScheduler(&timingTimeScheduler{})
}

// Stop cancels all further triggers for the scheduler.
func (s *timingScheduler) Stop() {
	s.schedulerImpl.Stop()
}

func coreNewTimedSink(original barSink, refreshFn func()) *coreTimedSink {
	t := &coreTimedSink{
		barSink:         original,
		timingScheduler: timingNewScheduler(),
		refreshFn:       refreshFn,
	}
	go t.runLoop()
	return t
}

// runLoop is one iteration of the wrapped module. It starts the wrapped
// module, and multiplexes events, replay notifications, and module output.
// It returns when the underlying module is ready to be restarted (i.e. it
// was stopped and an eligible click event was received).
func (m *coreModule) runLoop(realSink barSink) {
	started := false
	finished := false
	var refreshFn func()
	if r, ok := m.original.(barRefresherModule); ok {
		refreshFn = r.Refresh
	}
	timedSink := coreNewTimedSink(realSink, refreshFn)
	outputCh := make(chan barOutput)
	innerSink := func(o barOutput) { outputCh <- o }
	doneCh := make(chan struct{})

	go func(m barModule, innerSink barSink, doneCh chan<- struct{}) {
		m.Stream(innerSink)
		doneCh <- struct{}{}
	}(m.original, innerSink, doneCh)

	var out barOutput
	for {
		select {
		case out = <-outputCh:
			started = true
			timedSink.Output(out, true)
		case <-doneCh:
			finished = true
			timedSink.Stop()
			out = coreToSegments(out)
			timedSink.Output(coreAddRestartHandlers(out, m.restartFn), false)
		case <-m.replayCh:
			if started {
				timedSink.Output(out, true)
			}
		case <-m.restartCh:
			if finished {
				timedSink.Output(out, false)
				return // Stream will restart the run loop.
			}
		}
	}
}

// Stream runs the module with the given sink, automatically handling
// terminations/restarts of the wrapped module.
func (m *coreModule) Stream(sink barSink) {
	for {
		m.runLoop(sink)
	}
}

// coreModuleSet is a group of modules. It provides a channel for identifying module
// updates, and methods to get the last output of the set or a specific module.
type coreModuleSet struct {
	modules   []*coreModule
	updateCh  chan int
	outputs   []barSegments
	outputsMu sync.RWMutex
}

// Func creates a barSink that sends sends output in the form of barSegments.
func sinkFunc(s func(barSegments)) barSink {
	return func(o barOutput) {
		if o == nil {
			s(nil)
			return
		}
		s(o.barSegments())
	}
}

func (m *coreModuleSet) sinkFn(idx int) barSink {
	return sinkFunc(func(out barSegments) {
		m.outputsMu.Lock()
		m.outputs[idx] = out
		m.outputsMu.Unlock()
		m.updateCh <- idx
	})
}

// Stream starts streaming all modules and returns a channel that receives the
// index of the module any time one updates with new output.
func (m *coreModuleSet) Stream() <-chan int {
	for i, mod := range m.modules {
		go mod.Stream(m.sinkFn(i))
	}
	return m.updateCh
}

func notifierNotify(ch chan<- struct{}) {
	select {
	case ch <- struct{}{}:
	default:
	}
}

// New constructs a new notifier. It returns a func that triggers a notification,
// and a <-chan that consumes these notifications.
func notifierNew() (func(), <-chan struct{}) {
	ch := make(chan struct{}, 1)
	return func() { notifierNotify(ch) }, ch
}

// coreNewModule wraps an existing bar.Module with core barista functionality,
// such as restarts and the ability to replay the last output.
func coreNewModule(original barModule) *coreModule {
	m := &coreModule{original: original}
	m.replayFn, m.replayCh = notifierNew()
	m.restartFn, m.restartCh = notifierNew()
	return m
}

// barSegments implements Output for []*barSegment.
type barSegments []*barSegment

// coreNewModuleSet creates a ModuleSet with the given modules.
func coreNewModuleSet(modules []barModule) *coreModuleSet {
	set := &coreModuleSet{
		modules:  make([]*coreModule, len(modules)),
		outputs:  make([]barSegments, len(modules)),
		updateCh: make(chan int),
	}
	for i, m := range modules {
		set.modules[i] = coreNewModule(m)
	}
	return set
}

type barButton int

const (
	buttonLeft barButton = iota + 1
	buttonMiddle
	buttonRight
	scrollUp
	scrollDown
	scrollLeft
	scrollRight
	buttonBack
	buttonForward
)

/*
barEvent represents a mouse event meant for a single module.

Note: As before, name is not included because it's only required to determine
which module will handle an event from i3. Once the bar receives the event,
it provides only the information in this struct to individual modules.

The SegmentID is set to the Identifier of the output segment clicked, so
it can be used to filter events for a module with multiple output segments.

X, Y describe event co-ordinates relative to the output segment, and
Width, Height are set to the size of the output segment.

ScreenX, ScreenY are the event co-ordinates relative to the root window.
*/
type barEvent struct {
	Button  barButton `json:"button"`
	X       int       `json:"relative_x,omitempty"`
	Y       int       `json:"relative_y,omitempty"`
	Width   int       `json:"width,omitempty"`
	Height  int       `json:"height,omitempty"`
	ScreenX int       `json:"x,omitempty"`
	ScreenY int       `json:"y,omitempty"`
}

// barTextAlignment defines the alignment of text within a block.
// Using barTextAlignment rather than string opens up the possibility of i18n without
// requiring each module to know the current locale.
type barTextAlignment string

const (
	alignStart  = barTextAlignment("left")
	alignCenter = barTextAlignment("center")
	alignEnd    = barTextAlignment("right")
)

/*
barSegment is a single "block" of output that conforms to the i3bar protocol.
See https://i3wm.org/docs/i3bar-protocol.html#_blocks_in_detail for details.

Note: Name is not included because only the bar needs to know the name in
order to dispatch click events and maintain the output cache. Multiple segments
can still use the identifier to map click events to output segments.
The bar will map the unmodified identifier to i3bar's "instance", and set the
value from the clicked segment as the SegmentID of the generated event.

See segment.go for supported methods. All fields are unexported to make sure
that when setting a field, the attrSet mask is also updated.
*/
type barSegment struct {
	// A bitmask of attributes that are set. Needed because the go default
	// for some attributes behave differently from unset values when sent to
	// i3bar. (e.g. the default separatorWidth is not 0).
	attrSet int
	onClick func(barEvent)

	text      string
	pango     bool
	shortText string
	err       error

	color      color.Color
	background color.Color
	border     color.Color

	// Minimum width can be specified as either a numeric pixel value
	// or a string placeholder value. The unexported field is interface{}
	// but there are two methods on Segment that set this, one for each type.
	minWidth interface{}

	align     barTextAlignment
	urgent    bool
	separator bool
	padding   int
}

// Output is an interface for displaying objects on the bar.
type barOutput interface {
	barSegments() []*barSegment
}

// barSink represents a destination for module output.
type barSink func(barOutput)

// barModule represents a single bar module. A bar is just a list of modules.
type barModule interface {
	// Stream runs the main loop of a module, pushing updated outputs to
	// the provided Sink.
	// A module is considered active until Stream() returns, at which point
	// a click will restart the module by calling Stream() again.
	// The Sink passed to Stream is only valid for the one call to Stream;
	// subsequent calls may receive different instances.
	Stream(barSink)
}

var baristaInstance *baristaSwayBar
var baristaInstanceInit sync.Once

func baristaConstruct() {
	baristaInstanceInit.Do(func() {
		baristaInstance = &baristaSwayBar{
			update: make(chan struct{}, 1),
			events: make(chan baristaSwayEvent),
			reader: os.Stdin,
			writer: os.Stdout,
			// bar starts paused, will be resumed on Run().
			paused: true,
			// Default to swaynag when right-clicking errors.
			errorHandler: func(e barErrorEvent) {
				exec.Command("swaynag", "-m", e.Error.Error()).Run()
			},
		}
	})
}

// baristaRun sets up all the streams and enters the main loop.
// If any modules are provided, they are added to the bar now.
func baristaRun(modules ...barModule) error {
	baristaConstruct()
	// To allow TestMode to work, we need to avoid any references
	// to instance in the run loop.
	b := baristaInstance
	var signalChan chan os.Signal
	if !b.suppressSignals {
		// Set up signal handlers for USR1/2 to pause/resume supported modules.
		signalChan = make(chan os.Signal, 2)
		signal.Notify(signalChan, unix.SIGUSR1, unix.SIGUSR2)
	}

	b.modules = append(b.modules, modules...)
	b.moduleSet = coreNewModuleSet(b.modules)

	// Mark the bar as started.
	b.started = true

	go func(i <-chan int) {
		for range i {
			b.refresh()
		}
	}(b.moduleSet.Stream())

	errChan := make(chan error)
	// Read events from the input stream, pipe them to the events channel.
	go func(e chan<- error) {
		e <- b.readEvents()
	}(errChan)

	// Write header.
	header := baristaSwayHeader{
		Version:     1,
		ClickEvents: true,
	}

	if !b.suppressSignals {
		// Go doesn't allow us to handle the default SIGSTOP,
		// so we'll use SIGUSR1 and SIGUSR2 for pause/resume.
		header.StopSignal = int(unix.SIGUSR1)
		header.ContSignal = int(unix.SIGUSR2)
	}
	// Set up the encoder for the output stream,
	// so that module outputs can be written directly.
	b.encoder = json.NewEncoder(b.writer)
	if err := b.encoder.Encode(&header); err != nil {
		return err
	}
	// Start the infinite array.
	if _, err := io.WriteString(b.writer, "["); err != nil {
		return err
	}

	// Bar starts paused, so resume it to get the initial output.
	b.resume()

	// Infinite arrays on both sides.
	for {
		select {
		case <-b.update:
			// The complete bar needs to printed on each update.
			if err := b.print(); err != nil {
				return err
			}
		case event := <-b.events:
			if onClick, ok := b.clickHandlers[event.Name]; ok {
				go onClick(event.barEvent)
			}
		case sig := <-signalChan:
			switch sig {
			case unix.SIGUSR1:
				b.pause()
			case unix.SIGUSR2:
				b.resume()
			}
		case err := <-errChan:
			return err
		}
	}
}

type simpleClockModule struct {
	format   string
	interval time.Duration
}

// barTextSegment creates a new output segment with text content.
func barTextSegment(text string) *barSegment {
	return new(barSegment).Text(text)
}

// Text sets the text content of this segment. It clears any previous
// content and resets the markup style.
func (s *barSegment) Text(content string) *barSegment {
	s.text = content
	s.pango = false
	return s
}

func (s simpleClockModule) Stream(sink barSink) {
	sch := timingNewScheduler()
	for {
		now := time.Now()
		sink.Output(barTextSegment(now.Format(s.format)))
		next := now.Add(s.interval).Truncate(s.interval)
		sch.At(next).Tick()
	}
}

// wlanInfo represents the wireless card status.
type wlanInfo struct {
	Name           string
	IPs            []net.IP
	SSID           string
	AccessPointMAC string
}

// notifierSource can be used to notify multiple listeners of a signal. It provides both
// one-shot listeners that close the channel on the next signal, and continuous
// listeners that emit a struct{} on every signal (but need to be cleaned up).
type notifierSource struct {
	obs  []chan struct{}
	subs map[<-chan struct{}]func()
	mu   sync.Mutex
}

// valueValue provides atomic value storage with update notifications.
type valueValue struct {
	value  atomic.Value
	source notifierSource
}

// Module represents a wlan bar module.
type wlanModule struct {
	intf       string
	outputFunc valueValue // of func(Info) bar.Output
}

// To allow storing different concrete types in the atomic.Value, for example
// when the value just needs to store an interface.
type valueBox struct {
	value interface{}
}

// Notify notifies all interested listeners.
func (s *notifierSource) Notify() {
	s.mu.Lock()
	defer s.mu.Unlock()
	for _, o := range s.obs {
		close(o)
	}
	s.obs = nil
	for _, f := range s.subs {
		f()
	}
}

// Get returns the currently stored value.
func (v *valueValue) Get() interface{} {
	if b, ok := v.value.Load().(box); ok {
		return b.value
	}
	return nil
}

// Set updates the stored values and notifies any subscribers.
func (v *valueValue) Set(value interface{}) {
	v.value.Store(valueBox{value})
	v.source.Notify()
}

// Output configures a module to display the output of a user-defined function.
func (m *wlanModule) Output(outputFunc func(wlanInfo) barOutput) *wlanModule {
	m.outputFunc.Set(outputFunc)
	return m
}

//Text constructs a simple text output from the given string.
func outputsText(text string) *barSegment {
	return barTextSegment(text)
}

// wlanNamed constructs an instance of the wlan module for the specified interface.
func wlanNamed(iface string) *wlanModule {
	m := &wlanModule{intf: iface}
	// Default output is just the SSID when connected.
	m.Output(func(i wlanInfo) barOutput {
		return outputsText(i.SSID)
	})
	return m
}

// wlanAny constructs an instance of the wlan module that uses any available
// wireless interface, choosing the 'best' state from all available.
func wlanAny() *wlanModule {
	return wlanNamed("")
}

// Subscribe returns a channel that will receive an empty struct{} on the next
// signal, and a func to close the subscription.
func (s *notifierSource) Subscribe() (sub <-chan struct{}, done func()) {
	fn, sub := notifierNew()
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.subs == nil {
		s.subs = map[<-chan struct{}]func(){}
	}
	s.subs[sub] = fn
	return sub, func() {
		s.mu.Lock()
		defer s.mu.Unlock()
		delete(s.subs, sub)
	}
}

// Subscribe returns a channel that will receive an empty struct{} on each value
// change until it's cleaned up using the done func.
func (v *valueValue) Subscribe() (sub <-chan struct{}, done func()) {
	return v.source.Subscribe()
}

func wlanFillWifiInfo(info *wlanInfo) {
	ssid, err := iwgetid(info.Name, "-r")
	if err != nil {
		return
	}
	info.SSID = ssid
	info.AccessPointMAC, _ = iwgetid(info.Name, "-a")
	ch, _ := iwgetid(info.Name, "-c")
	info.Channel, _ = strconv.Atoi(ch)
	freq, _ := iwgetid(info.Name, "-f")
	freqFloat, _ := strconv.ParseFloat(freq, 64)
	info.Frequency = unit.Frequency(freqFloat) * unit.Hertz
}


func wlanHandleUpdate(link netlink.Link) Info {
	info := wlanInfo{
		Name:  link.Name,
		IPs:   link.IPs,
	}
	wlanFillWifiInfo(&info)
	return info
}

// Stream starts the module.
func (m *wlanModule) Stream(s barSink) {
	outputFunc := m.outputFunc.Get().(func(wlanInfo) barOutput)
	nextOutputFunc, done := m.outputFunc.Subscribe()
	defer done()

	var linkSub *netlink.Subscription
	if m.intf == "" {
		linkSub = netlink.WithPrefix("wl")
	} else {
		linkSub = netlink.ByName(m.intf)
	}
	defer linkSub.Unsubscribe()

	info := wlanHandleUpdate(linkSub.Get())
	for {
		s.Output(outputFunc(info))
		select {
		case <-linkSub.C:
			info = wlanHandleUpdate(linkSub.Get())
		case <-nextOutputFunc:
			outputFunc = m.outputFunc.Get().(func(wlanInfo) barOutput)
		}
	}
}

var wifi = wlanAny().Output(func(w wlanInfo) barOutput {
	out := fmt.Sprintf("W: (%s)", w.SSID)
	if len(w.IPs) > 0 {
		out += fmt.Sprintf(" %s", w.IPs[0])
	}
	return outputsText(out)
})

func main() {

	panic(baristaRun(wifi, simpleClockModule{"Mon 2 Jan 15:04:05 MST", time.Second}))
}
