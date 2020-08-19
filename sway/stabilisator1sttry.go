package main

import (
	"encoding/json"
	"fmt"
	"image/color"
	"io"
	"os"
	"os/signal"
	"strconv"
	"sync"
	"syscall"
	"time"

	"golang.org/x/sys/unix"
)

// barSegments implements Output for []*barSegment.
type barSegments []*barSegment

// moduleSet is a group of modules. It provides a channel for identifying module
// updates, and methods to get the last output of the set or a specific module.
type moduleSet struct {
	modules   []*module
	updateCh  chan int
	outputs   []barSegments
	outputsMu sync.RWMutex
}

// Module represents a bar.Module wrapped with core barista functionality.
// It is used as a building block for the main bar, modules that manipulate
// other modules (group, reformat), and for writing tests.
// It handles restarting the wrapped module on a left/right/middle click,
// as well as providing an option to "replay" the last output from the module.
// It also provides timed output functionality.
type module struct {
	original  barModule
	replayCh  <-chan struct{}
	replayFn  func()
	restartCh <-chan struct{}
	restartFn func()
}

// module represents a single bar module. A bar is just a list of modules.
type barModule interface {
	// stream runs the main loop of a module, pushing updated outputs to the provided sink. A module is considered active until stream() returns, at which point a click will restart the module by calling stream() again. The sink passed to stream is only valid for the one call to stream; subsequent calls may receive different instances.
	stream(barSink)
}

// runLoop is one iteration of the wrapped module. It starts the wrapped
// module, and multiplexes events, replay notifications, and module output.
// It returns when the underlying module is ready to be restarted (i.e. it
// was stopped and an eligible click event was received).
func (m *module) runLoop(realSink barSink) {
	started := false
	finished := false
	var refreshFn func()
	if r, ok := m.original.(bar.RefresherModule); ok {
		refreshFn = r.Refresh
	}
	timedSink := newTimedSink(realSink, refreshFn)
	outputCh := make(chan output)
	innerSink := func(o output) { outputCh <- o }
	doneCh := make(chan struct{})

	go func(m barModule, innerSink barSink, doneCh chan<- struct{}) {
		m.stream(innerSink)
		doneCh <- struct{}{}
	}(m.original, innerSink, doneCh)

	var out output
	for {
		select {
		case out = <-outputCh:
			started = true
			timedSink.Output(out, true)
		case <-doneCh:
			finished = true
			timedSink.Stop()
			out = toSegments(out)
			timedSink.Output(addRestartHandlers(out, m.restartFn), false)
		case <-m.replayCh:
			if started {
				timedSink.Output(out, true)
			}
		case <-m.restartCh:
			if finished {
				timedSink.Output(stripErrors(out, l.ID(m)), false)
				return // Stream will restart the run loop.
			}
		}
	}
}

// Stream runs the module with the given sink, automatically handling
// terminations/restarts of the wrapped module.
func (m *module) stream(sink barSink) {
	for {
		m.runLoop(sink)
	}
}

// stream starts streaming all modules and returns a channel that receives the
// index of the module any time one updates with new output.
func (m *moduleSet) stream() <-chan int {
	for i, mod := range m.modules {
		go mod.stream(m.sinkFn(i))
	}
	return m.updateCh
}

// Output is an interface for displaying objects on the bar.
type output interface {
	barSegments() []*barSegment
}

// Sink represents a destination for module output.
type barSink func(output)

// function creates a bar.Sink that sends sends output in the form of Segments.
func function(s func(barSegments)) barSink {
	return func(o output) {
		if o == nil {
			s(nil)
			return
		}
		s(o.barSegments())
	}
}

func (m *moduleSet) sinkFn(idx int) barSink {
	return function(func(out barSegments) {
		m.outputsMu.Lock()
		m.outputs[idx] = out
		m.outputsMu.Unlock()
		m.updateCh <- idx
	})
}

// swayBar is the "bar" instance that handles events and streams output.
type swayBar struct {
	sync.Mutex
	// The list of modules that make up this bar.
	modules   []module
	moduleSet *moduleSet
	// A map of previously set click handlers for each segment.
	clickHandlers map[string]func(barEvent)
	// The channel that receives a signal on module updates.
	update chan struct{}
	// The channel that aggregates all events from sway.
	events chan swayEvent
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
}

// barSegment is a single "block" of output that conforms to the swaybar protocol.
// See https://i3wm.org/docs/i3bar-protocol.html#_blocks_in_detail for details.
// Note: Name is not included because only the bar needs to know the name in
// order to dispatch click events and maintain the output cache. Multiple segments
// can still use the identifier to map click events to output segments.
// The bar will map the unmodified identifier to swaybar's "instance", and set the
// value from the clicked segment as the SegmentID of the generated event.
type barSegment struct {
	onClick func(barEvent)

	text      string
	pango     bool
	shortText string
	err       error

	color      color.RGBA64
	background color.RGBA64
	border     color.RGBA64

	// Minimum width can be specified as either a numeric pixel value
	// or a string placeholder value. The unexported field is interface{}
	// but there are two methods on barSegment that set this, one for each type.
	minWidth interface{}

	align     string // "left", "center" or "right"
	urgent    bool
	separator bool
	padding   int
}

var instance *swayBar

var instanceInit sync.Once

/*
// pangoSegment creates a new output segment with content that uses pango
// markup for formatting. Not all features may be supported.
// See https://developer.gnome.org/pango/stable/PangoMarkupFormat.html.
func pangoSegment(text string) *barSegment {
	return new(barSegment).barPango(text)
}

// Text sets the text content of this segment. It clears any previous
// content and resets the markup style.
func (s *barSegment) barText(content string) *barSegment {
	s.text = content
	s.pango = false
	return s
}

// Pango sets the pango content of this segment. It clears any previous
// content and sets the markup style to pango.
func (s *barSegment) barPango(content string) *barSegment {
	s.text = content
	s.pango = true
	return s
}

// barContent returns the text content of the segment, and whether or not
// it is using pango markup.
func (s *barSegment) barContent() (text string, isPango bool) {
	return s.text, s.pango
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

// HasClick returns whether this segment has a click handler defined.
// Modules can use this check to assign default handlers to segments
// where the user has not already assigned a click handler.
func (s *barSegment) HasClick() bool {
	return s.onClick != nil
}

// Click calls a previously set click handler with the given Event.
func (s *barSegment) Click(e barEvent) {
	if s.onClick != nil {
		s.onClick(e)
	}
}

// Segments implements bar.Output for a single Segment.
func (s *barSegment) Segments() []*barSegment {
	return []*barSegment{s}
}

// Clone makes a copy of the Segment that can be modified
// without the changes being reflected in the original.
func (s *barSegment) Clone() *barSegment {
	copied := &barSegment{}
	*copied = *s
	return copied
}

// Segments returns the list of segments as a bar.Output.
func (s Segments) Segments() []*barSegment {
	return s
}

// swayHeader is sent at the beginning of output.
type swayHeader struct {
	Version     int  `json:"version"`
	StopSignal  int  `json:"stop_signal,omitempty"`
	ContSignal  int  `json:"cont_signal,omitempty"`
	ClickEvents bool `json:"click_events"`
}

// Len returns the number of modules in this ModuleSet.
func (m *moduleSet) Len() int {
	return len(m.modules)
}

// LastOutput returns the last output from the module at a specific position.
// If the module has not yet updated, an empty output will be used.
func (m *moduleSet) LastOutput(idx int) bar.Segments {
	m.outputsMu.RLock()
	defer m.outputsMu.RUnlock()
	return m.outputs[idx]
}

// LastOutputs returns the last output from all modules in order. The returned
// slice will have exactly Len() elements, and if a module has not yet updated
// an empty output will be placed in its position.
func (m *moduleSet) LastOutputs() []bar.Segments {
	m.outputsMu.RLock()
	defer m.outputsMu.RUnlock()
	cp := make([]bar.Segments, len(m.outputs))
	copy(cp, m.outputs)
	return cp
}

// newScheduler creates a new scheduler.
// The scheduler is backed by "time" package. Its "At" implementation
// is unreliable, as it's unable to take system suspend and time adjustments
// into account.
func newScheduler() *Scheduler {
	if testModeScheduler := maybeNewTestModeScheduler(); testModeScheduler != nil {
		return newScheduler(testModeScheduler)
	}
	return newScheduler(&timeScheduler{})
}

// swayEvent instances are received from swaybar on stdin.
type swayEvent struct {
	barEvent
	Name string `json:"name"`
}

// colorString returns the hex "html" representation of the color, as in #ff0080.
func colorString(col color.RGBA64) string {
	// Add 0.5 for rounding
	return fmt.Sprintf("#%02x%02x%02x", uint8(col.R*255.0), uint8(col.G*255.0), uint8(col.B*255.0))
}

// readEvents parses the infinite stream of events received from sway.
func (b *swayBar) readEvents() {
	decoder := json.NewDecoder(b.reader)
	// Consume opening '['
	_, err := decoder.Token()
	if err != nil {
		return
	}
	for decoder.More() {
		var event swayEvent
		err = decoder.Decode(&event)
		if err != nil {
			return
		}
		b.events <- event
	}
	return
}

// maybeUpdate signals the update channel unless already signalled.
func (b *swayBar) maybeUpdate() {
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

*/

// swayMap serialises the attributes of the Segment in
// the format used by swaybar.
func swayMap(s *barSegment) map[string]interface{} {
	swayMap := make(map[string]interface{})
	txt, pango := s.barContent()
	swayMap["full_text"] = txt
	swayMap["short_text"] = s.shortText
	swayMap["color"] = s.color
	swayMap["background"] = s.background
	swayMap["border"] = s.border
	swayMap["min_width"] = s.minWidth
	swayMap["align"] = s.align
	swayMap["urgent"] = s.urgent
	swayMap["separator"] = s.separator
	swayMap["separator_block_width"] = s.padding
	if pango {
		swayMap["markup"] = "pango"
	} else {
		swayMap["markup"] = "none"
	}
	return swayMap
}

type mouseButton int

const (
	mouseButtonLeft mouseButton = iota + 1
	buttonMiddle
	buttonRight
	scrollUp
	scrollDown
	scrollLeft
	scrollRight
	buttonBack
	buttonForward
)

// barEvent represents a mouse event meant for a single module.
// Note: As before, name is not included because it's only required to determine which module will handle an event from i3. Once the bar receives the event, it provides only the information in this struct to individual modules. The SegmentID is set to the Identifier of the output segment clicked, so it can be used to filter events for a module with multiple output segments. X, Y describe event co-ordinates relative to the output segment, and Width, Height are set to the size of the output segment. ScreenX, ScreenY are the event co-ordinates relative to the root window.
type barEvent struct {
	Button  mouseButton `json:"button"`
	X       int         `json:"relative_x,omitempty"`
	Y       int         `json:"relative_y,omitempty"`
	Width   int         `json:"width,omitempty"`
	Height  int         `json:"height,omitempty"`
	ScreenX int         `json:"x,omitempty"`
	ScreenY int         `json:"y,omitempty"`
}

// sink represents a destination for module output.
type sink func(output)

// Output updates the module's output on the bar.
func (s sink) Output(o output) {
	s(o)
}

// swayEvent instances are received from swaybar on stdin.
type swayEvent struct {
	barEvent
	Name string `json:"name"`
}

// maybeUpdate signals the update channel unless already signalled.
func (b *swayBar) maybeUpdate() {
	select {
	case b.update <- struct{}{}:
	default:
		// Since b.update has a buffer of 1, a failure to send to it implies that an update is already queued. Since refresh is only be called after individual modules' lastOutput is set, when the previous update is consumed, each module will already have the latest output.
	}
}

var (
	// A set of channels to be closed by resume. This allows schedulers to wait for timingResume, without requiring a reference to each created scheduler.
	timingWaiters []chan struct{}
	timingPaused  = false
	timingMutex   sync.Mutex
)

// timingPause timing.
func timingPause() {
	timingMutex.Lock()
	defer timingMutex.Unlock()
	timingPaused = true
}

// pause instructs all pausable modules to suspend processing.
func (b *swayBar) pause() {
	b.Lock()
	defer b.Unlock()
	if b.paused {
		return
	}
	b.paused = true
	timingPause()
}

// refresh requests an update of the bar's output.
func (b *swayBar) refresh() {
	b.Lock()
	defer b.Unlock()
	// If paused, defer the refresh until the bar resumes.
	if b.paused {
		b.refreshOnResume = true
		return
	}
	b.maybeUpdate()
}

// Resume timing.
func timingResume() {
	timingMutex.Lock()
	defer timingMutex.Unlock()
	timingPaused = false
	for _, ch := range timingWaiters {
		close(ch)
	}
	timingWaiters = nil
}

// resume instructs all pausable modules to continue processing.
func (b *swayBar) resume() {
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

// print outputs the entire bar, using the last output for each module.
func (b *swayBar) print() {
	// Store the set of click handlers for any segments that can handle clicks. When swaybar sends us the click event, it will include an identifier that we can use to look up the function to call.
	b.clickHandlers = map[string]func(barEvent){}
	// i3bar requires the entire bar to be printed at once, so we just take the
	// last cached value for each module and construct the current bar.
	output := make([]map[string]interface{}, 0)
	for _, segments := range b.moduleSet.LastOutputs() {
		for _, segment := range segments {
			out := swayMap(segment)
			var clickHandler func(barEvent)
			if err := segment.GetError(); err != nil {
				// because go.
				segment := segment
				clickHandler = func(e barEvent) {
					segment.Click(e)
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
		return
	}
	io.WriteString(b.writer, ",\n")
}

// newScheduler creates a new scheduler.
//
// The scheduler is backed by "time" package. Its "At" implementation
// is unreliable, as it's unable to take system suspend and time adjustments
// into account.
func newScheduler() *Scheduler {
	if testModeScheduler := maybeNewTestModeScheduler(); testModeScheduler != nil {
		return newScheduler(testModeScheduler)
	}
	return newScheduler(&timeScheduler{})
}

// newModuleSet creates a ModuleSet with the given modules.
func newModuleSet(modules []bar.Module) *moduleSet {
	set := &moduleSet{
		modules:  make([]*module, len(modules)),
		outputs:  make([]bar.Segments, len(modules)),
		updateCh: make(chan int),
	}
	for i, m := range modules {
		set.modules[i] = NewModule(m)
	}
	return set
}

// textSegment creates a new output segment with text content.
func textSegment(text string) *barSegment {
	return new(barSegment).barText(text)
}

type simpleClockModule struct {
	format   string
	interval time.Duration
}

func (s simpleClockModule) stream(sink sink) {
	sch := newScheduler()
	for {
		sink.Output(textSegment(time.Now().Format(s.format)))
		next := time.Now().Add(s.interval).Truncate(s.interval)
		sch.At(next).Tick()
	}
}

type diskSpaceModule string

func (d diskSpaceModule) stream(sink sink) {
	sch := newScheduler().Every(5 * time.Second)
	for {
		var stat syscall.Statfs_t
		err := syscall.Statfs(string(d), &stat)
		if err != nil {
			return
		}
		sink.Output(textSegment(string(stat.Bavail * uint64(stat.Bsize))))
		sch.Tick()
	}
}

// run sets up all the streams and enters the main loop. If any modules are provided, they are added to the bar now. This allows both styles of bar construction: `bar.Add(a); bar.Add(b); run()`, and `run(a, b)`.
func run(modules ...module) {
	instanceInit.Do(func() {
		instance = &swayBar{
			update: make(chan struct{}, 1),
			events: make(chan swayEvent),
			reader: os.Stdin,
			writer: os.Stdout,
			// bar starts paused, will be resumed on run().
			paused: true,
		}
	})
	var signalChan chan os.Signal
	if !instance.suppressSignals {
		// Set up signal handlers for USR1/2 to pause/resume supported modules.
		signalChan = make(chan os.Signal, 2)
		signal.Notify(signalChan, unix.SIGUSR1, unix.SIGUSR2)
	}

	// interessantes Pattern, warum ???
	instance.modules = append(instance.modules, modules...)
	instance.moduleSet = newModuleSet(instance.modules)

	instance.started = true

	go func(i <-chan int) {
		for range i {
			instance.refresh()
		}
	}(instance.moduleSet.stream())

	type swayHeader struct {
		Version     int  `json:"version"`
		StopSignal  int  `json:"stop_signal,omitempty"`
		ContSignal  int  `json:"cont_signal,omitempty"`
		ClickEvents bool `json:"click_events"`
	}

	header := swayHeader{
		Version:     1,
		ClickEvents: true,
	}

	if !instance.suppressSignals {
		// Go doesn't allow us to handle the default SIGSTOP, so we'll use SIGUSR1 and SIGUSR2 for pause/resume.
		header.StopSignal = int(unix.SIGUSR1)
		header.ContSignal = int(unix.SIGUSR2)
	}
	// Set up the encoder for the output stream,
	// so that module outputs can be written directly.
	instance.encoder = json.NewEncoder(instance.writer)
	instance.encoder.Encode(&header)
	// Start the infinite array.
	io.WriteString(instance.writer, "[")

	// Bar starts paused, so resume it to get the initial output.
	instance.resume()

	// Infinite arrays on both sides.
	for {
		select {
		case <-instance.update:
			instance.print()
		case event := <-instance.events:
			if onClick, ok := instance.clickHandlers[event.Name]; ok {
				go onClick(event.barEvent)
			}
		case sig := <-signalChan:
			switch sig {
			case unix.SIGUSR1:
				instance.pause()
			case unix.SIGUSR2:
				instance.resume()
			}
		}
	}
}

func main() {
	run(diskSpaceModule("/home"), simpleClockModule{"Mon Jan 02", time.Hour}, simpleClockModule{"15:04:05", time.Second})
}
