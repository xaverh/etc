package dateandtime

import (
	"time"

	"../../config"
)

// DateAndTime gives the current time. Format is defined
// through reference time Mon Jan 2 15:04:05 MST 2006 =
// 01/02 03:04:05PM '06 -0700 = Unix time 1136239445
type DateAndTime struct {
	Format string
	Output chan string
}

func (dateAndTime DateAndTime) PrintToChannel() {
	for {
		dateAndTime.Output <- time.Now().Local().Format(config.DateAndTimeFormat)
		// sleep until beginning of next second
		var now = time.Now()
		time.Sleep(now.Truncate(time.Second).Add(time.Second).Sub(now))
	}
}

func (dateAndTime DateAndTime) GetOutputChannel() chan string {
	return dateAndTime.Output
}
