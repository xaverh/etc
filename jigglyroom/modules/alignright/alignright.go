package alignright

import "../../lemonbar"

// AlignRight is a dummy module, that lets the user define, where the bar changes its alignment to the right
type AlignRight struct {
	Output chan string
}

func (alignRight AlignRight) PrintToChannel() {
	alignRight.Output <- lemonbar.AlignRight
}

func (alignRight AlignRight) GetOutputChannel() chan string {
	return alignRight.Output
}
