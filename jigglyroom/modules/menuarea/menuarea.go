package menuarea

import "../../lemonbar"

// MenuArea is a dummy module, that lets the user define, where the menu can be accessed
type MenuArea struct {
	Output chan string
}

func (menuarea MenuArea) PrintToChannel() {
	menuarea.Output <- lemonbar.MakeClickable("%%", `9menu_run -font -sgi-screen-medium-r-normal--14-140-72-72-m-70-iso8859-1 -fg '#e5e6e6' -bg '#1e1e1e' `, lemonbar.MouseLeft)
}

func (menuarea MenuArea) GetOutputChannel() chan string {
	return menuarea.Output
}
