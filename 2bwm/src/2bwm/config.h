///---User configurable stuff---///
#define MAINFONT "-sgi-screen-medium-r-normal--14-140-72-72-m-70-iso8859-1"
#define XFT_MAINFONT "SGI Screen:pixelsize=14"
#define NORM_BG_COLOR "#1E1E1E"
#define NORM_FG_COLOR "#E5E6E6"
#define NORM_BORDER_COLOR "#1E1E1E"
#define SEL_FG_COLOR "#1E1E1E"
#define SEL_BG_COLOR "#E5E6E6"
#define SEL_BORDER_COLOR "#6DA741"

///---Modifiers---///
#define MOD             XCB_MOD_MASK_4       /* Super/Windows key  or check xmodmap(1) with -pm  defined in /usr/include/xcb/xproto.h */
///--Speed---///
/* Move this many pixels when moving or resizing with keyboard unless the window has hints saying otherwise.
 *0)move step slow   1)move step fast
 *2)mouse slow       3)mouse fast     */
static const uint16_t movements[] = {5,180,15,400};
/* resize by line like in mcwm -- jmbi */
static const bool     resize_by_line          = false;
/* the ratio used when resizing and keeping the aspect */
static const float    resize_keep_aspect_ratio= 1.03;
///---Offsets---///
/*0)offsetx          1)offsety
 *2)maxwidth         3)maxheight */
static const uint8_t offsets[] = {0,18,0,18};
///---Colors---///
/*0)focuscol         1)unfocuscol
 *2)fixedcol         3)unkilcol
 *4)fixedunkilcol    5)outerbordercol
 *6)emptycol         */
static const char *colors[] = {
                               "#cdd5d5",
                               "#82a7c7",
                               "#faf669",
                               "#b6fa69",
                               "#faad69",
                               "#87737b",
                               "#1E1E1E"};
/* if this is set to true the inner border and outer borders colors will be swapped */
static const bool inverted_colors = true;
///---Cursor---///
/* default position of the cursor:
 * correct values are:
 * TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_RIGHT, MIDDLE
 * All these are relative to the current window. */
#define CURSOR_POSITION BOTTOM_RIGHT
///---Borders---///
/*0) Outer border size. If you put this negative it will be a square.
 *1) Full borderwidth    2) Magnet border size
 *3) Resize border size  */
static const uint8_t borders[] = {3, 7, 10, 7};
/* Windows that won't have a border.
 * It uses substring comparison with what is found in the WM_NAME
 * attribute of the window. You can test this using `xprop WM_NAME`
 */
#define LOOK_INTO "WM_NAME"
static const char *ignore_names[] = {"lemonbar"};
///--Menus and Programs---///
static const char *termcmd[]   = { "urxvtc", NULL };
static const char *hiddenmenu[]   = { "9menu_hidden", "-font", MAINFONT, "-bg", NORM_BG_COLOR, "-fg", NORM_FG_COLOR, NULL };
static const char* menucmd[] = {"9menu_run",   "-font", MAINFONT,      "-bg",
				NORM_BG_COLOR, "-fg",   NORM_FG_COLOR, NULL};
static const char* dmenucmd[] = {"dmenu_recent", "-m",          "0",     "-fn",         XFT_MAINFONT, "-nb",          NORM_BG_COLOR, "-nf",        NORM_FG_COLOR, "-sb", SEL_BG_COLOR,   "-sf",         SEL_FG_COLOR, NULL}; ///--Custom foo---///

static const char* musicplaypause[] = {"strawberry", "-t", NULL};
static const char* musicnext[] = {"strawberry", "-f", NULL};
static const char* musicprev[] = {"strawberry", "-r", NULL};
static const char* mutecmd[] = {"pamixer", "-m", NULL};
static const char* raisevolumecmd[] = {"sh", "-c", "pamixer -u ; pamixer -i 5", NULL};
static const char* lowervolumecmd[] = {"sh", "-c", "pamixer -u ; pamixer -d 5", NULL};

static void halfandcentered(const Arg *arg)
{
	Arg arg2 = {.i=TWOBWM_MAXHALF_VERTICAL_LEFT};
	maxhalf(&arg2);
	Arg arg3 = {.i=TWOBWM_TELEPORT_CENTER};
	teleport(&arg3);
}

static void focus_and_raise(const Arg* arg)
{
    focusnext(arg);
    raise_current_window();
}

///---Shortcuts---///

#define DESKTOPCHANGE(K,N) \
{  MOD ,             K,              changeworkspace, {.i=N}}, \
{  MOD |SHIFT,       K,              sendtoworkspace, {.i=N}},
static key keys[] = {
    /* modifier           key            function           argument */
    // Focus to next/previous window
    {  MOD ,              XK_Tab,        focus_and_raise,   {.i=TWOBWM_FOCUS_NEXT}},
    {  MOD |SHIFT,        XK_Tab,        focus_and_raise,   {.i=TWOBWM_FOCUS_PREVIOUS}},
    // Kill a window
    {  MOD ,              XK_q,          deletewin,         {}},
    // Resize a window
    {  MOD |SHIFT,        XK_k,          resizestep,        {.i=TWOBWM_RESIZE_UP}},
    {  MOD |SHIFT,        XK_j,          resizestep,        {.i=TWOBWM_RESIZE_DOWN}},
    {  MOD |SHIFT,        XK_l,          resizestep,        {.i=TWOBWM_RESIZE_RIGHT}},
    {  MOD |SHIFT,        XK_h,          resizestep,        {.i=TWOBWM_RESIZE_LEFT}},
    // Resize a window slower
    {  MOD |SHIFT|CONTROL,XK_k,          resizestep,        {.i=TWOBWM_RESIZE_UP_SLOW}},
    {  MOD |SHIFT|CONTROL,XK_j,          resizestep,        {.i=TWOBWM_RESIZE_DOWN_SLOW}},
    {  MOD |SHIFT|CONTROL,XK_l,          resizestep,        {.i=TWOBWM_RESIZE_RIGHT_SLOW}},
    {  MOD |SHIFT|CONTROL,XK_h,          resizestep,        {.i=TWOBWM_RESIZE_LEFT_SLOW}},
    // Move a window
    {  MOD ,              XK_k,          movestep,          {.i=TWOBWM_MOVE_UP}},
    {  MOD ,              XK_j,          movestep,          {.i=TWOBWM_MOVE_DOWN}},
    {  MOD ,              XK_l,          movestep,          {.i=TWOBWM_MOVE_RIGHT}},
    {  MOD ,              XK_h,          movestep,          {.i=TWOBWM_MOVE_LEFT}},
    // Move a window slower
    {  MOD |CONTROL,      XK_k,          movestep,          {.i=TWOBWM_MOVE_UP_SLOW}},
    {  MOD |CONTROL,      XK_j,          movestep,          {.i=TWOBWM_MOVE_DOWN_SLOW}},
    {  MOD |CONTROL,      XK_l,          movestep,          {.i=TWOBWM_MOVE_RIGHT_SLOW}},
    {  MOD |CONTROL,      XK_h,          movestep,          {.i=TWOBWM_MOVE_LEFT_SLOW}},
    // Teleport the window to an area of the screen.
    // Center:
    {  MOD ,              XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER}},
    // Center y:
    {  MOD |SHIFT,        XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER_Y}},
    // Center x:
    {  MOD |CONTROL,      XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER_X}},
    {  MOD |SHIFT|CONTROL,XK_g,          halfandcentered,   {.i=0}},
    // Top left:
    {  MOD ,              XK_y,          teleport,          {.i=TWOBWM_TELEPORT_TOP_LEFT}},
    // Top right:
    {  MOD ,              XK_u,          teleport,          {.i=TWOBWM_TELEPORT_TOP_RIGHT}},
    // Bottom left:
    {  MOD ,              XK_n,          teleport,          {.i=TWOBWM_TELEPORT_BOTTOM_LEFT}},
    // Bottom right:
    {  MOD ,              XK_m,          teleport,          {.i=TWOBWM_TELEPORT_BOTTOM_RIGHT}},
    // Resize while keeping the window aspect
    {  MOD ,              XK_equal,       resizestep_aspect, {.i=TWOBWM_RESIZE_KEEP_ASPECT_GROW}},
    {  MOD ,              XK_minus,        resizestep_aspect, {.i=TWOBWM_RESIZE_KEEP_ASPECT_SHRINK}},
    // Maximize (ignore offset and no EWMH atom)
    {  MOD ,              XK_f,          maximize,          {}},
    // Full screen (disregarding offsets and adding EWMH atom)
    {  MOD |SHIFT ,       XK_f,          fullscreen,        {}},
    // Maximize vertically
    {  MOD ,              XK_x,          maxvert_hor,       {.i=TWOBWM_MAXIMIZE_VERTICALLY}},
    // Maximize horizontally
    {  MOD |SHIFT,        XK_x,          maxvert_hor,       {.i=TWOBWM_MAXIMIZE_HORIZONTALLY}},
    // Maximize and move
    // vertically left
    {  MOD,                XK_Left,          maxhalf,           {.i=TWOBWM_MAXHALF_VERTICAL_LEFT}},
    // vertically right
    {  MOD,                XK_Right,          maxhalf,           {.i=TWOBWM_MAXHALF_VERTICAL_RIGHT}},
    // horizontally left
    {  MOD,                XK_Down,          maxhalf,           {.i=TWOBWM_MAXHALF_HORIZONTAL_BOTTOM}},
    // horizontally right
    {  MOD,                XK_Up,          maxhalf,           {.i=TWOBWM_MAXHALF_HORIZONTAL_TOP}},
    //fold half vertically
    {  MOD |SHIFT,        XK_Up,          maxhalf,           {.i=TWOBWM_MAXHALF_FOLD_VERTICAL}},
    //fold half horizontally
    {  MOD |SHIFT,        XK_Left,          maxhalf,           {.i=TWOBWM_MAXHALF_FOLD_HORIZONTAL}},
    //unfold vertically
    {  MOD |SHIFT,        XK_Down,          maxhalf,           {.i=TWOBWM_MAXHALF_UNFOLD_VERTICAL}},
    //unfold horizontally
    {  MOD |SHIFT,        XK_Right,          maxhalf,           {.i=TWOBWM_MAXHALF_UNFOLD_HORIZONTAL}},
    // Next/Previous screen
    {  MOD ,              XK_comma,      changescreen,      {.i=TWOBWM_NEXT_SCREEN}},
    {  MOD ,              XK_period,     changescreen,      {.i=TWOBWM_PREVIOUS_SCREEN}},
    // Raise or lower a window
    {  MOD ,              XK_r,          raiseorlower,      {}},
    // Next/Previous workspace
    {  MOD ,              XK_v,          nextworkspace,     {}},
    {  MOD ,              XK_c,          prevworkspace,     {}},
    // Move to Next/Previous workspace
    {  MOD |SHIFT ,       XK_v,          sendtonextworkspace,{}},
    {  MOD |SHIFT ,       XK_c,          sendtoprevworkspace,{}},
    // Iconify the window
    {  MOD ,              XK_i,          hide,              {}},
    // Make the window unkillable
    {  MOD ,              XK_a,          unkillable,        {}},
    // Make the window appear always on top
    {  MOD,               XK_w,          always_on_top,     {}},
    // Make the window stay on all workspaces
    {  MOD ,              XK_s,          fix,               {}},
    // // Move the cursor
    // {  MOD ,              XK_Up,         cursor_move,       {.i=TWOBWM_CURSOR_UP_SLOW}},
    // {  MOD ,              XK_Down,       cursor_move,       {.i=TWOBWM_CURSOR_DOWN_SLOW}},
    // {  MOD ,              XK_Right,      cursor_move,       {.i=TWOBWM_CURSOR_RIGHT_SLOW}},
    // {  MOD ,              XK_Left,       cursor_move,       {.i=TWOBWM_CURSOR_LEFT_SLOW}},
    // // Move the cursor faster
    // {  MOD |SHIFT,        XK_Up,         cursor_move,       {.i=TWOBWM_CURSOR_UP}},
    // {  MOD |SHIFT,        XK_Down,       cursor_move,       {.i=TWOBWM_CURSOR_DOWN}},
    // {  MOD |SHIFT,        XK_Right,      cursor_move,       {.i=TWOBWM_CURSOR_RIGHT}},
    // {  MOD |SHIFT,        XK_Left,       cursor_move,       {.i=TWOBWM_CURSOR_LEFT}},
    // Start programs
    {  MOD ,              XK_space,      start,             {.com = dmenucmd}},
    {  MOD ,              XK_Return,     start,             {.com = termcmd}},
    { 0,                  0x1008ff13,    start,             {.com = raisevolumecmd } },
	{ 0,                  0x1008ff11,    start,             {.com = lowervolumecmd } },
	{ 0,                  0x1008ff12,    start,             {.com = mutecmd } },
	{ MOD,                0xffab,        start,             {.com = raisevolumecmd } },
	{ MOD,                0xffad,        start,             {.com = lowervolumecmd } },
	{ MOD,                XK_KP_Insert,  start,             {.com = mutecmd } },
	{ MOD,                XK_KP_Multiply,start,             {.com = musicnext } },
	{ MOD,                XK_KP_End,     start,             {.com = musicplaypause } },
	{ MOD,                XK_KP_Divide,  start,             {.com = musicprev } },
    // Exit or restart 2bwm
    {  MOD |CONTROL,      XK_q,          twobwm_exit,       {.i=0}},
    {  MOD |CONTROL,      XK_r,          twobwm_restart,    {.i=0}},
    // Change current workspace
       DESKTOPCHANGE(     XK_1,                             0)
       DESKTOPCHANGE(     XK_2,                             1)
       DESKTOPCHANGE(     XK_3,                             2)
       DESKTOPCHANGE(     XK_4,                             3)
       DESKTOPCHANGE(     XK_5,                             4)
       DESKTOPCHANGE(     XK_6,                             5)
       DESKTOPCHANGE(     XK_7,                             6)
       DESKTOPCHANGE(     XK_8,                             7)
       DESKTOPCHANGE(     XK_9,                             8)
       DESKTOPCHANGE(     XK_0,                             9)
};
// the last argument makes it a root window only event
static Button buttons[] = {
    {  MOD,        XCB_BUTTON_INDEX_1,     mousemotion,     {.i=TWOBWM_MOVE}, false},
    {  0,          9,                      mousemotion,     {.i=TWOBWM_MOVE}, false},
    {  MOD,        XCB_BUTTON_INDEX_3,     mousemotion,     {.i=TWOBWM_RESIZE}, false},
    {  0,          8,                      mousemotion,     {.i=TWOBWM_RESIZE}, false},
    {  0,          XCB_BUTTON_INDEX_3,     start,           {.com = hiddenmenu}, true},
    {  0,          XCB_BUTTON_INDEX_1,     start,           {.com = menucmd}, true},
    {  MOD|SHIFT,  XCB_BUTTON_INDEX_3,     maximize,        {}, false},
    {  MOD|SHIFT,  XCB_BUTTON_INDEX_1,     hide,            {}, false},
    {  MOD|ALT,    XCB_BUTTON_INDEX_1,     changescreen,    {.i=1}, false},
    {  MOD|ALT,    XCB_BUTTON_INDEX_3,     changescreen,    {.i=0}, false},
};
