///---User configurable stuff---///
///---Modifiers---///
#define MOD             XCB_MOD_MASK_4       /* Super/Windows key  or check xmodmap(1) with -pm  defined in /usr/include/xcb/xproto.h */
///--Speed---///
/* Move this many pixels when moving or resizing with keyboard unless the window has hints saying otherwise.
 *0)move step slow   1)move step fast
 *2)mouse slow       3)mouse fast     */
static const uint16_t movements[] = {4,40,15,400};
/* resize by line like in mcwm -- jmbi */
static const bool     resize_by_line          = true;
/* the ratio used when resizing and keeping the aspect */
static const float    resize_keep_aspect_ratio= 1.03;
///---Offsets---///
/*0)offsetx          1)offsety
 *2)maxwidth         3)maxheight */
static const uint8_t offsets[] = {0,0,0,0};
///---Colors---///
/*0)focuscol         1)unfocuscol
 *2)fixedcol         3)unkilcol
 *4)fixedunkilcol    5)outerbordercol
 *6)emptycol         */
static const char *colors[] = {"#81D8D0","#515151","#E3C472","#E32791","#FC9520","#005577","#E5E6E6"};
/* if this is set to true the inner border and outer borders colors will be swapped */
static const bool inverted_colors = true;
///---Cursor---///
/* default position of the cursor:
 * correct values are:
 * TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_RIGHT, MIDDLE
 * All these are relative to the current window. */
#define CURSOR_POSITION MIDDLE
///---Borders---///
/*0) Outer border size. If you put this negative it will be a square.
 *1) Full borderwidth    2) Magnet border size
 *3) Resize border size  */
static const uint8_t borders[] = {4,8,8,5};
/* Windows that won't have a border.
 * It uses substring comparison with what is found in the WM_NAME
 * attribute of the window. You can test this using `xprop WM_NAME`
 */
#define LOOK_INTO "WM_NAME"
static const char *ignore_names[] = {"bar", "xclock"};
///--Menus and Programs---///
static const char *menucmd[]   = { "dmenu_recent", "-m", "0", "-fn", "InputSansCondensed:style=Regular:pixelsize=15:antialias=true:hintstyle=1:lcdfilter=1:rgba=1", "-nb", "#1e1e1e", "-nf", "#b8b8b8", "-sb", "#005577", "-sf", "#e5e6e6", NULL };
static const char *termcmd[] = { "st", "-e", "tmux", NULL };
static const char *vanillatermcmd[] = { "st", NULL };
static const char *filemanagercmd[]  = { "fman", NULL };
static const char *playpausecmd[] = {"playerctl", "play-pause", NULL};
static const char *playnextcmd[] = {"playerctl", "next", NULL};
static const char *playpreviouscmd[] = {"playerctl", "previous", NULL};

/* TODO: feedback to bar or dunst */
static const char *mutecmd[] = { "/bin/sh", "-c", "ponymix mute\nponymix is-muted && xsetroot -name \"mute\" || xsetroot -name \"volume: \"`ponymix get-volume`\" %\"", NULL };
static const char *raisevolumecmd[] = { "/bin/sh", "-c", "ponymix unmute\nponymix increase 5\nponymix is-muted && xsetroot -name \"mute\" || xsetroot -name \"volume: \"`ponymix get-volume`\" %\"", NULL };
static const char *lowervolumecmd[] = { "/bin/sh", "-c", "ponymix unmute\nponymix decrease 5\nponymix is-muted && xsetroot -name \"mute\" || xsetroot -name \"volume: \"`ponymix get-volume`\" %\"", NULL };
static const char *lockcmd[]  = { "slock", NULL };
static const char *brightnessupcmd[] = { "xbacklight", "-inc", "7", NULL };
static const char *brightnessdowncmd[] = { "xbacklight", "-dec", "7", NULL };
static const char *connect_setubal[] = {"/bin/sh", "-c", "echo -e 'connect 88:C6:26:F4:8A:90\nquit' | bluetoothctl\nsleep 3\npactl set-card-profile bluez_card.88_C6_26_F4_8A_90 a2dp_sink", NULL };
static const char *disconnect_setubal[] = {"/bin/sh", "-c", "echo -e 'disconnect 88:C6:26:F4:8A:90\nquit' | bluetoothctl", NULL };
static const char *suspendcmd[] = { "systemctl", "suspend", NULL };
static const char *screenshotcmd[] = { "/bin/sh", "-c", "import \"/home/xha/tmp/Screenshot_`date +%Y-%m-%d-%H-%M-%S`.png\"", NULL };
static const char *screenshotallcmd[] = { "/bin/sh", "-c", "import -window root \"/home/xha/tmp/Screenshot_`date +%Y-%m-%d-%H-%M-%S`.png\"", NULL };
static const char *tmuxcmd[] = {"/bin/sh", "-c", "st -e tmux new-session -A -s $(tmux list-clients -F \"#S\" | dmenu -l 5 -p 'Attach to tmux session:' -m 0 -fn 'InputSansCondensed:style=Regular:pixelsize=15:antialias=true:hintstyle=1:lcdfilter=1:rgba=1' -nb '#1e1e1e' -nf '#b8b8b8' -sb '#005577' -sf '#e5e6e6')", NULL};
static const char *urlcmd[]  = { "clipmenu-url", NULL };
static const char *clipcmd[]  = { "clipmenu", "-m", "0", "-fn", "InputSansCondensed:style=Regular:pixelsize=15:antialias=true:hintstyle=1:lcdfilter=1:rgba=1", "-nb", "#1e1e1e", "-nf", "#b8b8b8", "-sb", "#005577", "-sf", "#e5e6e6", NULL };
/*
static const char *showclipboardcmd[] = {"/bin/sh", "-c", "tmptitle=\"`tail --lines=1 /tmp/clipmenu.3.xha/line_cache`\"\nxsetroot -name \"$tmptitle\"", NULL };
bindsym $mod+ssharp exec "dunstify `tail --lines=1 $XDG_RUNTIME_DIR/clipmenu.5.$USER/line_cache` -t 1000"
*/

///--Custom foo---///
static void halfandcentered(const Arg *arg)
{
	Arg arg2 = {.i=TWOBWM_MAXHALF_VERTICAL_LEFT};
	maxhalf(&arg2);
	Arg arg3 = {.i=TWOBWM_TELEPORT_CENTER};
	teleport(&arg3);
}

#define DESKTOPCHANGE(K,N) \
{  MOD ,             K,              changeworkspace, {.i=N}}, \
{  MOD |SHIFT,       K,              sendtoworkspace, {.i=N}},
static key keys[] = {
    /* modifier           key            function           argument */
    // Focus to next/previous window
    {  MOD ,              XK_Tab,        focusnext,         {.i=TWOBWM_FOCUS_NEXT}},
    {  MOD |SHIFT,        XK_Tab,        focusnext,         {.i=TWOBWM_FOCUS_PREVIOUS}},
    // Kill a window
    {  MOD ,              XK_q,          deletewin,         {}},
// Resize a window
    {  MOD |SHIFT|CONTROL,XK_k,          resizestep,        {.i=TWOBWM_RESIZE_UP}},
    {  MOD |SHIFT|CONTROL,XK_j,          resizestep,        {.i=TWOBWM_RESIZE_DOWN}},
    {  MOD |SHIFT|CONTROL,XK_l,          resizestep,        {.i=TWOBWM_RESIZE_RIGHT}},
    {  MOD |SHIFT|CONTROL,XK_h,          resizestep,        {.i=TWOBWM_RESIZE_LEFT}},
    // Resize a window slower
    {  MOD |SHIFT,        XK_k,          resizestep,        {.i=TWOBWM_RESIZE_UP_SLOW}},
    {  MOD |SHIFT,        XK_j,          resizestep,        {.i=TWOBWM_RESIZE_DOWN_SLOW}},
    {  MOD |SHIFT,        XK_l,          resizestep,        {.i=TWOBWM_RESIZE_RIGHT_SLOW}},
    {  MOD |SHIFT,        XK_h,          resizestep,        {.i=TWOBWM_RESIZE_LEFT_SLOW}},
    // Move a window
    {  MOD | CONTROL ,    XK_k,          movestep,          {.i=TWOBWM_MOVE_UP}},
    {  MOD | CONTROL ,    XK_j,          movestep,          {.i=TWOBWM_MOVE_DOWN}},
    {  MOD | CONTROL ,    XK_l,          movestep,          {.i=TWOBWM_MOVE_RIGHT}},
    {  MOD | CONTROL ,    XK_h,          movestep,          {.i=TWOBWM_MOVE_LEFT}},
    // Move a window slower
    {  MOD ,              XK_k,          movestep,          {.i=TWOBWM_MOVE_UP_SLOW}},
    {  MOD ,              XK_j,          movestep,          {.i=TWOBWM_MOVE_DOWN_SLOW}},
    {  MOD ,              XK_l,          movestep,          {.i=TWOBWM_MOVE_RIGHT_SLOW}},
    {  MOD ,              XK_h,          movestep,          {.i=TWOBWM_MOVE_LEFT_SLOW}},
    // Teleport the window to an area of the screen.
    // Center:
    {  MOD ,              XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER}},
    // Center y:
    {  MOD |SHIFT,        XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER_Y}},
    // Center x:
    {  MOD |CONTROL,      XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER_X}},
    // Top left:
    {  MOD ,              XK_p,          teleport,          {.i=TWOBWM_TELEPORT_TOP_LEFT}},
    // Top right:
    {  MOD ,              XK_udiaeresis, teleport,          {.i=TWOBWM_TELEPORT_TOP_RIGHT}},
    // Bottom left:
    {  MOD ,              XK_odiaeresis, teleport,          {.i=TWOBWM_TELEPORT_BOTTOM_LEFT}},
    // Bottom right:
    {  MOD ,              XK_adiaeresis, teleport,          {.i=TWOBWM_TELEPORT_BOTTOM_RIGHT}},
    // Resize while keeping the window aspect
    {  MOD ,              XK_Home,       resizestep_aspect, {.i=TWOBWM_RESIZE_KEEP_ASPECT_GROW}},
    {  MOD ,              XK_End,        resizestep_aspect, {.i=TWOBWM_RESIZE_KEEP_ASPECT_SHRINK}},
    // Maximize (ignore offset and no EWMH atom)
    {  MOD ,              XK_x,          maximize,          {}},
    // Full screen (disregarding offsets and adding EWMH atom)
    {  MOD |SHIFT ,       XK_x,          fullscreen,        {}},
    // Maximize vertically
    {  MOD ,              XK_m,          maxvert_hor,       {.i=TWOBWM_MAXIMIZE_VERTICALLY}},
    // Maximize horizontally
    {  MOD |SHIFT,        XK_m,          maxvert_hor,       {.i=TWOBWM_MAXIMIZE_HORIZONTALLY}},
    // Maximize and move
    // vertically left
    {  MOD |SHIFT,        XK_p,          maxhalf,           {.i=TWOBWM_MAXHALF_VERTICAL_LEFT}},
    // vertically right
    {  MOD |SHIFT,        XK_udiaeresis, maxhalf,           {.i=TWOBWM_MAXHALF_VERTICAL_RIGHT}},
    // horizontally left
    {  MOD |SHIFT,        XK_odiaeresis, maxhalf,           {.i=TWOBWM_MAXHALF_HORIZONTAL_BOTTOM}},
    // horizontally right
    {  MOD |SHIFT,        XK_adiaeresis, maxhalf,           {.i=TWOBWM_MAXHALF_HORIZONTAL_TOP}},
    //fold half vertically
    {  MOD |SHIFT|CONTROL,XK_p,          maxhalf,           {.i=TWOBWM_MAXHALF_FOLD_VERTICAL}},
    //fold half horizontally
    {  MOD |SHIFT|CONTROL,XK_odiaeresis, maxhalf,           {.i=TWOBWM_MAXHALF_FOLD_HORIZONTAL}},
    //unfold vertically
    {  MOD |SHIFT|CONTROL,XK_udiaeresis, maxhalf,           {.i=TWOBWM_MAXHALF_UNFOLD_VERTICAL}},
    //unfold horizontally
    {  MOD |SHIFT|CONTROL,XK_adiaeresis, maxhalf,           {.i=TWOBWM_MAXHALF_UNFOLD_HORIZONTAL}},
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
    //{  MOD ,              XK_i,          hide,              {}},
    // Make the window unkillable
    {  MOD ,              XK_a,          unkillable,        {}},
    // Make the window appear always on top
    {  MOD,               XK_t,          always_on_top,     {}},
    // Make the window stay on all workspaces
    {  MOD ,              XK_f,          fix,               {}},
    // Move the cursor
    {  MOD ,              XK_Up,         cursor_move,       {.i=TWOBWM_CURSOR_UP_SLOW}},
    {  MOD ,              XK_Down,       cursor_move,       {.i=TWOBWM_CURSOR_DOWN_SLOW}},
    {  MOD ,              XK_Right,      cursor_move,       {.i=TWOBWM_CURSOR_RIGHT_SLOW}},
    {  MOD ,              XK_Left,       cursor_move,       {.i=TWOBWM_CURSOR_LEFT_SLOW}},
    // Move the cursor faster
    {  MOD |SHIFT,        XK_Up,         cursor_move,       {.i=TWOBWM_CURSOR_UP}},
    {  MOD |SHIFT,        XK_Down,       cursor_move,       {.i=TWOBWM_CURSOR_DOWN}},
    {  MOD |SHIFT,        XK_Right,      cursor_move,       {.i=TWOBWM_CURSOR_RIGHT}},
    {  MOD |SHIFT,        XK_Left,       cursor_move,       {.i=TWOBWM_CURSOR_LEFT}},
    // Start programs
    {  MOD ,              XK_space,      start,             {.com = menucmd}},
    // Exit or restart 2bwm
    {  MOD |CONTROL,      XK_q,          twobwm_exit,       {.i=0}},
    {  MOD |CONTROL,      XK_r,          twobwm_restart,    {.i=0}},
    {  MOD ,              XK_w,          halfandcentered,   {.i=0}},
    {  MOD ,              XK_Return,     start,             {.com=termcmd}},
    {  MOD | SHIFT,       XK_Return,     start,             {.com=vanillatermcmd}},
    {  MOD ,              XK_e,          start,             {.com=filemanagercmd}},
    {  0 ,                0x1008ff13,    start,             {.com=raisevolumecmd}},
    {  0 ,                0x1008ff11,    start,             {.com=lowervolumecmd}},
    {  0 ,                0x1008ff12,    start,             {.com=mutecmd}},
    {  MOD ,              0xffab,        start,             {.com=raisevolumecmd}},
    {  MOD ,              0xffad,        start,             {.com=lowervolumecmd}},
    {  MOD ,              0xffaa,        start,             {.com=mutecmd}},
    {  0 ,                0x1008ff17,    start,             {.com=playnextcmd}},
    {  0 ,                0x1008ff14,    start,             {.com=playpausecmd}},
    {  0 ,                0x1008ff16,    start,             {.com=playpreviouscmd}},
    {  0 ,                0x1008ff02,    start,             {.com=brightnessupcmd}},
    {  0 ,                0x1008ff03,    start,             {.com=brightnessdowncmd}},
    {  MOD ,              XK_Escape,     start,             {.com=lockcmd}},
    {  MOD | SHIFT ,      XK_Escape,     start,             {.com=suspendcmd}},
    {  MOD ,              XK_o,          start,             {.com=urlcmd}},
    {  MOD ,              XK_Insert,     start,             {.com=clipcmd}},
    {  0 ,                XK_Print,      start,             {.com=screenshotallcmd}},
    {  MOD ,              XK_Print,      start,             {.com=screenshotcmd}},
    {  MOD ,              XK_minus,      start,             {.com=tmuxcmd}},
    {  MOD ,              XK_F9,         start,             {.com=connect_setubal}},
    {  MOD | SHIFT,       XK_F9,         start,             {.com=disconnect_setubal}},
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
    {  MOD        ,XCB_BUTTON_INDEX_1,     mousemotion,   {.i=TWOBWM_MOVE}, false},
    {  MOD        ,XCB_BUTTON_INDEX_3,     mousemotion,   {.i=TWOBWM_RESIZE}, false},
    {  0          ,XCB_BUTTON_INDEX_3,     start,         {.com = menucmd}, true},
    {  MOD|SHIFT,  XCB_BUTTON_INDEX_1,     changeworkspace, {.i=0}, false},
    {  MOD|SHIFT,  XCB_BUTTON_INDEX_3,     changeworkspace, {.i=1}, false},
    {  MOD|ALT,    XCB_BUTTON_INDEX_1,     changescreen,    {.i=1}, false},
    {  MOD|ALT,    XCB_BUTTON_INDEX_3,     changescreen,    {.i=0}, false}
};
