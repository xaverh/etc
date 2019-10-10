/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char* fonts[] = {
    "sans-serif:size=11"};
static const char dmenufont[] =
    "sans-serif:size=11";
static const char col_gray1[] = "#1e1e1e";
// static const char col_gray2[] = "#424242";
static const char col_gray3[] = "#b8b8b8";
static const char col_gray4[] = "#e5e6e6";
static const char col_cyan[] = "#005577";
static const char col_cerise[] = "#e32791";
static const char* colors[][3] = {
    /*               fg         bg         border   */
    [SchemeNorm] = {col_gray3, col_gray1, col_cyan},
    [SchemeSel] = {col_gray4, col_cyan, col_cerise},
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class       instance    title        tags mask     isfloating   monitor
     */
    {"brave", NULL, NULL, 1 << 0, 0, -1},
    // { "Code",           NULL,       NULL,        1 << 1,       0, -1 },
    {"discord", NULL, NULL, 1 << 2, 0, -1},
    {"Firefox", NULL, NULL, 1 << 0, 0, -1},
    {"Firefox", "Places", "Library", 0, 1, -1},
    /*	{ "Gimp",           NULL,       NULL,        1 << 6,       0, -1 },*/
    {"Google-chrome", NULL, NULL, 1 << 0, 0, -1},
    /*	{ "mpv",            NULL,       NULL,        0xFFFF,       0, -1 },*/
    {"Opera", NULL, NULL, 1 << 0, 0, -1},
    {"presenter", "sent", "sent", 0, 1, -1},
    {"Spotify", NULL, NULL, 1 << 5, 0, -1},
    {"st-256color", NULL, "journalctl", 1 << 8, 0, -1},
    {"URxvt", NULL, "journalctl", 1 << 8, 0, -1},
    /*	{ "Steam",          NULL,       NULL,        1 << 3,       0, -1 },*/
    {"TelegramDesktop", NULL, NULL, 1 << 2, 0, -1}
    // { "Zathura",        NULL,       NULL,        1 << 3,       0, -1 }
};

/* layout(s) */
static const float mfact = 0.6666666f; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/zsh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf", col_gray4, NULL };
static const char *termcmd[]  = { "urxvtc", NULL };
static const char* vanillatermcmd[] = {"st", NULL};
static const char* filemanagercmd[] = {"fman", NULL};
static const char* playpausecmd[] = {"playerctl", "play-pause", NULL};
static const char* playnextcmd[] = {"playerctl", "next", NULL};
static const char* playpreviouscmd[] = {"playerctl", "previous", NULL};
// static const char *mpdpausecmd[] = {"mpc", "toggle", NULL};
// static const char *mpdnextcmd[] = {"mpc", "next", NULL};
// static const char *mpdpreviouscmd[] = {"mpc", "prev", NULL};
// static const char *raisevolumecmd[] = { "/bin/sh", "-c", "pactl --
// set-sink-mute 0 false\npactl -- set-sink-volume 0 +5%\nxsetroot -name \"vol.
// `pamixer --get-volume` %\"", NULL }; static const char *lowervolumecmd[] = {
// "/bin/sh", "-c", "pactl -- set-sink-mute 0 false\npactl -- set-sink-volume 0
// -5%\nxsetroot -name \"vol. `pamixer --get-volume` %\"", NULL }; static const
// char *mutecmd[] = { "/bin/sh", "-c", "pactl -- set-sink-mute 0 true\nif [
// \"`pamixer --get-mute`\" == \"true\" ]; then xsetroot -name \"mute\"; else
// xsetroot -name \"NOT MUTE\"; fi", NULL };
static const char* mutecmd[] = {
    "/bin/sh", "-c",
    "ponymix mute\nponymix is-muted && xsetroot -name \"mute\" || xsetroot "
    "-name \"volume: \"`ponymix get-volume`\" %\"",
    NULL};
static const char* raisevolumecmd[] = {
    "/bin/sh", "-c",
    "ponymix unmute\nponymix increase 5\nponymix is-muted && xsetroot -name "
    "\"mute\" || xsetroot -name \"volume: \"`ponymix get-volume`\" %\"",
    NULL};
static const char* lowervolumecmd[] = {
    "/bin/sh", "-c",
    "ponymix unmute\nponymix decrease 5\nponymix is-muted && xsetroot -name "
    "\"mute\" || xsetroot -name \"volume: \"`ponymix get-volume`\" %\"",
    NULL};
static const char* lockcmd[] = {"slock", NULL};
static const char* brightnessupcmd[] = {"xbacklight", "-inc", "7", NULL};
static const char* brightnessdowncmd[] = {"xbacklight", "-dec", "7", NULL};
static const char* screenshotcmd[] = {
    "/bin/sh", "-c", "scrot \"$HOME/tmp/Screenshot_%Y-%m-%d-%H-%M-%S.png\"",
    NULL};
static const char* nodeadkeyscmd[] = {"setxkbmap", "-variant", "nodeadkeys",
                                      NULL};
static const char* deadtildecmd[] = {"setxkbmap", "-variant", "deadtilde",
                                     NULL};
static const char* urlcmd[] = {"clipmenu-url", NULL};
static const char* clipcmd[] = {
    "clipmenu", "-m",      dmenumon, "-fn",    dmenufont, "-nb",     col_gray1,
    "-nf",      col_gray3, "-sb",    col_cyan, "-sf",     col_gray4, NULL};
static const char* showclipboardcmd[] = {
    "/bin/sh", "-c",
    "tmptitle=\"`tail --lines=1 /tmp/clipmenu.3.xha/line_cache`\"\nxsetroot "
    "-name \"$tmptitle\"",
    NULL};
static const char* tmuxcmd[] = {
    "/bin/sh", "-c",
    "st -e tmux new-session -A -s $(tmux list-clients -F \"#S\" | dmenu -l 5 "
    "-p 'Attach to tmux session:' -m 0 -fn "
    "'InputSansCondensed:style=Regular:pixelsize=15:antialias=true:hintstyle=1:"
    "lcdfilter=1:rgba=1' -nb '#1e1e1e' -nf '#b8b8b8' -sb '#005577' -sf "
    "'#e5e6e6')",
    NULL};
// static const char *tmuxcmd[] = {"/bin/sh", "-c", "st -e tmux new-session -A
// -s $(tmux list-clients -F \"#S\" | dmenu -l 5 -p 'Attach to tmux session:' -m
// 0 -fn 'IBM Plex
// Mono:style=Regular:pixelsize=15:antialias=true:hintstyle=1:lcdfilter=1:rgba=1'
// -nb '#1e1e1e' -nf '#b8b8b8' -sb '#005577' -sf '#e5e6e6')", NULL};
static const char* connect_setubal[] = {
    "/bin/sh", "-c",
    "echo -e 'connect 88:C6:26:F4:8A:90\nquit' | bluetoothctl\nsleep 3\npactl "
    "set-card-profile bluez_card.88_C6_26_F4_8A_90 a2dp_sink",
    NULL};
static const char* disconnect_setubal[] = {
    "/bin/sh", "-c",
    "echo -e 'disconnect 88:C6:26:F4:8A:90\nquit' | bluetoothctl", NULL};
static const char* suspendcmd[] = {"systemctl", "suspend", NULL};

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/*
	{ MODKEY,                       XK_space,      spawn,          {.v = dmenucmd } },
	{ MODKEY,                       XK_Return,     spawn,          {.v = termcmd } },
	{ MODKEY|ShiftMask,             XK_Return,     spawn,          {.v = vanillatermcmd } },
	{ MODKEY,                       XK_e,          spawn,          {.v = filemanagercmd } },
	{ 0,                            0x1008ff13,    spawn,          {.v = raisevolumecmd } },
	{ 0,                            0x1008ff11,    spawn,          {.v = lowervolumecmd } },
	{ 0,                            0x1008ff12,    spawn,          {.v = mutecmd } },
	{ MODKEY,                       0xffab,        spawn,          {.v = raisevolumecmd } },
	{ MODKEY,                       0xffad,        spawn,          {.v = lowervolumecmd } },
	{ MODKEY,                       0xffaa,        spawn,          {.v = mutecmd } },
	{ 0,                            0x1008ff17,    spawn,          {.v = playnextcmd } },
	{ 0,                            0x1008ff14,    spawn,          {.v = playpausecmd } },
	{ 0,                            0x1008ff16,    spawn,          {.v = playpreviouscmd } },
	// { MODKEY,                       0xff56,        spawn,          {.v = mpdnextcmd } },
	// { MODKEY,                       0xff13,        spawn,          {.v = mpdpausecmd } },
	// { MODKEY,                       0xff55,        spawn,          {.v = mpdpreviouscmd } },
	// { MODKEY,                       XK_numbersign, spawn,          {.v = ncmpcppcmd } },
	{ MODKEY,                       XK_b,          togglebar,      {0} },
	{ MODKEY,                       XK_j,          focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,          focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,          incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,          incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,          setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,          setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_odiaeresis, zoom,           {0} },
	{ MODKEY,                       XK_Tab,        swapfocus,      {0} },
	{ MODKEY,                       XK_adiaeresis, view,           {0} },
	{ MODKEY,                       XK_q,          killclient,     {0} },
	{ MODKEY,                       XK_c,          setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_t,          setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,          setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_f,          setlayout,      {.v = &layouts[3]} },
	{ MODKEY|ShiftMask,             XK_f,          togglefloating, {0} },
	{ MODKEY,                       XK_0,          view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,          tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,      focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period,     focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,      tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period,     tagmon,         {.i = +1 } },
	TAGKEYS(                        XK_1,                          0)
	TAGKEYS(                        XK_2,                          1)
	TAGKEYS(                        XK_3,                          2)
	TAGKEYS(                        XK_4,                          3)
	TAGKEYS(                        XK_5,                          4)
	TAGKEYS(                        XK_6,                          5)
	TAGKEYS(                        XK_7,                          6)
	TAGKEYS(                        XK_8,                          7)
	TAGKEYS(                        XK_9,                          8)
	{ MODKEY,                       XK_Escape,	spawn,		{.v = lockcmd } },
	{ MODKEY|ShiftMask,		XK_Escape,	spawn,		{.v = suspendcmd } },
	{ MODKEY|ShiftMask,             XK_q,          quit,           {0} },
	{ 0,				0x1008ff02,    spawn,          {.v = brightnessupcmd} },
	{ 0,				0x1008ff03,    spawn,          {.v = brightnessdowncmd} },
	{ 0, 				0xff61,        spawn,          {.v = screenshotcmd} },
	{ MODKEY,                       XK_Insert,     spawn,          {.v = clipcmd } },
	{ MODKEY,                    	XK_o,          spawn,          {.v = urlcmd } },
	{ MODKEY|Mod1Mask|ShiftMask,	XK_ssharp,	spawn,         {.v = deadtildecmd}},
	{ MODKEY|Mod1Mask,		XK_ssharp,	spawn,	       {.v = nodeadkeyscmd}},
	{ MODKEY,			XK_ssharp,      spawn,          {.v = showclipboardcmd}},
	{ MODKEY,                       XK_Prior,      	shiftview,      {.i = -1 } },
	{ MODKEY,                       XK_Next,      	shiftview,      {.i = +1 } },
	{ MODKEY,                       XK_dead_acute, 	togglescratch,  {.v = scratchpadcmd }},
	{ MODKEY,			XK_a,		spawn,		{.v = tmuxcmd }},
	{ MODKEY,			XK_F9,		spawn,		{.v = connect_setubal }},
	{ MODKEY|ShiftMask,		XK_F9,		spawn,		{.v = disconnect_setubal }},
	{ MODKEY,                       XK_numbersign,  runorraise,     {.v = firefox }},
	{ MODKEY,                       XK_plus,        runorraise,     {.v = fman }},
	{ MODKEY,                       XK_udiaeresis,  runorraise,     {.v = vscode }},
*/

#define Button6 6
#define Button7 7
#define Button8 8
#define Button9 9

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

/*
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button1,        swapfocus,      {0} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkWinTitle,          0,              Button3,        focusstack,     {.i = +1 } },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	// { ClkClientWin,         0,              Button9,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	// { ClkClientWin,         0,              Button8,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
	{ ClkClientWin,         0,              Button9,        zoom,           {0} },
	{ ClkClientWin,         0,              Button8,        view,           {0} },
	{ ClkRootWin,           0,              Button8,        view,           {0} },
	// { ClkClientWin,         0,              Button8,        shiftview,      {.i = +1 } },
	{ ClkClientWin,         MODKEY,         Button6,        setmfact,       {.f = -0.05} },
	{ ClkClientWin,         MODKEY,         Button7,        setmfact,       {.f = +0.05} },
	*/
