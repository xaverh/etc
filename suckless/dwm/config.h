/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx = 1; /* border pixel of windows */
static const unsigned int snap = 32;    /* snap pixel */
static const int showbar = 1;           /* 0 means no bar */
static const int topbar = 1;            /* 0 means bottom bar */
static const char* fonts[] = {"sans-serif:size=11"};
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
static const char* tags[] = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class            instance    title         tags mask     isfloating
       monitor */
    {"brave", NULL, NULL, 1 << 0, 0, -1},
    // { "Code",                       NULL,       NULL,         1 << 1, 0, -1
    // },
    {"discord", NULL, NULL, 1 << 2, 0, -1},
    {"Firefox", NULL, NULL, 1 << 0, 0, -1},
    {"Firefox", "Places", "Library", 0, 1, -1},
    {"Google-chrome", NULL, NULL, 1 << 0, 0, -1},
    /*	{ "mpv",              NULL,         NULL,         0xFFFF,       0, -1
       },*/
    {"Opera", NULL, NULL, 1 << 0, 0, -1},
    {"presenter", "sent", "sent", 0, 1, -1},
    {"Spotify", NULL, NULL, 1 << 5, 0, -1},
    {"strawberry", NULL, NULL, 1 << 5, 0, -1},
    {NULL, "Journalctl", NULL, 1 << 8, 0, -1},
    {"Steam", NULL, NULL, 1 << 7, 0, -1},
    {"TelegramDesktop", NULL, NULL, 1 << 2, 0, -1}};

/* layout(s) */
static const float mfact =
    0.6666666f;               /* factor of master area size [0.05..0.95] */
static const int nmaster = 1; /* number of clients in master area */
static const int resizehints =
    1; /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
    /* symbol     arrange function */
    {"[]=", tile}, /* first entry is default */
    {"><>", NULL}, /* no layout function means floating behavior */
    {"[M]", monocle},
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY, TAG)                                                      \
	{MODKEY, KEY, view, {.ui = 1 << TAG}},                                 \
	    {MODKEY | ControlMask, KEY, toggleview, {.ui = 1 << TAG}},         \
	    {MODKEY | ShiftMask, KEY, tag, {.ui = 1 << TAG}},                  \
	    {MODKEY | ControlMask | ShiftMask,                                 \
	     KEY,                                                              \
	     toggletag,                                                        \
	     {.ui = 1 << TAG}},

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd)                                                             \
	{                                                                      \
		.v = (const char*[])                                           \
		{                                                              \
			"/bin/zsh", "-c", cmd, NULL                            \
		}                                                              \
	}

/* commands */
static char dmenumon[2] =
    "0"; /* component of dmenucmd, manipulated in spawn() */
static const char* dmenucmd[] = {"rofi", "-show", "run", NULL};
static const char* termcmd[] = {"urxvtc", "-name", "Qillqaq", NULL};
static const char* alttermcmd[] = {"urxvtc", "-name", "Ysgrifennwr", NULL};
static const char* emojicmd[] = {
    "/bin/zsh", "-c",
    "rofi -dmenu -i -p Emoji -input ~/.local/share/emoji.txt | awk '{printf "
    "$1}' | xsel -ib",
    NULL};
static const char* mansplaincmd[] = {
    "/bin/zsh", "-c",
    "mkdir -p /tmp/mansplain-`date +\"%G-%V\"`\nmanpage=`apropos . | rofi "
    "-dmenu -i | awk '{gsub(/[()]/,\"\"); print $2\" "
    "\"$1}'`\nfilename=\"/tmp/mansplain-`date "
    "+\"%G-%V\"`/${^manpage}.pdf\"\n[[ -f \"$filename\" ]] || man -Tpdf "
    "${=manpage} > \"$filename\"\nmupdf-gl \"$filename\""};
static const char* playpausecmd[] = {"strawberry", "--play-pause", NULL};
static const char* playnextcmd[] = {"strawberry", "--next", NULL};
static const char* playpreviouscmd[] = {"strawberry", "--restart-or-previous",
                                        NULL};
static const char* raisevolumecmd[] = {
    "/bin/zsh", "-c",
    "amixer sset Master unmute && exec amixer sset Master 5%+", NULL};
static const char* lowervolumecmd[] = {
    "/bin/zsh", "-c",
    "amixer sset Master unmute && exec amixer sset Master 5%-", NULL};
static const char* mutecmd[] = {"amixer", "sset", "Master", "mute", NULL};
// char *mutecmd[] = { "/bin/sh", "-c", "pactl -- set-sink-mute 0
// true\nif [
// \"`pamixer --get-mute`\" == \"true\" ]; then xsetroot -name \"mute\";
// else
// xsetroot -name \"NOT MUTE\"; fi", NULL };
static const char* lockcmd[] = {"slock", NULL};
static const char* brightnessupcmd[] = {"xbacklight", "-inc", "7", NULL};
static const char* brightnessdowncmd[] = {"xbacklight", "-dec", "7", NULL};
static const char* screenshotcmd[] = {
    "/bin/zsh", "-c",
    "mkdir -p ~/tmp/scr && maim -u -d 3 -m 10 > ~/tmp/scr/\"screenshot-$(date "
    "--iso-8601=ns).png\"",
    NULL};
static const char* screenshotselectioncmd[] = {
    "/bin/zsh", "-c",
    "mkdir -p ~/tmp/scr && maim -c "
    "0.88671875,0.15294117647059,0.56862745098039 -u "
    "-s -m 10 > ~/tmp/scr/\"screenshot-$(date --iso-8601=ns).png\"",
    NULL};
static const char* urlcmd[] = {"clipmenu-url", NULL};
static const char* clipcmd[] = {"clipmenu", "-p", "Clipboard", NULL};
static const char* showclipboardcmd[] = {
    "/bin/zsh", "-c",
    "xsetroot -name \"$(tail --lines=1 "
    "$CM_DIR/clipmenu.5.$(whoami)/line_cache_clipboard)\"",
    NULL};
static const char* tmuxcmd[] = {
    "/bin/zsh", "-c",
    "urxvtc -e tmux new-session -A -s $(tmux list-clients -F \"#S\" | rofi "
    "-dmenu -i -p 'Attach to tmux session:')",
    NULL};
static const char* connect_setubal[] = {"bluetoothctl", "connect",
                                        "88:C6:26:F4:8A:90", NULL};
static const char* disconnect_setubal[] = {"bluetoothctl", "disconnect",
                                           "88:C6:26:F4:8A:90", NULL};
static const char* suspendcmd[] = {"systemctl", "suspend", NULL};
static const char* filemanagercmd[] = {"pcmanfm", NULL};
static const char* backdropcmd[] = {
    "/bin/zsh", "-c",
    "xsetroot -bitmap ~/.fvwm/backdrops/`ls ~/.fvwm/backdrops | shuf -n 1 | tr "
    "-d "
    "'\\n' | tee -a /tmp/wallpaper` `printf -- \" -fg #%06x -bg #%06x\\n\" "
    "$(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`",
    NULL};
static const char* nowallpapercmd[] = {"xsetroot", "-solid", col_gray1, NULL};

static Key keys[] = {
    /* modifier                     key        function        argument */
    {MODKEY, XK_p, spawn, {.v = dmenucmd}},
    {MODKEY | ShiftMask, XK_Return, spawn, {.v = termcmd}},
    {MODKEY, XK_b, togglebar, {0}},
    {MODKEY, XK_j, focusstack, {.i = +1}},
    {MODKEY, XK_k, focusstack, {.i = -1}},
    {MODKEY, XK_i, incnmaster, {.i = +1}},
    {MODKEY, XK_d, incnmaster, {.i = -1}},
    {MODKEY, XK_h, setmfact, {.f = -0.05}},
    {MODKEY, XK_l, setmfact, {.f = +0.05}},
    {MODKEY, XK_Return, zoom, {0}},
    {MODKEY, XK_Tab, view, {0}},
    {MODKEY | ShiftMask, XK_c, killclient, {0}},
    {MODKEY, XK_t, setlayout, {.v = &layouts[0]}},
    {MODKEY, XK_f, setlayout, {.v = &layouts[1]}},
    {MODKEY, XK_m, setlayout, {.v = &layouts[2]}},
    {MODKEY, XK_space, setlayout, {0}},
    {MODKEY | ShiftMask, XK_space, togglefloating, {0}},
    {MODKEY, XK_0, view, {.ui = ~0}},
    {MODKEY | ShiftMask, XK_0, tag, {.ui = ~0}},
    {MODKEY, XK_comma, focusmon, {.i = -1}},
    {MODKEY, XK_period, focusmon, {.i = +1}},
    {MODKEY | ShiftMask, XK_comma, tagmon, {.i = -1}},
    {MODKEY | ShiftMask, XK_period, tagmon, {.i = +1}},
    TAGKEYS(XK_1, 0) TAGKEYS(XK_2, 1) TAGKEYS(XK_3, 2) TAGKEYS(XK_4, 3)
        TAGKEYS(XK_5, 4) TAGKEYS(XK_6, 5) TAGKEYS(XK_7, 6) TAGKEYS(XK_8, 7)
            TAGKEYS(XK_9, 8){MODKEY | ShiftMask, XK_q, quit, {0}},
    {MODKEY, XK_ssharp, spawn, {.v = emojicmd}},
    {MODKEY | Mod1Mask, XK_Return, spawn, {.v = alttermcmd}},
    {MODKEY, XK_F1, spawn, {.v = mansplaincmd}},
    {MODKEY, XK_Insert, spawn, {.v = clipcmd}},
    {MODKEY, XK_acute, spawn, {.v = showclipboardcmd}},
    {MODKEY, XK_e, spawn, {.v = filemanagercmd}},
    {0, 0xff61, spawn, {.v = screenshotcmd}},
    {MODKEY, 0xff61, spawn, {.v = screenshotselectioncmd}},
    {0, 0x1008ff13, spawn, {.v = raisevolumecmd}},
    {0, 0x1008ff11, spawn, {.v = lowervolumecmd}},
    {0, 0x1008ff12, spawn, {.v = mutecmd}},
    {MODKEY, 0xffab, spawn, {.v = raisevolumecmd}},
    {MODKEY, 0xffad, spawn, {.v = lowervolumecmd}},
    {MODKEY, 0xff9e, spawn, {.v = mutecmd}},
    {0, 0x1008ff17, spawn, {.v = playnextcmd}},
    {0, 0x1008ff14, spawn, {.v = playpausecmd}},
    {0, 0x1008ff16, spawn, {.v = playpreviouscmd}},
    {MODKEY, 0xffaa, spawn, {.v = playnextcmd}},
    {MODKEY, 0xff9c, spawn, {.v = playpausecmd}},
    {MODKEY, 0xffaf, spawn, {.v = playpreviouscmd}},
    {MODKEY, XK_a, spawn, {.v = tmuxcmd}},
    {MODKEY | ShiftMask, XK_w, spawn, {.v = backdropcmd}},
    {MODKEY, XK_w, spawn, {.v = nowallpapercmd}},
    {MODKEY, XK_F9, spawn, {.v = connect_setubal}},
    {MODKEY | ShiftMask, XK_F9, spawn, {.v = disconnect_setubal}},
    {MODKEY, XK_o, spawn, {.v = urlcmd}},
    {0, 0x1008ff02, spawn, {.v = brightnessupcmd}},
    {0, 0x1008ff03, spawn, {.v = brightnessdowncmd}},
    {MODKEY | ShiftMask, XK_Escape, spawn, {.v = suspendcmd}},
    {MODKEY, XK_Escape, spawn, {.v = lockcmd}},
};

#define Button6 6
#define Button7 7
#define Button8 8
#define Button9 9

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle,
 * ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function argument
     */
    {ClkLtSymbol, 0, Button1, setlayout, {0}},
    {ClkLtSymbol, 0, Button3, setlayout, {.v = &layouts[2]}},
    {ClkWinTitle, 0, Button2, zoom, {0}},
    {ClkStatusText, 0, Button2, spawn, {.v = termcmd}},
    {ClkClientWin, MODKEY, Button1, movemouse, {0}},
    {ClkClientWin, MODKEY, Button2, togglefloating, {0}},
    {ClkClientWin, MODKEY, Button3, resizemouse, {0}},
    {ClkTagBar, 0, Button1, view, {0}},
    {ClkTagBar, 0, Button3, toggleview, {0}},
    {ClkTagBar, MODKEY, Button1, tag, {0}},
    {ClkTagBar, MODKEY, Button3, toggletag, {0}},
};

/*
        { ClkLtSymbol,          0,              Button1,        setlayout, {0}
   }, { ClkLtSymbol,          0,              Button3,        setlayout, {.v =
   &layouts[2]} }, { ClkWinTitle,          0,              Button1, swapfocus,
   {0} }, { ClkWinTitle,          0,              Button2,        zoom, {0} },
        { ClkWinTitle,          0,              Button3,        focusstack, {.i
   = +1 } }, { ClkStatusText,        0,              Button2,        spawn, {.v
   = termcmd } }, { ClkClientWin,         MODKEY,         Button1, movemouse,
   {0} },
        // { ClkClientWin,         0,              Button9,        movemouse,
   {0} }, { ClkClientWin,         MODKEY,         Button2, togglefloating, {0}
   }, { ClkClientWin,         MODKEY,         Button3,        resizemouse, {0}
   },
        // { ClkClientWin,         0,              Button8,        resizemouse,
   {0} }, { ClkTagBar,            0,              Button1,        view, {0} },
        { ClkTagBar,            0,              Button3,        toggleview, {0}
   }, { ClkTagBar,            MODKEY,         Button1,        tag, {0} }, {
   ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
        { ClkClientWin,         0,              Button9,        zoom, {0} }, {
   ClkClientWin,         0,              Button8,        view,           {0} },
        { ClkRootWin,           0,              Button8,        view, {0} },
        // { ClkClientWin,         0,              Button8,        shiftview,
   {.i = +1 } }, { ClkClientWin,         MODKEY,         Button6, setmfact, {.f
   = -0.05} }, { ClkClientWin,         MODKEY,         Button7,        setmfact,
   {.f = +0.05} },
        */
