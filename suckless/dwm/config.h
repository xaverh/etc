/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx = 2; /* border pixel of windows */
static const unsigned int snap = 4;     /* snap pixel */
static const int showbar = 1;           /* 0 means no bar */
static const int topbar = 1;            /* 0 means bottom bar */
static const char* fonts[] = {"sans-serif:size=11"};
#define COL_GRAY1 "#1e1e1e"
static const char col_gray1[] = COL_GRAY1;
static const char col_gray2[] = "#424242";
#define COL_GRAY3 "#b8b8b8"
static const char col_gray3[] = COL_GRAY3;
static const char col_gray4[] = "#e5e6e6";
static const char col_cyan[] = "#005577";
static const char col_cerise[] = "#e32791";
static const char* colors[][3] = {
    /*               fg         bg         border   */
    [SchemeNorm] = {col_gray3, col_gray1, col_gray2},
    [SchemeSel] = {col_gray4, col_cyan, col_cerise},
};

/* tagging */
static const char* tags[] = {"1:www",  "2:www",  "3:∞",     "4:๛",    "5:etc",
                             "F2:www", "F3:∞",   "F4:๛",    "F6:Ω",   "F7:irc",
                             "F8:mov", "F9:tty", "F10:mp3", "F12:log"};

#define BROWSERTAG         1U
#define BROWSERTAG2_FIXED  2U
#define BROWSERTAG2_TOGGLE 32U
#define BROWSERTAG2        BROWSERTAG2_FIXED | BROWSERTAG2_TOGGLE
#define ETCTAG             16U
#define CODINGTAG_FIXED    4U
#define CODINGTAG_TOGGLE   64U
#define CODINGTAG          CODINGTAG_FIXED | CODINGTAG_TOGGLE
#define CHATTAG            512U
#define PDFTAG_FIXED       8U
#define PDFTAG_TOGGLE      128U
#define PDFTAG             PDFTAG_FIXED | PDFTAG_TOGGLE
#define DUMPSTERTAG        256U
#define JOURNALTAG         8192U
#define MOVIETAG           1024U
#define TERMINALTAG        2048U
#define MUSICTAG           4096U
#define NONTOGGABLE_TAGS   0x1FU

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class  instance  title  tags mask  isfloating monitor */
    {"Firefox", "Places", "Library", 0, 1, -1},
    {"presenter", "sent", "sent", 0, 1, -1},
    {"Code", NULL, NULL, CODINGTAG, 0, -1},
    {"discord", NULL, NULL, CHATTAG, 0, -1},
    {"Firefox", NULL, NULL, BROWSERTAG2, 0, -1},
    {"Google-chrome", NULL, NULL, BROWSERTAG, 0, -1},
    {"kitty", NULL, NULL, TERMINALTAG, 0, -1},
    {"Kittyaskpass", NULL, NULL, TAGMASK, 1, -1},
    {"mpv", "gl", NULL, MOVIETAG, 0, -1},
    {NULL, "journalctl", NULL, JOURNALTAG, 0, -1},
    {"mpv", "FM0", NULL, MUSICTAG, 0, -1},
    {NULL, NULL, "MuPDF:", PDFTAG, 0, -1},
    {NULL, NULL, ".epub -", PDFTAG, 0, -1},
    {NULL, NULL, ".pdf -", PDFTAG, 0, -1},
    {NULL, NULL, ".cbz -", PDFTAG, 0, -1},
    {NULL, NULL, ".EPUB -", PDFTAG, 0, -1},
    {NULL, NULL, ".PDF -", PDFTAG, 0, -1},
    {NULL, NULL, ".CBZ -", PDFTAG, 0, -1},
    {NULL, NULL, "Picture in picture", MOVIETAG, 0, -1},
    {"Spotify", NULL, NULL, MUSICTAG, 0, -1},
    {"strawberry", NULL, NULL, MUSICTAG, 0, -1},
    {"Steam", NULL, NULL, DUMPSTERTAG, 0, -1},
    {"TelegramDesktop", NULL, NULL, CHATTAG, 0, -1},
    {"Vivaldi-stable", NULL, NULL, BROWSERTAG2, 0, -1},
    {NULL, "weechat", NULL, CHATTAG, 0, -1},
};

/* layout(s) */
#define DEFAULT_MFACT 0.5688140392f
static const float mfact =
    DEFAULT_MFACT;            /* factor of master area size [0.05..0.95] */
static const int nmaster = 1; /* number of clients in master area */
static const int resizehints =
    1; /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
    /* symbol     arrange function */
    {"[]=", tile}, /* first entry is default */
    {"[M]", monocle},
    {"><>", NULL}, /* no layout function means floating behavior */
};

void focusmaster();
void viewortoggleview(const Arg* arg);
void activatetag(const Arg* arg);

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(MYMODKEY, KEY, TAG, TAGBUDDIES)                                \
	{MYMODKEY, KEY, viewortoggleview, {.ui = TAG}},                        \
	    {MYMODKEY | ShiftMask, KEY, tag, {.ui = TAGBUDDIES}},              \
	    {MYMODKEY | ShiftMask, KEY, activatetag, {.ui = TAG}},             \
	{                                                                      \
		MYMODKEY | ControlMask, KEY, toggletag,                        \
		{                                                              \
			.ui = TAGBUDDIES                                       \
		}                                                              \
	}

#define SHELL "/bin/zsh"
/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd)                                                             \
	{                                                                      \
		.v = (const char*[])                                           \
		{                                                              \
			SHELL, "-c", cmd, NULL                                 \
		}                                                              \
	}

/* commands */
static char dmenumon[2] =
    "0"; /* component of dmenucmd, manipulated in spawn() */
static const char* dmenucmd[] = {"rofi", "-show", "run", NULL};
static const char* termcmd[] = {"kitty", "-1", "--listen-on", "unix:@mykitty",
                                NULL};
#define TERMCMD "kitty -1 --listen-on unix:@mykitty"
static const char* termifnotrunningcmd[] = {
    SHELL, "-c",
    "[[ `wmctrl -lx | grep kitty.kitty &> /dev/null` ]] && " TERMCMD, NULL};
static const char* emojicmd[] = {SHELL, "-c",
                                 "rofi -dmenu -i -p Emoji -input "
                                 "~/.local/share/emoji.txt | awk '{printf "
                                 "$1}' | xsel -ib",
                                 NULL};
static const char* mansplaincmd[] = {
    SHELL, "-c",
    "mupdf-gl =(man -Tpdf $(apropos . | rofi -dmenu -i -p mansplain | awk "
    "'{gsub(/[()]/,\"\"); print $2\" \"$1}'))"};
static const char* playpausecmd[] = {"strawberry", "--play-pause", NULL};
static const char* playnextcmd[] = {"strawberry", "--next", NULL};
static const char* playpreviouscmd[] = {"strawberry", "--restart-or-previous",
                                        NULL};
static const char* raisevolumecmd[] = {"amixer", "sset",   "Master",
                                       "5%+",    "unmute", NULL};
static const char* lowervolumecmd[] = {"amixer", "sset",   "Master",
                                       "5%-",    "unmute", NULL};
static const char* mutecmd[] = {"amixer", "sset", "Master", "mute", NULL};
#define GETVOLUME                                                              \
	"$(amixer get Master | awk 'END{gsub(/[\\[\\]]/,\"\"); print $6\" "    \
	"\"$5}')"
static const char* printvolumecmd[] = {
    SHELL, "-c", "sleep 0.1 && xsetroot -name \"" GETVOLUME "\"", NULL};
static const char* lockcmd[] = {"i3lock", "-c", col_gray1, NULL};
static const char* brightnessupcmd[] = {
    SHELL, "-c",
    "brightness=$(</sys/class/backlight/intel_backlight/brightness)\n"
    "max_brightness=$(</sys/class/backlight/intel_backlight/"
    "max_brightness)\n"
    "new_brightness=$(($max_brightness/10+$brightness))\n"
    "actual_brightness=$(($new_brightness < $max_brightness ? "
    "$new_brightness "
    ": $max_brightness))\n"
    "echo $actual_brightness > "
    "/sys/class/backlight/intel_backlight/brightness\n"
    "xsetroot -name \"brightness: $((100 * $actual_brightness / "
    "$max_brightness)) %\"",
    NULL};
static const char* brightnessdowncmd[] = {
    SHELL, "-c",
    "brightness=$(</sys/class/backlight/intel_backlight/brightness)\n"
    "max_brightness=$(</sys/class/backlight/intel_backlight/"
    "max_brightness)\n"
    "new_brightness=$((-$max_brightness/10+$brightness))\n"
    "actual_brightness=$(($new_brightness > 0 ? $new_brightness : 0))\n"
    "echo $actual_brightness > "
    "/sys/class/backlight/intel_backlight/brightness\n"
    "xsetroot -name \"brightness: $((100 * $actual_brightness / "
    "$max_brightness)) %\"",
    NULL};
static const char* screenshotcmd[] = {"flameshot", "screen", "-c",
                                      "-p",        "/tmp",   NULL};
static const char* screenshotselectioncmd[] = {"flameshot", "gui", "-p", "/tmp",
                                               NULL};
static const char* urlcmd[] = {
    SHELL, "-c",
    "xdg-open $(\\ls -1Qt ${CM_DIR}/clipmenu.5.${USER}/*\\ * | xargs awk 1 "
    "| "
    "grep "
    "--only-matching --perl-regexp "
    "\"http(s?):\\/\\/[^ \\\"\\(\\)\\<\\>\\]]*\" | uniq |rofi -dmenu -i -p "
    "'open URL')",
    NULL};
static const char* clipcmd[] = {"clipmenu", "-p", "Clipboard", NULL};
static const char* showclipboardcmd[] = {
    SHELL, "-c",
    "xsetroot -name \"$(cat ${CM_DIR}/clipmenu.5.${USER}/line_cache_* | "
    "sort | "
    "tail --lines=1 | cut -d ' ' -f 2- -- -)\"",
    NULL};
static const char* tmuxcmd[] = {
    SHELL, "-c",
    TERMCMD " -- tmux new-session -A -s $(tmux list-clients -F \"#S\" | rofi "
            "-dmenu -i -p 'Attach to tmux session:')",
    NULL};
static const char* connect_setubal[] = {"bluetoothctl", "connect",
                                        "88:C6:26:F4:8A:90", NULL};
static const char* disconnect_setubal[] = {"bluetoothctl", "disconnect",
                                           "88:C6:26:F4:8A:90", NULL};
static const char* suspendcmd[] = {"systemctl", "suspend", NULL};
static const char* backdropcmd[] = {
    SHELL, "-c",
    "xsetroot -bitmap ~/etc/suckless/backdrops/`\\ls "
    "~/etc/suckless/backdrops "
    "| "
    "shuf -n 1 | tr "
    "-d "
    "'\\n' | tee -a /tmp/wallpaper` `printf -- \" -fg #%06x -bg #%06x\\n\" "
    "$(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`",
    NULL};
static const char* nowallpapercmd[] = {"xsetroot", "-solid", col_gray1, NULL};
static const char* journalctlcmd[] = {
    SHELL, "-c",
    "pidof journalctl || " TERMCMD
    " --class journalctl -T journalctl -- journalctl -b -f -n 1000",
    NULL};
static const char* irccmd[] = {
    SHELL, "-c",
    "pidof weechat || " TERMCMD " --class weechat -T weechat -- weechat", NULL};
static const char* abridorcmd[] = {"/home/xha/etc/suckless/abridor/abridor.lua",
                                   NULL};
static const char* fm0cmd[] = {"/home/xha/etc/suckless/fm0/fm0.lua", NULL};

#define SSHADDKEYCMD                                                           \
	TERMCMD " --class=Kittyaskpass --override 'initial_window_width=56c' " \
	        "--override 'initial_window_height=5c' -- /bin/zsh -c "        \
	        "ssh-add </dev/null"

static const char* sshaddcmd[] = {
    SHELL, "-c",
    "[[ '256 SHA256:1uG7O27tlFijVrOw9wW5taw+5lcNsS7ItGC4k+6doJI "
    "xaver@hellauer.bayern (ED25519)' = `ssh-add -l` ]] || " SSHADDKEYCMD,
    NULL};

static const char* sshdelcmd[] = {"ssh-add", "-D", NULL};
#define FONT_9MENU                                                             \
	"-adobe-helvetica-medium-r-normal--14-140-75-75-p-77-iso10646-1"
static const char* strawberry9menucmd[] = {
    "9menu",
    "-font",
    FONT_9MENU,
    "-shell",
    SHELL,
    "-bg",
    col_gray1,
    "-fg",
    col_gray3,
    "-popup",
    "-teleport",
    "-label",
    "9music",
    "9music",
    "---:true",
    "play/pause:strawberry -t",
    "stop:strawberry -s",
    "stop after current:strawberry -q",
    "previous:strawberry  -r",
    "next:strawberry -f",
    "restart or prev:strawberry --restart-or-previous",
    "what's playing?:strawberry -y",
    "---:true",
    "exit",
    NULL};

static const char* main9menucmd[] = {
    "9menu",
    "-font",
    FONT_9MENU,
    "-shell",
    SHELL,
    "-bg",
    col_gray1,
    "-fg",
    col_gray3,
    "-popup",
    "-teleport",
    "9menu:false",
    "---:true",
    "Connect Set\372bal:bluetoothctl connect 88:C6:26:F4:8A:90",
    "Disconnect Set\372bal:bluetoothctl disconnect 88:C6:26:F4:8A:90",
    "---:true",
    "Screenshot (full):flameshot full -p /tmp -c -d 10",
    "Screenshot (selection):flameshot gui -p /tmp -d 10",
    "---:true",
    "Add SSH key 'xaver@hellauer.bayern (ED25519)':" SSHADDKEYCMD,
    "Remove all SSH keys:/usr/bin/ssh-add -D",
    "---:true",
    "Quit:9menu -font '" FONT_9MENU "' -shell " SHELL " -bg '" COL_GRAY1
    "' -fg '" COL_GRAY3 "' -popup -teleport -label 'Quit?' 'Quit?:true' 'Lock "
    "screen:i3lock -c \"" COL_GRAY1 "\"' 'Restart dwm:killall -USR1 dwm' "
    "'---:true' "
    "'Suspend:systemctl suspend' "
    "'Reboot:systemctl reboot' "
    "'Poweroff:systemctl poweroff'"
    " '---:true'"
    " 'Cancel:false'",
    "---:true",
    "Cancel:false",
    NULL};

static char const* kittybrightcmd[] = {
    "/bin/zsh", "-c",
    "kitty @ --to unix:@mykitty get-colors | grep -q '#1e1e1e' && kitty @ --to "
    "unix:@mykitty set-colors -a -c foreground='#424242' background='#f9f8f4' "
    "color0='#f9f8f4' color1='#e32791' color2='#488432' color3='#a25d0e' "
    "color4='#2c65b5' color5='#B062A7' color6='#27BBBE' color7='#999999' "
    "color8='#B8B8B8' color9='#9F1B66' color10='#325D23' color11='#71410A' "
    "color12='#1F477F' color13='#7B4474' color14='#1B8486' color15='#424242' "
    "cursor='#FC9520' || kitty @ --to unix:@mykitty set-colors -a -c "
    "foreground='#e5e6e6' background='#1e1e1e' color0='#1e1e1e' "
    "color8='#515151' color1='#e32791' color9='#e566ad' color2='#30c798' "
    "color10='#6cd1b2' color3='#e3c472' color11='#e4cf98' color4='#6769e6' "
    "color12='#9091e6' color5='#e59fdf' color13='#e5b6e1' color6='#81d8d0' "
    "color14='#a2dcd7' color7='#969696' color15='#e5e6e6' cursor='#20bbfc'",
    NULL};

static Key keys[] = {
    /* modifier  key  function  argument */
    {MODKEY, XK_p, spawn, {.v = dmenucmd}},
    {MODKEY, XK_Return, spawn, {.v = termifnotrunningcmd}},
    {MODKEY | ShiftMask, XK_Return, spawn, {.v = termcmd}},
    {MODKEY, XK_Return, activatetag, {.ui = TERMINALTAG}},
    {MODKEY | ShiftMask, XK_Return, activatetag, {.v = TERMINALTAG}},
    {MODKEY, XK_b, togglebar, {0}},
    {MODKEY, XK_j, focusstack, {.i = +1}},
    {MODKEY, XK_k, focusstack, {.i = -1}},
    {MODKEY, XK_i, incnmaster, {.i = +1}},
    {MODKEY, XK_d, incnmaster, {.i = -1}},
    {MODKEY, XK_h, setmfact, {.f = -0.05}},
    {MODKEY, XK_l, setmfact, {.f = +0.05}},
    {MODKEY, XK_space, zoom, {0}},
    {MODKEY, XK_Tab, view, {0}},
    {MODKEY, XK_q, killclient, {0}},
    {MODKEY, XK_t, setlayout, {.v = &layouts[0]}},
    {MODKEY, XK_f, setlayout, {.v = &layouts[2]}},
    {MODKEY, XK_m, setlayout, {.v = &layouts[1]}},
    {MODKEY, XK_BackSpace, setlayout, {0}},
    {MODKEY | ShiftMask, XK_space, togglefloating, {0}},
    {MODKEY, XK_comma, focusmon, {.i = -1}},
    {MODKEY, XK_period, focusmon, {.i = +1}},
    {MODKEY | ShiftMask, XK_comma, tagmon, {.i = -1}},
    {MODKEY | ShiftMask, XK_period, tagmon, {.i = +1}},
    TAGKEYS(MODKEY, XK_1, BROWSERTAG, BROWSERTAG),
    TAGKEYS(MODKEY, XK_2, BROWSERTAG2_FIXED, BROWSERTAG2),
    TAGKEYS(MODKEY, XK_3, CODINGTAG_FIXED, CODINGTAG),
    TAGKEYS(MODKEY, XK_4, PDFTAG_FIXED, PDFTAG),
    TAGKEYS(MODKEY, XK_5, ETCTAG, ETCTAG),
    TAGKEYS(0, XK_F2, BROWSERTAG2_TOGGLE, BROWSERTAG2),
    TAGKEYS(0, XK_F3, CODINGTAG_TOGGLE, CODINGTAG),
    TAGKEYS(0, XK_F4, PDFTAG_TOGGLE, PDFTAG),
    TAGKEYS(0, XK_F6, DUMPSTERTAG, DUMPSTERTAG),
    TAGKEYS(0, XK_F7, CHATTAG, CHATTAG),
    TAGKEYS(0, XK_F8, MOVIETAG, MOVIETAG),
    TAGKEYS(0, XK_F9, TERMINALTAG, TERMINALTAG),
    TAGKEYS(0, XK_F10, MUSICTAG, MUSICTAG),
    TAGKEYS(0, XK_F12, JOURNALTAG, JOURNALTAG),
    {MODKEY, XK_0, view, {.ui = ~0}},
    {MODKEY | ShiftMask, XK_q, quit, {0}},
    {MODKEY, XK_dead_acute, spawn, {.v = emojicmd}},
    {MODKEY, XK_acute, spawn, {.v = emojicmd}},
    {MODKEY, XK_equal, spawn, {.v = emojicmd}},
    {MODKEY, XK_F1, spawn, {.v = mansplaincmd}},
    {MODKEY, XK_Insert, spawn, {.v = clipcmd}},
    {MODKEY, XK_ssharp, spawn, {.v = showclipboardcmd}},
    {MODKEY, XK_minus, spawn, {.v = showclipboardcmd}},
    {0, 0xff61, spawn, {.v = screenshotcmd}},
    {MODKEY, 0xff61, spawn, {.v = screenshotselectioncmd}},
    {0, 0x1008ff13, spawn, {.v = raisevolumecmd}},
    {0, 0x1008ff13, spawn, {.v = printvolumecmd}},
    {0, 0x1008ff11, spawn, {.v = lowervolumecmd}},
    {0, 0x1008ff11, spawn, {.v = printvolumecmd}},
    {0, 0x1008ff12, spawn, {.v = mutecmd}},
    {0, 0x1008ff12, spawn, {.v = printvolumecmd}},
    {MODKEY, 0xffab, spawn, {.v = raisevolumecmd}},
    {MODKEY, 0xffab, spawn, {.v = printvolumecmd}},
    {MODKEY, 0xffad, spawn, {.v = lowervolumecmd}},
    {MODKEY, 0xffad, spawn, {.v = printvolumecmd}},
    {MODKEY, 0xff9e, spawn, {.v = mutecmd}},
    {MODKEY, 0xff9e, spawn, {.v = printvolumecmd}},
    {0, 0x1008ff17, spawn, {.v = playnextcmd}},
    {0, 0x1008ff14, spawn, {.v = playpausecmd}},
    {0, 0x1008ff16, spawn, {.v = playpreviouscmd}},
    {MODKEY, 0xffaa, spawn, {.v = playnextcmd}},
    {MODKEY, 0xff9c, spawn, {.v = playpausecmd}},
    {MODKEY, 0xffaf, spawn, {.v = playpreviouscmd}},
    {MODKEY, XK_a, spawn, {.v = tmuxcmd}},
    {MODKEY | ShiftMask, XK_w, spawn, {.v = backdropcmd}},
    {MODKEY, XK_w, spawn, {.v = nowallpapercmd}},
    {MODKEY, XK_c, spawn, {.v = connect_setubal}},
    {MODKEY | ShiftMask, XK_c, spawn, {.v = disconnect_setubal}},
    {MODKEY, XK_u, spawn, {.v = urlcmd}},
    {0, 0x1008ff02, spawn, {.v = brightnessupcmd}},
    {0, 0x1008ff03, spawn, {.v = brightnessdowncmd}},
    {MODKEY | ShiftMask, XK_Escape, spawn, {.v = suspendcmd}},
    {MODKEY, XK_s, spawn, {.v = sshaddcmd}},
    {MODKEY | ShiftMask, XK_s, spawn, {.v = sshdelcmd}},
    {MODKEY, XK_Escape, spawn, {.v = sshdelcmd}},
    {MODKEY, XK_Escape, spawn, {.v = lockcmd}},
    {MODKEY, XK_o, spawn, {.v = abridorcmd}},
    {0, XK_F12, spawn, {.v = journalctlcmd}},
    {MODKEY | ShiftMask, XK_Delete, setlayout, {.v = &layouts[0]}},
    {MODKEY | ShiftMask, XK_Delete, setmfact, {.f = 1.0f + DEFAULT_MFACT}},
    {MODKEY | ShiftMask, XK_Delete, incnmaster, {.i = INT_MIN}},
    {MODKEY | ShiftMask, XK_Delete, incnmaster, {.i = +1}},
    {MODKEY, XK_r, spawn, {.v = fm0cmd}},
    {MODKEY, XK_y, spawn, {.v = kittybrightcmd}}};

#define Button6 6
#define Button7 7
#define Button8 8
#define Button9 9

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle,
 * ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click  event mask button  function argument */
    {ClkLtSymbol, 0, Button1, setlayout, {.v = &layouts[0]}},
    {ClkLtSymbol, 0, Button2, setlayout, {.v = &layouts[1]}},
    {ClkLtSymbol, 0, Button3, setlayout, {.v = &layouts[2]}},
    {ClkWinTitle, 0, Button1, zoom, {0}},
    {ClkWinTitle, 0, Button2, setmfact, {.f = -0.02}},
    {ClkWinTitle, 0, Button3, setmfact, {.f = +0.02}},
    {ClkStatusText, 0, Button1, zoom, {0}},
    {ClkStatusText, 0, Button2, spawn, {.v = strawberry9menucmd}},
    {ClkStatusText, 0, Button3, spawn, {.v = main9menucmd}},
    {ClkClientWin, MODKEY, Button1, movemouse, {0}},
    {ClkClientWin, MODKEY, Button2, togglefloating, {0}},
    {ClkClientWin, MODKEY, Button3, resizemouse, {0}},
    {ClkClientWin, MODKEY, Button4, setmfact, {.f = +0.02}},
    {ClkClientWin, MODKEY, Button5, setmfact, {.f = -0.02}},
    {ClkTagBar, 0, Button1, viewortoggleview, {0}},
    {ClkTagBar, 0, Button2, toggleview, {0}},
    {ClkTagBar, 0, Button3, view, {0}}};

void focusmaster()
{
	Client* c = nexttiled(selmon->clients);
	if (!selmon->sel || selmon->lt[selmon->sellt] != &layouts[0]) return;
	focus(c);
}

void viewortoggleview(const Arg* arg)
{
	if (arg->ui & NONTOGGABLE_TAGS) {
		unsigned int newtagset = selmon->tagset[selmon->seltags] &
		                             TAGMASK & ~NONTOGGABLE_TAGS |
		                         arg->ui & TAGMASK;
		selmon->seltags ^= 1;
		selmon->tagset[selmon->seltags] = newtagset;
	} else {
		selmon->tagset[selmon->seltags] ^= arg->ui & TAGMASK;
	}
	focus(NULL);
	arrange(selmon);
	if (arg->ui & NONTOGGABLE_TAGS) focusmaster();
}

void activatetag(const Arg* arg)
{
	selmon->tagset[selmon->seltags] |= arg->ui & TAGMASK;
	focus(NULL);
	arrange(selmon);
	if (arg->ui & NONTOGGABLE_TAGS) focusmaster();
}
