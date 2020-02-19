#define YS_W "f9f8f4" // Floral White, R=249, G=248, B=244
#define YS_W_80 "edece8" // Grey 90%, R=237, G=236, B=232
#define YS_R "e32791" // Deep Cerise, R=227, G=39, B=145
#define YS_G "488432" // La Palma, R=72, G=132, B=50
#define YS_Y "a25d0e" // Golden Brown, R=162, G=93, B=14
#define YS_B "2c65b5" // Cerulean Blue, R=44, G=101, B=181
#define YS_M "b062a7" // Violet Blue, R=176, G=98, B=167
#define YS_C "27bbbe" // Light Sea Green, R=39, G=187, B=190
#define YS_K "999999" // Grey 60%, R=153, G=153, B=153
#define YS_B_W "b8b8b8" // Grey 70%, R=184, G=184, B=184
#define YS_B_R "9f1b66" // Jazzberry Jam, R=159, G=27, B=102
#define YS_B_G "325d23" // Parsley, R=50, G=93, B=35
#define YS_B_Y "71410a" // Raw Umber, R=113, G=65, B=10
#define YS_B_B "1f477f" // Bahama Blue, R=31, G=71, B=127
#define YS_B_M "7b4474" // Eminence, R=123, G=68, B=116
#define YS_B_C "1b8486" // Atoll, R=27, G=132, B=134
#define YS_B_K "424242" // Grey 20%, R=66, G=66, B=66
#define QI_W "1e1e1e" // Grey 10%, R=30, G=30, B=30
#define QI_W_80 "333333" // Grey 20%, R=51, G=51, B=51
#define QI_R "e32791" // Deep Cerise, R=227, G=39, B=145
#define QI_G "30c798" // Shamrock, R=48, G=199, B=152
#define QI_Y "e3c472" // Chenin, R=227, G=196, B=114
#define QI_B "6796e6" // Cornflower Blue, R=103, G=150, B=230
#define QI_M "e59fdf" // Plum, R=229, G=159, B=223
#define QI_C "81d8d0" // Riptide, R=129, G=216, B=208
#define QI_K "969696" // Grey 60%, R=150, G=150, B=150
#define QI_B_W "515151" // Grey 30%, R=81, G=81, B=81
#define QI_B_R "e466ad" // Hot Pink, R=228, G=102, B=173
#define QI_B_G "6cd1b2" // Medium Aquamarine, R=108, G=209, B=178
#define QI_B_Y "e4cf98" // Double Colonial White, R=228, G=207, B=152
#define QI_B_B "91b0e6" // Jordy Blue, R=145, G=181, B=230
#define QI_B_M "e5b6e1" // French Lilac, R=229, G=182, B=225
#define QI_B_C "a2dcd7" // Sinbad, R=162, G=220, B=215
#define QI_B_K "e5e6e6" // Grey 90%, R=229, G=230, B=230
#define CURSOR_COLOR "20bbfc" // Deep Sky Blue, R=32, G=187, B=252


#if MACHINE_ID == 0xb2d5751b41ed4edc // airolo
#define HAS_TEN_KEYS
// ISO 3166 Country codes, US=840, DE=276
#define KBD_LAYOUT 840
#elif MACHINE_ID == 0x1
#define HAS_SCR_BACKLIGHT_KEYS
#define HAS_KBD_BACKLIGHT_KEYS
#define HAS_VOLUME_KEYS
#define HAS_MULTIMEDIA_KEYS
#define KBD_LAYOUT 276
#define IS_MACINTOSH
#endif

#if !defined(KBD_LAYOUT)
#define KBD_LAYOUT 276
#endif

#define MAINFONT "IBM Plex Sans:size=9"

/* appearance */
static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { MAINFONT };
static const char dmenufont[]       = MAINFONT;
static const char col_cyan[]        = "#005577";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { "#"YS_B_W, "#"QI_W, "#"YS_B_K },
	[SchemeSel]  = { "#"QI_B_K, col_cyan,  "#"CURSOR_COLOR  },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Google-chrome",  NULL,       NULL,       5,            0,           -1 },
	{ "Code",           NULL,       NULL,       6,            0,           -1 },
	{ "Journalctl",     NULL,       NULL,     256,            0,           -1 },
	{ "strawberry",     NULL,       NULL,      64,            0,            0 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

void focusmaster()
{
	Client* c = nexttiled(selmon->clients);
	if (!selmon->sel || selmon->lt[selmon->sellt] != &layouts[0]) return;
	focus(c);
}

void view_and_focus(const Arg* arg)
{
	view(arg);
	focusmaster();
}

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view_and_focus, {.ui = 1 << TAG} }, \
	{ MODKEY|Mod1Mask,              KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/usr/bin/zsh", "-c", cmd, NULL } }

#define TERMINAL "alacritty"

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = {"dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", "#"QI_W, "-nf", "#"YS_B_W, "-sb", col_cyan, "-sf", "#"QI_B_K, NULL };
static const char *termcmd[]  = { TERMINAL, NULL };
static const char *screenshotcmd[] = { "flameshot", "full", "-c", "-p", "/tmp", NULL };
static const char *screenshotselcmd[] = { "flameshot", "gui", "-p", "/tmp", NULL };
static const char raisevolcmd[] = "pactl set-sink-mute @DEFAULT_SINK@ false; [[ $(pacmd list-sinks | grep -A 15 '* index' | awk '/volume: front/{gsub(\"%\",\"\",$5); print $5 }') -lt 95 ]] && pactl set-sink-volume @DEFAULT_SINK@ +5% || pactl set-sink-volume @DEFAULT_SINK@ 65536; dunstify -r 7549 \"🔊 $(pacmd list-sinks | grep -A 15 '* index' | awk '/volume: front/{gsub(\"%\",\"\",$5); print $5 }') %\"";
static const char lowervolcmd[] = "pactl set-sink-mute @DEFAULT_SINK@ false; pactl set-sink-volume @DEFAULT_SINK@ -5%; dunstify -r 7549 \"🔉 $(pacmd list-sinks | grep -A 15 '* index' | awk '/volume: front/{gsub(\"%\",\"\",$5); print $5 }') %\"";

void mute()
{
	spawn(&(Arg){.v = (char*[]){ "pactl", "set-sink-mute", "@DEFAULT_SINK@", "true", NULL }});
	spawn(&(Arg){.v = (char*[]){ "dunstify", "-r", "7549", "🔇 mute", NULL }});
}

static _Bool is_ysgrifennwr = 0;
void toggle_theme()
{
	if (is_ysgrifennwr) {
		spawn(&(Arg){.v = (char*[]){ "sed", "-i", "s/"YS_K"/"QI_K"/;s/"YS_R"/"QI_R"/;s/"YS_G"/"QI_G"/;s/"YS_B"/"QI_B"/;s/"YS_C"/"QI_C"/;s/"YS_M"/"QI_M"/;s/"YS_Y"/"QI_Y"/;s/"YS_W"/"QI_W"/;s/"YS_B_K"/"QI_B_K"/;s/"YS_B_R"/"QI_B_R"/;s/"YS_B_G"/"QI_B_G"/;s/"YS_B_B"/"QI_B_B"/;s/"YS_B_C"/"QI_B_C"/;s/"YS_B_M"/"QI_B_M"/;s/"YS_B_Y"/"QI_B_Y"/;s/"YS_B_W"/"QI_B_W"/;s/"YS_W_80"/"QI_W_80"/", HOME"/.config/alacritty/alacritty.yml", HOME"/.config/rofi/config.rasi", HOME"/.Xdefaults", HOME"/.config/dunst/dunstrc", NULL }});
		spawn(&(Arg){.v = (char*[]){ "sed", "-i", "s/Ysgrifennwr/Qillqaq/", HOME"/.config/Code/User/settings.json", NULL }});
	} else {
		spawn(&(Arg){.v = (char*[]){ "sed", "-i", "s/"QI_K"/"YS_K"/;s/"QI_R"/"YS_R"/;s/"QI_G"/"YS_G"/;s/"QI_B"/"YS_B"/;s/"QI_C"/"YS_C"/;s/"QI_M"/"YS_M"/;s/"QI_Y"/"YS_Y"/;s/"QI_W"/"YS_W"/;s/"QI_B_K"/"YS_B_K"/;s/"QI_B_R"/"YS_B_R"/;s/"QI_B_G"/"YS_B_G"/;s/"QI_B_B"/"YS_B_B"/;s/"QI_B_C"/"YS_B_C"/;s/"QI_B_M"/"YS_B_M"/;s/"QI_B_Y"/"YS_B_Y"/;s/"QI_B_W"/"YS_B_W"/;s/"QI_W_80"/"YS_W_80"/", HOME"/.config/alacritty/alacritty.yml", HOME"/.config/rofi/config.rasi", HOME"/.Xdefaults", HOME"/.config/dunst/dunstrc", NULL }});
		spawn(&(Arg){.v = (char*[]){ "sed", "-i", "s/Qillqaq/Ysgrifennwr/", HOME"/.config/Code/User/settings.json", NULL }});
	}
	is_ysgrifennwr = !is_ysgrifennwr;
	spawn(&(Arg){.v = (char*[]){ "pkill", "-x", "dunst", NULL }});
}

// int r = rand();
// int length = snprintf(NULL, 0, "%d", r);
// char* str = malloc(++length);
// snprintf(str, length, "%x", r);
// free(str);

// static const char* main9menucmd[] = { "9menu", "-font", FONT_9MENU, "-shell", SHELL, "-bg", col_gray1, "-fg", col_gray3, "-popup", "-teleport", "9menu:false", "---:true", "Connect Set\372bal:bluetoothctl connect 88:C6:26:F4:8A:90", "Disconnect Set\372bal:bluetoothctl disconnect 88:C6:26:F4:8A:90", "---:true", "Screenshot (full):flameshot full -p /tmp -c -d 10", "Screenshot (selection):flameshot gui -p /tmp -d 10", "---:true", "Add SSH key 'xaver@hellauer.bayern (ED25519)':" SSHADDKEYCMD, "Remove all SSH keys:/usr/bin/ssh-add -D", "---:true", "Quit:9menu -font '" FONT_9MENU "' -shell " SHELL " -bg '" COL_GRAY1 "' -fg '" COL_GRAY3 "' -popup -teleport -label 'Quit?' 'Quit?:true' 'Lock " "screen:i3lock -c \"" COL_GRAY1 "\"' 'Restart dwm:killall -USR1 dwm' " "'---:true' " "'Suspend:systemctl suspend' " "'Reboot:systemctl reboot' " "'Poweroff:systemctl poweroff'" " '---:true'" " 'Cancel:false'", "---:true", "Cancel:false", NULL};

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_space,  spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_plus,   incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_minus,  incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.01} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.01} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_BackSpace,setlayout,      {0} },
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
	{ MODKEY|ShiftMask,             XK_Escape, quit,           {0} },
	{ MODKEY,			XK_o,      spawn,          {.v = (const char*[]){"abridor.lua", NULL}}},
	{ 0,			        0x1008ff4b,spawn,          {.v = screenshotcmd}},
	{ 0,			        XK_Print,  spawn,          {.v = screenshotcmd}},
	{ MODKEY,			0x1008ff4b,spawn,          {.v = screenshotselcmd}},
	{ MODKEY,			XK_Print,  spawn,          {.v = screenshotselcmd}},
	{ MODKEY,			XK_e,      spawn,          SHCMD("awk 'BEGIN {FS=\"# \"} /;[[:blank:]]fully-qualified/ { sub(\" E[[:digit:]]*.[[:digit:]]* \", \"\\t\", $2); print $2 }' /usr/share/unicode/emoji/emoji-test.txt | rofi -dmenu -i -p '¯\\_(ツ)_/¯' -no-custom | awk '{printf $1}' | xsel -ib") },
	{ MODKEY,			XK_u,      spawn,          SHCMD("xdg-open $(\\ls -1Qt ${CM_DIR}/clipmenu.5.${USER}/*\\ * | xargs awk 1 | grep --only-matching --perl-regexp \"http(s?):\\/\\/[^ \\\"\\(\\)\\<\\>\\]]*\" | uniq | dmenu -l 10 -i -p 'open URL')") },
	{ MODKEY,			XK_i,      spawn,          {.v = (const char*[]){"clipmenu", "-p", "clipboard", NULL}}},
	{ MODKEY,			XK_Escape, spawn,          {.v = (const char*[]){"slock", "ssh-add", "-D", NULL}}},
	{ MODKEY,			XK_F9,     spawn,          {.v = (const char*[]){"bluetoothctl", "connect", "88:C6:26:F4:8A:90", NULL}}},
	{ MODKEY|ShiftMask,		XK_F9,     spawn,          {.v = (const char*[]){"bluetoothctl", "disconnect", "88:C6:26:F4:8A:90", NULL}}},
	{ MODKEY,			XK_F10,    spawn,          {.v = (const char*[]){"toupeira.lua", NULL}}},
	{ MODKEY,			XK_F12,    spawn,          {.v = (const char*[]){TERMINAL, "--class", "Journalctl,Journalctl", "-e", "journalctl", "-b", "-f", "-n", "1000", NULL}}},
	{ MODKEY,			XK_F1,     spawn,          SHCMD("mupdf-gl =(man -Tpdf $(apropos . | rofi -dmenu -i -p mansplain | awk '{gsub(/[()]/,\"\"); print $2\" \"$1}'))")},
#if defined(HAS_VOLUME_KEYS)
	{ 0,			        0x1008ff13,spawn,          SHCMD(raisevolcmd)},
	{ 0,			        0x1008ff11,spawn,          SHCMD(lowervolcmd)},
	{ 0,			        0x1008ff12,mute,           {0}},
	{ MODKEY,			0x1008ff13,spawn,          {.v = (const char*[]){"strawberry", "--volume-up", NULL}}},
	{ MODKEY,			0x1008ff11,spawn,          {.v = (const char*[]){"strawberry", "--volume-down", NULL}}},
	{ MODKEY,			0x1008ff12,spawn,          {.v = (const char*[]){"strawberry", "-v", "0", NULL}}},
#elif defined (HAS_TEN_KEYS)
	{ MODKEY,		        XK_KP_Add, spawn,          SHCMD(raisevolcmd)},
	{ MODKEY,		        XK_KP_Subtract,spawn,      SHCMD(lowervolcmd)},
	{ MODKEY,		        XK_KP_Insert,mute,         {0}},
#endif
	{ 0,				0x1008ff14,spawn,          {.v = (const char*[]){"strawberry", "-t", NULL}}},
	{ 0,				0x1008ff17,spawn,          {.v = (const char*[]){"strawberry", "-f", NULL}}},
	{ 0,				0x1008ff16,spawn,          {.v = (const char*[]){"strawberry", "--restart-or-previous", NULL}}},
	{ MODKEY,			0x1008ff14,spawn,          {.v = (const char*[]){"fm0.lua", NULL}}},
	{ MODKEY,			0x1008ff17,spawn,          {.v = (const char*[]){"strawberry", "--seek-by", "1", NULL}}},
	{ MODKEY,			0x1008ff16,spawn,          {.v = (const char*[]){"strawberry", "--seek-by", "-1", NULL}}},
#if defined (HAS_KBD_BACKLIGHT_KEYS)
	{ 0,			        0x1008ff05,spawn,          SHCMD("maxbrightness=$(</sys/class/leds/smc::kbd_backlight/max_brightness); brightness=$(</sys/class/leds/smc::kbd_backlight/brightness) ; new_brightness=$(($maxbrightness/10+$brightness)) ; actual_brightness=$(($new_brightness < $maxbrightness ? $new_brightness : $maxbrightness)) ; echo $actual_brightness > /sys/class/leds/smc::kbd_backlight/brightness && dunstify -r 8754 \"⌨️ $(( $(</sys/class/leds/smc::kbd_backlight/brightness) * 100 / $maxbrightness )) %\"")},
	{ 0,			        0x1008ff06,spawn,          SHCMD("maxbrightness=$(</sys/class/leds/smc::kbd_backlight/max_brightness); brightness=$(</sys/class/leds/smc::kbd_backlight/brightness); new_brightness=$((-$maxbrightness/10+$brightness)); actual_brightness=$(($new_brightness > 0 ? $new_brightness : 0)); echo $actual_brightness > /sys/class/leds/smc::kbd_backlight/brightness && dunstify -r 8754 \"⌨️ $(( $(</sys/class/leds/smc::kbd_backlight/brightness) * 100 / $maxbrightness )) %\"")},
#endif
#if defined (HAS_SCR_BACKLIGHT_KEYS)
	{ 0,			        0x1008ff02,spawn,          SHCMD("maxbrightness=$(</sys/class/backlight/intel_backlight/max_brightness); brightness=$(</sys/class/backlight/intel_backlight/brightness) ; new_brightness=$(($maxbrightness/20+$brightness)) ; actual_brightness=$(($new_brightness < $maxbrightness ? $new_brightness : $maxbrightness)) ; echo $actual_brightness > /sys/class/backlight/intel_backlight/brightness && dunstify -r 5994 \"🔆 $(( $(</sys/class/backlight/intel_backlight/brightness) * 100 / $maxbrightness )) %\"")},
	{ 0,			        0x1008ff03,spawn,          SHCMD("maxbrightness=$(</sys/class/backlight/intel_backlight/max_brightness); brightness=$(</sys/class/backlight/intel_backlight/brightness); new_brightness=$((-$maxbrightness/20+$brightness)); actual_brightness=$(($new_brightness > 0 ? $new_brightness : 0)); echo $actual_brightness > /sys/class/backlight/intel_backlight/brightness && dunstify -r 5994 \"🔅 $(( $(</sys/class/backlight/intel_backlight/brightness) * 100 / $maxbrightness )) %\"")},
#endif
	{ MODKEY,			XK_F6,     toggle_theme,   {0}},
	{ MODKEY,			XK_F7,     spawn,          SHCMD("xsetroot -bitmap "HOME"/.local/share/backdrops/`\\ls $HOME/.local/share/backdrops | shuf -n 1 | tr -d '\\n' | tee -a /tmp/wallpaper` `printf -- \" -fg #%06x -bg #%06x\\n\" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`")},
	{ MODKEY|Mod1Mask,		XK_F7,     spawn,          SHCMD("xsetroot -bitmap $(printf -- \"$HOME/.local/share/backdrops/$(shuf -n 1 etc/Factory/quality_backdrops.txt)\" | xargs)")},
	{ MODKEY|ShiftMask,		XK_F7,     spawn,          SHCMD("dunstify -- 🖼️ \"`tail -n 1 /tmp/wallpaper | tee -a $HOME/etc/Factory/quality_backdrops.txt`\"; sort -o $HOME/etc/Factory/quality_backdrops.txt -u $HOME/etc/Factory/quality_backdrops.txt")},
	{ MODKEY|ControlMask,		XK_F7,     spawn,          {.v = (const char*[]){"xsetroot", "-solid", "#"QI_W, NULL}}},
};

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
