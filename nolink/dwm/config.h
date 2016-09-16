/* See LICENSE file for copyright and license details. */

/* appearance */
static const char *fonts[] = {
	"HelveticaNeue:size=10",
	"NotoEmoji:size=11"
};
static const char dmenufont[]       = "HelveticaNeue:size=11";
static const char normbordercolor[] = "#333333";
static const char normbgcolor[]     = "#1e1e1e";
static const char normfgcolor[]     = "#d4d4d4";
static const char selbordercolor[]  = "#81d8d0";
static const char selbgcolor[]      = "#1e1e1e";
static const char selfgcolor[]      = "#8b4e86";
static const unsigned int borderpx  = 3;        /* border pixel of windows */
static const unsigned int snap      = 10;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const unsigned int gappx     = 20;       /* gap pixel between windows (part of the uselessgaps patch) */

/* tagging */
static const char *tags[] = { "🏄", "🏢", "🎸", "📖", "📧", "🍺" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Code",     NULL,       NULL,       1 << 1,       0,           -1 },
	{ "Zathura",  NULL,       NULL,       1 << 3,       1,           -1 },
	{ "Firefox",  NULL,       NULL,       1 << 0,       0,           -1 }
};

/* layout(s) */
static const float mfact     = 0.618034f; /* factor of master area size [0.05f..0.95f] */
static const int nmaster     = 1;      /* number of clients in master area */
static const int resizehints = 1;      /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "🔪",      tile },    /* first entry is default */
	{ "☁️",      NULL },    /* no layout function means floating behavior */
	{ "👓",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
// static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *dmenucmd[] = {"/home/xha/Software/dotfiles/bin/dmenu_recent", NULL};
static const char *termcmd[]  = { "urxvtcd", NULL };
static const char *filemanagercmd[]  = { "thunar", NULL };
static const char *playpausecmd[] = {"/home/xha/Software/dotfiles/bin/hey_dj.sh","PlayPause", NULL};
static const char *playnextcmd[] = {"/home/xha/Software/dotfiles/bin/hey_dj.sh","Next", NULL};
static const char *playpreviouscmd[] = {"/home/xha/Software/dotfiles/bin/hey_dj.sh","Previous", NULL};
static const char *raisevolumecmd[]    = { "amixer", "-D", "pulse", "set", "Master", "unmute", "5%+", "-q", NULL };
static const char *lowervolumecmd[]  = { "amixer", "-D", "pulse", "set", "Master", "unmute", "5%-", "-q", NULL };
static const char *mutecmd[]  = { "amixer", "-D", "pulse", "set", "Master", "toggle", "-q", NULL };

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_space,  spawn,          {.v = dmenucmd } },
	{ MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_e,      spawn,          {.v = filemanagercmd } },
	{ 0,                            0x1008ff13,spawn,          {.v = raisevolumecmd } },
	{ 0,                            0x1008ff11,spawn,          {.v = lowervolumecmd } },
	{ 0,                            0x1008ff12,spawn,          {.v = mutecmd } },
	{ 0,                            0x1008ff17,spawn,          {.v = playnextcmd } },
	{ 0,                            0x1008ff14,spawn,          {.v = playpausecmd } },
	{ 0,                            0x1008ff16,spawn,          {.v = playpreviouscmd } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY|ShiftMask,             XK_Return, zoom,           {0} },
	{ MODKEY|ShiftMask,             XK_j,      pushdown,       {0} },
    { MODKEY|ShiftMask,             XK_k,      pushup,         {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY,                       XK_q,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY|ShiftMask,             XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_f,      togglefloating, {0} },
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
	// TAGKEYS(                        XK_7,                      6)
	// TAGKEYS(                        XK_8,                      7)
	// TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
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

