static const unsigned int borderpx = 2; /* border pixel of windows */
static const unsigned int snap = 10;    /* snap pixel */
static const int showbar = 1;           /* 0 means no bar */
static const int topbar = 1;            /* 0 means bottom bar */
static const char* fonts[] = {"PragmataPro Liga:size=12", "PingFang SC:size=12",
			      "JoyPixels:size=12"};
static const char* colors[][3] = {
	/*               fg         bg         border   */
	[SchemeNorm] = {"#e5e6e6", "#171717", "#444444"},
	[SchemeSel] = {"#e5e6e6", "#0f3a4b", "#affe69"},
};

/* tagging */
static const char* tags[] = {"1", "2", "3", "4", "5", "ⅵ", "ⅶ", "ⅷ", "ⅸ"};

#define WORKSPACEMASK 0x1f

#define MAX_BACKLIGHT 4882

void classic_view(const Arg* arg)
{
	if ((arg->ui & WORKSPACEMASK) == ((selmon->tagset[selmon->seltags]) & WORKSPACEMASK))
		return;
	unsigned int newtagset = TAGMASK & ((arg->ui & WORKSPACEMASK) |
					    ((selmon->tagset[selmon->seltags]) & ~WORKSPACEMASK));
	selmon->seltags ^= 1; /* toggle sel tagset */
	if (newtagset) {
		selmon->tagset[selmon->seltags] = newtagset;
		focus(NULL);
		arrange(selmon);
	}
}

void view_scratchpad(const Arg* arg)
{
	unsigned int tag;
	if (!arg->ui) {
		unsigned int t = TAGMASK & (selmon->tagset[selmon->seltags]) & ~WORKSPACEMASK;
		tag = t ? t : (~WORKSPACEMASK & TAGMASK);
	} else {
		tag = arg->ui;
	}
	unsigned int newtagset = (selmon->tagset[selmon->seltags] ^ tag) & TAGMASK;
	if (newtagset) {
		selmon->tagset[selmon->seltags] = newtagset;
		focus(NULL);
		arrange(selmon);
	}
}

static void read_int(const char* file_name, int* i)
{
	FILE* file = fopen(file_name, "r");
	fscanf(file, "%d", i);
	fclose(file);
}

static void write_int(const char* file_name, int i)
{
	FILE* file = fopen(file_name, "w");
	fprintf(file, "%d", i);
	fclose(file);
}

void write_to_xob(int i)
{
	FILE* stream;
	stream = fopen("/var/tmp/xobpipe", "w");
	fprintf(stream, "%d\n", i);
	fclose(stream);
}

static void change_backlight(const Arg* arg)
{
	int cur;
	read_int("/sys/class/backlight/intel_backlight/actual_brightness", &cur);
	int new = cur + arg->i;
	new = (new < MAX_BACKLIGHT) ? new : MAX_BACKLIGHT;
	new = (new >= 0) ? new : 0;
	write_int("/sys/class/backlight/intel_backlight/brightness", new);
	write_to_xob(new);
}

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{"Code", "code", NULL, 1 << 1, 0, -1},
	{"Firefox", NULL, NULL, 1, 0, -1},
	{NULL, "nnn", NULL, 1 << 8, 0, -1},
};

static const float mfact = 0.5373352766f;
static const int nmaster = 1;
static const int resizehints = 1;

static void deck(Monitor* m)
{
	unsigned int i, n, h, mw, my;
	Client* c;

	for (n = 0, c = nexttiled(m->clients); c; c = nexttiled(c->next), n++)
		;
	if (n == 0) return;

	if (n > m->nmaster) {
		mw = m->nmaster ? m->ww * m->mfact : 0;
		snprintf(m->ltsymbol, sizeof m->ltsymbol, "[][%d]", n - m->nmaster);
	} else
		mw = m->ww;
	for (i = my = 0, c = nexttiled(m->clients); c; c = nexttiled(c->next), i++)
		if (i < m->nmaster) {
			h = (m->wh - my) / (MIN(n, m->nmaster) - i);
			resize(c, m->wx, m->wy + my, mw - (2 * c->bw), h - (2 * c->bw), 0);
			my += HEIGHT(c);
		} else
			resize(c, m->wx + mw, m->wy, m->ww - mw - (2 * c->bw), m->wh - (2 * c->bw),
			       0);
}

static const Layout layouts[] = {
	/* symbol     arrange function */
	{"[]=", tile}, /* first entry is default */
	{"[][]", deck},
	{"[M]", monocle},
	{"><>", NULL}, /* no layout function means floating behavior */
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY, TAG)                                                                          \
	{MODKEY, KEY, toggleview, {.ui = 1 << TAG}},                                               \
		{MODKEY | ControlMask, KEY, view_scratchpad, {.ui = 1 << TAG}},                    \
		{MODKEY | ShiftMask, KEY, tag, {.ui = 1 << TAG}},                                  \
	{                                                                                          \
		MODKEY | ControlMask | ShiftMask, KEY, toggletag,                                  \
		{                                                                                  \
			.ui = 1 << TAG                                                             \
		}                                                                                  \
	}

#define WORKSPACEKEYS(KEY, TAG)                                                                    \
	{MODKEY, KEY, classic_view, {.ui = 1 << TAG}},                                             \
		{MODKEY | ControlMask, KEY, toggleview, {.ui = 1 << TAG}},                         \
		{MODKEY | ShiftMask, KEY, tag, {.ui = 1 << TAG}},                                  \
	{                                                                                          \
		MODKEY | ControlMask | ShiftMask, KEY, toggletag,                                  \
		{                                                                                  \
			.ui = 1 << TAG                                                             \
		}                                                                                  \
	}

#define SHCMD(cmd)                                                                                 \
	{                                                                                          \
		.v = (const char*[])                                                               \
		{                                                                                  \
			"/bin/sh", "-c", cmd, NULL                         \
		}                                                                                  \
	}

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char* dmenucmd[] = {"dmenu_run", "-m", dmenumon, NULL};
static const char* termcmd[] = {"alacritty", NULL};

static Key keys[] = {
	/* modifier                     key        function        argument */
	{MODKEY, XK_p, spawn, {.v = dmenucmd}},
	{MODKEY | ShiftMask, XK_Return, spawn, {.v = termcmd}},
	{MODKEY, XK_b, togglebar, {0}},
	{MODKEY, XK_j, focusstack, {.i = +1}},
	{MODKEY, XK_k, focusstack, {.i = -1}},
	{MODKEY, XK_i, incnmaster, {.i = +1}},
	{MODKEY, XK_d, incnmaster, {.i = -1}},
	{MODKEY, XK_h, setmfact, {.f = -0.001464128844f}},
	{MODKEY, XK_l, setmfact, {.f = 0.001464128844f}},
	{MODKEY, XK_Return, zoom, {0}},
	{MODKEY, XK_Tab, view, {0}},
	{MODKEY | ShiftMask, XK_c, killclient, {0}},
	{MODKEY, XK_t, setlayout, {.v = &layouts[0]}},
	{MODKEY, XK_c, setlayout, {.v = &layouts[1]}},
	{MODKEY, XK_m, setlayout, {.v = &layouts[2]}},
	{MODKEY, XK_f, setlayout, {.v = &layouts[3]}},
	{MODKEY, XK_space, setlayout, {0}},
	{MODKEY | ShiftMask, XK_space, togglefloating, {0}},
	{MODKEY, XK_0, view, {.ui = ~0}},
	{MODKEY | ShiftMask, XK_0, tag, {.ui = ~0}},
	{MODKEY, XK_asciicircum, view_scratchpad, {.ui = 0}},
	{MODKEY, XK_comma, focusmon, {.i = -1}},
	{MODKEY, XK_period, focusmon, {.i = +1}},
	{MODKEY | ShiftMask, XK_comma, tagmon, {.i = -1}},
	{MODKEY | ShiftMask, XK_period, tagmon, {.i = +1}},
	WORKSPACEKEYS(XK_1, 0),
	WORKSPACEKEYS(XK_2, 1),
	WORKSPACEKEYS(XK_3, 2),
	WORKSPACEKEYS(XK_4, 3),
	WORKSPACEKEYS(XK_5, 4),
	TAGKEYS(XK_6, 5),
	TAGKEYS(XK_7, 6),
	TAGKEYS(XK_8, 7),
	TAGKEYS(XK_9, 8),
	{MODKEY | ShiftMask, XK_q, quit, {0}},
	{MODKEY, XK_Escape, spawn, {.v = (const char*[]){"slock", NULL}}},
	{MODKEY, XK_o, spawn, {.v = (const char*[]){"clipmenu", NULL}}},
	{MODKEY, XK_n, spawn, {.v = (const char*[]){"alacritty", "-name", "nnn", "-e", "nnn", NULL}}},
	{0, 0x1008ff03, change_backlight, {.i = -128}},
	{0, 0x1008ff02, change_backlight, {.i = 128}},
	{MODKEY, XK_ssharp, toggleview, {.ui = TAGMASK}},
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
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

/* vim: set ft=c: */