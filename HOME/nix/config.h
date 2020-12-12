static int topbar = 1;
static const char *fonts[] = {
	"PragmataPro:pixelsize=18",
	"JoyPixels:pixelsize=40"
};
static const char *prompt = NULL;
static const char *colors[SchemeLast][2] = {
	/*     fg         bg       */
	[SchemeNorm] = { "#e5e6e6", "#171717" },
	[SchemeSel] = { "#e5e6e6", "#0f3a4b" },
	[SchemeOut] = { "#e3c472", "#171717" },
};
static unsigned int lines = 0;
/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";
