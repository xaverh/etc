--
-- Options to get some programs work more nicely (or at all)
--

defwinprop {
    class = "Xpdf",
    instance = "openDialog_popup",
    ignore_cfgrq = true,
}

-- Put all dockapps in the statusbar's systray, also adding the missing size
-- hints necessary for this to work.
defwinprop {
    is_dockapp = true,
    statusbar = "systray",
    max_size = { w = 64, h = 64},
    min_size = { w = 64, h = 64},
}

-- Make an exception for Docker, which sets correct size hints.
defwinprop {
    is_dockapp = true,
    class = "Docker",
    statusbar = "systray",
}

defwinprop {
    class = "st-256color",
    instance = "st-256color",
    ignore_resizeinc = true
}

defwinprop {
    class = "URxvt",
    instance = "urxvt",
    ignore_resizeinc = true
}

-- https://sourceforge.net/tracker/?func=detail&aid=3471910&group_id=314802&atid=1324528
-- defwinprop {
--     class = "Gimp",
--     acrobatic = true,
-- }

-- InteiilJ IDEA - I wonder whether we should do this for *all*
-- sun-awt-X11-XWindowPeer windows.
defwinprop {
    class = "jetbrains-idea-ce",
    instance = "sun-awt-X11-XWindowPeer",
    transient_mode = "current",
}
defwinprop {
    class = "jetbrains-idea-ce",
    instance = "sun-awt-X11-XDialogPeer",
    transient_mode = "current",
}
defwinprop {
    class = "jetbrains-idea",
    instance = "sun-awt-X11-XWindowPeer",
    transient_mode = "current",
}
defwinprop {
    class = "jetbrains-idea",
    instance = "sun-awt-X11-XDialogPeer",
    transient_mode = "current",
}

defwinprop{
    class = "SshAskpass",
    interface = "ssh-askpass",
    float = true
}

-- Define some additional title shortening rules to use when the full title
-- doesn't fit in the available space. The first-defined matching rule that
-- succeeds in making the title short enough is used.
ioncore.defshortening("(.*) - Mozilla(<[0-9]+>)", "$1$2$|$1$<...$2")
ioncore.defshortening("(.*) - Mozilla", "$1$|$1$<...")
ioncore.defshortening("(.*) Developer Edition", "$1$|$1$<...")
ioncore.defshortening("XMMS - (.*)", "$1$|...$>$1")
ioncore.defshortening("[^:]+: (.*)(<[0-9]+>)", "$1$2$|$1$<...$2")
ioncore.defshortening("[^:]+: (.*)", "$1$|$1$<...")
ioncore.defshortening("(.*)(<[0-9]+>)", "$1$2$|$1$<...$2")
ioncore.defshortening("(.*)", "$1$|$1$<...")
