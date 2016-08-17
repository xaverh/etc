-- Diese Datei wurde durch notion generiert. Nicht editieren.
return {
    [0] = {
        ["name"] = "WScreen",
        ["type"] = "WScreen",
        ["managed"] = {
            [1] = {
                ["name"] = "WGroupWS",
                ["managed"] = {
                    [1] = {
                        ["name"] = "WTiling",
                        ["bottom"] = true,
                        ["level"] = 0,
                        ["geom"] = {
                            ["y"] = 0,
                            ["h"] = 900,
                            ["w"] = 1440,
                            ["x"] = 0,
                        },
                        ["split_tree"] = {
                            ["tls"] = 720,
                            ["tl"] = {
                                ["tls"] = 885,
                                ["tl"] = {
                                    ["type"] = "WSplitRegion",
                                    ["regparams"] = {
                                        ["managed"] = {
                                        },
                                        ["name"] = "Notion WFrame",
                                        ["type"] = "WFrame",
                                        ["mode"] = 1,
                                    },
                                },
                                ["dir"] = "vertical",
                                ["brs"] = 15,
                                ["type"] = "WSplitSplit",
                                ["br"] = {
                                    ["type"] = "WSplitST",
                                },
                            },
                            ["dir"] = "horizontal",
                            ["brs"] = 720,
                            ["type"] = "WSplitSplit",
                            ["br"] = {
                                ["type"] = "WSplitRegion",
                                ["regparams"] = {
                                    ["managed"] = {
                                        [1] = {
                                            ["name"] = "WGroupCW",
                                            ["sizepolicy"] = "full",
                                            ["level"] = 0,
                                            ["geom"] = {
                                                ["y"] = 16,
                                                ["h"] = 883,
                                                ["w"] = 718,
                                                ["x"] = 1,
                                            },
                                            ["managed"] = {
                                                [1] = {
                                                    ["level"] = 1,
                                                    ["bottom"] = true,
                                                    ["sizepolicy"] = "full",
                                                    ["geom"] = {
                                                        ["y"] = 0,
                                                        ["h"] = 883,
                                                        ["w"] = 718,
                                                        ["x"] = 0,
                                                    },
                                                    ["checkcode"] = 1,
                                                    ["type"] = "WClientWin",
                                                    ["windowid"] = 10485788,
                                                },
                                            },
                                            ["type"] = "WGroupCW",
                                            ["switchto"] = true,
                                        },
                                    },
                                    ["name"] = "Notion WFrame<1>",
                                    ["type"] = "WFrame",
                                    ["mode"] = 1,
                                },
                            },
                        },
                        ["type"] = "WTiling",
                        ["sizepolicy"] = "full",
                    },
                },
                ["initial_outputs"] = {
                    [1] = "LVDS-0",
                },
                ["level"] = 0,
                ["geom"] = {
                    ["y"] = 0,
                    ["h"] = 900,
                    ["w"] = 1440,
                    ["x"] = 0,
                },
                ["hidden"] = true,
                ["type"] = "WGroupWS",
                ["sizepolicy"] = "full",
            },
            [2] = {
                ["name"] = "WGroupCW<1>",
                ["sizepolicy"] = "full",
                ["level"] = 0,
                ["geom"] = {
                    ["y"] = 0,
                    ["h"] = 900,
                    ["w"] = 1440,
                    ["x"] = 0,
                },
                ["managed"] = {
                    [1] = {
                        ["level"] = 1,
                        ["bottom"] = true,
                        ["sizepolicy"] = "full",
                        ["geom"] = {
                            ["y"] = 4,
                            ["h"] = 892,
                            ["w"] = 1438,
                            ["x"] = 1,
                        },
                        ["checkcode"] = 2,
                        ["type"] = "WClientWin",
                        ["windowid"] = 12582918,
                    },
                },
                ["type"] = "WGroupCW",
                ["switchto"] = true,
            },
            [3] = {
                ["level"] = 1,
                ["initial_outputs"] = {
                },
                ["hidden"] = true,
                ["pseudomodal"] = true,
                ["managed"] = {
                    [1] = {
                        ["bottom"] = true,
                        ["name"] = "*scratchpad*",
                        ["sizepolicy"] = "free_glue",
                        ["level"] = 1025,
                        ["geom"] = {
                            ["y"] = 210,
                            ["h"] = 480,
                            ["w"] = 640,
                            ["x"] = 400,
                        },
                        ["managed"] = {
                        },
                        ["type"] = "WFrame",
                        ["mode"] = 0,
                    },
                },
                ["unnumbered"] = true,
                ["geom"] = {
                    ["y"] = 0,
                    ["h"] = 900,
                    ["w"] = 1440,
                    ["x"] = 0,
                },
                ["name"] = "*scratchws*",
                ["type"] = "WGroupWS",
                ["sizepolicy"] = "full",
            },
        },
    },
}

