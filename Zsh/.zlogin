export TERMINAL=termite
export XCURSOR_THEME="DMZ-White"
# export MOZ_ENABLE_WAYLAND=1

export YS_W="#f9f8f4" # Floral White, R=249, G=248, B=244
export YS_W_80="#edece8" # Grey 90%, R=237, G=236, B=232
export YS_R="#e32791" # Deep Cerise, R=227, G=39, B=145
export YS_G="#488432" # La Palma, R=72, G=132, B=50
export YS_Y="#a25d0e" # Golden Brown, R=162, G=93, B=14
export YS_B="#2c65b5" # Cerulean Blue, R=44, G=101, B=181
export YS_M="#b062a7" # Violet Blue, R=176, G=98, B=167
export YS_C="#27bbbe" # Light Sea Green, R=39, G=187, B=190
export YS_K="#999999" # Grey 60%, R=153, G=153, B=153
export YS_B_W="#b8b8b8" # Grey 70%, R=184, G=184, B=184
export YS_B_R="#9f1b66" # Jazzberry Jam, R=159, G=27, B=102
export YS_B_G="#325d23" # Parsley, R=50, G=93, B=35
export YS_B_Y="#71410a" # Raw Umber, R=113, G=65, B=10
export YS_B_B="#1f477f" # Bahama Blue, R=31, G=71, B=127
export YS_B_M="#7b4474" # Eminence, R=123, G=68, B=116
export YS_B_C="#1b8486" # Atoll, R=27, G=132, B=134
export YS_B_K="#424242" # Grey 20%, R=66, G=66, B=66
export QI_W="#1e1e1e" # Grey 10%, R=30, G=30, B=30
export QI_W_80="#333333" # Grey 20%, R=51, G=51, B=51
export QI_R="#e32791" # Deep Cerise, R=227, G=39, B=145
export QI_G="#30c798" # Shamrock, R=48, G=199, B=152
export QI_Y="#e3c472" # Chenin, R=227, G=196, B=114
export QI_B="#6796e6" # Cornflower Blue, R=103, G=150, B=230
export QI_M="#e59fdf" # Plum, R=229, G=159, B=223
export QI_C="#81d8d0" # Riptide, R=129, G=216, B=208
export QI_K="#969696" # Grey 60%, R=150, G=150, B=150
export QI_B_W="#515151" # Grey 30%, R=81, G=81, B=81
export QI_B_R="#e466ad" # Hot Pink, R=228, G=102, B=173
export QI_B_G="#6cd1b2" # Medium Aquamarine, R=108, G=209, B=178
export QI_B_Y="#e4cf98" # Double Colonial White, R=228, G=207, B=152
export QI_B_B="#91b0e6" # Jordy Blue, R=145, G=181, B=230
export QI_B_M="#e5b6e1" # French Lilac, R=229, G=182, B=225
export QI_B_C="#a2dcd7" # Sinbad, R=162, G=220, B=215
export QI_B_K="#e5e6e6" # Grey 90%, R=229, G=230, B=230
export CURSOR_COLOR="#20bbfc" # Deep Sky Blue, R=32, G=187, B=252


# Following automatically calls "startx" when you login:
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp >| ~/.xorg.log 2>&1

# exec sway
