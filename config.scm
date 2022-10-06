(use-modules (gnu)
             (guix packages)
             (guix download)
             (guix utils)
             (guix build-system trivial)
             (guix gexp)
             (guix git-download)
             (guix build-system font)
             (guix build-system emacs)
             (guix build-system glib-or-gtk)
             (guix build-system gnu)
             (guix build utils)
             (guix build emacs-utils)
             ((guix licenses)
              #:prefix license:)
             ((gnu packages admin)
              #:prefix admin:)
             ((gnu packages base)
              #:prefix base:)
             ((gnu packages certs)
              #:prefix certs:)
             ((gnu packages compression)
              #:prefix compression:)
             (gnu packages dns)
             ((gnu packages elf)
              #:prefix elf:)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages groff)
             (gnu packages lua)
             (gnu packages gcc)
             (gnu packages pkg-config)
             (gnu packages glib)
             (gnu packages tex)
             (gnu packages perl)
             ((gnu packages gnupg)
              #:prefix gnupg:)
             ((gnu packages image-viewers)
              #:prefix image-viewers:)
             ((gnu packages linux)
              #:prefix linux:)
             ((gnu packages llvm)
              #:prefix llvm:)
             ((gnu packages pdf)
              #:prefix pdf:)
             (gnu packages networking)
             ((gnu packages shells)
              #:prefix shells:)
             ((gnu packages suckless)
              #:prefix suckless:)
             ((gnu packages version-control)
              #:prefix version-control:)
             ((gnu packages video)
              #:prefix video:)
             (gnu packages vim)
             ((gnu packages xdisorg)
              #:prefix xdisorg:)
             (gnu packages xorg)
             (gnu services shepherd)
             (nongnu packages linux)
             (nongnu packages messaging)
             ((nongnu packages mozilla)
              #:prefix mozilla:)
             (nongnu system linux-initrd)
             (nonguix licenses)
             (guix records)
             (rnrs io ports))
(use-service-modules cups
                     dbus
                     desktop
                     networking
                     ssh
                     xorg)

(define-record-type* <color-scheme>
                     color-scheme
                     make-color-scheme
                     color-scheme?
                     this-color-scheme
                     (name color-scheme-name)
                     (background color-scheme-background)
                     (foreground color-scheme-foreground)
                     (black color-scheme-black)
                     (red color-scheme-red)
                     (green color-scheme-green)
                     (yellow color-scheme-yellow)
                     (blue color-scheme-blue)
                     (magenta color-scheme-magenta)
                     (cyan color-scheme-cyan)
                     (white color-scheme-white)
                     (bright-black color-scheme-bright-black)
                     (bright-red color-scheme-bright-red)
                     (bright-green color-scheme-bright-green)
                     (bright-yellow color-scheme-bright-yellow)
                     (bright-blue color-scheme-bright-blue)
                     (bright-magenta color-scheme-bright-magenta)
                     (bright-cyan color-scheme-bright-cyan)
                     (bright-white color-scheme-bright-white)
                     (cursor color-scheme-cursor)
                     (selection-background color-scheme-selection-background))

(define qillqaq
  (color-scheme (name "Qillqaq")
                (background "#16161D") ;Eigengrau
                (foreground "#F0EDE5") ;Coconut Milk
                ;; selection_foreground    #000000
                ;; *.background: #030303
                (black "#16161D")
                (red "#e32791")
                (green "#30c798")
                (yellow "#F3DF4D") ;Illuminating
                (blue "#6796e6")
                (magenta "#e59fdf")
                (cyan "#81d8d0")
                (white "#939597") ;Ultimate Gray
                (bright-black "#515151")
                (bright-red "#e466ad")
                (bright-green "#6cd1b2")
                (bright-yellow "#e4cf98")
                (bright-blue "#91b0e6")
                (bright-magenta "#e5b6e1")
                (bright-cyan "#a2dcd7")
                (bright-white "#F0EDE5") ;Coconut Milk
                ;; url_color #1680ac
                ;; # 5aaadf
                ;; # ff7135
                (cursor "#ff7315")
                (selection-background "#553a63")))

(define ysgrifennwr
  (color-scheme (name "Ysgrifennwr")
                (background "#F0EDE5") ;Coconut Milk
                (foreground "#3D4140") ;Pirate Black
                (black "#F0EDE5") ;Coconut Milk
                (red "#e32791")
                (green "#488432")
                (yellow "#EEAF0C") ;Daylilly
                (blue "#2c65b5")
                (magenta "#b062a7")
                (cyan "#27bbbe")
                (white "#96999B") ;Ultimate Gray
                (bright-black "#666666")
                (bright-red "#9f1b66")
                (bright-green "#325d23")
                (bright-yellow "#DFA41E")
                (bright-blue "#1f477f")
                (bright-magenta "#7b4474")
                (bright-cyan "#1b8486")
                (bright-white "#3D4140") ;Pirate Black
                ;; url_color #20bbfc
                ;; "#563C66" ;; Acal
                ;; "#A2B9BC"
                (cursor "#0f3a4b")
                ;; light goldenrod yellow
                (selection-background "#fafad2")))

(define lex-murphy
  (color-scheme (name "LexMurphy")
                (background "#010040")
                (foreground "#ffffff")
                (black "#000000")
                ;; X11 red3
                (red "#cd0000")
                ;; X11 green3
                (green "#00cd00")
                ;; X11 yellow3
                (yellow "#cdcd00")
                ;; X11 DodgerBlue1
                (blue "#1e90ff")
                ;; X11 magenta3
                (magenta "#cd00cd")
                ;; X11 cyan3
                (cyan "#00cdcd")
                ;; X11 gray90
                (white "#e5e5e5")
                ;; X11 gray50
                (bright-black "#7f7f7f")
                ;; X11 red
                (bright-red "#ff0000")
                ;; X11 green
                (bright-green "#00ff00")
                ;; X11 yellow
                (bright-yellow "#ffff00")
                ;; X11 SteelBlue1
                (bright-blue "#cae1ff")
                ;; X11 magenta
                (bright-magenta "#ff00ff")
                ;; X11 cyan
                (bright-cyan "#00ffff")
                (bright-white "#ffffff")
                ;; LightSalmon1
                (cursor "#ffa07a")
                ;; *color4: blue2
                ;; *color4: blue3
                ;; *color12: rgb:5c/5c/ff
                ;; *color12: blue
                ;; X11 DodgerBlue3
                (selection-background "#1874cd")))

(define fish-n-chips
  (color-scheme (name "FishNChips")
                (background "#fff1e5")
                (foreground "#192126")
                (black "#fff1e5")
                (red "#cf191d")
                (green "#008845")
                (yellow "#FFCB19")
                (blue "#0f5499")
                (magenta "#990f3d")
                (cyan "#089995")
                (white "#736c67")
                (bright-black "#ccc1b7")
                (bright-red "#FF383B")
                (bright-green "#00D46A")
                (bright-yellow "#FFDB66")
                (bright-blue "#177EE5")
                (bright-magenta "#E5175C")
                (bright-cyan "#0BE5DE")
                (bright-white "#192126")
                (cursor "#553a63")
                ;; #5aaadf
                ;; #ff7135
                ;; X11 MistyRose
                (selection-background "#ffe4e1")))

;; https://git.sr.ht/~krevedkokun/dotfiles/tree/master/item/channel/system/services/networking.scm
;; https://github.com/rroohhh/guix_system/blob/master/services/iwd.scm
;; https://git.sr.ht/~akagi/guixrc/tree/master/item/magi/system/services/networking.scm
(define (iwd-shepherd-service config)
  "Return a shepherd service for iwd"
  (let ((environment #~(list (string-append "PATH="
                                            (string-append #$openresolv
                                                           "/sbin") ":"
                                            (string-append #$coreutils "/bin")))))
    (list (shepherd-service (documentation "Run iwd")
                            (provision '(networking))
                            (requirement '(user-processes dbus-system loopback))
                            (start #~(make-forkexec-constructor (list (string-append #$iwd
                                                                       "/libexec/iwd"))
                                      #:log-file "/var/log/iwd.log"
                                      #:environment-variables #$environment))
                            (stop #~(make-kill-destructor))))))

(define iwd-service-type
  (let ((iwd-package (const (list iwd))))
    (service-type (name 'iwd)
                  (extensions (list (service-extension
                                     shepherd-root-service-type
                                     iwd-shepherd-service)
                                    (service-extension dbus-root-service-type
                                                       iwd-package)
                                    (service-extension profile-service-type
                                                       iwd-package)))
                  (default-value '())
                  (description ""))))

(define-public visual-studio-code
  (package
    (name "visual-studio-code")
    (version "1.71.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://update.code.visualstudio.com/"
                                  version "/linux-x64/stable"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ar8gpklaa0aa3k1934jyg2vh65hzncx0awl1f0wz8n4fjasfrpc"))))
    (build-system trivial-build-system)
    (native-inputs `(("patchelf" ,elf:patchelf)
                     ("tar" ,tar)
                     ("gzip" ,compression:gzip)
                     ("binutils" ,binutils)))
    (inputs `(("alsa-lib" ,linux:alsa-lib)
              ("atk" ,(@ (gnu packages gtk) atk))
              ("at-spi2-atk" ,(@ (gnu packages gtk) at-spi2-atk))
              ("at-spi2-core" ,(@ (gnu packages gtk) at-spi2-core))
              ("bash" ,(@ (gnu packages bash) bash))
              ("cairo" ,(@ (gnu packages gtk) cairo))
              ;; ("cups" ,(@ (gnu packages cups) cups))
              ("curl" ,(@ (gnu packages curl) curl))
              ("dbus" ,dbus)
              ("expat" ,(@ (gnu packages xml) expat))
              ("fontconfig" ,(@ (gnu packages fontutils) fontconfig))
              ("gcc" ,gcc "lib")
              ("gdk-pixbuf" ,(@ (gnu packages gtk) gdk-pixbuf))
              ("glib" ,glib)
              ("glibc" ,glibc)
              ;; ("gobject-introspection" ,gobject-introspection)
              ;; ("gsettings-desktop-schemas" ,(@ (gnu packages gnome) gsettings-desktop-schemas))
              ("gtk+" ,(@ (gnu packages gtk) gtk+))
              ("libdrm" ,(@ (gnu packages xdisorg) libdrm))
              ("libsecret" ,(@ (gnu packages gnome) libsecret))
              ("libx11" ,(@ (gnu packages xorg) libx11))
              ("libxcb" ,(@ (gnu packages xorg) libxcb))
              ("libxcomposite" ,(@ (gnu packages xorg) libxcomposite))
              ("libxcursor" ,(@ (gnu packages xorg) libxcursor))
              ("libxdamage" ,(@ (gnu packages xorg) libxdamage))
              ("libxext" ,(@ (gnu packages xorg) libxext))
              ("libxfixes" ,(@ (gnu packages xorg) libxfixes))
              ("libxkbfile" ,(@ (gnu packages xorg) libxkbfile))
              ("libxkbcommon" ,(@ (gnu packages xdisorg) libxkbcommon))
              ("libxrandr" ,(@ (gnu packages xorg) libxrandr))
              ;; ("libxscrnsaver" ,(@ (gnu packages xorg) libxscrnsaver))
              ;; ("libxtst" ,(@ (gnu packages xorg) libxtst))
              ("mesa" ,(@ (gnu packages gl) mesa))
              ("nspr" ,(@ (gnu packages nss) nspr))
              ("nss" ,(@ (gnu packages nss) nss))
              ("pango" ,(@ (gnu packages gtk) pango))
              ;; ("wayland" ,(@ (gnu packages freedesktop) wayland))
              ;; below are the results from pldd
              ("libffi" ,(@ (gnu packages libffi) libffi))
              ("pcre" ,(@ (gnu packages pcre) pcre))
              ("zlib" ,compression:zlib)
              ("util-linux" ,linux:util-linux)
              ("libxinerama" ,(@ (gnu packages xorg) libxinerama))
              ("libcloudproviders" ,(@ (gnu packages gnome) libcloudproviders))
              ("libxi" ,(@ (gnu packages xorg) libxi))
              ("pixman" ,(@ (gnu packages xdisorg) pixman))
              ("libxrender" ,(@ (gnu packages xorg) libxrender))
              ("libxau" ,(@ (gnu packages xorg) libxau))
              ("libxdmcp" ,(@ (gnu packages xorg) libxdmcp))
              ("gtk" ,(@ (gnu packages gtk) gtk))
              ("libepoxy" ,(@ (gnu packages gl) libepoxy))
              ("fribidi" ,(@ (gnu packages fribidi) fribidi))
              ("harfbuzz" ,(@ (gnu packages gtk) harfbuzz))
              ("graphite2" ,(@ (gnu packages fontutils) graphite2))
              ("freetype" ,(@ (gnu packages fontutils) freetype))
              ("bzip2" ,compression:bzip2)
              ("libpng" ,(@ (gnu packages image) libpng))
              ("libthai" ,(@ (gnu packages gtk) libthai))
              ("libdatrie" ,(@ (gnu packages gtk) libdatrie))
              ("eudev" ,linux:eudev)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((output (assoc-ref %outputs "out"))
                          (source (assoc-ref %build-inputs "source"))
                          (working-dir output)
                          (tar (string-append (assoc-ref %build-inputs "tar")
                                              "/bin/tar"))
                          (gzip (string-append (assoc-ref %build-inputs "gzip")
                                               "/bin/gzip"))
                          (patchelf (string-append (assoc-ref %build-inputs
                                                              "patchelf")
                                                   "/bin/patchelf")))

                     ;; Extraction phase
                     (mkdir-p working-dir)
                     (setenv "PATH"
                             (string-append (assoc-ref %build-inputs "gzip")
                                            "/bin"))
                     (zero? (system* tar
                                     "--strip-components=1"
                                     "-xvf"
                                     source
                                     "-C"
                                     working-dir))

                     ;; Patching phase
                     (invoke patchelf "--set-interpreter"
                             (string-append (assoc-ref %build-inputs "glibc")
                                            "/lib/ld-linux-x86-64.so.2")
                             (string-append working-dir "/code"))
                     (invoke patchelf "--set-interpreter"
                             (string-append (assoc-ref %build-inputs "glibc")
                                            "/lib/ld-linux-x86-64.so.2")
                             (string-append working-dir
                                            "/chrome_crashpad_handler"))
                     (invoke patchelf "--set-interpreter"
                             (string-append (assoc-ref %build-inputs "glibc")
                                            "/lib/ld-linux-x86-64.so.2")
                             (string-append working-dir "/chrome-sandbox"))

                     ;; Wrapping phase - give it libraries and some other envars
                     (setenv "PATH"
                             (string-append (assoc-ref %build-inputs "bash")
                                            "/bin"))
                     (wrap-program (string-append working-dir "/code")
                                   `("LD_LIBRARY_PATH" ":" prefix
                                     (,(string-append (assoc-ref %build-inputs
                                                       "nss") "/lib/nss"))))
                     (wrap-program (string-append working-dir "/code")
                                   `("LD_LIBRARY_PATH" ":" prefix
                                     ,(map (lambda (input)
                                             (string-append (assoc-ref
                                                             %build-inputs
                                                             input) "/lib"))
                                           '("libxcomposite"
                                             ;; "libxtst"
                                             "nspr"
                                             ;; "cups"
                                             "curl"
                                             "libxrandr"
                                             ;; "libxscrnsaver"
                                             "alsa-lib"
                                             "gcc"
                                             "libdrm"
                                             "libsecret"
                                             "libx11"
                                             "libxcb"
                                             "libxcursor"
                                             "libxext"
                                             "libxfixes"
                                             "libxkbcommon"
                                             "libxkbfile"
                                             "libxdamage"
                                             "mesa"
                                             ;; "gobject-introspection"
                                             "glib"
                                             "dbus"
                                             "expat"
                                             "pango"
                                             "cairo"
                                             "atk"
                                             "at-spi2-atk"
                                             "at-spi2-core"
                                             "gtk+"
                                             "gdk-pixbuf"
                                             ;; "wayland"
                                             "libffi"
                                             "pcre"
                                             "zlib"
                                             "util-linux"
                                             "libxinerama"
                                             "libcloudproviders"
                                             "libxi"
                                             "pixman"
                                             "libxrender"
                                             "libxau"
                                             "libxdmcp"
                                             "gtk"
                                             "libepoxy"
                                             "fribidi"
                                             "harfbuzz"
                                             "graphite2"
                                             "fontconfig"
                                             "freetype"
                                             "bzip2"
                                             "libthai"
                                             "libdatrie"
                                             "eudev")))
                                   ;; Reads fontconfig config (version 1.12.6 compatible required)
                                   `("FONTCONFIG_PATH" ":" prefix
                                     (,(string-append (assoc-ref %build-inputs
                                                       "fontconfig")
                                                      "/etc/fonts"))))
                     ;; Polishing phase
                     ;; (mkdir-p (string-append working-dir "/bin"))
                     ;; (symlink "../opt/brave.com/brave/brave" (string-append working-dir "/bin/brave"))

                     ;; (with-directory-excursion (string-append working-dir "/opt/brave.com/brave")
                     ;; (for-each (lambda (size)
                     ;; (let ((icons (string-append output
                     ;; "/share/icons/hicolor/"
                     ;; size
                     ;; "x"
                     ;; size
                     ;; "/apps")))
                     ;; (mkdir-p icons)
                     ;; (copy-file (string-append "product_logo_"
                     ;; size ".png")
                     ;; (string-append icons
                     ;; "/brave.png"))))
                     ;; '("24" "48" "64" "128" "256")))
                     ;;
                     ;; (mkdir-p (string-append (assoc-ref %outputs "out") "/share/applications"))
                     ;; (with-output-to-file (string-append (assoc-ref %outputs "out") "/share/applications/brave.desktop")
                     ;; (lambda _
                     ;; (format #t
                     ;; "[Desktop Entry]~@\nName=Brave~@\nComment=Browse 3x faster than Chrome~@\nExec=~a/bin/brave~@\nIcon=brave~@\nCategories=Network~@\nType=Application~%"
                     ;; (assoc-ref %outputs "out"))))))))
                     ))))
    (home-page "https://code.visualstudio.com")
    (synopsis "Code editing. Redefined.")
    (description
     "Visual Studio Code is a new choice of tool that combines the simplicity of a code editor with what developers need for the core edit-build-debug cycle.")
    (license #f)))

(define-public brave
  (package
    (name "brave")
    (version "1.44.105")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/brave/brave-browser/releases/download/v"
                    version "/brave-browser_" version "_amd64.deb"))
              (sha256
               (base32
                "0dxiv0r7c3jy6kaqkq4p4w2fz0vxx730kwkg52hryw24rr8jd34v"))))
    (build-system trivial-build-system)
    (native-inputs `(("patchelf" ,elf:patchelf)
                     ("tar" ,tar)
                     ("xz" ,compression:xz)
                     ("binutils" ,binutils)))
    (inputs `(("alsa-lib" ,linux:alsa-lib)
              ("atk" ,(@ (gnu packages gtk) atk))
              ("at-spi2-atk" ,(@ (gnu packages gtk) at-spi2-atk))
              ("at-spi2-core" ,(@ (gnu packages gtk) at-spi2-core))
              ("bash" ,(@ (gnu packages bash) bash))
              ("cairo" ,(@ (gnu packages gtk) cairo))
              ("cups" ,(@ (gnu packages cups) cups))
              ("curl" ,(@ (gnu packages curl) curl))
              ("dbus" ,dbus)
              ("expat" ,(@ (gnu packages xml) expat))
              ("fontconfig" ,(@ (gnu packages fontutils) fontconfig))
              ("gcc" ,gcc "lib")
              ("gdk-pixbuf" ,(@ (gnu packages gtk) gdk-pixbuf))
              ("glib" ,glib)
              ("glibc" ,glibc)
              ("gobject-introspection" ,gobject-introspection)
              ("gsettings-desktop-schemas" ,(@ (gnu packages gnome)
                                               gsettings-desktop-schemas))
              ("gtk+" ,(@ (gnu packages gtk) gtk+))
              ("gtk" ,(@ (gnu packages gtk) gtk))
              ("libdrm" ,(@ (gnu packages xdisorg) libdrm))
              ("libx11" ,(@ (gnu packages xorg) libx11))
              ("libxcb" ,(@ (gnu packages xorg) libxcb))
              ("libxcomposite" ,(@ (gnu packages xorg) libxcomposite))
              ("libxcursor" ,(@ (gnu packages xorg) libxcursor))
              ("libxdamage" ,(@ (gnu packages xorg) libxdamage))
              ("libxext" ,(@ (gnu packages xorg) libxext))
              ("libxfixes" ,(@ (gnu packages xorg) libxfixes))
              ("libxkbcommon" ,(@ (gnu packages xdisorg) libxkbcommon))
              ("libxrandr" ,(@ (gnu packages xorg) libxrandr))
              ("libxscrnsaver" ,(@ (gnu packages xorg) libxscrnsaver))
              ("libxtst" ,(@ (gnu packages xorg) libxtst))
              ("mesa" ,(@ (gnu packages gl) mesa))
              ("nspr" ,(@ (gnu packages nss) nspr))
              ("nss" ,(@ (gnu packages nss) nss))
              ("pango" ,(@ (gnu packages gtk) pango))
              ("libffi" ,(@ (gnu packages libffi) libffi))
              ("pcre" ,(@ (gnu packages pcre) pcre))
              ("gnutls" ,(@ (gnu packages tls) gnutls))
              ("zlib" ,compression:zlib)
              ("util-linux" ,linux:util-linux)
              ("libxau" ,(@ (gnu packages xorg) libxau))
              ("libxdmcp" ,(@ (gnu packages xorg) libxdmcp))
              ("libxrender" ,(@ (gnu packages xorg) libxrender))
              ("fribidi" ,(@ (gnu packages fribidi) fribidi))
              ("libthai" ,(@ (gnu packages gtk) libthai))
              ("harfbuzz" ,(@ (gnu packages gtk) harfbuzz))
              ("pixman" ,(@ (gnu packages xdisorg) pixman))
              ("freetype" ,(@ (gnu packages fontutils) freetype))
              ("libpng" ,(@ (gnu packages image) libpng))
              ("libxi" ,(@ (gnu packages xorg) libxi))
              ("p11-kit" ,(@ (gnu packages tls) p11-kit))
              ("libidn2" ,(@ (gnu packages libidn) libidn2))
              ("libunistring" ,(@ (gnu packages libunistring) libunistring))
              ("libtasn1" ,(@ (gnu packages tls) libtasn1))
              ("nettle" ,(@ (gnu packages nettle) nettle))
              ("gmp" ,(@ (gnu packages multiprecision) gmp))
              ("libdatrie" ,(@ (gnu packages gtk) libdatrie))
              ("graphite2" ,(@ (gnu packages fontutils) graphite2))
              ("bzip2" ,compression:bzip2)
              ("libxinerama" ,(@ (gnu packages xorg) libxinerama))
              ("libcloudproviders" ,(@ (gnu packages gnome) libcloudproviders))
              ("libepoxy" ,(@ (gnu packages gl) libepoxy))
              ("libxxf86vm" ,(@ (gnu packages xorg) libxxf86vm))
              ("libxshmfence" ,(@ (gnu packages xorg) libxshmfence))
              ("libpciaccess" ,(@ (gnu packages xorg) libpciaccess))
              ("eudev" ,linux:eudev)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((output (assoc-ref %outputs "out"))
                          (source (assoc-ref %build-inputs "source"))
                          (working-dir output)
                          (ar (string-append (assoc-ref %build-inputs
                                                        "binutils") "/bin/ar"))
                          (tar (string-append (assoc-ref %build-inputs "tar")
                                              "/bin/tar"))
                          (patchelf (string-append (assoc-ref %build-inputs
                                                              "patchelf")
                                                   "/bin/patchelf")))

                     ;; Extraction phase
                     (mkdir-p working-dir)
                     (setenv "PATH"
                             (string-append (assoc-ref %build-inputs "xz")
                                            "/bin"))
                     (zero? (system* ar "x" source "data.tar.xz"))
                     (zero? (system* tar
                                     "xvf"
                                     "data.tar.xz"
                                     "-C"
                                     working-dir
                                     "./opt"))

                     ;; Patching phase
                     (invoke patchelf "--set-interpreter"
                             (string-append (assoc-ref %build-inputs "glibc")
                                            "/lib/ld-linux-x86-64.so.2")
                             (string-append working-dir
                                            "/opt/brave.com/brave/brave"))
                     (invoke patchelf "--set-interpreter"
                             (string-append (assoc-ref %build-inputs "glibc")
                                            "/lib/ld-linux-x86-64.so.2")
                             (string-append working-dir
                              "/opt/brave.com/brave/chrome_crashpad_handler"))
                     (invoke patchelf "--set-interpreter"
                             (string-append (assoc-ref %build-inputs "glibc")
                                            "/lib/ld-linux-x86-64.so.2")
                             (string-append working-dir
                              "/opt/brave.com/brave/chrome-sandbox"))

                     ;; Wrapping phase - give it libraries and some other envars
                     (setenv "PATH"
                             (string-append (assoc-ref %build-inputs "bash")
                                            "/bin"))
                     ;; (wrap-program (string-append working-dir "/opt/brave.com/brave/brave") `("LD_LIBRARY_PATH" ":" prefix ("/run/current-system/profile/lib")))
                     (wrap-program (string-append working-dir
                                    "/opt/brave.com/brave/brave")
                                   `("LD_LIBRARY_PATH" ":" prefix
                                     (,(string-append (assoc-ref %build-inputs
                                                       "nss") "/lib/nss"))))
                     (wrap-program (string-append working-dir
                                    "/opt/brave.com/brave/brave")
                                   `("LD_LIBRARY_PATH" ":" prefix
                                     ,(map (lambda (input)
                                             (string-append (assoc-ref
                                                             %build-inputs
                                                             input) "/lib"))
                                           '("libxcomposite" "libxtst"
                                             "nspr"
                                             "cups"
                                             "curl"
                                             "libxrandr"
                                             "libxscrnsaver"
                                             "alsa-lib"
                                             "gcc"
                                             "libdrm"
                                             "libx11"
                                             "libxcb"
                                             "libxcursor"
                                             "libxext"
                                             "libxfixes"
                                             "libxkbcommon"
                                             "libxdamage"
                                             "mesa"
                                             "gobject-introspection"
                                             "glib"
                                             "dbus"
                                             "expat"
                                             "pango"
                                             "cairo"
                                             "atk"
                                             "at-spi2-atk"
                                             "at-spi2-core"
                                             "gtk+"
                                             "gdk-pixbuf"
                                             "libffi"
                                             "pcre"
                                             "gnutls"
                                             "zlib"
                                             "util-linux"
                                             "libxau"
                                             "libxdmcp"
                                             "libxrender"
                                             "fribidi"
                                             "libthai"
                                             "harfbuzz"
                                             "pixman"
                                             "fontconfig"
                                             "freetype"
                                             "libpng"
                                             "libxi"
                                             "p11-kit"
                                             "libidn2"
                                             "libunistring"
                                             "libtasn1"
                                             "nettle"
                                             "gmp"
                                             "libdatrie"
                                             "graphite2"
                                             "bzip2"
                                             "libxinerama"
                                             "libcloudproviders"
                                             "libepoxy"
                                             "libxxf86vm"
                                             "libxshmfence"
                                             "libpciaccess"
                                             "eudev")))
                                   ;; Reads fontconfig config (version 1.12.6 compatible required)
                                   `("FONTCONFIG_PATH" ":" prefix
                                     (,(string-append (assoc-ref %build-inputs
                                                       "fontconfig")
                                                      "/etc/fonts"))))
                     ;; --enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=VaapiVideoDecoder --use-gl=egl

                     ;; Polishing phase
                     (mkdir-p (string-append working-dir "/bin"))
                     (symlink "../opt/brave.com/brave/brave"
                              (string-append working-dir "/bin/brave"))

                     (with-directory-excursion (string-append working-dir
                                                "/opt/brave.com/brave")
                       (for-each (lambda (size)
                                   (let ((icons (string-append output
                                                 "/share/icons/hicolor/"
                                                 size
                                                 "x"
                                                 size
                                                 "/apps")))
                                     (mkdir-p icons)
                                     (copy-file (string-append "product_logo_"
                                                 size ".png")
                                                (string-append icons
                                                               "/brave.png"))))
                                 '("24" "48" "64" "128" "256")))

                     (mkdir-p (string-append (assoc-ref %outputs "out")
                                             "/share/applications"))
                     (with-output-to-file (string-append (assoc-ref %outputs
                                                                    "out")
                                           "/share/applications/brave.desktop")
                       (lambda _
                         (format #t
                                 "[Desktop Entry]~@\nName=Brave~@\nComment=Browse 3x faster than Chrome~@\nExec=~a/bin/brave~@\nIcon=brave~@\nCategories=Network~@\nType=Application~%"
                                 (assoc-ref %outputs "out"))))))))
    (home-page "https://www.brave.com")
    (synopsis "Brave web browser.")
    (description "Brave web browser.")
    (license #f)))

(define-public font-sf-mono
  (package
    (name "font-sf-mono")
    (version "18.0d1e1")
    (native-inputs `(("p7zip" ,(specification->package "p7zip"))))
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg"))
              (sha256
               (base32
                "10v1m04yx606qf5bxwkijamkl6yclqyrrszpzwxy1jqpk6xs2nds"))))
    (build-system font-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((source (assoc-ref inputs "source")))
                        (invoke "7z" "x" source
                                "SFMonoFonts/SF Mono Fonts.pkg")
                        (invoke "7z" "x" "SFMonoFonts/SF Mono Fonts.pkg")
                        (invoke "7z" "x" "Payload~")))))))
    (home-page "https://developer.apple.com/fonts/")
    (synopsis "SF Mono monospace typeface")
    (description
     "This monospaced variant of San Francisco enables alignment between rows and columns of text, and is used in coding environments like Xcode. SF Mono features six weights and supports Latin, Greek, and Cyrillic scripts.")
    (license (nonfree "https://www.apple.com/legal/sla/"))))

(define-public font-joypixels
  (package
    (name "font-joypixels")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cdn.joypixels.com/arch-linux/font/"
                    version "/joypixels-android.ttf"))
              (sha256
               (base32
                "1mxc15riz8bb4pm0pgsyb0xfybhncabkf48m9vcgass93iqxya5d"))))
    (build-system font-build-system)
    (home-page "https://www.joypixels.com")
    (synopsis "Emoji as a Service")
    (description "An emoji font with originally crafted icon designs.")
    (license (nonfree
              "https://cdn.joypixels.com/arch-linux/license/free-license.pdf"))))



(operating-system
  (kernel linux)
  (kernel-arguments '("mitigations=off"
                      "sysctl.fs.inotify.max_user_watches=524288"
                      "i915.fastboot=1"
                      "vt.default_red=0x00,0xe3,0x30,0xe3,0x67,0xe5,0x81,0x99,0x51,0xe4,0x6c,0xe4,0x91,0xe5,0xa2,0xe5"
                      "vt.default_grn=0x00,0x27,0xc7,0xc4,0x96,0x9f,0xd8,0x99,0x51,0x66,0xd1,0xcf,0xb0,0xb6,0xdc,0xe6"
                      "vt.default_blu=0x00,0x91,0x98,0x72,0xe6,0xdf,0xd0,0x99,0x51,0xad,0xb2,0x98,0xe6,0xe1,0xd7,0xe6"))
  (initrd (lambda (file-systems . rest)
            (apply microcode-initrd
                   file-systems
                   #:initrd base-initrd
                   #:microcode-packages (list intel-microcode)
                   rest)))
  (firmware (cons* iwlwifi-firmware %base-firmware))
  (locale "en_US.utf8") ;[TODO] C.utf8 C
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "us"
                                    "altgr-intl"
                                    #:model "latitude"
                                    #:options '("compose:caps" "rupeesign:4")))
  (host-name "andermatt")
  (sudoers-file (plain-file "sudoers"
                            "root ALL=(ALL) ALL\n%wheel ALL=(ALL) ALL\nDefaults insults\n")) ;[TODO] make insults work, probably needs a compile flag

  ;; [TODO] use host-name
  (hosts-file (file-append (package
                             (name "scripttiger-github-io")
                             (version "2022-10-03")
                             (source (origin
                                       (method git-fetch)
                                       (uri (git-reference
                                             (url
                                              "https://github.com/ScriptTiger/scripttiger.github.io.git")
                                             (commit
                                              "7da94f6b8ce1d61b4901ed81e973a1e8e7332950")))
                                       (file-name (git-file-name name version))
                                       (sha256
                                        (base32
                                         "1ibmlka5gdmcn0vqhb2bmh98vxcwjvr65vcj115gx0q3lcb3yr9q"))))
                             (build-system trivial-build-system)
                             (arguments
                              `(#:modules ((guix build utils))
                                #:builder (begin
                                            (use-modules (guix build utils))
                                            (let ((source (assoc-ref
                                                           %build-inputs
                                                           "source"))
                                                  (out (assoc-ref %outputs
                                                                  "out")))
                                              (install-file (string-append
                                                             source
                                                             "/alts/compressed/blacklist-p.txt")
                                                            (string-append out
                                                             "/hosts"))
                                              (substitute* (string-append out
                                                            "/hosts/blacklist-p.txt")
                                                (("0.0.0.0 (.*)$" all end)
                                                 (string-append all ":: " end))
                                                ((":: 0.0.0.0")
                                                 ":: ::"))))))
                             (home-page "https://scripttiger.github.io")
                             (synopsis "Blacklist hosts files")
                             (description
                              "Assorted versions of Steven Black’s unified hosts files reformatted for various other applications for additional support.")
                             (license license:expat)) "/hosts/blacklist-p.txt"))
  (users (cons* (user-account
                  (name "xha")
                  (comment "Xaver Hellauer")
                  (group "users")
                  (home-directory "/home/xha")
                  (shell (file-append shells:zsh "/bin/zsh"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video"
                                          "input"))) %base-user-accounts))
  (packages (append (list brave
                          visual-studio-code
                          font-joypixels
                          font-sf-mono
                          font-praggnanandhaa-pro
                          font-modernsuite
                          (package
                            (inherit suckless:dmenu)
                            (version "5.1-git") ;[TODO] better version name
                            (source (origin
                                      (method git-fetch)
                                      (uri (git-reference
                                            (url
                                             "git://git.suckless.org/dmenu")
                                            (commit
                                             "fce06f437dcec646ee0a2728fe695f3084cc6ccb")))
                                      (file-name (git-file-name "dmenu"
                                                                "5.1-git")) ;[TODO] better version name
                                      (sha256
                                       (base32
                                        "1k45942p6ri08l4f7i3b3ly4y9hgs6qlvxc3brwvihj4x8035gv4"))
                                      (patches (list (plain-file
                                                      "config.patch"
                                                      "--- a/config.h\n+++ b/config.h\n@@ -0,0 +1 @@\n+static int topbar = 1;static const char *fonts[] = {\"monospace:size=8\"};static const char *prompt = NULL;static const char *colors[SchemeLast][2] = {[SchemeNorm] = { \"#FFFFFF\", \"#000000\" },[SchemeSel] = { \"#eeeeee\", \"#24986C\" },[SchemeOut] = { \"#000000\", \"#00ffff\" }};static unsigned int lines = 0;static const char worddelimiters[] = \" /?\\\"&[]-_\";\n"))))))
                          (package
                            (inherit (specification->package "rxvt-unicode"))
                            (source (origin
                                      (inherit (package-source (specification->package
                                                                "rxvt-unicode")))
                                      (patch-flags '("-p0"))
                                      (patches (list (origin
                                                       (method url-fetch)
                                                       (uri
                                                        "https://aur.archlinux.org/cgit/aur.git/plain/enable-wide-glyphs.patch?h=rxvt-unicode-truecolor-wide-glyphs")
                                                       (file-name
                                                        "enable-wide-glyphs.patch")
                                                       (sha256 (base32
                                                                "1w84mp2pjk715s7azi4i00ydxsf6w77jiykd31njiwmbwk0a4wd2")))
                                                     (origin
                                                       (method url-fetch)
                                                       (uri
                                                        "https://aur.archlinux.org/cgit/aur.git/plain/improve-font-rendering.patch?h=rxvt-unicode-truecolor-wide-glyphs")
                                                       (file-name
                                                        "improve-font-rendering.patch")
                                                       (sha256 (base32
                                                                "09nhvp1l28maw13qj3s87rk36gvxfw8dpwababid248vq98vqjnc")))))))
                            (arguments
                             ;; This sets the destination when installing the necessary terminal
                             ;; capability data, which are not provided by 'ncurses'.  See
                             ;; https://lists.gnu.org/archive/html/bug-ncurses/2009-10/msg00031.html
                             `(#:configure-flags (list "--enable-xft"
                                                  "--enable-font-styles"
                                                  "--enable-xim"
                                                  "--enable-unicode3"
                                                  "--enable-combining"
                                                  "--disable-pixbuf" ;brauchen wir das für das Icon, brauchen wir 1 Icon?
                                                  "--enable-startup-notification" ;kann das weg?
                                                  "--disable-afterimage"
                                                  "--disable-transparency"
                                                  "--enable-fading"
                                                  "--disable-rxvt-scroll"
                                                  "--disable-next-scroll"
                                                  "--disable-xterm-scroll"
                                                  "--enable-frills"
                                                  "--disable-iso14755"
                                                  "--disable-keepscrolling"
                                                  "--enable-selectionscrolling"
                                                  "--enable-mousewheel"
                                                  "--disable-slipwheeling"
                                                  "--disable-smart-resize"
                                                  "--with-name=rxvt-unicode"
                                                  "--enable-text-blink"
                                                  "--enable-pointer-blank"
                                                  "--enable-perl" ;[TODO] kann das weg?
                                                  "--disable-256-color"
                                                  "--enable-wide-glyphs")
                               #:make-flags (list (string-append "TERMINFO="
                                                   (assoc-ref %outputs "out")
                                                   "/share/terminfo"))
                               #:phases (modify-phases %standard-phases
                                          (add-after 'install 'install-desktop-urxvt
                                            (lambda* (#:key outputs
                                                      #:allow-other-keys)
                                              (let* ((output (assoc-ref
                                                              outputs "out"))
                                                     (desktop (string-append
                                                               output
                                                               "/share/applications")))
                                                (mkdir-p desktop)
                                                (with-output-to-file (string-append
                                                                      desktop
                                                                      "/urxvt.desktop")
                                                  (lambda _
                                                    (format #t
                                                     "[Desktop Entry]~@\nName=rxvt-unicode~@\nComment=~@\nExec=~a/bin/urxvt~@\nTryExec=~@*~a/bin/urxvt~@\nIcon=~@\nType=Application~%"
                                                     output))))))
                                          (add-after 'install 'install-desktop-urxvtc
                                            (lambda* (#:key outputs
                                                      #:allow-other-keys)
                                              (let* ((output (assoc-ref
                                                              outputs "out"))
                                                     (desktop (string-append
                                                               output
                                                               "/share/applications")))
                                                (mkdir-p desktop)
                                                (with-output-to-file (string-append
                                                                      desktop
                                                                      "/urxvtc.desktop")
                                                  (lambda _
                                                    (format #t
                                                     "[Desktop Entry]~@\nName=rxvt-unicode (client)~@\nComment=Rxvt clone with XFT and unicode support~@\nExec=~a/bin/urxvtc~@\nTryExec=~@*~a/bin/urxvtc~@\nIcon=~@\nType=Application~%"
                                                     output))))))))))
                          ;; (specification->package "fontconfig") ;; Fonts werden sonst nicht gefunden. Soll das so?
                          (package
                            (inherit suckless:dwm)
                            (version "6.3-git") ;[TODO] better version name
                            (source (origin
                                      (method git-fetch)
                                      (uri (git-reference
                                            (url "git://git.suckless.org/dwm")
                                            (commit
                                             "c2b748e7931e5f28984efc236f9b1a212dbc65e8")))
                                      (file-name "dwm")
                                      (sha256
                                       (base32
                                        "182zkcpdanxb3i1gfw21pzxz9sd17n0plgq7s1z63b2sps9ljaxj"))))
                            (native-inputs `(("config.h" ,(local-file
                                                           "dwm-config.h"))
                                             ,@(package-native-inputs
                                                suckless:dwm)))
                            (arguments
                             `(#:tests? #f
                               #:make-flags (list (string-append
                                                   "FREETYPEINC="
                                                   (assoc-ref %build-inputs
                                                              "freetype")
                                                   "/include/freetype2"))
                               #:phases (modify-phases %standard-phases
                                          (replace 'configure
                                            (lambda _
                                              (substitute* "Makefile"
                                                (("\\$\\{CC\\}")
                                                 "gcc")) #t))
                                          (add-after 'configure 'config-file
                                            (lambda* (#:key inputs
                                                      #:allow-other-keys)
                                              (copy-file (assoc-ref inputs
                                                          "config.h")
                                                         "config.h")))
                                          (replace 'install
                                            (lambda* (#:key outputs
                                                      #:allow-other-keys)
                                              (let ((out (assoc-ref outputs
                                                                    "out")))
                                                (invoke "make" "install"
                                                        (string-append
                                                         "DESTDIR=" out)
                                                        "PREFIX="))))))))
                          dbus
                          xdisorg:sx
                          video:mpv
                          mozilla:firefox
                          xhost
                          version-control:git
                          gnupg:gnupg
                          gnupg:pinentry-tty
                          admin:nnn
                          linux:btrfs-progs
                          pdf:mupdf
                          image-viewers:nsxiv
                          suckless:sent
                          shells:zsh
                          ;; emacs-no-x-toolkit
                          (package
                            (inherit emacs)
                            (name "emacs-athena")
                            (synopsis
                             "The extensible, customizable, self-documenting text editor with the Athena toolkit")
                            (inputs `(("libxaw" ,libxaw)
                                      ,@(alist-delete "gtk+"
                                                      (package-inputs emacs))))
                            (arguments
                             (substitute-keyword-arguments (package-arguments
                                                            emacs)
                               ((#:configure-flags flags)
                                #~(cons "--with-x-toolkit=athena"
                                        #$flags)))))
                          llvm:emacs-clang-format
                          emacs-guix
                          emacs-ivy
                          emacs-multiple-cursors
                          emacs-rainbow-delimiters
                          emacs-geiser
                          emacs-geiser-guile
                          emacs-paredit
                          neovim
                          ;; (specification->package "telegram-desktop")
                          linux:pipewire
                          linux:wireplumber
                          compression:zstd
                          signal-desktop
                          ;; https://github.com/codemac/guix-pkgs/blob/master/src/codemac/packages/notion.scm
                          ;; (package (name "notion") (version "4.0.2") (source (origin (method url-fetch) (uri (string-append "https://github.com/raboof/" name "/archive/" (string-map (lambda (x) (if (equal? x #\.) #\- x)) version) ".tar.gz")) (sha256 (base32 "01f3qncgvvy19i94a18lssvhajhsga7y4vxpcg11f3gvm4srz79w")) (file-name (string-append name "-" version ".tar.gz")))) (build-system gnu-build-system) (arguments `(#:phases (modify-phases %standard-phases (delete 'check) (replace 'configure (lambda* (#:key outputs #:allow-other-keys) (setenv "PREFIX" (assoc-ref outputs "out")) (setenv "CC" "gcc")))))) (inputs `(("glib" ,glib)
                          ;; ("gettext" ,gettext)
                          ;; ("lua" ,lua@5.3.5) ("libxext" ,libxext) ("libsm" ,libsm) ("libxinerama" ,libxinerama) ("libxrandr" ,libxrandr))) (native-inputs `(("pkg-config" ,pkg-config) ("rubber" ,rubber) ("perl" ,perl)
                          ;; ("which" ,which)
                          ;; ("gcc" ,gcc) ("groff" ,groff) ("texlive-bin" ,texlive-bin))) (home-page "http://notion.sourceforge.net") (synopsis "Tiling tabbed window manager") (description "Notion is a tiled tabbed window manager, which unlike many other tiled window managers has static window configurations. This allows for a more declarative approach to window sizing.") (license license:lgpl2.1))
                          certs:nss-certs) %base-packages))

  (services
   (append (list (bluetooth-service #:auto-enable? #t)
                 ;; (service network-manager-service-type)
                 ;; (service wpa-supplicant-service-type)
                 (simple-service 'zshenvironment
                                 session-environment-service-type
                                 '(("EDITOR" . "/run/current-system/profile/bin/emacs") ;[FIXME] use path from package
                                   ("PAGER" . "less") ;[FIXME] use path from package
                                   ("MAN_POSIXLY_CORRECT" . "1")
                                   ("AWS_REGION" . "eu-central-1")
                                   ("LESS_TERMCAP_mb" . "\x1b[00;34m")
                                   ("LESS_TERMCAP_md" . "\x1b[01;36m")
                                   ("LESS_TERMCAP_us" . "\x1b[01;35m")
                                   ("LESS_TERMCAP_ue" . "\x1b[0m")
                                   ("LESS_TERMCAP_me" . "\x1b[0m")
                                   ("GROFF_NO_SGR" . "1")
                                   ("NNN_PLUG" . "'i:imgview;c:-_code -r $nnn*;x:sx;h:-hexview;v:-_mpv --force-window=yes $nnn*;V:-_mpv --shuffle --force-window=yes $nnn*;u:-uidgid;G:getplugs'")
                                   ("NNN_COLORS" . "4256")
                                   ("NNN_OPTS" . "xe")
                                   ("NNN_FCOLORS" . "0b0304010f0e060740020a08")
                                   ("PF_INFO" . "ascii title os host shell editor wm de kernel uptime pkgs memory palette")
                                   ("GCC_COLORS" . "'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'")
                                   ("LS_COLORS" . "'rs=0:di=1;34:tw=1;3;94:ow=1;94:st=1;3;34:ex=1;31:sg=1;3;31:su=1;3;91:ca=1;4;31:ln=36:mh=96:or=38;5;64:mi=37:bd=93:cd=33:pi=32:so=92:do=4;92:*.js=38;2;23;23;23;48;2;221;224;90:*.jsx=38;2;23;23;23;48;2;221;224;90:*.ts=48;2;43;116;137;38;2;229;230;230:*.tsx=48;2;43;116;137;38;2;229;230;230:*.vue=38;2;44;62;80;48;2;65;184;131:*.cpp=48;2;243;75;125:*.cxx=48;2;243;75;125:*.cc=48;2;243;75;125:*.hpp=48;2;243;75;125:*.hxx=48;2;243;75;125:*.hh=48;2;243;75;125:*.c=7:*.h=7:*.go=38;2;229;230;230;48;2;0;173;216:*.hs=38;2;94;80;134;48;2;235;228;243:*.svelte=48;2;229;230;230;38;2;255;62;0:*.lua=48;2;0;0;128;38;2;229;230;230:*.html=38;2;229;230;230;48;2;227;76;38:*.htm=38;2;229;230;230;48;2;227;76;38:*.xhtml=38;2;229;230;230;48;2;227;76;38:*.css=38;2;229;230;230;48;2;86;61;124:*.scss=38;2;229;230;230;48;2;207;100;154:*.sass=38;2;229;230;230;48;2;207;100;154:*.nix=48;2;126;126;255:*.vim=48;2;25;159;75;38;2;204;204;153:*vimrc=48;2;25;159;75;38;2;204;204;153:*Makefile.in=37:*CMakeCache.txt=37:*.la=37:*.o=37:*.lo=37:*.dyn_hi=37:*.cache=37:*.dyn_o=37:*.hi=37:*.errors=37:*.class=37:*.aux=37:*.bbl=37:*.ilg=37:*.idx=37:*.blg=37:*.out=37:*.toc=37:*.ind=37:*.sty=37:*.synctex.gz=37:*.fdb_latexmk=37:*.fls=37:*.bcf=37:*.bc=37:*.pyc=37:*.rlib=37:*.sconsign.dblite=37:*.scons_opt=37:*.git=37:*package-lock.json=37:*.avi=38;5;68:*.flv=38;5;68:*.m4v=38;5;68:*.mkv=38;5;68:*.mov=38;5;68:*.mp4=38;5;68:*.mpeg=38;5;68:*.mpg=38;5;68:*.ogv=38;5;68:*.vid=38;5;68:*.webm=38;5;68:*.wmv=38;5;68:*.aac=38;5;70:*.aup=38;5;70:*.flac=38;5;70:*.m4a=38;5;70:*.mp3=38;5;70:*.oga=38;5;70:*.ogg=38;5;70:*.opus=38;5;70:*.wav=38;5;70:*.wma=38;5;70:*.wv=38;5;70:*.jpeg=38;5;44:*.cbr=38;5;33:*.cbz=38;5;33:*.epub=38;5;33:*.7z=35:*.a=35:*.ace=35:*.alz=35:*.apk=35:*.arc=35:*.arj=35:*.bz=35:*.bz2=35:*.cab=35:*.cpio=35:*.deb=35:*.gz=35:*.jar=35:*.lha=35:*.lz=35:*.lzh=35:*.lzma=35:*.lzo=35:*.nar=35:*.pax=35:*.rar=35:*.rpm=35:*.rz=35:*.t7z=35:*.tar=35:*.tbz=35:*.tbz2=35:*.tgz=35:*.tlz=35:*.txz=35:*.tZ=35:*.tzo=35:*.war=35:*.xbps=35:*.xpi=35:*.xz=35:*.Z=35:*.zip=35:*.zstd=35:*.pid=90:*.swp=90:*.tmp=90:*.bak=90:*.orig=90:*.lock=90:*.log=90:*~=90:*COPYRIGHT=90:*LICENSE=90:*LICENSE-MIT=90:*COPYING=90:*LICENSE-APACHE=90:'")))
                 (simple-service 'etcfiles etc-service-type
                                 (list `("zshenv" ,(plain-file "zshenv"
                                                    "export XDG_DATA_HOME=$HOME/.local/share\nexport XDG_CONFIG_HOME=$HOME/.config\nexport XDG_CACHE_HOME=$HOME/.cache\nexport GOPATH=$XDG_DATA_HOME/go\nexport NPMPATH=$XDG_DATA_HOME/npm\nexport npm_config_userconfig=$XDG_CONFIG_HOME/npmrc\nexport npm_config_prefix=$NPMPATH\nexport npm_config_cache=$XDG_CACHE_HOME/npm\nexport NODE_REPL_HISTORY=$XDG_DATA_HOME/node_repl_history\nexport LESSHISTFILE=$XDG_CACHE_HOME/lesshst\nexport GNUPGHOME=$XDG_CONFIG_HOME/gnupg\nexport XAUTHORITY=$XDG_DATA_HOME/Xauthority\nexport XENVIRONMENT=$XDG_CONFIG_HOME/Xdefaults\nexport CCACHE_DIR=$XDG_CACHE_HOME/ccache\n"))
                                       `("zprofile" ,(plain-file "zprofile"
                                                      ". /etc/profile\n"))
                                       `("zlogin" ,(plain-file "zlogin"
                                                    "if [ -z \"${DISPLAY}\" ] && [ \"${XDG_VTNR}\" -eq 1 ]; then; exec sx; fi\n"))
                                       `("zshrc" ,(local-file "./zshrc"
                                                              "zshrc"))))
                 (service iwd-service-type)
                 (elogind-service)
                 (dbus-service)
                 (service openssh-service-type)
                 (service ntp-service-type)
                 (service xorg-server-service-type
                          (xorg-configuration (keyboard-layout keyboard-layout)))
                 (screen-locker-service suckless:slock)) ;[TODO] configure slock package
           (modify-services %base-services
             (guix-service-type config =>
                                (guix-configuration (inherit config)
                                                    (substitute-urls (append (list
                                                                              "https://substitutes.nonguix.org")
                                                                      %default-substitute-urls))
                                                    (authorized-keys (append (list
                                                                              (plain-file
                                                                               "non-guix.pub"
                                                                               "(public-key \n                                       (ecc \n                                        (curve Ed25519)\n                                        (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)\n                                        )\n                                       )"))
                                                                      %default-authorized-guix-keys)))))))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  ;; rewrite file-systems as lambda function taking subvol and mountpoints as arguments
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid "e2bc7478-31ec-4326-bd8e-8ae86d128738"
                                       'btrfs))
                         (flags '(no-atime))
                         (options
                          "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@GNUGuix")
                         (type "btrfs"))
                       (file-system
                         (mount-point "/home/xha")
                         (device (uuid "e2bc7478-31ec-4326-bd8e-8ae86d128738"
                                       'btrfs))
                         (flags '(no-atime))
                         (options
                          "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@home@xha")
                         (type "btrfs"))
                       (file-system
                         (mount-point "/var/lib/bluetooth")
                         (device (uuid "e2bc7478-31ec-4326-bd8e-8ae86d128738"
                                       'btrfs))
                         (flags '(no-atime))
                         (options
                          "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@var@lib@bluetooth")
                         (type "btrfs"))
                       (file-system
                         (mount-point "/var/lib/iwd")
                         (device (uuid "e2bc7478-31ec-4326-bd8e-8ae86d128738"
                                       'btrfs))
                         (flags '(no-atime))
                         (options
                          "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@var@lib@iwd")
                         (type "btrfs"))
                       (file-system
                         (mount-point "/var/log")
                         (device (uuid "e2bc7478-31ec-4326-bd8e-8ae86d128738"
                                       'btrfs))
                         (flags '(no-atime))
                         (options
                          "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@var@log")
                         (type "btrfs"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "D300-61FF"
                                       'fat32))
                         (type "vfat"))
                       %base-file-systems)))

;; [TODO]
;; Disable zsh-newuser even if there is no zshrc file, cf. https://www.zsh.org/mla/users/2007/msg00400.html
;; iwd does not show in `herd status`
;; visual-studio-code’s files are all over the place, should they be in /gnu/.../opt?
;; LANG=C, ...
;; Caps Lock as compose key
;; use prefix pkg: for packages
;; custom kernel
;; compile flags for packages we're compiling anyway
