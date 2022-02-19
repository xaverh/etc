(use-modules (gnu)
             (srfi srfi-1)
             (guix packages)
             (guix download)
             (guix build-system font)
             (guix build-system trivial)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (nonguix licenses))
(use-service-modules desktop networking ssh xorg)

(define-public brave
  (package
   (name "brave")
   (version "1.35.101")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/brave/brave-browser/releases/download/v" version "/brave-browser_" version "_amd64.deb"))
     (sha256
      (base32
       "02gfrp02lggly6qqw5cd7igmxr8842aa0g935sc7hvrv3zlqp4db"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("patchelf" ,(@ (gnu packages elf) patchelf))
      ("tar" ,(@ (gnu packages base) tar))
      ("xz" ,(@ (gnu packages compression) xz))
      ("binutils" ,(@ (gnu packages base) binutils))))
   (inputs
    `(("alsa-lib"              ,(@ (gnu packages linux) alsa-lib))
      ("atk"                   ,(@ (gnu packages gtk) atk))
      ("bash"                  ,(@ (gnu packages bash) bash))
      ("cairo"                 ,(@ (gnu packages gtk) cairo))
      ("cups"                  ,(@ (gnu packages cups) cups))
      ("dbus"                  ,(@ (gnu packages glib) dbus))
      ("expat"                 ,(@ (gnu packages xml) expat))
      ("fontconfig"            ,(@ (gnu packages fontutils) fontconfig))
      ("gcc"                   ,(@ (gnu packages gcc) gcc) "lib")
      ("gdk-pixbuf"            ,(@ (gnu packages gtk) gdk-pixbuf))
      ("glib"                  ,(@ (gnu packages glib) glib))
      ("glibc"                 ,(@ (gnu packages base) glibc))
      ("gobject-introspection" ,(@ (gnu packages glib) gobject-introspection))
      ("gtk+"                  ,(@ (gnu packages gtk) gtk+))
      ("libxcomposite"         ,(@ (gnu packages xorg) libxcomposite))
      ("libxcursor"            ,(@ (gnu packages xorg) libxcursor))
      ("libxdamage"            ,(@ (gnu packages xorg) libxdamage))
      ("libxrandr"             ,(@ (gnu packages xorg) libxrandr))
      ("libxscrnsaver"         ,(@ (gnu packages xorg) libxscrnsaver))
      ("libxtst"               ,(@ (gnu packages xorg) libxtst))
      ("nspr"                  ,(@ (gnu packages nss) nspr))
      ("nss"                   ,(@ (gnu packages nss) nss))
      ("pango"                 ,(@ (gnu packages gtk) pango))
      ("pulseaudio"            ,(@ (gnu packages pulseaudio) pulseaudio))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((output (assoc-ref %outputs "out"))
               (source (assoc-ref %build-inputs "source"))
               (working-dir output)
               (ar (string-append (assoc-ref %build-inputs "binutils") "/bin/ar"))
               (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (patchelf (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf")))

          ;; Extraction phase
          (mkdir-p working-dir)
          (setenv "PATH" (string-append (assoc-ref %build-inputs "xz") "/bin"))
          (zero? (system* ar "x" source "data.tar.xz"))
          (zero? (system* tar "xvf" "data.tar.xz" "-C" working-dir "./opt"))

          ;; Patching phase
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/brave.com/brave/brave"))
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/brave.com/brave/chrome_crashpad_handler"))
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/brave.com/brave/chrome-sandbox"))

          ;; Wrapping phase - give it libraries and some other envars
          (setenv "PATH" (string-append (assoc-ref %build-inputs "bash") "/bin"))
          (wrap-program (string-append working-dir "/opt/brave.com/brave/brave")
			`("LD_LIBRARY_PATH" ":" prefix ("/run/current-system/profile/lib")))
          (wrap-program (string-append working-dir "/opt/brave.com/brave/brave")
			`("LD_LIBRARY_PATH" ":" prefix (,(string-append
							  (assoc-ref %build-inputs "nss")
							  "/lib/nss"))))
          (wrap-program (string-append working-dir "/opt/brave.com/brave/brave")
			`("LD_LIBRARY_PATH" ":" prefix
			  ,(map (lambda (input)
				  (string-append (assoc-ref %build-inputs input) "/lib"))
				'("libxcomposite" "libxtst" "nss" "nspr"
				  "cups" "libxrandr" "libxscrnsaver" "alsa-lib"
				  "gcc" "libxcursor" "libxdamage"
				  "gobject-introspection" "glib" "dbus" "expat"
				  "pango" "cairo" "atk" "gtk+" "gdk-pixbuf"
				  "pulseaudio")))
			;; Reads fontconfig config (version 1.12.6 compatible required)
			`("FONTCONFIG_PATH" ":" prefix (,(string-append
							  (assoc-ref %build-inputs "fontconfig")
							  "/etc/fonts"))))
	  ;; --enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=VaapiVideoDecoder --use-gl=egl

          ;; Polishing phase
          (mkdir-p (string-append working-dir "/bin"))
          (symlink
           "../opt/brave.com/brave/brave"
           (string-append working-dir "/bin/brave"))

          (with-directory-excursion (string-append working-dir "/opt/brave.com/brave")
				    (for-each
				     (lambda (size)
				       (let ((icons (string-append output "/share/icons/hicolor/"
								   size "x" size "/apps")))
					 (mkdir-p icons)
					 (copy-file (string-append "product_logo_" size ".png")
						    (string-append icons "/brave.png"))))
				     '("24" "48" "64" "128" "256")))

          (mkdir-p (string-append (assoc-ref %outputs "out") "/share/applications"))
          (with-output-to-file
              (string-append (assoc-ref %outputs "out") "/share/applications/brave.desktop")
            (lambda _
              (format #t "[Desktop Entry]~@
                    Name=Brave~@
                    Comment=Browse 3x faster than Chrome~@
                    Exec=~a/bin/brave~@
                    Icon=brave~@
                    Categories=Network~@
                    Type=Application~%"
                      (assoc-ref %outputs "out"))))
          ))))
   (home-page "https://www.brave.com")
   (synopsis "Brave web browser.")
   (description "Brave web browser.")
   (license #f)))

#!

(define-public discord
  (package
   (name "discord")
   (version "0.0.16")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://cdn.discordapp.com/apps/linux/"
                                version
                                "/"
                                name
                                "-"
                                version
                                ".tar.gz"))
            (sha256
             (base32
              "1s9qym58cjm8m8kg3zywvwai2i3adiq6sdayygk2zv72ry74ldai"))))
   (build-system trivial-build-system)
   (inputs `(("alsa-lib"      ,(@ (gnu packages linux) alsa-lib))
	     ("atk"           ,(@ (gnu packages gtk) atk))
             ("at-spi2-atk"   ,(@ (gnu packages gtk) atk))
             ("cairo"         ,(@ (gnu packages gtk) cairo))
	     ("cups"          ,(@ (gnu packages cups) cups))
             ("dbus"          ,(@ (gnu packages glib) dbus))
	     ("expat"         ,(@ (gnu packages xml) expat))
             ("fontconfig"    ,(@ (gnu packages fontutils) fontconfig))
             ("freetype"      ,(@ (gnu packages fontutils) freetype))
	     ("gdk-pixbuf"    ,(@ (gnu packages gtk) gdk-pixbuf))
             ("glib"          ,(@ (gnu packages glib) glib))
             ("gtk3"          ,(@ (gnu packages gtk) gtk+))
             ("libcxx"        ,(@ (gnu packages llvm) clang-runtime))
             ("libffmpeg"     ,(@ (gnu packages video) ffmpeg))
	     ("libnotify"     ,(@ (gnu packages gnome) libnotify))
	     ("libpulseaudio" ,(@ (gnu packages pulseaudio) pulseaudio))
             ("libuuid"       ,(@ (gnu packages linux) util-linux))
             ("libx11"        ,(@ (gnu packages xorg) libx11))
             ("libxcb"        ,(@ (gnu packages xorg) libxcb))
             ("libxcomposite" ,(@ (gnu packages xorg) libxcomposite))
             ("libxcursor"    ,(@ (gnu packages xorg) libxcursor))
             ("libxdamage"    ,(@ (gnu packages xorg) libxdamage))
             ("libxext"       ,(@ (gnu packages xorg) libxext))
             ("libxfixes"     ,(@ (gnu packages xorg) libxfixes))
             ("libxi"         ,(@ (gnu packages xorg) libxi))
             ("libxrandr"     ,(@ (gnu packages xorg) libxrandr))
             ("libxrender"    ,(@ (gnu packages xorg) libxrender))
	     ("libxscrnsaver" ,(@ (gnu packages xorg) libxscrnsaver))
             ("libxtst"       ,(@ (gnu packages xorg) libxtst))
             ("nspr"          ,(@ (gnu packages nss) nspr))
             ("nss"           ,(@ (gnu packages nss) nss))
             ("pango"         ,(@ (gnu packages gtk) pango))
	     ("gcc"           ,(@ (gnu packages gcc) gcc) "lib")))
   (native-inputs `(("patchelf" ,(@ (gnu packages elf) patchelf))
                    ("tar"      ,(@ (gnu packages base) tar))
                    ("gzip"     ,(@ (gnu packages compression) gzip))
                    ("glibc"    ,(@ (gnu packages base) glibc))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils)
                               (srfi srfi-26))
                  (let* ((patchelf (assoc-ref %build-inputs "patchelf"))
                         (glibc    (assoc-ref %build-inputs "glibc"))
                         (source   (assoc-ref %build-inputs "source"))
                         (tar      (assoc-ref %build-inputs "tar"))
                         (gzip     (assoc-ref %build-inputs "gzip"))
                         (output   (assoc-ref %outputs "out")))
                    (define libpath "")
                    (for-each
                     (lambda (i)
                       (set! libpath
                         (string-append libpath (string-append (cdr i) "/lib") ":")))
                     %build-inputs)
                    (set! libpath
                      (string-append libpath (assoc-ref %build-inputs "nss") "/lib/nss"))
                    (setenv "PATH" (string-append gzip "/bin"))
                    (mkdir-p (string-append output "/opt/discord"))
                    (invoke (string-append tar "/bin/tar") "xpvf" source)
                    (copy-recursively "./Discord" (string-append output "/opt/discord"))
                    (mkdir-p (string-append output "/bin")) 
                    (mkdir-p (string-append output "/share/pixmaps"))
                    (chmod (string-append output "/opt/discord/Discord") #o755)
                    (let ((output-port (open-file (string-append output "/bin/discord") "a")))
                      (display "#!/bin/sh\n" output-port)
                      (display (string-append "cd " output "/opt/discord\n") output-port)
                      (display (string-append "LD_LIBRARY_PATH=" libpath " " "./Discord\n") output-port)
                      (close output-port))
                    (chmod (string-append output "/bin/discord") #o755) 
                    (invoke (string-append patchelf "/bin/patchelf")
                            "--set-interpreter"
                            (string-append glibc "/lib/ld-linux-x86-64.so.2")
                            (string-append output "/opt/discord/Discord"))
                    ;(link (string-append %output "/opt/discord/Discord")
                    ;      (string-append %output "/bin/discord"))
                    (link (string-append %output "/opt/discord/discord.png")
                          (string-append %output "/share/pixmaps/discord.png"))
                    #t))))
   (synopsis "Discord chat client.")
   (description "Discord chat client.")
   (license #f)
   (home-page "https://discordapp.com")))

!#

(define-public font-joypixels
  (package
   (name "font-joypixels")
   (version "6.6.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://cdn.joypixels.com/arch-linux/font/"
				version
				"/joypixels-android.ttf"))
            (sha256
             (base32
              "17gjaz7353zyprmds64p01qivy2r8pwf88nvvhi57idas2qd604n"))))
   (build-system font-build-system)
   (home-page "https://www.joypixels.com")
   (synopsis "Emoji as a Service")
   (description "An emoji font with originally crafted icon designs.")
   (license (nonfree "https://cdn.joypixels.com/arch-linux/license/free-license.pdf"))))

(define-public font-sf-mono
  (package
   (name "font-sf-mono")
   (version "16.0d2e1")
   (native-inputs `(("p7zip" ,(specification->package "p7zip"))))
   (source (origin
            (method url-fetch)
            (uri (string-append "https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg"))
            (sha256
             (base32
              "0spqlf4ndxh8k6dr7vcrhqrsb8b767mgab12a6sz46g19lz8jy7j"))))
   (build-system font-build-system)
   (arguments
    '(#:phases (modify-phases %standard-phases
			      (replace 'unpack
				       (lambda* (#:key inputs #:allow-other-keys)
					 (let ((source (assoc-ref inputs "source")))
					   (invoke "7z" "x" source "SFMonoFonts/SF Mono Fonts.pkg")
					   (invoke "7z" "x" "SFMonoFonts/SF Mono Fonts.pkg")
					   (invoke "7z" "x" "Payload~")))))))
    (home-page "https://developer.apple.com/fonts/")
    (synopsis "SF Mono monospace typeface")
    (description
     "This monospaced variant of San Francisco enables alignment between rows and columns of text, and is used in coding environments like Xcode. SF Mono features six weights and supports Latin, Greek, and Cyrillic scripts.")
    (license (nonfree "https://www.apple.com/legal/sla/"))))


(operating-system
  (kernel linux)
  (initrd (lambda (file-systems . rest)
            (apply microcode-initrd file-systems
              #:initrd base-initrd
              #:microcode-packages (list intel-microcode)
              rest)))
  (firmware (cons* iwlwifi-firmware
                   %base-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (host-name "andermatt")
  (users (cons* (user-account
                  (name "xha")
                  (comment "Xaver Hellauer")
                  (group "users")
                  (home-directory "/home/xha")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video" "input")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "nss-certs")
            (specification->package "sx")
            (specification->package "dwm")
            (specification->package "dmenu")
            brave
            ; discord
            font-joypixels
            font-sf-mono
            (specification->package "kitty")
            (specification->package "emacs-next-pgtk")
            (specification->package "firefox")
            (specification->package "htop")
            (specification->package "pavucontrol")
            (specification->package "mpv")
            (specification->package "pfetch"))
      %base-packages))
  (services
    (cons*
      (service xorg-server-service-type)
      (modify-services
        (remove (lambda (service)
                (member (service-kind service)
                  (list gdm-service-type)))
                (modify-services %desktop-services
                  (guix-service-type config => (guix-configuration
                    (inherit config)
                    (substitute-urls
                      (append (list "https://substitutes.nonguix.org")
                        %default-substitute-urls))
                    (authorized-keys
                      (append (list (plain-file "non-guix.pub"
                                      "(public-key 
                                        (ecc 
                                          (curve Ed25519)
                                          (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                        %default-authorized-guix-keys)))))))))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "3d70525a-3f5e-471d-b731-f6ade7c917c2")))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "e2bc7478-31ec-4326-bd8e-8ae86d128738"
                     'btrfs))
             (flags '(no-atime))
             (needed-for-boot? #t)
             (options "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/guix")
             (type "btrfs"))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "3397-BD8C" 'fat32))
             (type "vfat"))
           %base-file-systems)))
