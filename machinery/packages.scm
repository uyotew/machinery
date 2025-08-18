(define-module (machinery packages)
 #:use-module (gnu packages elf)
 #:use-module (gnu packages commencement)
 #:use-module (gnu packages pulseaudio)
 #:use-module (gnu packages xdisorg)
 #:use-module (gnu packages zig)
 #:use-module (guix gexp)
 #:use-module (guix packages)
 #:use-module (guix licenses)
 #:use-module (guix download)
 #:use-module (guix git-download)
 #:use-module (guix build-system copy)
 #:use-module (guix build-system zig))



(define (script-package name description gexp)
 (package
  (name name)
  (version "0.0")
  (source (program-file name gexp))
  (build-system copy-build-system)
  (arguments `(#:install-plan '((,name "bin/"))))
  (synopsis description)
  (description description)
  (home-page #f)
  (license #f)))

(define-public mpvl 
 (script-package "mpvl" "load previously played mpv media"
  #~(let* ((scandir (@ (ice-9 ftw) scandir))
           (read-line (@ (ice-9 rdelim) read-line))
           (read-string (@ (ice-9 rdelim) read-string))
           (open-pipe* (@ (ice-9 popen) open-pipe*))
           (dir (string-append (getenv "HOME") "/.local/state/mpv/watch_later/"))
           (files (scandir dir
             (lambda (name) (not (or (equal? name ".") (equal? name ".."))))
             (lambda (a b) (>
               (stat:mtime (stat (string-append dir a)))
               (stat:mtime (stat (string-append dir b)))))))
           (preview (false-if-exception (equal? (cadr (command-line)) "-p")))
           (index (or (false-if-exception (string->number
                       (if preview (caddr (command-line)) (cadr (command-line)))))
                   0))
           (url (begin
             (when (<= (length files) index) (error "index too high"))
             (with-input-from-file (string-append dir (list-ref files index))
               (lambda () (string-drop (read-line) 2))))))
     (display (string-append url "\n"))
     (if preview
       (system* "feh" ((compose car reverse string-tokenize read-string)
         (open-pipe* OPEN_READ "yt-dlp" "--list-thumbnails" url)))
       (system* "swaymsg" "exec" "mpv" url)))))

(define-public helix 
 (package
  (name "helix")
  (version "24.07")
  (source (origin 
   (method url-fetch)
   (uri (string-append "https://github.com/helix-editor/helix/releases/download/"
                        version "/helix-" version "-x86_64-linux.tar.xz"))
   (file-name (string-append "helix-" version ".tar.xz"))
   (sha256 (base32 "0p5a23z094233qzfh9ixdkgmgsyivjzpbds1s780w269j1320n62"))))
  (build-system copy-build-system)
  (arguments 
   (list 
     #:install-plan ''(("hx" "bin/helix") ("runtime" "/share/helix/runtime"))
     #:phases 
      #~(modify-phases %standard-phases
       (add-before 'validate-runpath 'patchelf
        (lambda _
         (let ((helix (string-append #$output "/bin/helix")) 
               (patchelf (string-append #+patchelf "/bin/patchelf"))
               (loader (string-append #$gcc-toolchain "/bin/ld.so"))
               (libgcc_s (string-append #$gcc-toolchain "/lib")))
          (invoke patchelf "--set-interpreter" loader helix)
          (invoke patchelf "--set-rpath" libgcc_s helix))))
       (add-after 'patchelf 'wrap-with-runtime
        (lambda _
         (wrap-program (string-append #$output "/bin/helix")
          `("HELIX_RUNTIME" = (,(string-append #$output "/share/helix/runtime")))))))))
  (synopsis "post-modern text editor")
  (description "it is text editor, edit text")
  (home-page "https://helix-editor.com")
  (license mpl2.0)))

(define-public statusbar
 (package
  (name "statusbar")
  (version "0.1")
  (source 
   (origin
    (method git-fetch)
    (uri (git-reference 
          (url "https://github.com/uyotew/statusbar")
          (commit "aa69d2d5ade2ca4cc9d102b466466f4f2f9e2317")))
    (sha256
     (base32 "0bs9xyyfqva1z9xval3dhg9cp3srfpfaladzdcj4hrkf0l4acqrj"))))
  (build-system zig-build-system)
  (arguments (list 
             #:tests? #f
             #:zig zig-0.14))
  (synopsis "statusbar for swaybar")
  (description "statusbar for swaybar")
  (home-page #f)
  (license #f)))
  
;;; has to have --search-prefix, since the build sysem adds -Dtarget=x86_64-linux-gnu
;;; which sets up the compiler for cross-compilation, which means it won't look for libraries
;;; https://github.com/ziglang/zig/issues/17384
;;; other guix zig packages doesn't seem to need this... don't know why
(define-public timer
 (package
  (name "timer")
  (version "0.1")
  (source 
   (origin
    (method git-fetch)
    (uri (git-reference 
          (url "https://github.com/uyotew/timer")
          (commit "357e1647df056a7bf87a370c3337614ab043f8b4")))
    (sha256
     (base32 "1fmf4xmsjbvq2067hhz4a1gr6ivzysn2fx9z5b03ak4ph3y5abkm"))))
  (build-system zig-build-system)
  (arguments (list 
              #:zig-build-flags
              #~(list "--search-prefix" #$pulseaudio)
              #:install-source? #f
              #:tests? #f
              #:zig zig-0.14))
  (inputs (list pulseaudio))
  (synopsis "timer")
  (description "timer")
  (home-page #f)
  (license #f)))

(define-public keypit
 (package
  (name "keypit")
  (version "0.1")
  (source 
   (origin
    (method git-fetch)
    (uri (git-reference 
          (url "https://github.com/uyotew/keypit.git")
          (commit "2e551c7530275d4fa2194b2f1d509242a9aa6c6d")))
    (sha256
     (base32 "1fxwjm4kbvk0lvv00ia215aa8awazh81y98hgqmydcp2syxjj2q2"))))
  (build-system zig-build-system)
  (arguments (list 
             #:tests? #f
             #:zig zig-0.14))
  (synopsis "password/secret manager")
  (description "password/secret manager")
  (home-page #f)
  (license #f)))
 
