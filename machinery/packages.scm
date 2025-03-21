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
             (when (< (length files) index) (error "index too high"))
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
          (commit "bb0e7f1c80d52ce02b845682362fe3be124c4c38")))
    (sha256
     (base32 "1xigdg9692y9l04rb6k9sgm18s1h4pj6cl857c3q3vjbnfw6q3sr"))))
  (build-system zig-build-system)
  (arguments (list 
             #:tests? #f
             #:zig zig-0.14))
  (synopsis "statusbar for swaybar")
  (description "statusbar for swaybar")
  (home-page #f)
  (license #f)))
  
(define-public timer
 (package
  (name "timer")
  (version "0.1")
  (source 
   (origin
    (method git-fetch)
    (uri (git-reference 
          (url "https://github.com/uyotew/timer")
          (commit "29df6fc5c963d50744b262a7d3261df0e3eb33f7")))
    (sha256
     (base32 "1schmqpr0v3h22jb1cakyf7b0f9gsgly3vnx1ccmrd567g2mh0w9"))))
  (build-system zig-build-system)
  (arguments (list 
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
          (commit "3cc08cbf5f0624e03d28befdfb70e54ac4f582cd")))
    (sha256
     (base32 "07jcs9vkllax06wmdxw67n6ljfjkg0l7yx9k6zwf4gzpsz17r5r2"))))
  (build-system zig-build-system)
  (arguments (list 
             #:tests? #f
             #:zig zig-0.14))
  (synopsis "password/secret manager")
  (description "password/secret manager")
  (home-page #f)
  (license #f)))
  
