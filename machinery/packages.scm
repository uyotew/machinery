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

(define-public statusbar
 (package
  (name "statusbar")
  (version "1.0")
  (source 
   (origin
    (method git-fetch)
    (uri (git-reference 
          (url "https://github.com/uyotew/statusbar")
          (commit version)))
    (sha256
     (base32 "0lbl387dmpqipws6xviyg0qf8aj09d4sylf6zjircl4nfqfw8prq"))))
  (build-system zig-build-system)
  (arguments (list 
              #:tests? #f
              #:install-source? #f
              #:zig zig-0.15))
  (synopsis "Statusbar for swaybar")
  (description "Statusbar for swaybar")
  (home-page #f)
  (license #f)))
  
;;; has to have --search-prefix, since the build sysem adds -Dtarget=x86_64-linux-gnu
;;; which sets up the compiler for cross-compilation, which means it won't look for libraries
;;; https://github.com/ziglang/zig/issues/17384
;;; other guix zig packages doesn't seem to need this... don't know why
(define-public timer
 (package
  (name "timer")
  (version "1.0")
  (source 
   (origin
    (method git-fetch)
    (uri (git-reference 
          (url "https://github.com/uyotew/timer")
          (commit version)))
    (sha256
     (base32 "0jx05xhm3bhdwl5srlbrpbyfb1wrc7wn348l06xsgc7csbad010f"))))
  (build-system zig-build-system)
  (arguments (list 
              #:phases
               #~(modify-phases %standard-phases
                  (delete 'validate-runpath)) ; validation fails, but the executable works anyways
              #:tests? #f
              #:install-source? #f
              #:zig-build-flags
              #~(list "--search-prefix" #$pulseaudio)
              #:zig zig-0.15))
  (inputs (list pulseaudio))
  (synopsis "timer")
  (description "timer")
  (home-page #f)
  (license #f)))

(define-public keypit
 (package
  (name "keypit")
  (version "1.0")
  (source 
   (origin
    (method git-fetch)
    (uri (git-reference 
          (url "https://github.com/uyotew/keypit")
          (commit version)))
    (sha256
     (base32 "1nnyr4isndayw4g8yjwgxmxmabrxgqr895iyhq2l35qa08cn1l85"))))
  (build-system zig-build-system)
  (arguments (list 
              #:tests? #f
              #:install-source? #f
              #:zig zig-0.15))
  (synopsis "password/secret manager")
  (description "password/secret manager")
  (home-page #f)
  (license #f)))
 
