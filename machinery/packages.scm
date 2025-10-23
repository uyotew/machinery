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

(define-public statusbar
 (let ((commit "6a03952140322ba188209f51f3a4eb0f8af2ea0d")
        (revision "1"))
  (package
   (name "statusbar")
   (version (git-version "1.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/uyotew/statusbar")
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0wwbrsc1dh95z0mbk8wgs73vzi9hfkbw435qd44mwb77jid40hyn"))))
   (build-system zig-build-system)
   (arguments (list
               #:tests? #f
               #:install-source? #f
               #:zig zig-0.15))
   (synopsis "Statusbar for swaybar")
   (description "Statusbar for swaybar")
   (home-page #f)
   (license #f))))
  
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
 (let ((commit "173054b05a24081550f501cab7a244f5f2ca6fd6")
       (revision "1"))
  (package
   (name "keypit")
   (version (git-version "1.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/uyotew/keypit")
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1fjxhc9sks360hgh5a6dp65xcln6i1m6myvh0w0n8yw74rm84q9a"))))
   (build-system zig-build-system)
   (arguments (list
               #:tests? #f
               #:install-source? #f
               #:zig zig-0.15))
   (synopsis "password/secret manager")
   (description "password/secret manager")
   (home-page #f)
   (license #f))))
 
