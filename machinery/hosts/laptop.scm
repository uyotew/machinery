(define-module (machinery hosts laptop)
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (nongnu packages video)
  #:use-module (machinery base)
  #:use-module (machinery themes)
  #:export (laptop-home
            laptop-system
            laptop-dark-theme
            laptop-light-theme))

(define laptop-dark-theme %default-dark-theme)
(define laptop-light-theme %default-light-theme)

(define (laptop-home theme)
  (let ((base (base-home theme)))
    (home-environment
      (inherit base)
      (services (cons*
       (simple-service 'exec-sway home-bash-service-type
        (home-bash-extension
         (bash-profile
          (list
           (plain-file "exec-sway" "\
if [ -z \"$WAYLAND_DISPLAY\" ] && [ -n \"$XDG_VTNR\" ] && [ \"$XDG_VTNR\" -eq 1 ] ; then
    exec sway
fi
")))))
       (home-environment-user-services base))))))

(define (laptop-system keypit)
  (let ((base (base-system "laptop" "EF1A-92A8" "10.4.4.2/24" keypit)))
    (operating-system
      (inherit base)
      (packages (cons*
                  intel-media-driver/nonfree
                  (operating-system-packages base))))))

