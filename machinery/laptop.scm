(define-module (machinery laptop)
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (machinery common)
  #:use-module (nongnu packages video)
  #:export (laptop-home laptop-system))

(define laptop-home
  (home-environment
    (inherit base-home)
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
     (home-environment-user-services base-home)))))

(define (laptop-system keypit)
  (let ((base (base-system "laptop" "EF1A-92A8" "10.4.4.2/24" laptop-home keypit)))
    (operating-system
      (inherit base)
      (packages (cons*
                  intel-media-driver/nonfree
                  %operating-system-packages base)))))

