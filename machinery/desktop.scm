(define-module (machinery desktop)
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages wm)
  #:use-module (nongnu packages game-client)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (machinery common)
  #:export (desktop-home desktop-system))

(define desktop-home
  (home-environment
    (inherit base-home)
    (packages (cons* steam-nvidia blender (replace-mesa sway)
                     (filter (lambda (p) (not (equal? (package-name p) "sway")))
                             (home-environment-packages base-home))))
    (services (cons*
     (simple-service 'exec-sway home-bash-service-type
      (home-bash-extension
       (bash-profile
        (list
         (plain-file "exec-sway" "\
if [ -z \"$WAYLAND_DISPLAY\" ] && [ -n \"$XDG_VTNR\" ] && [ \"$XDG_VTNR\" -eq 1 ] ; then
    exec sway --unsupported-gpu
fi
")))))
     (home-environment-user-services base-home)))))

(define (desktop-system keypit)
  (let ((base (base-system "desktop" "5D9C-BC48" "10.4.4.4/24" desktop-home keypit)))
    (operating-system
      (inherit base)
      (kernel-arguments '("modprobe.blacklist=nouveau" "nvidia_drm.modeset=1"))
      (services (cons*
                  (service nvidia-service-type)
                  (operating-system-user-services base))))))
