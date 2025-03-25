(define-module (machinery desktop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu packages graphics)
  #:use-module (nongnu packages game-client)
  #:use-module (nongnu services nvidia)
  #:use-module (machinery common)
  #:export (desktop-home desktop-system))

(define desktop-home
  (home-environment
    (inherit base-home)
    (packages (cons* steam-nvidia blender
                     (home-environment-packages base-home)))))

(define (desktop-system keypit)
  (let ((base (base-system "desktop" "5D9C-BC48" "10.4.4.4/24" desktop-home keypit)))
    (operating-system
      (inherit base)
      (kernel-arguments '("modprobe.blacklist=nouveau" "nvidia_drm.modeset=1"))
      (services (cons*
                  (service nvidia-service-type)
                  (operating-system-user-services base))))))
