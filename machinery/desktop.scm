(define-module (machinery desktop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu packages graphics)
  #:use-module (nongnu packages game-client)
  #:use-module (machinery common)
  #:export (desktop-home desktop-system))

(define desktop-home
  (home-environment
    (inherit base-home)
    (packages (cons* steam blender
                     (home-environment-packages base-home)))))

(define (desktop-system keypit)
  (operating-system
    (inherit (base-system "desktop" "5D9C-BC48" "10.4.4.4/24" desktop-home keypit))))
