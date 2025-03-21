(define-module (machinery desktop)
  #:use-module (gnu)
  #:use-module (machinery common)
  #:export (desktop-home desktop-system))

(define desktop-home base-home)

(define (desktop-system keypit)
  (operating-system
    (inherit (base-system "desktop" "XXXX-XXXX" "10.4.4.4/24" desktop-home keypit))))
