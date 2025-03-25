(define-module (machinery laptop)
  #:use-module (gnu)
  #:use-module (machinery common)
  #:use-module (nongnu packages video)
  #:export (laptop-home laptop-system))

(define laptop-home base-home)

(define (laptop-system keypit)
  (let ((base (base-system "laptop" "EF1A-92A8" "10.4.4.2/24" laptop-home keypit)))
    (operating-system
      (inherit base)
      (packages (cons*
                  intel-media-driver/nonfree
                  %operating-system-packages base)))))

