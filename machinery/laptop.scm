(define-module (machinery laptop)
  #:use-module (gnu)
  #:use-module (machinery common)
  #:use-module (nongnu packages video)
  #:export (laptop-home laptop-system))

(define laptop-home base-home)

(define (laptop-system keypit)
  (operating-system
    (inherit (base-system "laptop" "EF1A-92A8" "10.4.4.2/24" laptop-home keypit))
    (packages 
      (cons*
       intel-media-driver/nonfree
       %base-packages))))
       ; doesn't work for some reason
       ; (operating-system-packages this-operating-system)))))

