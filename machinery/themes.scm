(define-module (machinery themes)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:export (theme
            theme?
            this-theme
            theme-primary
            theme-secondary
            theme-tertiary
            theme-soft
            theme-background
            theme-soft-background
            theme-text
            %default-theme
            theme-csi-primary
            theme-csi-secondary
            theme-csi-tertiary
            theme-csi-soft
            theme-csi-background
            theme-csi-soft-background
            theme-csi-text
            theme-light?
            theme-dark?
            hex-string->num-list
            hex-string->csi-fg
            hex-string->csi-bg
            substitute-theme))



;; every field should be a 6 byte hex string "rrggbb"
(define-record-type* <theme> theme make-theme
  theme?
  this-theme
  (primary         theme-primary)
  (secondary       theme-secondary)
  (tertiary        theme-tertiary)
  (soft            theme-soft)
  (background      theme-background)
  (soft-background theme-soft-background)
  (text            theme-text))

(define %default-theme
  (theme
    (primary "b54877")
    (secondary "aa77ee")
    (tertiary "559970")
    (soft "907050")
    (background "121211")
    (soft-background "282822")
    (text "d5b085")
    ; (background "bfb7b0")
    ; (soft-background "aeaa9a")
    ; (text "151215")
    ))

;; get csi code for theme
(define (theme-csi-primary theme) (hex-string->csi-fg (theme-primary theme)))
(define (theme-csi-secondary theme) (hex-string->csi-fg (theme-secondary theme)))
(define (theme-csi-tertiary theme) (hex-string->csi-fg (theme-tertiary theme)))
(define (theme-csi-soft theme) (hex-string->csi-fg (theme-soft theme)))
(define (theme-csi-background theme) (hex-string->csi-bg (theme-background theme)))
(define (theme-csi-soft-background theme) (hex-string->csi-bg (theme-soft-background theme)))
(define (theme-csi-text theme) (hex-string->csi-fg (theme-text theme)))

(define (mean list) (/ (reduce + 0 list) (length list)))
(define (theme-light? theme) (<= 128 (mean (hex-string->num-list (theme-background theme)))))
(define (theme-dark? theme) (> 128 (mean (hex-string->num-list (theme-background theme)))))

(define (hex-string->num-list str)
 (let loop ((res '()) (str str))
  (if (equal? "" str)
      res
      (loop (append res (list (string->number (string-take str 2) 16)))
            (string-drop str 2)))))

;; remember to add escape and [ in front of the strings returned from these hex-string->csi functions
;; and m at the end
(define (hex-string->csi-fg str)
  (apply format #f "38;2;~a;~a;~a" (hex-string->num-list str)))

(define (hex-string->csi-bg str)
  (apply format #f "48;2;~a;~a;~a" (hex-string->num-list str)))

;; dir should be a file-like object (can be used in gexp)
;; returns a derivation
(define (substitute-theme theme dir)
  (run-with-store (open-connection)
   (mlet %store-monad ((dir-drv (lower-object dir)))
    (gexp->derivation
      (string-append "themed" (string-trim dir-drv (lambda (c) (not (eqv? #\- c)))))
      (with-imported-modules '((guix build utils))
        #~(begin
          (use-modules (guix build utils))
          (copy-recursively #$dir #$output)
          (substitute* (find-files #$output)
            (("theme-primary") #$(theme-primary theme))
            (("theme-secondary") #$(theme-secondary theme))
            (("theme-tertiary") #$(theme-tertiary theme))
            (("theme-soft-background") #$(theme-soft-background theme)) ; make sure this is above "theme-soft"
            (("theme-soft") #$(theme-soft theme))
            (("theme-background") #$(theme-background theme))
            (("theme-text") #$(theme-text theme))
            (("theme-light\\?(.*):(.*);" _ t f) (if #$(theme-light? theme) t f))
            (("theme-dark\\?(.*):(.*);" _ t f) (if #$(theme-dark? theme) t f)))))))))
