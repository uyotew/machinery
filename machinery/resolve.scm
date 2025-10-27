(define-module (machinery resolve)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix)
  #:use-module (machinery packages)
  #:use-module (machinery themes)
  #:export (host-system
            host-home-light
            host-home-dark))

;; use these functions to get the system and home based on the hostname
;; sudo guix system reconfigure -e "((@ (machinery resolve) host-system))"
;; and one of these to reconfigure home
;; guix home reconfigure -e "((@ (machinery resolve) host-home-light))"
;; guix home reconfigure -e "((@ (machinery resolve) host-home-dark))"


;; HOSTNAME is not exported by default in guix, but can be used to override the /etc/hostname file
;; set for example HOSTNAME=laptop before reconfiguring a new laptop
(define (get-hostname)
  (or (getenv "HOSTNAME")
      (with-input-from-file "/etc/hostname" read-line)))

;; returns a procedure that takes a theme to create a home profile
(define (host-home-maker)
  (let* ((hostname (get-hostname))
         (module (resolve-interface `(machinery ,(string->symbol hostname)))))
   (module-ref module (string->symbol (string-append hostname "-home")))))

(define (host-home-light) ((host-home-maker) %default-light-theme))
(define (host-home-dark) ((host-home-maker) %default-dark-theme))

;; returns a procedure taking the keypit procedure as an argument
(define (host-system-maker)
  (let* ((hostname (get-hostname))
         (module (resolve-interface `(machinery ,(string->symbol hostname)))))
   (module-ref module (string->symbol (string-append hostname "-system")))))


(define (init-keypit-func)
  (let ((password (getpass "Keypit password: "))
        (keypit (string-append
                  (with-store store (let ((drv (package-derivation store keypit)))
                                      (build-derivations store (list drv))
                                      (derivation->output-path drv)))
                 "/bin/keypit"))
        (filepath (string-append (current-source-directory) "/keypit.db")))
    (lambda (entry-name field-name)
      (let* ((port (open-pipe* OPEN_READ keypit "--stdout"
                             "-d" filepath "-p" password 
                             "get" entry-name field-name))
            (result (read-line port)))
        (if (zero? (status:exit-val (close-pipe port))) 
            result 
            (error "keypit error"))))))

(define (host-system) ((host-system-maker) (init-keypit-func)))
