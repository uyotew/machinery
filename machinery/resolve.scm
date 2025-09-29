(define-module (machinery resolve)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix)
  #:use-module (machinery packages)
  #:export (host-system
            host-home))

;; use these functions to get the system and home based on the hostname
;; sudo guix system reconfigure -e "((@ (machinery resolve) host-system))"
;; and this to reconfigure home
;; guix home reconfigure -e "((@ (machinery resolve) host-home))"
;; while qutebrowser has issues:
;; https://codeberg.org/guix/guix/issues/2998
;; remember to use --no-grafts


;; HOSTNAME is not exported by default in guix, but can be used to override the /etc/hostname file
;; set for example HOSTNAME=laptop before reconfiguring a new laptop
(define (get-hostname)
  (or (getenv "HOSTNAME")
      (with-input-from-file "/etc/hostname" read-line)))

(define (host-home)
  (let* ((hostname (get-hostname))
         (module (resolve-interface `(machinery ,(string->symbol hostname)))))
   (module-ref module (string->symbol (string-append hostname "-home")))))

;; should return a function taking the keypit function as an argument
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
