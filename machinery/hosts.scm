(define-module (machinery hosts)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix)
  #:use-module (machinery packages)
  #:use-module (machinery themes)
  #:export (host-system
            host-home-dark
            host-home-light))

;; reconfigure the system based on the hostname by running:
;; sudo guix system reconfigure -e "((@ (machinery hosts) host-system))"
;; and one of these to reconfigure home
;; guix home reconfigure -e "((@ (machinery hosts) host-home-dark))"
;; guix home reconfigure -e "((@ (machinery hosts) host-home-light))"


;; HOSTNAME is not exported by default in guix, but can be used to override the /etc/hostname file
;; set for example HOSTNAME=laptop before reconfiguring a new laptop
(define (get-hostname)
  (or (getenv "HOSTNAME")
      (with-input-from-file "/etc/hostname" read-line)))

(define (resolve-host-var hostname var)
  (module-ref (resolve-interface `(machinery hosts ,(string->symbol hostname)))
              (string->symbol (string-append hostname "-" var))))


(define (host-home-dark)
  (define hostname (get-hostname))
  ((resolve-host-var hostname "home") (resolve-host-var hostname "dark-theme")))

(define (host-home-light)
  (define hostname (get-hostname))
  ((resolve-host-var hostname "home") (resolve-host-var hostname "light-theme")))


(define (make-keypit-func)
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

(define (host-system)
  ((resolve-host-var (get-hostname) "system") (make-keypit-func)))
