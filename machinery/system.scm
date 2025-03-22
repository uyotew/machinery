(define-module (machinery system)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix utils)
  #:export (resolve-system))

;; use this to get the system based on the hostname
;; sudo guix system reconfigure -e "((@ (machinery system) resolve-system))"

;; should return a function taking the keypit function as an argument
(define (get-host-system)
  ;; HOSTNAME is not exported by default in guix, but can be used to override the /etc/hostname file
  ;; set for example HOSTNAME=laptop before reconfiguring a new laptop
  (let* ((hostname (or (getenv "HOSTNAME")
                       (with-input-from-file "/etc/hostname" read-line)))
         (module (resolve-interface `(machinery ,(string->symbol hostname)))))
   (module-ref module (string->symbol (string-append hostname "-system")))))

(define (get-keypit)
  (let ((password (read-line (open-input-pipe "read -s -p \"keypit password: \"; echo $REPLY")))
        (filepath (string-append (current-source-directory) "/keypit.db")))
    (lambda (entry-name field-name)
      (let* ((port (open-pipe* OPEN_READ "keypit" "--stdout" 
                             "-d" filepath "-p" password 
                             "get" entry-name field-name))
            (result (read-line port)))
        (if (zero? (status:exit-val (close-pipe port))) 
            result 
            (error "keypit error"))))))

(define (resolve-system) ((get-host-system) (get-keypit)))
