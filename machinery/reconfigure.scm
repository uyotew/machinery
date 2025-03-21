(define-module (machinery reconfigure)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (get-host-system get-keypit main))

;; use this to reconfigure the system based on the hostname
;; sudo guix system reconfigure -e "((@ (machinery reconfigure) main))"

;; host-system should be a function taking the keypit function as an argument  
(define (get-host-system)
  ;; HOSTNAME is not exported by default in guix, but can be used to override the /etc/hostname file
  ;; set for example HOSTNAME=laptop before reconfiguring a new laptop
  (let* ((hostname (or (getenv "HOSTNAME")
                       (with-input-from-file "/etc/hostname" read-line)))
         (module (resolve-interface `(machinery ,(string->symbol hostname)))))
   (module-ref module (string->symbol (string-append hostname "-system")))))

(define (get-keypit)
  (let ((password (read-line (open-input-pipe "read -s -p \"keypit password: \"; echo $REPLY")))
        ;; assumes keypit.db is stored in the same directory as this file
        (filepath (string-append (dirname (current-filename)) "/keypit.db")))
    (lambda (entry-name field-name)
      (let* ((port (open-pipe* OPEN_READ "keypit" "--stdout" 
                             "-d" filepath "-p" password 
                             "get" entry-name field-name))
            (result (read-line port)))
        (if (zero? (status:exit-val (close-pipe port))) 
            result 
            (error "keypit error"))))))

(define (main) ((get-host-system) (get-keypit)))
