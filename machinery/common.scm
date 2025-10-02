(define-module (machinery common)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services networking)
  #:use-module (gnu services dbus)
  #:use-module (gnu services vpn)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages zig)
  #:use-module (gnu packages pdf)
  #:use-module (guix channels)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (machinery packages)
  #:export (%channels
            base-system 
            base-home))

(define %channels
 (append %default-channels 
  (list
   (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    (introduction
     (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
    (name 'machinery)
    (url "https://github.com/uyotew/machinery")))))

(define nonguix-pubkey "\
(public-key 
 (ecc 
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
 )
)
")

(define sudoers-spec
  (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
%wheel ALL=(ALL) NOPASSWD: /run/current-system/profile/bin/light
"))

(define (base-system host-name boot-fs-uuid wg-address home keypit)
  (operating-system
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))

    (issue "")
    (host-name host-name)
    (timezone "Europe/Oslo")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "no"))

    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets '("/boot/efi"))))

    (file-systems (append
                   (list (file-system
                           (device (file-system-label "ROOT"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device (uuid boot-fs-uuid 'fat))
                           (mount-point "/boot/efi")
                           (type "vfat")))
                   %base-file-systems))

    (users (cons (user-account
                  (name "uyotew")
                  (group "users")
                  (supplementary-groups '("wheel" "netdev" 
                                          "audio" "video")))
                 %base-user-accounts))

    (packages (cons light %base-packages))
    (sudoers-file sudoers-spec)

    (services (cons* 
     fontconfig-file-system-service
     (service dbus-root-service-type)
     (service polkit-service-type)
     (service elogind-service-type)
     (service wpa-supplicant-service-type)
     (service ntp-service-type)
     (service bluetooth-service-type
             (bluetooth-configuration 
               (auto-enable? #t)))
     (service guix-home-service-type `(("uyotew" ,home)))
     (service wireguard-service-type 
      (wireguard-configuration
       (addresses `(,wg-address))
       (dns '("10.4.4.1"))
       (peers 
        (list 
         (wireguard-peer
          (name "bgl")
          ;; public ip might change, if so, rebuild after changing it's value
          (endpoint (string-append (keypit "home-router" "public_ip") ":36098"))
          ;; need to manually copy the public key over to bgl when running on a new machine
          ;; (see it by running sudo wg)
          (public-key "0DdOrmHLje5h2ysDHxQ9ljtW4RmIZhczN9DWV9Uz5wo=")
          (allowed-ips '("10.4.4.0/24")))))))
     ;; need to manually connect to wifi with nmcli
     (service network-manager-service-type)
     (modify-services %base-services
      (guix-service-type config => (guix-configuration
       (inherit config)
       (channels %channels)
       (substitute-urls (cons "https://substitutes.nonguix.org" %default-substitute-urls))
       (authorized-keys (cons (plain-file "nonguix.pub" nonguix-pubkey) %default-authorized-guix-keys)))))))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

(define* (base-home #:optional (prompt-color 107) (dollar-color 198))
 (home-environment
  (packages (list 
             zig-0.15 git openssh wireplumber bluez
             font-hack font-google-noto-emoji font-google-noto-sans-cjk
             sway statusbar wl-clipboard qutebrowser foot mpv mpvl yt-dlp feh
             helix keypit timer man-pages zathura zathura-pdf-poppler))
  (services (list 
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (service home-dotfiles-service-type
    (home-dotfiles-configuration
     (directories '("dotfiles"))))
   (service home-files-service-type
    `((".local/share/keypit.db" ,(local-file "keypit.db"))))
   (simple-service 'bash-ext home-bash-service-type
  		(home-bash-extension
  		 (bash-profile
        (list
         (plain-file "envs" "\
export HISTCONTROL='erasedups'
export LESS='-R --file-size --use-color'
export EDITOR='hx'
")))
		   (aliases
		    '(("ll" . "ls -lhp ")
		      ("la" . "ls -lhpa ")
          ("feh" . "feh -. ")
          ("info" . "info --vi-keys" )
		      ("duc" . "du -had 1 | sort -hr ")))
       (bashrc (list 
        (plain-file "prompt" (string-append
         "PS1='"
		     "\\[\\e[38;5;" (number->string prompt-color) "m\\]"
		     "[\\u@\\h \\W]\\[\\e[38;5;" (number->string dollar-color) "m\\]"
		     "${GUIX_ENVIRONMENT:+[env]}\\$\\[\\e[0m\\] " ; display [env] when in a guix shell
		     "\\[\\e]2;\\w - foot\\a\\]'")) ; append path to terminal window name 
        (plain-file "show-reminder"
                     "cat ~/reminder")))))))))
 
