(define-module (machinery bgl)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages video)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (gnu services ssh)
  #:use-module (gnu services vpn)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (machinery packages)
  #:use-module (machinery common)
  #:export (bgl-home bgl-system))

(define bgl-home
 (home-environment
  ;; if ssh is bad, maybe i need to get a hold of foot terminfo somehow..
  ;; like the foot-terminfo package on arch linux
  (packages (list font-hack font-google-noto-emoji font-google-noto-sans-cjk ; fonts for mpv to use
             helix mpv mpvl yt-dlp))
  (services (list 
   (service home-dbus-service-type)
   (service home-files-service-type
    `((".local/share/keypit.db" ,(local-file "keypit.db"))))
   (simple-service 'bash-ext home-bash-service-type
  		(home-bash-extension
  		 (bash-profile
  		  (list 
  		   (plain-file "envs" "\
export HISTCONTROL='erasedups'
export LESS='-R --file-size --use-color'
export EDITOR='helix'
")))
		   (aliases
		    '(("ll" . "ls -lhp ")
		      ("la" . "ls -lhpa ")
		      ("hx" . "helix ")
          ("info" . "info --vi-keys" )
		      ("duc" . "du -had 1 | sort -hr ")))
       (bashrc (list 
        (plain-file "prompt" (string-append
         "PS1='"
		     "\\[\\e[38;5;122m\\][\\u@\\h \\W]\\[\\e[38;5;198m\\]"
		     "${GUIX_ENVIRONMENT:+[env]}\\$\\[\\e[0m\\] ")))))) ; display [env] when in a guix shell
   (simple-service 'config-files 
                  home-xdg-configuration-files-service-type
                  ;; append to mpv.conf to run mpv over hdmi without any window manager
                  `(("mpv/mpv.conf" ,(mixed-text-file "bgl-mpv.conf"
                                                 (local-file "dotfiles/.config/mpv/mpv.conf")
                                                 "\
#if aspect-ratio looks strange, uncomment the line under this one
#no-keepaspect
drm-connector=HDMI-A-2
drm-device=/dev/dri/card0
audio-device=alsa/hdmi:CARD=PCH,DEV=0"))
                    ("mpv/input.conf" ,(local-file "dotfiles/.config/mpv/input.conf"))
                    ("helix/config.toml" ,(local-file "dotfiles/.config/helix/config.toml"))
                    ("helix/themes/clean.toml" ,(local-file "dotfiles/.config/helix/themes/clean.toml"))))))))


;; need to set up static ip as well 192.168.10.55
;; maybe rsync?? (rsync-service-type)
;; or restic-backup-service-type??
;; not sure if dnsmasq currently is set up to be a stub resolver
(define (bgl-system keypit) 
 (operating-system
  (inherit (base-system "bgl" "XXXX-XXXX" "10.4.4.1/24" bgl-home))
  (services (cons*
   (simple-service 'add-hosts 
                    hosts-service-type
                    (list
                     (host "10.4.4.1" "bgl.bgl")
                     (host "10.4.4.2" "laptop.bgl")
                     (host "10.4.4.3" "tablet.bgl")
                     (host "10.4.4.4" "desktop.bgl")
                     (host "10.4.4.5" "phone.bgl")))
   (service openssh-service-type
    (openssh-configuration
     (password-authentication? #f)
      (accepted-environment '("TERM")) ;; maybe necessary to make this server realize im using foot?
      (authorized-keys 
       `(("uyotew" 
         ,(plain-file "laptop.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIz001l0gb5PbhCpTtGEWw2oRHvkeanUwV2iRcrvejNy uyotew@laptop")
         ,(plain-file "tablet.pub" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQChNYDMInC4SKXsBbJ8/0aP5cVBVS6Rl8aip2Gl4aL8XS52BmWX1VqU2k53wHGuskG9v/1Xm/54MxfoCdNQLYU75MNJve+4rIXaubWyhmtGEuIU89wNbrHyTchRWq9F+VxtZJ0ouJgYNLW4vauDt1lPSP/VlZ5Xq6p1elElqeU5YtBYLew7a86RKYvXhLsrcIPcLGw/5+gbfJotDjGRYr7FKsZqG2B5jFUYu0UmtGiKkM+WGK6u6JfRRwYnDzLjMVuEXRQPuJTpfd5FkSR/Uk5oKJymhRRy+bDCAAQev622fGSwtaIJ5pO0lG67d72Yt5oDD05EykkmnS7Dh9WdrEE4mOXP2avAnO6fs4WdJfxFgx3RgaDimNha2M6iGx48H4oRk54HT4qxvvxyfcefILaxHYenE0tMwKlx+izP9ar8YKK2mrsPQZplX4Yd7ygCbav3pcfvPBNVD/WXuMydpwIOLgaMiPSdbFV2QI+oV9mVCAkolq2EXXKiva5RAaZuhWU= u0_a451@tablet.bgl")
         ,(plain-file "desktop.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGh8/rO6+jwjtIFB18UE4OhbKz9WMFsuLhcB+zYABsgo uyotew@desktop")
         ,(plain-file "phone.pub" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCZd0ZDffe3fllLKe6Uxqxa7h+t8M97VT8oIcXNaohvsTmdri37uF8LMIBTyIfcFyWzqcRoVB+fJvjx9TTMmwvasDxigtO4K7cwQ+0OykdS4KG8MIDm578NiGJF3RxqMKhPduSZpjpgAOHFm8l7DQBoatHADBuOMZoIeGDnhUJmaPJ3kRSsnhhfCEWeabIlERmncgq3pqPcSeQeZaC3jr1TCwb5dM/ST93GVjNWgMUEaVWD/6BVx5falf2I96eg0df2in0oQzNOzn5mnmMiWDk6ALujoBNf4QH6a/JeIvD9AT28B+sOJfyWr87sIMK2ywmznhiAXvvAtFT7mJMAw3HnR+twhh9iI+EMOVFQGV/6E5m5+hP+uJQ22pb3e39oQF1ShVhiCyyuFje0Cyelnlz4MSaoDFVELR6AvljatNmE6uJVEiJt8CIDJtaxoWiij17S6XoNwIqiAvVeu0LVBzW6YQh7dkyydk/NAp+bC1fJ654FjFXpntwMKnEhx/QEilc= u0_a346@phone.bgl"))))))
   (modify-services (operating-system-services this-operating-system)
    (delete bluetooth-service-type)
    (network-manager-service-type config => 
     (network-manager-configuration
      (inherit config)
      (dns "dnsmasq"))) ;; will make /etc/hosts resolvable when bgl is used as dns-server (hopefully)
    (wireguard-service-type config =>
     (wireguard-configuration
      (addresses '("10.4.4.1/24"))
      (bootstrap-private-key? #f)
      (private-key (plain-file "wg-private-key" (keypit "bgl-wg" "private-key")))
       ;; remember to port-forward this port on your router to this computer's ip
      (port 36098)
      (peers
       (list
        (wireguard-peer
         (name "laptop")
         (public-key "qoNGgD5cZzemCNRiycbWqdZSYnW+bVuWN/sHMQ396ic=")
         (allowed-ips "10.4.4.2"))
        (wireguard-peer
         (name "tablet")
         (public-key "5Wdpg5hSgATNDMHO0PFLSCh69uAxxGD9tcmY05XQbEQ=")
         (allowed-ips "10.4.4.3"))
        (wireguard-peer
         (name "desktop")
         (public-key "AjAbZRartwhuAZlHXuTVtubnzIksl2VwB80hz9xGJkk=")
         (allowed-ips "10.4.4.4"))
        (wireguard-peer
         (name "phone")
         (public-key "asLTma7DTi2CcoXGdcVCf8egD3soRS7/8Q5DDr1DtGs=")
         (allowed-ips "10.4.4.5"))))))
    (elogind-service-type config => 
     (elogind-configuration
      ;; prevent laptop from sleeping when lid is closed
      (handle-lid-switch 'ignore)
      (handle-lid-switch-external-power 'ignore))))))))
