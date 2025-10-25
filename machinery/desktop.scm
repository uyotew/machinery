(define-module (machinery desktop)
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages graphics)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages wm)
  #:use-module (nongnu packages game-client)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (machinery common)
  #:export (desktop-home desktop-system))

(define desktop-home
  (let ((base (base-home)))
    (home-environment
      (inherit base)
      ; (packages (cons* steam-nvidia blender (replace-mesa sway)
      ;                  (filter (lambda (p) (not (equal? (package-name p) "sway")))
      ;                          (home-environment-packages base))))
      (services (cons*
       (simple-service 'exec-sway home-bash-service-type
        (home-bash-extension
         (bash-profile
          (list
           (plain-file "exec-sway" "\
if [ -z \"$WAYLAND_DISPLAY\" ] && [ -n \"$XDG_VTNR\" ] && [ \"$XDG_VTNR\" -eq 1 ] ; then
    exec sway # --unsupported-gpu
fi
")))))
       (home-environment-user-services base))))))

(define (desktop-system keypit)
  (let ((base (base-system "desktop" "5D9C-BC48" "10.4.4.4/24" desktop-home keypit)))
    (operating-system
      (inherit base)
      ; (kernel-arguments '("modprobe.blacklist=nouveau" "nvidia_drm.modeset=1"))
      (services (cons*
        (service openssh-service-type
         (openssh-configuration
           (password-authentication? #f)
            (accepted-environment '("TERM")) ;; maybe necessary to make this server realize im using foot?
            (authorized-keys
             `(("uyotew"
               ,(plain-file "laptop.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIz001l0gb5PbhCpTtGEWw2oRHvkeanUwV2iRcrvejNy uyotew@laptop")
               ,(plain-file "tablet.pub" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQChNYDMInC4SKXsBbJ8/0aP5cVBVS6Rl8aip2Gl4aL8XS52BmWX1VqU2k53wHGuskG9v/1Xm/54MxfoCdNQLYU75MNJve+4rIXaubWyhmtGEuIU89wNbrHyTchRWq9F+VxtZJ0ouJgYNLW4vauDt1lPSP/VlZ5Xq6p1elElqeU5YtBYLew7a86RKYvXhLsrcIPcLGw/5+gbfJotDjGRYr7FKsZqG2B5jFUYu0UmtGiKkM+WGK6u6JfRRwYnDzLjMVuEXRQPuJTpfd5FkSR/Uk5oKJymhRRy+bDCAAQev622fGSwtaIJ5pO0lG67d72Yt5oDD05EykkmnS7Dh9WdrEE4mOXP2avAnO6fs4WdJfxFgx3RgaDimNha2M6iGx48H4oRk54HT4qxvvxyfcefILaxHYenE0tMwKlx+izP9ar8YKK2mrsPQZplX4Yd7ygCbav3pcfvPBNVD/WXuMydpwIOLgaMiPSdbFV2QI+oV9mVCAkolq2EXXKiva5RAaZuhWU= u0_a451@tablet.bgl")
               ,(plain-file "phone.pub" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCZd0ZDffe3fllLKe6Uxqxa7h+t8M97VT8oIcXNaohvsTmdri37uF8LMIBTyIfcFyWzqcRoVB+fJvjx9TTMmwvasDxigtO4K7cwQ+0OykdS4KG8MIDm578NiGJF3RxqMKhPduSZpjpgAOHFm8l7DQBoatHADBuOMZoIeGDnhUJmaPJ3kRSsnhhfCEWeabIlERmncgq3pqPcSeQeZaC3jr1TCwb5dM/ST93GVjNWgMUEaVWD/6BVx5falf2I96eg0df2in0oQzNOzn5mnmMiWDk6ALujoBNf4QH6a/JeIvD9AT28B+sOJfyWr87sIMK2ywmznhiAXvvAtFT7mJMAw3HnR+twhh9iI+EMOVFQGV/6E5m5+hP+uJQ22pb3e39oQF1ShVhiCyyuFje0Cyelnlz4MSaoDFVELR6AvljatNmE6uJVEiJt8CIDJtaxoWiij17S6XoNwIqiAvVeu0LVBzW6YQh7dkyydk/NAp+bC1fJ654FjFXpntwMKnEhx/QEilc= u0_a346@phone.bgl"))))))
        ; (service nvidia-service-type)
        (operating-system-user-services base))))))
