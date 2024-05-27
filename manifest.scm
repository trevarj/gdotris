(use-modules (guix packages)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest (list guile-3.0 guile-ncurses))
