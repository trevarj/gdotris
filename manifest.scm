(use-modules (guix packages)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages guile-xyz))

(define guile-ncursesw
  (package
   (inherit guile-ncurses)
   (arguments (list
               #:tests? #f
               #:configure-flags #~'("--with-ncursesw")))))

(packages->manifest (list guile-3.0 guile-ncursesw))
