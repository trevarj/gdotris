(use-modules (guix packages)
             (guix utils)
             (gnu packages guile)
             (gnu packages guile-xyz))

(define-public guile-ncurses-unicode
  (package
    (inherit guile-ncurses)
    (arguments
     (substitute-keyword-arguments (package-arguments guile-ncurses)
       ((#:configure-flags _) #~(list "--with-gnu-filesystem-hierarchy"))))))
    
(packages->manifest (list
                     glibc-locales
                     guile-3.0
                     guile-ncurses-unicode))
