(defsystem "lem-vi-sexp"
  :depends-on ("lem"
               "lem-vi-mode"
               "lem-paredit-mode")
  :components
  ((:file "vi-sexp")))
