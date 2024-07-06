;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
   (haskell-process-type . cabal-repl)
   (eval .
       (setq
         nix-bins/proj-dir
           (file-name-directory
             (let ((d (dir-locals-find-file ".")))
               (if (stringp d) d (car d))))
         haskell-process-path-cabal (concat nix-bins/proj-dir "bin/cabal")
         haskell-hoogle-server-command (lambda (port)
           (list (concat nix-bins/proj-dir "bin/hoogle") "server"
             "--local"
             "-p" (number-to-string port))))
     )
   ))
