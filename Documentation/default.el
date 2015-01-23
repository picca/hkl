(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-babel-load-languages
   (quote
    ((latex . t)
     (python . t)
     (emacs-lisp . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-latex-listings (quote minted))
 '(org-latex-minted-langs
   (quote
    ((emacs-lisp "common-lisp")
     (cc "c++")
     (cperl "perl")
     (shell-script "bash")
     (caml "ocaml")
     (python "python"))))
 '(org-latex-pdf-process
   (quote
    ("pdflatex -interaction nonstopmode --shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode --shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode --shell-escape -output-directory %o %f")))
 '(org-export-with-sub-superscripts nil)
 '(org-src-fontify-natively t))
