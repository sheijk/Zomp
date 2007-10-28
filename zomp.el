
;; (defvar zomp-basedir "/Users/sheijk/Documents/Development/Stuff/ocaml/lang/v3/")

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on paranthesis. Else go to the
   opening paranthesis one level up.
   Ripped from unknow source, probably emacswiki.org"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

(defun zomp-onkey-do (key code)
  (local-set-key key `(lambda () (interactive) (zomp-tl-do ,code))))


(defun zomp-tl-run (funcname)
  (interactive "MName of function: ")
  (zomp-tl-do (concat "!run " funcname)) )

(defun zomp-tl-list-bindings (regexps)
  (interactive "MList bindings matching: ")
  (zomp-tl-do (concat "!bindings " regexps)) )

(defun zomp-toplevel ()
  (interactive)
  (shell "*zomp-toplevel*")
  (zomp-tl-do (concat "cd " zomp-basedir))
  (zomp-tl-do "ocamlrun -b ./sexprtoplevel; exit")
  (message "Started zomp toplevel")
  )

(defun zomp-tl-do (code)
  (process-send-string (get-buffer-process "*zomp-toplevel*") (concat code ""))
  )

(defun zomp-tl-eval-region ()
  (interactive)
  (message "Evaluating region")
  (process-send-region (get-buffer-process "*zomp-toplevel*") (region-beginning) (region-end))
  (process-send-string (get-buffer-process "*zomp-toplevel*") "")
  )

(defun zomp-tl-eval-current ()
  (interactive)
  (message "Evaluating function at point")
  (push-mark)
  (search-backward-regexp "^(")
  (push-mark (point) t t)
  (goto-match-paren 4)
  (zomp-tl-eval-region)
  (pop-mark)
  (set-mark-command 1)
  )

(defun zomp-tl-eval-buffer ()
  (interactive)
  (message "Evaluating buffer")
  (process-send-region (get-buffer-process "*zomp-toplevel*") (buffer-end -1) (buffer-end 1))
  (process-send-string (get-buffer-process "*zomp-toplevel*") "")
  )

(defun zomp-tl-run-test ()
  (interactive)
  (message "Running function test")
  (zomp-tl-do "!run test")
  )

(define-generic-mode zomp-mode
  '(("/*" . "*/"))
  '("{" "}")
  '(
    ("///.*" 0 font-lock-doc-face)
    ("//.*" 0 font-lock-comment-face)
;;     ("/\\*\\*[^\\*]*\\*/" 0 font-lock-doc-face)
;;     ("/\\*[^\\*]*\\*/" 0 font-lock-comment-face)
    ("'[^']'" 0 font-lock-string-face)
;;     ("'\[0-9]+'" 0 font-lock-string-face)
     
    ("\\bconst\\b" 0 font-lock-keyword-face)
    ("\\bvar\\b" 0 font-lock-keyword-face)
    ("\\bfunc\\b" 0 font-lock-keyword-face)
    ("\\bret\\b" 0 font-lock-keyword-face)
    ("\\bbranch\\b" 0 font-lock-keyword-face)
    ("\\blabel\\b" 0 font-lock-keyword-face)
    ("\\bseq\\b" 0 font-lock-keyword-face)
    ("\\bassign\\b" 0 font-lock-keyword-face)
    ("\\btype\\b" 0 font-lock-keyword-face)
    
    ("\\bstore\\b" 0 font-lock-keyword-face)
    ("\\bload\\b" 0 font-lock-keyword-face)
    ("\\bnullptr\\b" 0 font-lock-keyword-face)
    ("\\bptradd\\b" 0 font-lock-keyword-face)
    ("\\bfieldptr\\b" 0 font-lock-keyword-face)
    ("\\bptr\\b" 0 font-lock-keyword-face)
    ("\\bmalloc\\b" 0 font-lock-keyword-face)
    
    ("\\bint\\b" 0 font-lock-type-face)
    ("\\bfloat\\b" 0 font-lock-type-face)
    ("\\bstring\\b" 0 font-lock-type-face)
    ("\\bbool\\b" 0 font-lock-type-face)
    ("\\bvoid\\b" 0 font-lock-type-face)
    ("\\bchar\\b" 0 font-lock-type-face)
    
    ("\\bmacro\\b" 0 font-lock-keyword-face)
    
    ("(\\([a-zA-Z][a-zA-Z0-9_]+\\)\\b" 1 font-lock-function-name-face)
    ("[()]" 0 font-lock-keyword-face)
    
    ("@[a-zA-Z][a-zA-Z0-9_]*\\b" 0 font-lock-variable-name-face)
    (":[a-zA-Z][a-zA-Z0-9_]*\\b" 0 font-lock-type-face)
    )
  '("\\.zomp")
  (list '(lambda ()
           (setq comment-start "//")
           (setq indent-tabs-mode nil)
           (local-set-key [(control c)(control s)] 'zomp-toplevel)
           (local-set-key [(control c)(control b)] 'zomp-tl-eval-buffer)
           (local-set-key [(control c)(control e)] 'zomp-tl-eval-current)
           (local-set-key [(control c)(control r)] 'zomp-tl-eval-region)
           (local-set-key [(control c)(control t)] 'zomp-tl-run-test)

           (local-set-key [(control c) (d)] 'zomp-tl-run)
           (local-set-key [(control c) (f)] 'zomp-tl-list-bindings)
           (zomp-onkey-do [(control c) (control f)] "!bindings")
           (zomp-onkey-do [(control c) (h)] "!elp")
           (zomp-onkey-do [(control c) (q)] "!exit")
           
           (zomp-onkey-do [(control c) (?.) (l)] "!llvm")
           (zomp-onkey-do [(control c) (?.) (e)] "!eval")
           ))
  "A simple mode for the zomp language")
  
