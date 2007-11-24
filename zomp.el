

;; (defvar zomp-basedir "/Users/sheijk/Documents/Development/Stuff/ocaml/lang/v3/")

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on paranthesis. Else go to the
   opening paranthesis one level up.
   Partially ripped from unknow source, probably emacswiki.org"
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

(defvar zomp-imenu-generic-expression
  '((nil "^(macro +\\([a-zA-Z0-9]+\\) " 1)
    (nil "^(func +[a-zA-Z0-9]+ +\\([a-zA-Z0-9]+\\) " 1)
    (nil "^(var +[a-zA-Z0-9]+ +\\([a-zA-Z0-9]+\\) " 1)
    (nil "^(const +[a-zA-Z0-9]+ +\\([a-zA-Z0-9]+\\) " 1) ))
    
(defun zomp-mark-sexp ()
  (interactive)
  (cond (mark-active
         (goto-match-paren 0)
         (goto-match-paren 0)
         (forward-char)))
        
  (cond ((not (looking-at "("))
          (goto-match-paren 0)))
   (set-mark (point))
   (goto-match-paren 0)
  )

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
  (save-excursion
    (forward-char)
    (search-backward-regexp "^(")
    (push-mark (point) t t)
    (goto-match-paren 4)
    (zomp-tl-eval-region) ))

(defun zomp-next-tl-expr ()
  (interactive)
  (forward-char)
  (search-forward-regexp "^(")
  (backward-char) )

(defun zomp-prev-tl-expr ()
  (interactive)
  (backward-char)
  (search-backward-regexp "^(") )

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

(defmacro zomp-add-seperator (seperator-id)
  `(local-set-key [menu-bar zomp ,seperator-id] '("--")))
   
(defmacro zomp-dofun (name command)
  `(defun ,name () (interactive) (zomp-tl-do ,command)))

(zomp-dofun zomp-tl-exit "!exit")
(zomp-dofun zomp-tl-list-all-bindings "!bindings")
(zomp-dofun zomp-tl-help "!help")
(zomp-dofun zomp-tl-toggle-llvm-printing "!llvm")

(defmacro zomp-add-action (command key caption)
  `(list (local-set-key ,key (quote ,command))
         (local-set-key [menu-bar zomp ,command] '(,caption . ,command))))

(defun zomp-forward-sexp ()
  (interactive)
  (forward-sexp) )

(defun zomp-backward-sexp ()
  (interactive)
  (backward-sexp) )


(defface todo-face
  '((t (:foreground "#A00")))
 "A face for todo items")
(defvar todo-face 'todo-face)

(define-generic-mode zomp-mode
  '(("/*" . "*/"))
  '("{" "}")
  '(
    ("//.*" 0 font-lock-comment-face t t)
    ("///.*" 0 font-lock-doc-face t t)
    ("/\\*\\*[^\\*]*\\*/" 0 font-lock-doc-face t t)
;;     ("/\\*[^\\*]*\\*/" 0 font-lock-comment-face)
    ("'[^']'" 0 font-lock-string-face)
;;     ("'\[0-9]+'" 0 font-lock-string-face)

    ("// *\\(TODO\\)" 1 todo-face t t)
     
    ("\\bconst\\b" 0 font-lock-keyword-face)
    ("\\bvar\\b" 0 font-lock-keyword-face)
    ("\\bfunc\\b" 0 font-lock-keyword-face)
    ("\\bret\\b" 0 font-lock-keyword-face)
    ("\\bbranch\\b" 0 font-lock-keyword-face)
    ("\\blabel\\b" 0 font-lock-keyword-face)
    ("\\bseq\\b" 0 font-lock-keyword-face)
    ("\\bassign\\b" 0 font-lock-keyword-face)
    ("\\btype\\b" 0 font-lock-keyword-face)

    ("\\btrue\\b" 0 font-lock-keyword-face)
    ("\\bfalse\\b" 0 font-lock-keyword-face)
    
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

           (setq imenu-generic-expression zomp-imenu-generic-expression)
           (local-set-key [(control ?')] 'imenu)
           
           ; highlight s-expression under cursor
           (hl-sexp-mode t)

           ; create zomp menu. order of the zomp-add-action commands is reversed order in menu
           (local-set-key [menu-bar zomp] (cons "Zomp" (make-sparse-keymap "Zomp")))

           (zomp-add-action zomp-tl-toggle-llvm-printing [(control c) (?.) (l)] "Toggle LLVM code printing")
           (zomp-add-action zomp-tl-toggle-eval [(control c) (?.) (e)] "Toggle code eval")

           (zomp-add-seperator zomp-sep-3)
           (zomp-add-action zomp-tl-run [(control c)(d)] "Run function...")
           (zomp-add-action zomp-tl-run-test [(control c)(t)] "Run 'void test()'")
           (zomp-add-action zomp-tl-list-all-bindings [(control c)(meta f)] "List all bindings")
           (zomp-add-action zomp-tl-list-bindings [(control c)(f)] "List bindings...")
           (zomp-add-action zomp-tl-help [(control c)(h)] "Show toplevel help")

           (zomp-add-seperator zomp-sep-2)
           (zomp-add-action zomp-tl-eval-buffer [(control c)(control b)] "Eval buffer")
           (zomp-add-action zomp-tl-eval-region [(control c)(control r)] "Eval region")
           (zomp-add-action zomp-tl-eval-current [(control c)(control e)] "Eval function at point")

           (zomp-add-seperator zomp-sep-1)
           (zomp-add-action zomp-tl-exit [(control c)(control q)] "Exit toplevel")
           (zomp-add-action zomp-toplevel [(control c)(control s)] "Start toplevel")

           (local-set-key [(meta n)] 'zomp-next-tl-expr)
           (local-set-key [(meta p)] 'zomp-prev-tl-expr)
           (local-set-key [(meta k)] 'zomp-mark-sexp)
           
           (local-set-key [(control ?.)] 'zomp-complete)
           ))
  "A simple mode for the zomp language")

(defun zomp-complete ()
  (interactive)
  (let* ((word (current-word)) (expansion (cdr (assoc word zomp-snippets))))
    (if (null expansion)
        (message (concat "Could not find a snippet named " word))
      (progn
        (backward-kill-word 1)
        (snippet-insert expansion))
      )))

(defvar zomp-snippets
  '(("func" .  "(func $${retval} $${name} ($${args}) (\n  $.\n  ))\n")
    ("xmacro" . "(xmacro $${name} $${args} (\n  $.\n  ))\n")
    ("main" . "(func int main () (\n  $.\n  ret 0\n  ))\n")
    ))

(defun zomp-add-snippet (name expansion)
  "Adds a snippet for zomp. The snippet will not be saved on exit!"
  (interactive "MName for macro? \nMExpansion? ")
  (setq zomp-snippets (cons `(,name . ,expansion) zomp-snippets)))

;; (defvar shk-recenter-pos  TODO
 
