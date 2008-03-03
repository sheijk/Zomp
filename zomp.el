
(defvar zomp-mode-hook nil)

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

(setq zomp-tl-expr-regexp "^[a-z(]")

(defvar zomp-imenu-generic-expression nil)
(defun zomp-id (str)
  (replace-regexp-in-string "ID" "[a-zA-Z0-9:*+-/!=><_]+" str t))
(setq zomp-imenu-generic-expression
      `((nil ,(zomp-id "^(?macro +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?func +ID +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?var +ID +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?const +ID +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?type +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?template +\\(ID\\)") 1)
        ))

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
  (process-send-string (get-buffer-process "*zomp-toplevel*") "")
  )

(defun zomp-mark-current ()
  "Marks the current toplevel function/(macro/"
  (interactive)
  (next-line)
  (beginning-of-line)
  (zomp-prev-tl-expr)
  (push-mark (point) t t)
  (if (looking-at "(")
      (goto-match-paren 4)
    (progn
      (next-line)
      (let ((next-line-pos (point)))
        (search-forward-regexp zomp-tl-expr-regexp)
        (backward-char)
        (if (looking-at "end")
            (forward-word)
          (goto-char next-line-pos)))
    )))
  
(defun zomp-tl-eval-current ()
  (interactive)
  (message "Evaluating function at point")
  (save-excursion
    (zomp-mark-current)
    (zomp-tl-eval-region) ))

(defun zomp-next-tl-expr ()
  (interactive)
  (forward-char)
  (search-forward-regexp zomp-tl-expr-regexp)
  (backward-char)
  (when (looking-at "end")
    (message "hit end statement")
    (next-line)
    (search-forward-regexp zomp-tl-expr-regexp)
    (backward-char)))

(defun zomp-prev-tl-expr ()
  (interactive)
  (backward-char)
  (search-backward-regexp zomp-tl-expr-regexp)
  (when (looking-at "end")
    (backward-char)
    (search-backward-regexp zomp-tl-expr-regexp)))

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

(defun zomp-indent-line ()
  (interactive)
  (let ((left 0) (oldindent 0))
    (save-excursion
      (back-to-indentation)
      (setq oldindent (current-column))
      (when (> (current-line) 1)
        (save-excursion
          ; goto previous non whitespace line
          (let ((dontabort t))
            (while dontabort
              (previous-line)
              (beginning-of-line)
              (when (not (looking-at " *$"))
                (setq dontabort nil))))

          ; get indentation of previous line
          (back-to-indentation)
          (setq left (current-column))

          (when (looking-at "\\*")
            (setq left (- left 1)))
          
          (let* ((line-start (progn (beginning-of-line) (point)))
                (line-end (progn (end-of-line) (point)))
                (open-parens (how-many "[({]" line-start line-end))
                (closing-parens (how-many "[)}]" line-start line-end)))
            (cond ((> open-parens closing-parens) (setq left (+ left 2)))
                  ((< open-parens closing-parens) (setq left (- left 2))))
            (next-line))))
      (beginning-of-line)

      ; lines like " * foo" are indented by one more space
      (when (looking-at " *\\*")
        (set 'left (+ left 1)))
      
      (just-one-space)
      (backward-delete-char 1)
      (indent-to-column left))
    ; place cursor correctly on newline-and-indent
    (when (equal (current-column) 0)
      (back-to-indentation))
;;     (when (equal (+ 2 (point)) (save-excursion (end-of-line) (point)))
;;       (forward-char 2))
    ))

(defun zomp-indent-current ()
  (interactive)
  (save-excursion
    (zomp-mark-current)
    (indent-region (region-beginning) (region-end))))

(defun zomp-indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region 0 (buffer-end 1))))


(defun zomp-newline ()
  (interactive)
  (let ((isComment nil)
        (isStar nil) )
    (save-excursion
      (move-beginning-of-line 1)
      ; /// on documentation comment
      (when (looking-at " *///")
        (set 'isComment t))
      ; /*
      ;  * nice aligned stars
      ;  */ (but not after this line)
      (when (and (looking-at " *\\(/\\)?\\*") (not (looking-at ".*\\*/")))
        (set 'isStar t))
      )
    (newline-and-indent)
    (when isComment
      (insert "/// "))
    (when isStar
      (insert " * ")
      (indent-for-tab-command) )
    ))

(defun zomp-electric-slash ()
  "Will change '* ' to '*/' when a newline was inserted
   previously, insert '/' otherwise"
  (interactive)
  (when (and
         (equal last-command 'zomp-newline)
         (looking-back "\\* *"))
    (backward-delete-char 1))
  (insert "/") )

(defun zomp-setup ()
;;   (setq comment-start "/*")
;;   (setq comment-end "*/")
  (setq comment-start "//")
  (setq comment-start-skip "// *")
  (setq comment-end "")
  
  (setq indent-tabs-mode nil)

                                        ; indexing of current file (control-')
  (setq imenu-generic-expression zomp-imenu-generic-expression)
  (local-set-key [(control ?')] 'imenu)

                                        ; display documentation for methods/macros/... in status line
  (set (make-local-variable 'eldoc-documentation-function) 'zomp-get-eldoc-string)
  (eldoc-mode t)

                                        ; auto indenting
  (setq indent-line-function 'zomp-indent-line)
           
                                        ; highlight s-expression under cursor
  (hl-sexp-mode t)

                                        ; quick navigation and marking expressions
  (local-set-key [(meta n)] 'zomp-next-tl-expr)
  (local-set-key [(meta p)] 'zomp-prev-tl-expr)
  (local-set-key [(meta k)] 'zomp-mark-sexp)

                                        ; expand templates
  (local-set-key [(control ?.)] 'snippetio-complete)

                                        ; extra comfort (insert ///, * in matching places, * / => */ etc.)
  (local-set-key "\r" 'zomp-newline)
  (local-set-key [(control j)] 'zomp-newline)
  (local-set-key [(?/)] 'zomp-electric-slash)
           
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

  (zomp-add-seperator zomp-sep4)
  (zomp-add-action zomp-indent-current [(meta q)] "Indent current")
  (zomp-add-action zomp-indent-buffer [(shift meta q)] "Indent buffer")

  (zomp-add-seperator zomp-sep-2)
  (zomp-add-action zomp-tl-eval-buffer [(control c)(control b)] "Eval buffer")
  (zomp-add-action zomp-tl-eval-region [(control c)(control r)] "Eval region")
  (zomp-add-action zomp-tl-eval-current [(control c)(control e)] "Eval function at point")

  (zomp-add-seperator zomp-sep-1)
  (zomp-add-action zomp-tl-exit [(control c)(control q)] "Exit toplevel")
  (zomp-add-action zomp-toplevel [(control c)(control s)] "Start toplevel")

                                        ; set additional keys on OS X
  (local-set-key [(alt r)] '(lambda () (interactive)
                              (zomp-tl-eval-current)
                              (zomp-tl-run-test) ))
  (local-set-key [(alt e)] 'zomp-tl-eval-current)
  (local-set-key [(alt d)] 'zomp-tl-run)
  (local-set-key [(alt shift d)] 'zomp-tl-run-test)

  (run-mode-hooks 'zomp-mode-hook)
  )

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
    ("\\bend\\b" 0 font-lock-keyword-face)

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
    
    ("(\\([a-zA-Z][a-zA-Z0-9_.:]+\\)\\b" 1 font-lock-function-name-face)
    ("[()]" 0 font-lock-keyword-face)
    
    ("@[a-zA-Z][a-zA-Z0-9_]*\\b" 0 font-lock-variable-name-face)
    (" :[a-zA-Z][a-zA-Z0-9_]*\\b" 0 font-lock-type-face)
    )
  '("\\.zomp")
  (list 'zomp-setup)
  "A simple mode for the zomp language")


;; (defun zomp-add-snippet (name expansion)
;;   "Adds a snippet for zomp. The snippet will not be saved on exit!"
;;   (interactive "MName for macro? \nMExpansion? ")
;;   (setq zomp-snippets (cons `(,name . ,expansion) zomp-snippets)))

(defvar zomp-symbol-file "/tmp/zomp-symbols")
(defvar zomp-symbol-buffer "*zomp-symbols*")
  
(defun zomp-build-symbol-buffer ()
  (interactive)
  (save-excursion
    (when (file-exists-p zomp-symbol-file)
      (set-buffer (get-buffer-create zomp-symbol-buffer))
      (insert-file-contents zomp-symbol-file nil nil nil t))
    (zomp-tl-do (concat "!silent !writeSymbols " zomp-symbol-file))
    ))

(defun zomp-symbol-at-point ()
  (interactive)
  (save-excursion
    (goto-match-paren 0)
    (forward-char)
    (let ((startpos (point)))
      (search-forward-regexp "[ )\n]")
      (backward-char)
      (buffer-substring startpos (point))
      )))
    
(defun zomp-get-eldoc-string ()
  (let ((symbol "unknown"))
    (condition-case nil
        (save-excursion
          (zomp-build-symbol-buffer)
          (setq symbol (zomp-symbol-at-point))
          (set-buffer (get-buffer-create zomp-symbol-buffer))
          (save-excursion
            (goto-char (point-max))
            (search-backward-regexp (concat "^" symbol " ="))
            (search-forward " =")
            (let ((startpos (point)))
              (end-of-line)
              (concat symbol ": " (buffer-substring startpos (point))))
            ))
      (error nil))))


  