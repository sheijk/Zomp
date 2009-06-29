;;; zomp.el --- Major mode for the Zomp programming language

;; Copyright 2008 Jan Rehders
;;
;; Author: Jan Rehders <cmdkeen@gmx.de>
;; Version: 0.3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Put the following into your .emacs:
;; (defvar zomp-basedir "/path/to/dir/where/zomp.el/and/zompc/resides")
;; (load "/path/to/zomp.el")
;;

(defgroup zomp nil
  "Major mode for Zomp programming language"
  :link '(emacs-library-link :tag "Source Lisp File" "zomp.el")
  :group 'programming
  :prefix "zomp-")

(defcustom zomp-indent-keywords (list "func" "macro" "template" "if2" "for2" "cee:for")
  "A list of identifiers which will cause the auto-indenter to
indent the next line when they occur at the beginning of a line"
  :group 'zomp
  :type `(repeat string))

(defcustom zomp-unindent-keywords (list "elseif")
  "A list of identifiers which will cause the auto-indenter to
  unindent the next line by one level when they are at the
  beginning of a line"
  :group 'zomp
  :type `(repeat string))

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
  (replace-regexp-in-string "ID" "\\(?:[a-zA-Z0-9:*+-/!=><_|&]\\|\\[\\|\\]\\)+" str t t))
(setq zomp-imenu-generic-expression
      `((nil ,(zomp-id "^(?macro +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?func +ID +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?var +ID +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?const +ID +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?type +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?struct +\\(ID\\)") 1)
        (nil ,(zomp-id "^(?template +\\(ID\\)") 1)
        (nil ,(zomp-id "^\\(/// *.* *///\\) *$") 1)
        (nil ,(zomp-id "^\\(/// +Section: +.*\\)") 1)
        (nil "^testf *$" 0)
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
   (goto-match-paren 0))

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

(defun zomp-onkey-do (key code)
  (local-set-key key `(lambda () (interactive) (zomp-tl-do ,code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toplevel interaction

(defvar zomp-toplevel-buffer-name "*zomp-toplevel*"
  "The name of the buffer in which the zomp toplevel runs")

(define-derived-mode zomp-tl-mode shell-mode "ZompTL"
  "Major mode for a zomp toplevel in which you can interactively
  compile and run zomp code"
  (set (make-variable-buffer-local 'comint-use-prompt-regexp) t)
  (set (make-variable-buffer-local 'comint-prompt-regexp) "  # "))

(defun zomp-toplevel ()
  (interactive)
  (let ((zomp-new-tl-buffer-name zomp-toplevel-buffer-name) (oldwin (selected-window)))
    (shell zomp-toplevel-buffer-name)
    ;; in case zomp-toplevel-buffer-name is buffer local we need to be sure it
    ;; has the same value in the toplevel buffer as in the zomp buffer invoking
    ;; the toplevel
    (zomp-tl-mode)
    (set (make-local-variable 'zomp-toplevel-buffer-name) zomp-new-tl-buffer-name)
    (zomp-tl-do (concat "cd " zomp-basedir))
    (zomp-tl-do "./sexprtoplevel.native; exit")
    (message "Started zomp toplevel")
    (select-window oldwin)))

(defun zomp-get-toplevel-buffer (&optional create-if-not-existing)
  "Get the zomp toplevel interaction buffer. If
  `create-if-not-existing' is equal to 'create the toplevel will
  be started if it is not not running, yet"
  (or
   (get-buffer-process zomp-toplevel-buffer-name)
   (when (equal create-if-not-existing 'create)
     (zomp-toplevel)
     (get-buffer-process zomp-toplevel-buffer-name))))

(defun zomp-start-or-show-toplevel (prefix)
  (interactive "P")
  (cond ((null prefix)
         (if (not (zomp-get-toplevel-buffer))
             (zomp-toplevel)
           (let ((oldwin (selected-window)))
             (switch-to-buffer-other-window (get-buffer zomp-toplevel-buffer-name))
             (select-window oldwin))))
        (t
         (make-variable-buffer-local 'zomp-toplevel-buffer-name)
         (setq zomp-toplevel-buffer-name (format "*zomp-toplevel<%s>*" (buffer-name)))
         (unless (zomp-get-toplevel-buffer)
           (zomp-toplevel)))))

(defun zomp-tl-do (code &optional create-if-not-existing)
  "Send some text to the zomp toplevel. If
  `create-if-not-existing' is 'create then the toplevel will be
  started if it is not running, yet"
  (process-send-string
   (zomp-get-toplevel-buffer create-if-not-existing)
   (concat code "")))

(defun zomp-tl-run (funcname)
  (interactive "MName of function: ")
  (when (= 0 (length funcname))
    (setq funcname "main"))
  (zomp-tl-do (concat "!run " funcname)))

(defun zomp-tl-list-bindings (regexps)
  (interactive "MList bindings matching: ")
  (zomp-tl-do (concat "!bindings " regexps)))

(defun zomp-tl-eval-region ()
  (interactive)
  (message "Evaluating region")
  (process-send-region (zomp-get-toplevel-buffer 'create) (region-beginning) (region-end))
  (process-send-string (zomp-get-toplevel-buffer) ""))

(defun zomp-tl-eval-current ()
  (interactive)
  (message "Evaluating function at point")
  (save-excursion
    (zomp-mark-current)
    (zomp-tl-eval-region)))

(defun zomp-tl-eval-current-and-goto-next ()
  (interactive)
  (call-interactively 'zomp-tl-eval-current)
  (zomp-next-tl-expr))

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
  (process-send-region (zomp-get-toplevel-buffer 'create) (buffer-end -1) (buffer-end 1))
  (process-send-string (zomp-get-toplevel-buffer) ""))

(defun zomp-tl-run-test ()
  (interactive)
  (message "Running function test")
  (zomp-tl-do "!run test"))

(defun zomp-run ()
  (interactive)
  (if (zomp-get-toplevel-buffer)
      (zomp-tl-eval-current)
    (zomp-tl-eval-buffer))
  (zomp-tl-run-test) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro zomp-add-seperator (seperator-id)
  `(local-set-key [menu-bar zomp ,seperator-id] '("--")))

(defmacro zomp-dofun (name command)
  `(defun ,name () (interactive) (zomp-tl-do ,command)))

(zomp-dofun zomp-tl-exit "!exit")
(zomp-dofun zomp-tl-list-all-bindings "!bindings")
(zomp-dofun zomp-tl-help "!help")
(zomp-dofun zomp-tl-toggle-llvm-printing "!llvm")
(zomp-dofun zomp-tl-toggle-decl-printing "!printDecl")
(zomp-dofun zomp-tl-toggle-parsed-ast-printing "!printAst")
(zomp-dofun zomp-tl-toggle-verify "!verify")

(defmacro zomp-add-action (command key caption &rest path)
  `(list (local-set-key ,key (quote ,command))
         (local-set-key [menu-bar zomp ,@path ,command] '(,caption . ,command))))

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

(unless (boundp 'current-line)
  (defun current-line ()
    "Return number of current line in current buffer
    (provided by `zomp.el` because it was not defined)"
    (count-lines (point-min) (if (eobp) (point) (1+ (point))))) )

(defun zomp-indent-line ()
  (interactive)
  (let ((left 0) (oldindent 0))
    (save-excursion
      (back-to-indentation)
      (setq oldindent (current-column))
      (when (> (current-line) 1)
        (save-excursion
          (setq left
                (condition-case nil
                    (progn
                      ;; goto previous non whitespace line
                      (let ((dontabort t))
                        (while dontabort
                          (previous-line)
                          (beginning-of-line)
                          (when (not (looking-at " *$"))
                            (setq dontabort nil))))

                      ;; get indentation of previous line
                      (back-to-indentation)
                      (current-column))
                  (error 0)))

          (let ((w (zomp-symbol-at-point)))
            (cond ((looking-at "\\*")
                   (setq left (- left 1)))
                  ((member w zomp-indent-keywords)
                   (setq left (+ left 2)))
                  ((looking-at "end}")
                   (setq left (+ left 2)))
                  ))
          ;; (when (looking-at "\\*")
          ;;   (setq left (- left 1)))

          (let* ((line-start (progn (beginning-of-line) (point)))
                (line-end (progn (end-of-line) (point)))
                (open-parens (how-many "[({]" line-start line-end))
                (closing-parens (how-many "[)}]" line-start line-end)))
            (cond ((> open-parens closing-parens) (setq left (+ left 2)))
                  ((< open-parens closing-parens) (setq left (- left 2))))
            (next-line))))

      (beginning-of-line)

      ;; lines like " * foo" are indented by one more space
      ;; (when (looking-at " *\\*")
        ;; (set 'left (+ left 1)))
      (when (looking-at " *\\*")
          (set 'left (+ left 1)))

      ;; unindent end lines
      (back-to-indentation)
      (when (string= (word-at-point) "end")
        (setq left (- left 2)))

      ;; unindent keywords like elseif etc.
      (when (member (zomp-symbol-at-point) zomp-unindent-keywords)
        (setq left (- left 2)))

      (delete-horizontal-space)
      (indent-to-column left))
    ;; place cursor correctly on newline-and-indent
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

(defun zomp-current-line-indent ()
  (let (lineBegin)
    (save-excursion
      (beginning-of-line)
      (setq lineBegin (point))
      (back-to-indentation)
      (- (point) lineBegin))))

(defun zomp-newline ()
  (interactive)

  (let ((isComment nil)
        (isStar nil)
        (isQuotation nil)
        (isAtEnd nil)
        (wordAtLineBeginning "")
        (endAtNextLine nil)
        (endQuotationAtNextLine nil)
        (nextLineIndentDiff 0))

    (setq isAtEnd (looking-at " *$"))

    (save-excursion
      (move-beginning-of-line 1)
      ;; /// on documentation comment
      (when (looking-at " *///")
        (set 'isComment t))
      ;; /*
      ;;  * nice aligned stars
      ;;  */ (but not after this line)
      (when (and (looking-at " *\\(/\\)?\\*") (not (looking-at ".*\\*/")))
        (set 'isStar t)))

    (save-excursion
      (end-of-line)
      (setq isQuotation (looking-back "${ *")))

    (unless (eobp)
      (save-excursion
        (let ((thisLineIndent (zomp-current-line-indent)))
          (next-line)
          (setq nextLineIndentDiff (- (zomp-current-line-indent) thisLineIndent)))))

    (save-excursion
      (back-to-indentation)
      (setq wordAtLineBeginning (zomp-symbol-at-point)))

    (unless (eobp)
      (save-excursion
        (next-line)
        (beginning-of-line)
        (setq endQuotationAtNextLine (looking-at " *end} *\\(//.*\\)?$"))
        (setq endAtNextLine (looking-at " *end *\\(//.*\\)?$"))))

    (indent-according-to-mode)
    (newline-and-indent)

    (when isComment
      (insert "/// "))
    (when isStar
      (insert " * ")
      (indent-for-tab-command))

    (when (and (not isStar)
               (not isComment)
               (member wordAtLineBeginning zomp-indent-keywords)
               (not (and endAtNextLine (>= nextLineIndentDiff 0)))
               (<= nextLineIndentDiff 0)
               isAtEnd)
      (insert "end")
      (indent-according-to-mode)
      (previous-line)
      (end-of-line)
      (newline-and-indent))

    (when (and isQuotation
               (not endQuotationAtNextLine)
               (<= nextLineIndentDiff 0))
      (insert "end}")
      (indent-according-to-mode)
      (previous-line)
      (end-of-line)
      (newline-and-indent))
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

(defun zomp-electric-backspace (prefix)
  (interactive "p")
  (delete-backward-char
   (cond ((looking-back "/// ") 4)
         ((looking-back "///") 3)
         ((looking-back "^ \\* ") 3)
         ((looking-back "^ \\*") 2)
         (t prefix))))

(defun zomp-setup ()
  (setq comment-start "//")
  (setq comment-start-skip "// *")
  (setq comment-end "")

  (modify-syntax-entry ?: "w")

  (setq indent-tabs-mode nil)

  ;; indexing of current file (control-')
  (setq imenu-generic-expression zomp-imenu-generic-expression)

  ;; display documentation for methods/macros/... in status line
  (set (make-local-variable 'eldoc-documentation-function) 'zomp-get-eldoc-string)
  (eldoc-mode t)

  ;; auto indenting
  (setq indent-line-function 'zomp-indent-line)

  ;; highlight s-expression under cursor
  (hl-sexp-mode t)

  ;; quick navigation and marking expressions
  (local-set-key [(meta n)] 'zomp-next-tl-expr)
  (local-set-key [(meta p)] 'zomp-prev-tl-expr)
  (local-set-key [(meta k)] 'zomp-mark-sexp)

  ;; expand templates
  ;; (local-set-key [(control ?.)] 'snippetio-complete)

  ;; extra comfort (insert ///, * in matching places, * / => */ etc.)
  (local-set-key "\r" 'zomp-newline)
  (local-set-key [(control j)] 'zomp-newline)
  (local-set-key [(?/)] 'zomp-electric-slash)
  (local-set-key (kbd "DEL") 'zomp-electric-backspace)


  (local-set-key [(control c)(control k)] '(lambda () (interactive)
                                             (zomp-tl-do "!")))

  ;; create zomp menu. order of the zomp-add-action commands is reversed order in menu
  (local-set-key [menu-bar zomp] (cons "Zomp" (make-sparse-keymap "Zomp")))

  ;; create toggle sub menu
  (local-set-key [menu-bar zomp toggle]
                 (cons "Toggle" (make-sparse-keymap "Zomp/Toggle")))

  (zomp-add-action zomp-tl-toggle-llvm-printing
                   [(control c) (?.) (l)] "Printing of LLVM code" toggle)
  (zomp-add-action zomp-tl-toggle-decl-printing
                   [(control c) (?.) (d)] "Printing of declarations" toggle)
  (zomp-add-action zomp-tl-toggle-parsed-ast-printing
                   [(control c) (?.) (p)] "Printing of parsed ASTs" toggle)
  ;; (zomp-add-action zomp-tl-toggle-verify
  ;;                  [(control c) (?.) (v)] "Verification of LLVM code" toggle)

  (local-set-key [(control c) (?.) (?s)] '(lambda () (interactive)
                                            (zomp-tl-do "!")
                                            (zomp-tl-do "!syntax sexpr")))
  (local-set-key [(control c) (?.) (?i)] (lambda () (interactive)
                                           (zomp-tl-do "!")
                                           (zomp-tl-do "!syntax indent")))

  (zomp-add-seperator zomp-sep-3)
  (zomp-add-action zomp-tl-run [(control c)(control d)] "Run function...")
  (zomp-add-action zomp-tl-run-test [(control c)(control t)] "Run 'void test()'")
  (zomp-add-action zomp-tl-list-all-bindings [(control c)(meta f)] "List all bindings")
  (zomp-add-action zomp-tl-list-bindings [(control c)(control f)] "List bindings...")
  (zomp-add-action zomp-tl-help [(control c)(control h)] "Show toplevel help")

  (zomp-add-seperator zomp-sep4)
  (zomp-add-action zomp-indent-current [(meta q)] "Indent current")
  (zomp-add-action zomp-indent-buffer [(shift meta q)] "Indent buffer")

  (zomp-add-seperator zomp-sep-2)
  (zomp-add-action zomp-tl-eval-buffer [(control c)(control b)] "Eval buffer")
  (zomp-add-action zomp-tl-eval-region [(control c)(control r)] "Eval region")
  (zomp-add-action zomp-tl-eval-current [(control c)(control e)] "Eval function at point")
  (zomp-add-action zomp-tl-eval-current-and-goto-next
                   [(control c)(control shift e)]
                   "Eval function at point and goto next")

  (zomp-add-seperator zomp-sep-1)
  (zomp-add-action zomp-tl-exit [(control c)(control q)] "Exit toplevel")
  (zomp-add-action zomp-start-or-show-toplevel [(control c)(control s)] "Start toplevel")

  ;; set additional keys on OS X
  (local-set-key [(alt r)]' zomp-run)
  (local-set-key [(alt e)] 'zomp-tl-eval-current)
  (local-set-key [(alt shift e)] 'zomp-tl-eval-current-and-goto-next)
  (local-set-key [(alt d)] 'zomp-tl-run)
  (local-set-key [(alt shift d)] 'zomp-tl-run-test)

  ;; (outline-minor-mode t)
  ;; (setq outline-regexp "\\([a-df-z]\\| *\\(if\\|else\\|while\\)\\)")
  ;; (local-set-key [(control c)(-)] 'outline-cycle)
  ;; (local-set-key [(control c)(=)] 'show-entry)
  ;; (local-set-key [(control c)(+)] 'show-all)

  (run-mode-hooks 'zomp-mode-hook)
  )

(define-generic-mode zomp-mode
  '(("/*" . "*/"))
  '("{" "}")
  '(
    ("//.*" 0 font-lock-comment-face t t)
    ("///.*" 0 font-lock-doc-face t t)
    ("/\\*\\*[^\\*]*\\*/" 0 font-lock-doc-face t t)
    ("'[^']'" 0 font-lock-string-face)

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

    ;; ("^ +$" 0 compilation-error-face)

    ;; ("(\\([a-zA-Z][a-zA-Z0-9_.:]+\\)\\b" 1 font-lock-function-name-face)
    ("\\([a-zA-Z][a-zA-Z0-9_.:]*\\)\\b(" 1 font-lock-function-name-face)
    ("^ *\\([a-zA-Z][a-zA-Z0-9_.:]+\\)\\b" 1 font-lock-variable-name-face)
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

(defconst zomp-identifier-chars "a-zA-Z0-9:*")
(defconst zomp-identifier-regexp "[a-zA-Z][a-zA-Z0-9:*]*")

(defun zomp-symbol-at-point ()
  (interactive)
  (let ((linestart 0) (parenopen 0) funcend linesym funcsym)
    (setq linesym
          (save-excursion
            (back-to-indentation)
            (setq linestart (point))
            ;; (forward-word)
            (condition-case nil
                (search-forward-regexp zomp-identifier-regexp)
              (error nil))
            (buffer-substring linestart (point))))
    (setq exprsym
          (save-excursion
            (when (search-backward-regexp " [:=/*+-]+ " linestart t)
              (forward-char 1)
              (search-forward-regexp "[ a-aA-Z0-9]")
              (forward-word 1)
              (let ((wordend (point)))
                (backward-word 1)
                (buffer-substring (point) wordend)))))
    (setq funcsym
          (save-excursion
            (condition-case nil
                (progn
                  (goto-match-paren 0)
                  (setq parenopen (point))
                  (when (> (point) 0)
                    (if (looking-back "\\( \\|\\$\\)")
                        (progn
                          (setq funcend (1+ (point)))
                          (forward-char)
                          (search-forward-regexp (format "[^%s]" zomp-identifier-chars))
                          (backward-char))
                      (setq funcend (point))
                      (search-backward-regexp (format "[^%s]" zomp-identifier-chars))
                      (forward-char))
                    (buffer-substring (point) funcend)))
              (error nil))))
    (when (and (> linestart parenopen) linesym funcsym)
      (setq funcsym nil))
    (or exprsym funcsym linesym "nothing found")))

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

(defun zomp-region-to-html (regbegin regend)
  "Will replace the current region with html. Requires a matching
  css section. To get it copy the css annotations from the
  *html*<n> buffer and rename the 'body' section to '#code'"

  (interactive "r")
  (save-window-excursion
    (save-excursion
      (kill-region regbegin regend)
      (switch-to-buffer (generate-new-buffer "*zomp-to-html*"))
      (yank)
      (zomp-mode)
      (switch-to-buffer (htmlize-buffer))
      (beginning-of-buffer)
      (search-forward "<body>")
      (next-line 2)
      (beginning-of-line)
      (let (htmlbegin htmlend)
        (setq htmlbegin (point))
        (search-forward "</body>")
        (previous-line)
        (beginning-of-line)
        (setq htmlend (point))
        (kill-ring-save htmlbegin htmlend)
        ))
    (insert "<pre id=\"code\"><code>")
    (indent-according-to-mode)
    (insert "\n")
    (yank)
    (insert "\n</code></pre>")
    (indent-according-to-mode)
    (insert "\n")))


