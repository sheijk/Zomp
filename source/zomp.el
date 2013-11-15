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

(require 'thingatpt)

;;; Code:

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

(defcustom zomp-continued-line-regexp
  "\\(?: [-+*/=><&|:]*[-+*/=><&|]\\|[,({\\[]\\) *\\(?:///.*\\)?"
  "A line whose ends matches this string (using `looking-back' is
  considered to be one which continues on the next line"
  :group 'zomp
  :type 'string)

(defcustom zomp-highlight-after-eval-seconds 0.2
  "Number of seconds to highlight an expression after it has been evaluated."
  :group 'zomp
  :type 'float)

(defface zomp-highlight-after-eval-face
  '((t (:inherit highlight)))
  "A face used to highlight expression for a brief time after they have been
evaluated.")

(defface zomp-todo-face
  '((t (:foreground "#A00")))
  "A face for todo items")
(defvar zomp-todo-face 'zomp-todo-face)

(defface zomp-testsuite-directive-face
  '((t (:inherit font-lock-doc-face :weight bold)))
  "A face to highlight directives for the Zomp testsuite. For example:
//// error type
The word \"error\" will be highlighted in this face.")

(defvar zomp-mode-hook nil
  "Hook that will be run after entering zomp-mode.")

(defun zomp-goto-match-paren (arg)
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
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

(defconst zomp-toplevel-expr-regexp "^[a-z(]"
  "Regular expression to match the beginning of a toplevel expression.")

(defconst zomp-identifier-chars "a-zA-Z0-9:*_"
  "Valid characters in Zomp identifiers.")
(defconst zomp-identifier-regexp "[a-zA-Z][a-zA-Z0-9:*_]*"
  "Regular expression to match a valid Zomp identifier.")

(defun zomp-id (str)
  "Helper function to produce imenu expressions. In the given string ID will be
replaced with a regular expression matching a Zomp identifier. This is not using
`zomp-identifier-regexp' because imenu is a bit more liberal about identifiers
to also match op_, preop_, and postop_
TODO: unify this"
  (replace-regexp-in-string "ID" "\\(?:[a-zA-Z0-9:*+-/!=><_|&^]\\|\\[\\|\\]\\)+" str t t))

(defvar zomp-imenu-generic-expression
  `((nil ,(zomp-id "^(?macro +\\(ID\\)") 1)
    (nil ,(zomp-id "^(?func +ID +\\(ID\\)") 1)
    (nil ,(zomp-id "^(?ofunc +ID +\\(ID(ID\\)") 1)
    (nil ,(zomp-id "^(?std:base:func +ID +\\(ID\\)") 1)
    (nil ,(zomp-id "^(?var +ID +\\(ID\\)") 1)
    (nil ,(zomp-id "^(?const +ID +\\(ID\\)") 1)
    (nil ,(zomp-id "^(?type +\\(ID\\)") 1)
    (nil ,(zomp-id "^(?struct +\\(ID\\):") 1)
    (nil ,(zomp-id "^(?template +\\(ID\\)") 1)
    (nil ,(zomp-id "^(?alias +\\(ID\\)") 1)
    (nil ,(zomp-id "^\\(/// *.* *///\\) *$") 1)
    (nil ,(zomp-id "^\\(/// +Section: +.*\\)") 1)
    (nil ,(zomp-id "^(?unittest:testCase +\\(ID\\)") 1)
    (nil "^testf *$" 0))
  "A list of regular expressions that will be used to identify imenu items.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for find file at point (ffap)

(defvar zomp-symbol-at-point-before-ff nil
  "The symbol at point before ff-find-other-file was
invoked. Used by zomp-ff-extract-lib.")

(defun zomp-ff-pre-find-hook ()
  "Called before ffap starts looking for files and moves the point around"
  (when (equal major-mode 'zomp-mode)
    (when (string-match (word-at-point) "requireLibs?")
      (forward-word 2))
    (setq zomp-symbol-at-point-before-ff
          (let ((illegal-char "[^a-zA-Z0-9_]"))
            (save-excursion
              (buffer-substring
               (progn (search-backward-regexp illegal-char) (forward-char 1) (point))
               (progn (search-forward-regexp illegal-char) (backward-char 1) (point))))))))

(defun zomp-ff-extract-lib ()
  "Will produce the file name for the lib under point which is stored in
`zomp-symbol-at-point-before-ff'.

TODO: check if this will work with source files that are not inside the compiler
source repository."
  (format "../libs/%s.zomp" zomp-symbol-at-point-before-ff))

(defcustom zomp-ff-special-constructs
  '(("^requireLibs? " . zomp-ff-extract-lib))
  "Value for ff-special-constructs in zomp buffers. Set to nil to
use global one"
  :group 'zomp)

(defun zomp-ffap-init ()
  "Initialize support for `find-file-at-point' support."
  (add-hook 'ff-pre-find-hook 'zomp-ff-pre-find-hook)
  (make-variable-buffer-local 'ff-special-constructs)
  (setq ff-special-constructs zomp-ff-special-constructs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing

(defun zomp-mark-current ()
  "Marks the current toplevel function/(macro/"
  (interactive)
  (next-line)
  (beginning-of-line)
  (zomp-prev-toplevel-expr)
  (push-mark (point) t t)
  (if (looking-at "(")
      (zomp-goto-match-paren 4)
    (progn
      (next-line)
      (let ((next-line-pos (point)))
        (search-forward-regexp zomp-toplevel-expr-regexp)
        (backward-char)
        (if (looking-at "end")
            (forward-word)
          (goto-char next-line-pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell interaction

(defvar zomp-shell-buffer-name-base "zomp-shell"
  "The base name of the buffer in which the zomp shell runs")

(defvar zomp-shell-buffer-name (format "*%s*" zomp-shell-buffer-name-base)
  "The name of the buffer in which the zomp shell runs")

(defvar zomp-build-architecture "i386"
  "The name of the architecture to be used")

(defvar zomp-build-variant "release"
  "Whether to use the Zomp \"debug\" or \"release\" build.")

(defun zomp-build-name ()
  "Returns the variant of Zomp used. This value is composed of the build variant
and the architecture like this: \"variant-architecture\"."
  (format "%s-%s" zomp-build-variant zomp-build-architecture))

(defun zomp-zompsh-command ()
  "Returns the command to run zompsh."
  (format "./build/%s/deploy/zompsh" (zomp-build-name)))

(define-derived-mode zomp-shell-mode shell-mode "ZompSh"
  "Major mode for a zomp shell in which you can interactively
  compile and run zomp code"
  (set (make-variable-buffer-local 'comint-use-prompt-regexp) t)
  (set (make-variable-buffer-local 'comint-prompt-regexp) "  # ")
  (compilation-shell-minor-mode))

(defun zomp-start-shell ()
  "Will create a comint buffer in which zompsh will be started. Uses
`zomps-zompsh-command' to determine executable and options."
  (interactive)
  (let ((zomp-new-shell-buffer-name zomp-shell-buffer-name)
        (default-directory zomp-basedir))
    (display-buffer
     (save-window-excursion
       (set-buffer (make-comint zomp-shell-buffer-name-base (zomp-zompsh-command)))
       (unless (equal (buffer-name) zomp-new-shell-buffer-name)
         (error (format "expected comint buffer to be named %s" zomp-shell-buffer-name)))
       ;; in case zomp-shell-buffer-name is buffer local we need to be sure it
       ;; has the same value in the shell buffer as in the zomp buffer invoking
       ;; the shell
       (zomp-shell-mode)
       (set (make-local-variable 'zomp-shell-buffer-name) zomp-new-shell-buffer-name)
       (current-buffer)))
    (message "Started zomp shell")))

(defun zomp-get-shell-buffer (&optional create-if-not-existing)
  "Get the zomp shell interaction buffer. If `create-if-not-existing' is equal
 to 'create the shell will be started if it is not not running, yet"
  (or
   (get-buffer-process zomp-shell-buffer-name)
   (when (equal create-if-not-existing 'create)
     (zomp-start-shell)
     (get-buffer-process zomp-shell-buffer-name))))

(defun zomp-toggle-variant ()
  "Toggle between using Zomp variants. See `zomp-build-variant'."
  (interactive)
  (setq zomp-build-variant
        (if (string= "release" zomp-build-variant)
            "debug"
          "release"))
  (minibuffer-message "Now using Zomp build %s%s" (zomp-build-name)
                      (if (zomp-get-shell-buffer nil)
                        ", need to restart zompsh to take effect"
                        "")))

(defun zomp-shell-move-point-to-end ()
  "Will move the point to the end of the shell buffer for all
windows displaying it"
  (let ((original-window (selected-window)))
    (dolist (wnd (get-buffer-window-list (get-buffer zomp-shell-buffer-name)))
      (select-window wnd)
      (goto-char (point-max)))
    (select-window original-window)))

(defun zomp-prepare-use-buffer-local-shell ()
  "Will setup local variables to use a buffer local shell buffer."
  (set (make-variable-buffer-local 'zomp-shell-buffer-name-base)
       (format "zomp-shell<%s>" (buffer-name)))
  (set (make-variable-buffer-local 'zomp-shell-buffer-name)
       (format "*%s*" zomp-shell-buffer-name-base)))

(defun zomp-start-or-show-shell (&optional prefix)
  "Shows the shell in a buffer and starts it if it wasn't
  running, yet. With a prefix this will start a buffer local zomp
  shell which you can use to run multiple zomp instances at
  once"
  (interactive "P")
  (cond ((null prefix)
         (if (not (zomp-get-shell-buffer))
             (zomp-start-shell)
           (let ((oldwin (selected-window)))
             (display-buffer (get-buffer zomp-shell-buffer-name)))))
        (t
         (zomp-prepare-use-buffer-local-shell)
         (unless (zomp-get-shell-buffer)
           (zomp-start-shell)))))

(defun zomp-shell-do (code &optional create-if-not-existing append)
  "Send some text to the zomp shell. If
  `create-if-not-existing' is 'create then the shell will be
  started if it is not running, yet"
  (if (not (or create-if-not-existing (zomp-get-shell-buffer)))
      (message "Zomp shell is not running")
    (process-send-string (zomp-get-shell-buffer create-if-not-existing) "!prompt")
    (process-send-string
     (zomp-get-shell-buffer create-if-not-existing)
     (concat code (or append "")))
    (process-send-string (zomp-get-shell-buffer) "!prompt #")))

(defun zomp-shell-run-immediate (&optional code)
  "Will run the given Zomp code as if it was inside a function body."
  (interactive)
  (unless code
    (setq code (read-from-minibuffer "Code: ")))
  (let ((function-code (format "std:base:run:\n  %s\nend\n\n\n" code)))
    (zomp-shell-do function-code 'create)))

(defun zomp-shell-run-function (funcname)
  "Will execute the given function in the Zomp shell."
  (interactive "MName of function: ")
  (when (= 0 (length funcname))
    (setq funcname "main"))
  (zomp-shell-move-point-to-end)
  (zomp-shell-do (concat "!run " funcname)))

(defun zomp-shell-list-bindings (regexps)
  "Will list all known symbols (functions, variables, macros, types, etc.) in zompsh."
  (interactive "MList bindings matching: ")
  (zomp-shell-do (concat "!bindings " regexps)))

(defun zomp-shell-update-source-location (&optional pos)
  "Will set the source location of the running zompsh process. Code evaluated
following this command will have source locations relative to pos."
  (setq pos (or pos (region-beginning)))
  (let* ((line (save-restriction
                 (widen)
                 (count-lines (point-min) pos)))
         (file (or (buffer-file-name) "zompsh"))
         (cmd (format "!setSourceLocation %s %s" file (- line 1))))
    (message "cmd = %s" cmd)
    (zomp-shell-do cmd)))

(defun zomp-shell-eval-region ()
  "Will send the region between `mark' and `point' to the zompsh process."
  (interactive)
  (when (called-interactively-p)
    (message "Evaluating region"))
  (zomp-shell-request-execution-abort)
  (when (fboundp 'nav-flash-show)
    (save-window-excursion
      (nav-flash-show (region-beginning)
                      (region-end)
                      'zomp-highlight-after-eval-face
                      zomp-highlight-after-eval-seconds)))
  (zomp-shell-update-source-location)
  (zomp-shell-do (buffer-substring (region-beginning) (region-end)) nil ""))

(defun zomp-shell-eval-current ()
  "Will send the current toplevel expression `point' is in to the Zomp shell."
  (interactive)
  (when (called-interactively-p)
    (message "Evaluating function at point"))
  (save-excursion
    (zomp-mark-current)
    (zomp-shell-eval-region))
  (zomp-shell-move-point-to-end))

(defun zomp-shell-eval-current-and-goto-next ()
  "Will call `zomp-shell-eval-current' and navigate to the beginning of the next
toplevel expression."
  (interactive)
  (call-interactively 'zomp-shell-eval-current)
  (zomp-next-toplevel-expr))

(defun zomp-next-toplevel-expr ()
  "Will move `point' to the beginning of the next toplevel expression."
  (interactive)
  (forward-char)
  (search-forward-regexp zomp-toplevel-expr-regexp)
  (backward-char)
  (when (looking-at "end")
    (next-line)
    (search-forward-regexp zomp-toplevel-expr-regexp)
    (backward-char)))

(defun zomp-prev-toplevel-expr ()
  "Will move `point' to the beginning of the next toplevel expression."
  (interactive)
  (backward-char)
  (search-backward-regexp zomp-toplevel-expr-regexp)
  (when (looking-at "end")
    (backward-char)
    (search-backward-regexp zomp-toplevel-expr-regexp)))

(defun zomp-shell-eval-buffer ()
  "Will send the current buffer to the Zomp shell."
  (interactive)
  (when (called-interactively-p)
    (message "Evaluating buffer"))
  (zomp-shell-update-source-location (buffer-end -1))
  (zomp-shell-do (buffer-substring (buffer-end -1) (buffer-end 1)) 'create "")
  (zomp-shell-move-point-to-end))

(defun zomp-shell-discard-input ()
  "Will tell Zomp shell to discard all input submitted that has not been
evaluated, yet."
  (interactive)
  (zomp-shell-do "!"))

(defun zomp-shell-run-test ()
  "Will run the function called test in the Zomp shell."
  (interactive)
  (when (called-interactively-p)
    (message "Running function test"))
  (zomp-shell-move-point-to-end)
  (zomp-shell-do "!run test"))

(defun zomp-compile-command (target)
  "Will create the compile command string to build the given
  `target'. `target' must be relative to main zomp dir"
  (format "%sbuild.sh ARCH=%s DEBUG=%s -ks %s"
          zomp-basedir
          zomp-build-architecture
          (if (string= zomp-build-variant "debug") "1" "0")
          target))

(defun zomp-test-current-file ()
  "Will test the current file. Files in the test suite will be
  compiled and executed"
  (interactive)
  (let ((test-file (buffer-file-name)))
    (cond
     ((string-match "\\(.*/\\)\\(testsuite/.*/test_.*\\)\\.zomp" test-file)
      (let* ((file-basename (match-string 2 test-file))
             (zomp-basedir (match-string 1 test-file))
             (report-file (format "%s.testreport" file-basename))
             (compile-command (zomp-compile-command report-file)))
        (call-interactively 'compile)))
     ((string-match "\\(.*\\)\\(examples/.*\\)\\.zomp" test-file)
      (let* ((source-file (match-string 2 test-file))
             (zomp-basedir (match-string 1 test-file))
             (exe-file (format "%s.exe" source-file))
             (compile-command (zomp-compile-command exe-file)))
        (call-interactively 'compile)))
     ((string-match "\\(.*\\)\\(libs/.*\\)\\.zomp" test-file)
      (let* ((source-file (match-string 2 test-file))
             (zomp-basedir (match-string 1 test-file))
             (ll-file (format "%s.ll" source-file))
             (compile-command (zomp-compile-command ll-file)))
        (call-interactively 'compile)))
     (t
      (message "Sorry, do not know how to test file %s" test-file)))))

(defun zomp-run (&optional prefix)
"Will run test() after evaluating
- the current toplevel expression (function) if shell is running
- the current buffer if shell is not running
so you can use this as a quick way to start working on a project.

Run this with a prefix to start a shell specific to this buffer"
  (interactive "P")
  (when prefix
    (zomp-prepare-use-buffer-local-shell))
  (if (zomp-get-shell-buffer)
      (progn
        (zomp-shell-request-execution-abort)
        (zomp-shell-eval-current))
    (zomp-start-or-show-shell prefix)
    (zomp-shell-eval-buffer))
  (zomp-shell-run-test))

(defun zomp-shell-request-execution-abort ()
  "Send a signal to the shell to request it to abort the
current main() execution.

Will cause the zompRequestedPause() function to return true. Any
application honoring this will then return from main to allow the
editor to trigger recompilations etc. and possibly resume main()"
  (interactive)
  (when (zomp-get-shell-buffer)
    (with-current-buffer zomp-shell-buffer-name
      (interrupt-process nil comint-ptyp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro zomp-dofun (name command &optional confirm-msg)
  "Utility to define a method that will evaluate the given string in the Zomp
shell. If `confirm-msg' is t the function will ask for confirmation."
  (if confirm-msg
      `(defun ,name () (interactive)
         (when (y-or-n-p ,confirm-msg)
           (zomp-shell-do ,command)))
    `(defun ,name () (interactive) (zomp-shell-do ,command))))

(defun zomp-shell-exit ()
  "Will abort running program and kill Zomp shell."
  (interactive)
  (when (y-or-n-p "Really quit running shell? ")
    (zomp-shell-request-execution-abort)
    (zomp-shell-do "!exit")))

(zomp-dofun zomp-shell-list-all-bindings "!bindings")
(zomp-dofun zomp-shell-help "!help")
(zomp-dofun zomp-shell-toggle-llvm-printing "!llvm")
(zomp-dofun zomp-shell-toggle-decl-printing "!printDecl")
(zomp-dofun zomp-shell-toggle-parsed-ast-printing "!printAst")
(zomp-dofun zomp-shell-toggle-trace-macros "!traceMacros")
(zomp-dofun zomp-shell-toggle-verify "!verify")

(defun zomp-shell-sexpr-syntax ()
  "Use indentation based syntax"
  (interactive)
  (zomp-shell-do "!")
  (zomp-shell-do "!syntax indent"))
(defun zomp-shell-indent-syntax ()
  "Use sepxr based syntax (deprecated)"
  (interactive)
  (zomp-shell-do "!")
  (zomp-shell-do "!syntax sexpr"))

(defmacro zomp-add-seperator ()
  "Will add a seperator line to the Zomp mode menu."
  `(local-set-key [menu-bar zomp ,(gensym "zomp-seperator")] '("--")))

(defmacro zomp-add-action (command key caption &rest path)
  "Will add a command with a key binding to the Zomp menu."
  `(list (local-set-key ,key (quote ,command))
         (local-set-key [menu-bar zomp ,@path ,command] '(,caption . ,command))))

(defun zomp-current-line ()
  "Helper function which returns number of current line in current buffer."
  (count-lines (point-min) (if (eobp) (point) (1+ (point)))))

(defun zomp-indent-line ()
  "Will indent the current line. Tries to be smart about various situation but
still screws up sometimes. Please file bug reports!"
  (interactive)
  (let ((left 0) (oldindent 0))
    (save-excursion
      (back-to-indentation)
      (setq oldindent (current-column))
      ;; not in first line
      (when (> (zomp-current-line) 1)
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
                            (setq dontabort nil))
                          (when (<= (zomp-current-line) 1)
                            (setq dontabort nil))))

                      ;; get indentation of previous line
                      (back-to-indentation)
                      (current-column))
                  (error 0)))

          (let ((w (zomp-function-before-point)))
            (cond ((looking-at "\\*")
                   (setq left (- left 1)))))

          (end-of-line)
          (when (looking-back ": *")
            (setq left (+ left 2)))
          ;; lines ending with an operator or '(' probably continue in the next
          ;; line, double-indent them (to keep visually seperate from indent
          ;; block)
          (when (and (looking-back zomp-continued-line-regexp)
                     ;; not in line comments
                     (not (looking-back "//.*"))
                     ;; don't double-indent after comments
                     (not (looking-back "\\*/ *")))
            (setq left (+ left 4)))

          ;; if previous line was already a continued line, don't indent this
          ;; one again
          (when (> (zomp-current-line) 1)
            (previous-line)
            (end-of-line)
            (when (looking-back zomp-continued-line-regexp)
              (setq left (- left 4))))
          ))

      (beginning-of-line)

      ;; lines like " * foo" are indented by one more space
      (when (looking-at " *\\*")
        (set 'left (+ left 1)))

      ;; unindent end lines
      (back-to-indentation)
      (when (string= (word-at-point) "end")
        (setq left (- left 2)))

      ;; unindent keywords like elseif etc.
      (when (member (zomp-function-before-point) zomp-unindent-keywords)
        (setq left (- left 2)))

      (delete-horizontal-space)
      (indent-to-column left))

    ;; place cursor correctly on newline-and-indent
    (when (equal (current-column) 0)
      (back-to-indentation))))

(defun zomp-point-is-in-comment (pt)
  "Will return true if the `point' is in a comment based on font-lock text
properties."
  (interactive "d")
  (let ((prop (get-char-property pt 'face)))
    (or (equal prop 'font-lock-comment-face)
        (equal prop 'font-lock-doc-face))))

(defun zomp-indent-current-or-fill ()
  "Inside comments this will call `fill-paragraph'. Everywhere else this will
indent the current toplevel expression."
  (interactive)
  (if (zomp-point-is-in-comment (point))
      (fill-paragraph nil)
    (save-excursion
      (zomp-mark-current)
      (indent-region (region-beginning) (region-end)))))

(defun zomp-indent-buffer ()
  "Will auto indent the whole buffer. Use this with great care!"
  (interactive)
  (save-excursion
    (indent-region 0 (buffer-end 1))))

(defun zomp-current-line-indent ()
  "Helper method which returns the number of spaces at the beginning of the
current line."
  (let (lineBegin)
    (save-excursion
      (beginning-of-line)
      (setq lineBegin (point))
      (back-to-indentation)
      (- (point) lineBegin))))

(defun zomp-newline ()
  "A smart newline function that will indent and insert various kinds of text
depending on context."
  (interactive)

  (let ((isComment nil)
        (isStar nil)
        (isQuotation nil)
        (isAtEnd nil)
        (wordAtLineBeginning "")
        (endAtNextLine nil)
        (startOfIndentBlock nil)
        (endQuotationAtNextLine nil)
        (nextLineIndentDiff 0))

    (setq isAtEnd (looking-at " *$"))

    (save-excursion
      (end-of-line)
      (setq startOfIndentBlock (looking-back " *:"))
      (setq isQuotation (looking-back "${[^}]*")))

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
      (back-to-indentation)
      (setq wordAtLineBeginning (zomp-function-before-point)))

    (unless (eobp)
      (save-excursion
        (let ((thisLineIndent (zomp-current-line-indent)))
          (next-line)
          (setq nextLineIndentDiff (- (zomp-current-line-indent) thisLineIndent))))

      (save-excursion
        (next-line)
        (beginning-of-line)
        (setq endQuotationAtNextLine (looking-at " *end} *\\(//.*\\)?$"))
        (setq endAtNextLine (looking-at " *end *\\(//.*\\)?$"))))

    ;; (indent-according-to-mode)
    (newline-and-indent)

    (when isComment
      (insert "/// "))
    (when isStar
      (insert " * ")
      (indent-for-tab-command))

    (when (and (not isStar)
               (not isComment)
               (not isQuotation)
               ;; (member wordAtLineBeginning zomp-indent-keywords)
               startOfIndentBlock
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
      (newline-and-indent))))

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
  "Will delete comment markers inserted by zomp-electric-*, zomp-newline, etc."
  (interactive "p")
  (delete-backward-char
   (cond ((looking-back "/// ") 4)
         ((looking-back "///") 3)
         ((looking-back "^ \\* ") 3)
         ((looking-back "^ \\*") 2)
         (t prefix))))

(defun zomp-electric-colon ()
  "Insert \":\" and indent line."
  (interactive)
  (zomp-indent-line)
  (insert ":"))

(defun zomp-comment-block ()
  "Will comment out the current indentation block"
  (interactive)
  (save-excursion
    (let ((start (save-excursion (beginning-of-line) (point)))
          (column (save-excursion (back-to-indentation) (current-column))))
      (next-line)
      (while (progn (back-to-indentation)
                    (or (eolp)
                        (> (current-column) column)))
        (next-line))
      (next-line)
      (beginning-of-line)
      (comment-region start (point)))))

(defun zomp-move-up (count)
  "Move cursor up one indentation level"
  (interactive "p")
  ;; skip empty line(s)
  (while (save-excursion
           (beginning-of-line)
           (looking-at " *$"))
    (previous-line))

  (let ((oldpoint (point))
        (moved-lines 0)
        (column (save-excursion (back-to-indentation) (current-column))))
    (previous-line)
    (while (or
            (>= (progn (back-to-indentation) (current-column)) column)
            (looking-at " *$"))
      (previous-line)
      (setq moved-lines (+ 1 moved-lines)))
    (when (> moved-lines 3)
      (push-mark oldpoint)))
  (when (and count (> count 1))
    (zomp-move-up (- count 1))))

(defun zomp-move-until-same-or-less-indent (line-change-func)
  "Helper method moving the `point' line until a line is reached whose
indentation is the same or less than the line where we started."
  (let ((initial-indent (save-excursion
                           (back-to-indentation)
                           (current-column))))
    (funcall line-change-func)
    (while (or (< initial-indent (save-excursion
                                   (back-to-indentation)
                                   (current-column)))
               (looking-at "\s*$"))
      (funcall line-change-func))))

(defun zomp-next-block (arg)
  "Will move `point' to the next line at the same indentation level."
  (interactive "P")
  (zomp-move-until-same-or-less-indent 'next-line))

(defun zomp-prev-block (arg)
  "Will move `point' to the previous line at the same indentation level."
  (interactive "P")
  (zomp-move-until-same-or-less-indent 'previous-line))

(defun zomp-setup ()
  "Setup function for `zomp-mode'."
  (setq comment-start "//")
  (setq comment-start-skip "\\(?://+\\|/\\*+\\) *")
  (setq comment-end "")

  (dolist (op (list ?+ ?- ?= ?! ?? ?* ?/ ?& ?| ?^))
    (modify-syntax-entry op "."))
  (dolist (op (list ?_ ?:))
    (modify-syntax-entry op "w"))

  (setq indent-tabs-mode nil)

  ;; indexing of current file (control-')
  (setq imenu-generic-expression zomp-imenu-generic-expression)

  ;; display documentation for methods/macros/... in status line
  (set (make-local-variable 'eldoc-documentation-function) 'zomp-get-eldoc-string)
  (eldoc-mode t)
  (make-variable-buffer-local 'ac-sources)
  (add-to-list 'ac-sources 'zomp-ac-source)
  (ignore-errors
    (auto-complete-mode 1))

  ;; auto indenting
  (setq indent-line-function 'zomp-indent-line)

  ;; quick navigation and marking expressions
  (local-set-key [(meta n)] 'zomp-next-toplevel-expr)
  (local-set-key [(meta p)] 'zomp-prev-toplevel-expr)
  (local-set-key [(control c)(control u)] 'zomp-move-up)
  (local-set-key [(control c)(control n)] 'zomp-next-block)
  (local-set-key [(control c)(control p)] 'zomp-prev-block)

  ;; extra comfort (insert ///, * in matching places, * / => */ etc.)
  (local-set-key "\r" 'zomp-newline)
  (local-set-key [(control j)] 'zomp-newline)
  (local-set-key [(?/)] 'zomp-electric-slash)
  (local-set-key (kbd "DEL") 'zomp-electric-backspace)
  (local-set-key [(control c)(control ?/)] 'zomp-comment-block)
  (local-set-key [(?:)] 'zomp-electric-colon)

  (local-set-key [(meta ?.)] 'zomp-goto-definition)

  ;; create zomp menu. order of the zomp-add-action commands is reversed order in menu
  (local-set-key [menu-bar zomp] (cons "Zomp" (make-sparse-keymap "Zomp")))

  ;; create toggle sub menu
  (local-set-key [menu-bar zomp toggle]
                 (cons "Toggle" (make-sparse-keymap "Zomp/Toggle")))

  (zomp-add-action zomp-shell-toggle-llvm-printing
                   [(control c) (?.) (l)] "Printing of LLVM code" toggle)
  (zomp-add-action zomp-shell-toggle-decl-printing
                   [(control c) (?.) (d)] "Printing of declarations" toggle)
  (zomp-add-action zomp-shell-toggle-parsed-ast-printing
                   [(control c) (?.) (p)] "Printing of parsed ASTs" toggle)
  (zomp-add-action zomp-shell-toggle-trace-macros
                   [(control c) (?.) (m)] "Tracing of macro expansion" toggle)
  ;; (zomp-add-action zomp-shell-toggle-verify
  ;;                  [(control c) (?.) (v)] "Verification of LLVM code" toggle)

  (local-set-key [(control c) (?.) (?s)] 'zomp-shell-sexpr-syntax)
  (local-set-key [(control c) (?.) (?i)] 'zomp-shell-indent-syntax)

  (local-set-key [(control c) (?.) (?D)] 'zomp-toggle-variant)

  (zomp-add-seperator)
  (zomp-add-action zomp-shell-run-function [(control c)(control d)] "Run function...")
  (zomp-add-action zomp-shell-run-test [(control c)(control t)] "Run 'void test()'")
  (zomp-add-action zomp-test-current-file [(control c)(control c)] "Test current file")
  (zomp-add-action zomp-shell-list-all-bindings [(control c)(meta f)] "List all bindings")
  (zomp-add-action zomp-shell-list-bindings [(control c)(control f)] "List bindings...")
  (zomp-add-action zomp-shell-help [(control c)(control ??)] "Show Zomp shell help")

  (zomp-add-seperator)
  (zomp-add-action zomp-indent-current-or-fill [(meta q)] "Indent current")
  (zomp-add-action zomp-indent-buffer [(shift meta q)] "Indent buffer")

  (zomp-add-seperator)
  (zomp-add-action zomp-shell-discard-input [(control c)(control l)] "Discard entered text")
  (zomp-add-action zomp-shell-eval-buffer [(control c)(control b)] "Eval buffer")
  (zomp-add-action zomp-shell-eval-region [(control c)(control r)] "Eval region")
  (zomp-add-action zomp-shell-eval-current [(control c)(control e)] "Eval function at point")
  (zomp-add-action zomp-shell-eval-current-and-goto-next
                   [(control c)(control shift e)]
                   "Eval function at point and goto next")
  (zomp-add-action zomp-shell-run-immediate [(control c)(control i)] "Enter code to run")

  (zomp-add-seperator)
  (zomp-add-action zomp-start-or-show-shell
                   [(control c)(control s)]
                   "Start Zomp shell")
  (zomp-add-action zomp-shell-exit [(control c)(control q)] "Exit Zomp shell")
  (zomp-add-action zomp-shell-request-execution-abort
                   [(control c)(control k)]
                   "Request app to pause")


  ;; set additional keys on OS X
  (local-set-key [(alt r)]' zomp-run)
  (local-set-key [(alt shift r)]' zomp-shell-run-test)
  (local-set-key [(alt d)] 'zomp-shell-run-immediate)
  (local-set-key [(alt shift d)] 'zomp-shell-run-function)
  (local-set-key [(alt e)] 'zomp-shell-eval-current)
  (local-set-key [(alt shift e)] 'zomp-shell-eval-current-and-goto-next)

  ;; (outline-minor-mode t)
  ;; (setq outline-regexp "\\([a-df-z]\\| *\\(if\\|else\\|while\\)\\)")
  ;; (local-set-key [(control c)(-)] 'outline-cycle)
  ;; (local-set-key [(control c)(=)] 'show-entry)
  ;; (local-set-key [(control c)(+)] 'show-all)

  (zomp-ffap-init)

  (run-mode-hooks 'zomp-mode-hook))

(define-generic-mode zomp-mode
  '(("/*" . "*/"))
  '("{" "}")
  '(
    ("//.*" 0 font-lock-comment-face t t)
    ("///.*" 0 font-lock-doc-face t t)
    ("//// +\\(error\\|error-no-location\\|compiler-output\\|warning\\|info\\|print\\|exit-code\\) .*" 1 'zomp-testsuite-directive-face t t)
    ("//// +\\(compilation-fails\\) *" 1 'zomp-testsuite-directive-face t t)
    ("/\\*\\*[^\\*]*\\*/" 0 font-lock-doc-face t t)
    ("'[^']'" 0 font-lock-string-face)

    ("// *\\(TODO\\)" 1 zomp-todo-face t t)

    ("\\(\\b[a-zA-Z0-9_]+\\b:\\) +[^ \n]" 1 font-lock-keyword-face)
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

(defvar zomp-symbol-file "/tmp/zomp-symbols"
  "File into which Zomp shell is asked to write symbols.")
(defvar zomp-symbol-buffer "*zomp-symbols*"
  "Name of the buffer which contains symbols of the Zomp shell.")

(defun zomp-build-symbol-buffer ()
  "Will ask Zomp shell to write all symbols to a file `zomp-symbol-file' and
load it into buffer `zomp-symbol-buffer'."
  (interactive)
  (save-excursion
    ;; TODO: why this order and not update file first?
    (when (file-exists-p zomp-symbol-file)
      (set-buffer (get-buffer-create zomp-symbol-buffer))
      (insert-file-contents zomp-symbol-file nil nil nil t))
    (process-send-string
     (zomp-get-shell-buffer)
     (concat "!silent !writeSymbols " zomp-symbol-file ""))))

(defun zomp-function-before-point ()
  "Returns the function of the current expression. When curser is at '|'
f(10, |20) will return f, print 10| will return print, etc."
  (interactive)
  (let ((linestart 0) (parenopen 0) exprstart funcend linesym funcsym)
    (setq linesym
          (save-excursion
            (back-to-indentation)
            (setq linestart (point))
            (ignore-errors
              (search-forward-regexp zomp-identifier-regexp))
            (buffer-substring linestart (point))))
    (setq exprsym
          (save-excursion
            (when (search-backward-regexp " [:=/*+-]+ " linestart t)
              (forward-char 1)
              (search-forward-regexp "[ a-aA-Z0-9]")
              (setq exprstart (point))
              (ignore-errors
                (search-forward-regexp zomp-identifier-regexp))
              (buffer-substring exprstart (point)))))
    (setq funcsym
          (save-excursion
            (ignore-errors
              (zomp-goto-match-paren 0)
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
                (buffer-substring (point) funcend)))))
    (when (and (> linestart parenopen) linesym funcsym)
      (setq funcsym nil))
    (replace-regexp-in-string ":$" "" (or exprsym funcsym linesym "nothing found"))))

(defun zomp-symbol-at-point ()
  "Return the Zomp identifier at point."
  (let ((w (word-at-point)))
    (replace-regexp-in-string ":$" "" w)))

(defun zomp-goto-definition ()
  "Navigate to the definition of symbol at point. This mimics `find-tag' but
uses the feedback from the Zomp shell.
TODO: feedback when Zomp shell is missing, fall back on imenu"
  (interactive)
  (let (symbol info file line)
    (setq symbol (zomp-symbol-at-point))
    (unless symbol
      (error "No symbol at point"))
    (save-excursion
      (zomp-build-symbol-buffer)
      (setq info (zomp-symbol-info symbol))
      (unless info
        (error "Symbol \"%s\" is not defined anywhere" symbol))
      (setq file (plist-get info :file))
      (setq line (plist-get info :line))
      (message "Symbol is at file %s line %s" file line))
    (if (equal "builtin" file)
        (message "Symbol %s is a compiler built-in" symbol)
      (ignore-errors
        (require 'etags)
        (ring-insert find-tag-marker-ring (point-marker)))
      (find-file-existing file)
      (goto-line line)
      (recenter))))

(defun zomp-get-doc-line-for-symbol (symbol)
  "Returns the line containing information about symbol generated
by the Zomp shell."
  (and symbol
       (ignore-errors
         (save-excursion
           (set-buffer (get-buffer-create zomp-symbol-buffer))
           (goto-char (point-max))
           (search-backward-regexp (format "^%s =" symbol))
           (buffer-substring
            (point)
            (save-excursion
              (end-of-line 1)
              (point)))))))

(defun zomp-split-doc-line (docline)
  "Extracts fields with information about a symbol from a line produced by
`zomp-get-doc-line-for-symbol'."
  (when (and docline
             (string-match "\\([a-zA-Z0-9_:]+\\) =\\([^@\n]+\\)\\(?: @\\([^:]*\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\)?$" docline))
    (list :name (match-string 1 docline)
          :short-doc (match-string 2 docline)
          :file (ignore-errors (match-string 3 docline))
          :line (ignore-errors (string-to-number (match-string 4 docline)))
          :column (ignore-errors (string-to-number (match-string 5 docline))))))

(defun zomp-symbol-info (symbol)
  "Returns information about the given symbol produced by Zomp shell."
  (zomp-split-doc-line (zomp-get-doc-line-for-symbol symbol)))

(defun zomp-get-eldoc-string ()
  "Gets a short documentation string for eldoc."
  (ignore-errors
    (zomp-build-symbol-buffer)
    (let ((info (zomp-symbol-info (zomp-function-before-point))))
      (when info
        (format "%s: %s"
                (plist-get info :name)
                (plist-get info :short-doc))))))

(defun zomp-ac-symbols-source ()
  "Function to be used as source for `auto-complete' that returns all symbols
known to the Zomp shell."
  (ignore-errors
    (save-excursion
      (zomp-build-symbol-buffer)
      (set-buffer (get-buffer-create zomp-symbol-buffer))
      (let ((lines (split-string (buffer-string) "\n" t)))
        (mapcar (lambda (line)
                  (plist-get (zomp-split-doc-line line) :name))
                lines)))))

(defun zomp-ac-help (symbol)
  "Produces help string to be displayed by `auto-complete'"
  (ignore-errors
    (let ((info (zomp-symbol-info symbol)))
      (if info
          (format "%s\n%s\n\n%s:%s"
                  (plist-get info :name)
                  (plist-get info :short-doc)
                  (or (plist-get info :file) "unknown")
                  (or (plist-get info :line) 0))
        (format "no help for %s" symbol)))))

(defvar zomp-ac-source
  '((candidates . zomp-ac-symbols-source)
    (document . zomp-ac-help))
  "`auto-complete' source for Zomp.")

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

(defvar zomp-marker-overlay nil
  "The overlay used by zomp to mark something. Always reusing
  this one to be able to clean it up on errors")

(defun zomp-query-adapt-indent-blocks ()
  "Function to help change code from old to new indent
  syntax. Will add ask the user if a colon should be inserted for
  all places where the indent increases in the next line and no
  colon is present, yet"
  (interactive)
  (when zomp-marker-overlay
    (delete-overlay zomp-marker-overlay))
  (let (previndent nextindent)
    (while t
      (setq previndent (zomp-current-line-indent))
      (next-line 1)
      (beginning-of-line)
      (unless (save-excursion
                (previous-line)
                (looking-at " *$"))
        (setq nextindent (zomp-current-line-indent))
        (when (> nextindent previndent)
          (previous-line)
          (end-of-line)
          (setq zomp-marker-overlay (make-overlay (save-excursion (beginning-of-line) (point)) (point)))
          (overlay-put zomp-marker-overlay 'face 'highlight-symbol-face)
          (when (and (not (looking-back " *:"))
                     (y-or-n-p "Insert ':'? "))
            (if (looking-back "{")
                (insert "seq:")
              (insert ":")))
          (delete-overlay zomp-marker-overlay)
          (next-line)
          )))))

(provide 'zomp)

;;; zomp.el ends here
