;;; yacas.el --- Major modes for writing Yacas code

;; Copyright (C) 2001,2002 Jay Belanger

;; Author: Jay Belanger
;; Maintainer: Jay Belanger <belanger@truman.edu>
;; Keywords: yacas

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;
;; Please send suggestions and bug reports to <belanger@truman.edu>. 
;; You will need yacas.el, yacas-names.el and yacas-functions.el

;;; Commentary:

;; Quick intro
;;
;; To install, put this file (as well as yacas-names.el and
;; yacas-functions.el) somewhere in your emacs load path.
;; To make sure that `yacas.el' is loaded when necessary, whether to
;; edit a file in yacas-mode or interact with Yacas in an Emacs buffer,
;; put the lines
;;  (autoload 'yacas-mode "yacas" "Yacas mode" t)
;;  (autoload 'yacas "yacas" "Yacas interaction" t)
;; in your `.emacs' file.  If you want any file ending in `.ys' to begin
;; in `yacas-mode', for example, put the line
;;  (setq auto-mode-alist (cons '("\\.ys" . yacas-mode) auto-mode-alist))
;; to your `.emacs' file.
;;
;; By default, yacas-mode assumes that the Yacas documentation is in
;; the directory "/usr/share/yacas/documentation/"
;; If the documentation is somewhere else, you need to change the value
;; of the variable `yacas-documentation-directory'
;;
;; Yacas mode:
;; This is a mode intended to support program development in Yacas.
;; The following control constructs can be entered automatically.

;;  C-c C-p Start a function (procedure)    
;;  C-c C-l Local
;;  C-c TAB If
;;  C-c C-e Else
;;  C-c C-f For
;;  C-c C-w While
;;  C-c C-a ForEach
;;  C-c C-u Until

;; The Local command is used to add new local variables in a procedure.
;; The If, While and Until commands will prompt for a predicate.
;; The For command will prompt for start, predicate and increment,
;; the ForEach command will prompt for an item and a list.

;; C-M-b and C-M-f 
;; move backward and forward respectively to the next line having the same 
;; (or lesser) level of indentation.
;; The variable yacas-indent controls the number of spaces for each indentation.
;; C-c ; will comment out a region, and 
;; C-c : will un-comment it out.
;; C-c ] will indent a region to the level of the line before the region.
;; C-c # will add an inline comment, and
;; C-c * will add an environment for a longer comment.

;; Use C-c C-r to run Yacas on the current region under
;; a special subshell.  C-c C-b does the whole buffer, and
;; C-c C-c just does the current line.  C-c C-k can
;; be used to kill the yacas process.
;;
;; Inferior Yacas mode:
;; Major mode for interacting with an inferior Yacas process.
;;
;; <M-tab> will complete the Yacas symbol as much as possible, providing
;;      a completion buffer if there is more than one possible completion.
;; <C-tab> will cycle through possible completions.
;;
;; C-c C-h will get help on a Yacas topic.
;; C-c C-k will kill the process and the buffer, after asking for
;;   confirmation.  To kill without confirmation, give C-c C-k an
;;   argument.
;;
;; To scroll through previous commands,
;; M-p (or up arrow) will bring the previous input to the current prompt,
;; M-n (or down arrow) will bring the next input to the prompt.
;; M-r will bring the previous input matching
;;   a regular expression to the prompt,
;; M-s will bring the next input matching
;;   a regular expression to the prompt.


(require 'font-lock)
(require 'yacas-names)
(require 'yacas-functions)
(require 'comint)
(provide 'yacas)


;; First of all, some values that the user might wish to change

(defgroup yacas nil
  "Yacas mode"
  :prefix "yacas-"
  :tag "Yacas")

(defcustom yacas-documentation-directory
  "/usr/share/yacas/documentation/"
  "Path where the yacas documentation is kept."
  :group 'yacas
  :type 'directory)

(defcustom yacas-command "yacas"
  "The command to run Yacas on a file."
  :group 'yacas
  :type 'string)

(defcustom yacas-args "-p"
  "Arguments passed to yacas-command."
  :group 'yacas
  :type 'string)

(defcustom yacas-indent 3 
  "*The indentation in yacas-mode"
  :group 'yacas
  :type 'integer)

;(defcustom yacas-prompt-regexp "^\\(In> \\|Out> \\)"
;  "A regular expression for the Yacas prompt."
;  :group 'yacas
;  :type 'string)

(defcustom yacas-input-prompt-string "In> "
  "A string for the standard Yacas prompt."
  :group 'yacas
  :type 'string)

(defcustom yacas-output-prompt-string "Out> "
  "A string for the standard Yacas prompt."
  :group 'yacas
  :type 'string)

;;;;;;;;;;;;;

(defvar yacas-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defvar inferior-yacas-process nil)

(defvar yacas-input-end nil
  "The position of the last part of the last input.")

(setq yacas-prettyform nil)

(if (fboundp 'subst-char-in-string)
    (defsubst yacas-replace-chars-in-string (from to string)
      (subst-char-in-string from to string))
  (defun yacas-replace-chars-in-string (from to string)
    "Replace characters in STRING from FROM to TO."
    (let ((string (substring string 0)) ;Copy string.
      (len (length string))
      (idx 0))
      ;; Replace all occurrences of FROM with TO.
      (while (< idx len)
    (when (= (aref string idx) from)
      (aset string idx to))
    (setq idx (1+ idx)))
      string)))

(defvar yacas-mode-syntax-table nil
  "Syntax table in use in Yacas-mode buffers.")

(if yacas-mode-syntax-table
    ()
  (setq yacas-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/  "_ 124b"   yacas-mode-syntax-table)
  (modify-syntax-entry ?*  "_ 23"     yacas-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"      yacas-mode-syntax-table)
  (modify-syntax-entry ?\; "."        yacas-mode-syntax-table)
  (modify-syntax-entry ?$ "."         yacas-mode-syntax-table)
  (modify-syntax-entry ?_ "_"         yacas-mode-syntax-table))

(defvar yacas-mode-map nil
  "Keymap used in yacas-mode")

(if yacas-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-?" 'yacas-untab)
    (define-key map (kbd "TAB") 'yacas-tab)
    (define-key map "\C-m" 'yacas-newline)
    ;; The control constructs
    (define-key map "\C-c\C-e" 'yacas-else)
    (define-key map "\C-c\C-f" 'yacas-for)
    (define-key map "\C-c\C-a" 'yacas-foreach)
    (define-key map "\C-c\C-i" 'yacas-if)
    (define-key map "\C-c\C-l" 'yacas-local)
    (define-key map "\C-c\C-p" 'yacas-proc)
    (define-key map "\C-c\C-w" 'yacas-while)
    (define-key map "\C-c\C-u" 'yacas-until)
    (define-key map "\C-c\C-n" 'yacas-function)
    ;; Other formatting commands
    (define-key map "\C-c#" 'yacas-short-comment)
    (define-key map "\C-c*" 'yacas-long-comment)
    (define-key map "\C-c]" 'yacas-indent-region)
    (define-key map "\C-c;" 'comment-region)
    (define-key map "\C-c:" 'yacas-uncomment-region)
    (define-key map "\C-c\C-t" 'yacas-title)
    (define-key map "\C-c\C-m" 'yacas-modify)
    ;; Motion commands
    (define-key map "\M-\C-b" 'yacas-backward-to-same-indent)
    (define-key map "\M-\C-f" 'yacas-forward-to-same-indent)
    ;; The run Yacas commands
    (define-key map "\C-c=" 'yacas-display-buffer)
    (define-key map "\C-c\C-r" 'yacas-region)
    (define-key map "\C-c\C-b" 'yacas-buffer)
    (define-key map "\C-c\C-c" 'yacas-line)
    (define-key map "\C-c\C-k" 'yacas-stop)
    (define-key map "\C-c\C-s" 'yacas-eval-string)
    (define-key map "\C-c\C-h" 'yacas-help)
    (define-key map "\C-c\C-q" 'yacas-reset)
    ;; Misc
    (define-key map (kbd "M-TAB") 'yacas-complete)
    (define-key map (kbd "C-TAB") 'yacas-dynamic-complete)
    (setq yacas-mode-map map)))

;;; Font-locking 
(setq yacas-match-symbol-predicates
      (regexp-opt yacas-symbol-predicates 'word))

(setq yacas-match-predicates 
      (regexp-opt yacas-predicates 'word))

(setq yacas-match-functions 
      (regexp-opt yacas-functions 'word))

(setq yacas-match-constants
      (regexp-opt yacas-constants 'word))

(setq yacas-match-control
      (regexp-opt yacas-control 'word))

(setq yacas-match-symbol-operators
      (regexp-opt yacas-symbol-operators))

(defconst yacas-font-lock-keywords
   (list
   ; Constants
    (list yacas-match-constants 0 font-lock-constant-face)
   ; Control flow
    (list yacas-match-control  0 font-lock-keyword-face)
   ; Operators
    (list yacas-match-symbol-operators 0 font-lock-keyword-face)
   ;Predicates
    (list yacas-match-symbol-predicates 0 font-lock-type-face)
    (list yacas-match-predicates 0 font-lock-type-face)
   ; Built in functions 
    (list yacas-match-functions 0 font-lock-builtin-face)
   ; New functions
    (list "^\\(.*\\):=" 1 font-lock-function-name-face))
   "Expressions to highlight in Yacas mode.")

;;; Completion

(defvar yacas-words nil
  "A list of words to use in completions")

(setq yacas-words
      (append yacas-predicates 
              yacas-constants 
              yacas-functions 
              yacas-control 
              yacas-words))

(setq yacas-words (mapcar (lambda (x) (list x)) yacas-words))

(defun yacas-word-beginning ()
  (save-excursion
    (backward-word 1)
    (point)))

(defun yacas-complete ()
  "Dynamically complete word from list of candidates.
A completions listing will be shown in a help buffer 
if completion is ambiguous."
  (interactive)
  (let* ((stub  (buffer-substring-no-properties 
                 (yacas-word-beginning) (point)))
	 (completions (all-completions stub yacas-words)))
    (cond ((null completions)
	   (message "No completions of %s" stub))
	  ((= 1 (length completions))	; Gotcha!
	   (let ((completion (car completions)))
	     (if (string-equal completion stub)
		 (message "Sole completion")
	       (insert (substring completion (length stub)))
	       (message "Completed"))))
	  (t				; There's no unique completion.
             (comint-dynamic-list-completions completions)))))

(defun yacas-he-try (old)
  (interactive)
  (if (not old)
      ;;; let beg be the beginning of the word
      (progn
        (he-init-string (yacas-word-beginning) (point))
        (setq he-expand-list 
              (all-completions (downcase he-search-string) yacas-words))
        (setq he-expand-list 
              (mapcar (function 
                      (lambda (x) (he-transfer-case he-search-string x)))
                      he-expand-list))
        (if he-expand-list
            (he-substitute-string (car he-expand-list))
          nil))
    (setq he-expand-list (cdr he-expand-list))
    (if he-expand-list
        (he-substitute-string (car he-expand-list))
      (he-reset-string)
      nil)))

(fset 'yacas-dynamic-complete 
      (make-hippie-expand-function '(yacas-he-try)))

(defun yacas-mode ()
  "This is a mode intended to support program development in Yacas.
The following control constructs can be entered automatically.

 \\[yacas-proc] Start a function (procedure)    
 \\[yacas-local] Local
 \\[yacas-if] If
 \\[yacas-else] Else
 \\[yacas-for] For
 \\[yacas-while] While
 \\[yacas-foreach] ForEach
 \\[yacas-until] Until

The Local command is used to add new local variables in a procedure.
The If, While and Until commands will prompt for a predicate.
The For command will prompt for start, predicate and increment,
the ForEach command will prompt for an item and a list.

\\[yacas-backward-to-same-indent] and \\[yacas-forward-to-same-indent] 
move backward and forward respectively to the next line having the same 
(or lesser) level of indentation.
The variable yacas-indent controls the number of spaces for each indentation.
\\[comment-region] will comment out a region, and 
\\[yacas-uncomment-region] will un-comment it out.
\\[yacas-indent-region] will indent a region to the level of the line before the region.
\\[yacas-short-comment] will add an inline comment, and
\\[yacas-long-comment] will add an environment for a longer comment.

Use \\[yacas-region] to run Yacas on the current region under
a special subshell.  \\[yacas-buffer] does the whole buffer, and
\\[yacas-line] just does the current line.  \\[yacas-stop] can
be used to kill the yacas process.

In the Yacas subshell buffer, the command C-c C-n will move to the next 
prompt, and C-c C-p will move to the previous prompt.  C-up and C-down 
can be used to scroll through the input history.

\\{yacas-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map yacas-mode-map)
  (setq major-mode 'yacas-mode)
  (setq mode-name "Yacas")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(yacas-font-lock-keywords))
  (set-syntax-table yacas-mode-syntax-table)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (yacas-menu-install-menus)
  (run-hooks 'yacas-mode-hooks))

(defun yacas-help ()
  (interactive)
  (let ((fn (read-string "Help on: ")))
    (select-window (split-window))
    (if (string= fn "")
        (browse-url
         (concat yacas-documentation-directory "books.html"))
      (browse-url
       (concat yacas-documentation-directory "ref.html#" fn)))
    (define-key (current-local-map) "q"
      '(lambda ()
         (interactive)
         (delete-window)))))

(defun yacas-newline ()
  "Insert a newline and indent following line like previous line."
  (interactive)
  (let ((hpos (current-indentation)))
    (newline)
    (indent-line-to hpos)))

(defun yacas-tab ()
  "Indent to next tab stop."
  (interactive)
  (if (> (current-column) (current-indentation))
      (insert "\t")
    (back-to-indentation)
    (let ((ci (current-indentation)))
      (backward-delete-char-untabify ci)
      (indent-to (* (1+ (/ ci yacas-indent)) yacas-indent)))))

(defun yacas-tabsize (s)
  "Changes the spacing used for indentation. Reads spacing from minibuffer."
  (interactive "new indentation spacing: ")
  (setq yacas-indent s))

(defun yacas-untab ()
  "Delete backwards to previous tab stop."
  (interactive)
  (backward-delete-char-untabify
   (let ((ind (current-indentation)))
     (if (and (= ind (current-column)) (>= ind yacas-indent))
         yacas-indent 1))))

(defun yacas-go-to-this-indent (step indent-level)
  "Move point repeatedly by <step> lines till the current line
has given indent-level or less, or the start/end of the buffer is hit.
Ignore blank lines and comments."
  (while (and
          (zerop (forward-line step))
          (or (looking-at "^[   ]*$")
              (looking-at "^[   ]*#")
              (looking-at "^<<[A-Za-z0-9_]+>>")
              (looking-at "^[A-Za-z0-9_]+:")
              (> (current-indentation) indent-level)))
    nil))

(defun yacas-backward-to-same-indent ()
  "Move point backwards to nearest line with same indentation or less.
If not found, point is left at top of buffer."
  (interactive)
  (yacas-go-to-this-indent -1 (current-indentation))
  (back-to-indentation))

(defun yacas-forward-to-same-indent ()
  "Move point forwards to nearest line with same indentation or less.
If not found, point is left at start of last line in buffer."
  (interactive)
  (yacas-go-to-this-indent 1 (current-indentation))
  (back-to-indentation))

(defun yacas-indent-region (beg end)
  "Indent the given region to the same level as the previous non-blank line"
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (let ((ri (current-indentation)) (pi) (di))
      (forward-line -1)
      (while (looking-at "^[   ]*$")
	(forward-line -1))
      (setq pi (current-indentation))
      (setq di (- pi ri))
      (indent-rigidly beg end di))))

;; Control constructs

(defun yacas-while ()
  "Build skeleton while statment, prompting for the conditional."
  (interactive)
  (insert "While(")
  (insert (read-string "predicate: "))
  (insert ")")
  (yacas-newline)
  (insert "[")
  (yacas-newline)
  (yacas-newline)
  (insert "];")
  (end-of-line 0)
  (yacas-tab))

(defun yacas-for ()
  "Build skeleton for statement, prompting for the loop parameters."
  (interactive)
  (insert "For(")
  (insert (read-string "start: ") ", ")
  (insert (read-string "predicate: ") ", ")
  (insert (read-string "increment: "))
  (insert ")")
  (yacas-newline)
  (insert "[")
  (yacas-newline)
  (yacas-newline)
  (insert "];")
  (end-of-line 0)
  (yacas-tab))

(defun yacas-foreach ()
  "Build skeleton foreach statement, prompting for the loop parameters."
  (interactive)
  (insert "ForEach(")
  (insert (read-string "item: ") ", ")
  (insert (read-string "list: "))
  (insert ")")
  (yacas-newline)
  (insert "[")
  (yacas-newline)
  (yacas-newline)
  (insert "];")
  (end-of-line 0)
  (yacas-tab))

(defun yacas-until ()
  "Build skeleton until statment, prompting for the conditional."
  (interactive)
  (insert "Until(")
  (insert (read-string "predicate: "))
  (insert ")")
  (yacas-newline)
  (insert "[")
  (yacas-newline)
  (yacas-newline)
  (insert "];")
  (end-of-line 0)
  (yacas-tab))

(defun yacas-if ()
  "Build skeleton if statment, prompting for the conditional."
  (interactive)
  (insert "If(")
  (insert (read-string "predicate: "))
  (insert ",")
  (yacas-newline)
  (insert "[")
  (yacas-newline)
  (yacas-newline)
  (insert "]);")
  (end-of-line 0)
  (yacas-tab))

(defun yacas-else ()
  "Build skeleton until statment, prompting for the conditional."
  (interactive)
  (yacas-untab)
  (insert "],")
  (yacas-newline)
  (insert "[")
  (yacas-newline)
  (yacas-tab))

(defun yacas-function ()
  "Build skeleton function statement, prompting for information"
  (interactive)
  (insert "Function(")
  (insert (read-string "operator: ") ", ")
  (insert (read-string "argument list: "))
  (insert ")")
  (yacas-newline)
  (insert "[")
  (yacas-newline)
  (yacas-newline)
  (insert "];")
  (end-of-line 0)
  (yacas-tab))

(defun yacas-title (arg)
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (if arg
      (yacas-goback-after-title)
    (if (eq (point) (point-min))
	nil
      (set-mark (point))
      (goto-char 1))
    (if (looking-at "/\\*\\+\\+    -\\*-Yacas-\\*-")
	(progn
	  (message "There already is a title")
	  (goto-char (mark)))
      (insert "/*++    -*-Yacas-*-\n\n\n")
      (insert "Author: " (user-full-name) "\n")
      (insert "        " "(" (user-login-name) "@" (system-name) ")\n")
      (insert "Created: " 
	      (format-time-string "%A, %e %B %Y, %l:%M %p %Z") "\n")
      (insert "Last Update: " (format-time-string "%A, %e %B %Y, %l:%M %p %Z"))
      (insert "\n")
      (insert "++*/\n")
      (forward-line -7))))

(defun yacas-goback-after-title ()
  "Return to the previous point after making the title"
  (interactive)
  (goto-char (mark)))

(defun yacas-modify ()
  "Insert a comment block containing the modification, author, etc."
  (interactive)
  (set-mark (point))
  (goto-char 1)
  (if (not (looking-at "/\\*\\+\\+"))
      (progn
	(message "You need to insert a title first (\"C-cC-ct\")")
	(goto-char (mark)))
    (if (search-forward "Last Update:" nil t)
	(progn			
	  (beginning-of-line)
	  (kill-line)
	  (insert "Last Update: " 
		  (format-time-string "%A, %e %B %Y, %l:%M %p %Z"))
	  (goto-char (mark)))
      (if (search-forward "++*/" nil t)
	  (progn
	    (forward-line -1)
	    (end-of-line)
	    (insert "\n")
	    (insert "Last Update: " 
		    (format-time-string "%A, %e %B %Y, %l:%M %p %Z"))
	    (goto-char (mark)))
	(message "Missing ++*/ (end-of-title)")
	(goto-char (mark))))))

(defun yacas-local ()
  "Add a new local variable, inserting the word local if necessary."
  (interactive)
  (save-excursion
    (let ((mil (current-indentation)))
      (beginning-of-line)
      (while (or (not (< (current-indentation) mil))
		 (not (looking-at "[ \t]*\\[")))
	(forward-line -1))
      (let ((first-time))
	(forward-line 1) (skip-chars-forward " \t")
	(if (looking-at "Local(")
	    (setq first-time nil)
	  (insert "Local( );")
	  (yacas-newline)
	  (forward-line -1)
	  (setq first-time t))
	(search-forward ");")
	(backward-char 2)
	(let ((newvar (read-string "New variable: ")))
	  (if first-time (insert newvar)
	    (insert ", " newvar)))))))

(defun yacas-proc ()
  "Build skeleton Yacas procedure, prompting for information."
  (interactive)
  (let ((name (read-string "Name: " ))
        args)
    (insert name " :=")
    (yacas-newline)
    (insert "[")
    (yacas-newline)
    (let ((local (read-string "Local variables: ")))
      (if (not (string-equal local ""))
	  (progn
	    (yacas-tab)
	    (insert "Local(" local ");")
	    (yacas-newline)
	    (yacas-untab))))
    (yacas-newline)
    (insert "]; /* ")
    (insert name)
    (insert " */")
    (end-of-line 0)
    (yacas-tab)))

(defun yacas-comment-region (beg end)
 "A comment-region to use with the Yacas menu"
 (interactive "r")
 (comment-region beg end))

(defun yacas-uncomment-region (beg end)
 "An uncomment-region to use with the Yacas menu"
 (interactive "r")
 (comment-region beg end (universal-argument)))

(defun yacas-short-comment ()
  "Insert a short comment"
  (interactive)
  (insert "/* " (read-string "Comment: ") " */")
  (yacas-newline))

(defun yacas-long-comment ()
  "Insert a comment enviroment"
  (interactive)
  (insert "/*")
  (yacas-newline)
  (yacas-newline)
  (insert "*/")
  (forward-line -1)
  (yacas-tab))


;;; Invoking Yacas in an inferior shell.

(defvar inferior-yacas-mode-map nil)

(if inferior-yacas-mode-map ()
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-c\C-k"    'yacas-stop) 
    (define-key map "\C-a" 'comint-bol)
    (define-key map "\C-c\C-h" 'yacas-help)
    (define-key map [up] 'comint-previous-input)
    (define-key map [down] 'comint-next-input)    
    (define-key map [(control up)] 'previous-line)
    (define-key map [(control down)] 'next-line)
    (define-key map (kbd "M-TAB") 'yacas-complete)
    (define-key map (kbd "C-TAB") 'yacas-dynamic-complete)
    (setq inferior-yacas-mode-map map)))
    
(defun inferior-yacas-mode ()
  "Inferior Yacas mode:
Major mode for interacting with an inferior Yacas process.

<M-tab> will complete the Yacas symbol as much as possible, providing
     a completion buffer if there is more than one possible completion.
<C-tab> will cycle through possible completions.

C-c C-h will get help on a Yacas topic.
C-c C-k will kill the process and the buffer, after asking for
  confirmation.  To kill without confirmation, give C-c C-k an
  argument.

To scroll through previous commands,
M-p (or up arrow) will bring the previous input to the current prompt,
M-n (or down arrow) will bring the next input to the prompt.
M-r will bring the previous input matching
  a regular expression to the prompt,
M-s will bring the next input matching
  a regular expression to the prompt.
"
  (interactive)
  (comint-mode)
  (setq major-mode 'inferior-yacas-mode)
  (setq mode-name "Inferior Yacas")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-yacas-mode-map)
  (make-local-variable 'comint-process-echoes)
  (setq comint-process-echoes nil)
  (make-local-variable 'comint-scroll-show-maximum-output)
  (setq comint-scroll-show-maximum-output t)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp (concat "^" yacas-input-prompt-string))
  (set-syntax-table yacas-mode-syntax-table)
  (inferior-yacas-menu-install-menus)
  (add-hook 'kill-buffer-hook
            (function
             (lambda ()
               (if (processp inferior-yacas-process)
                   (delete-process inferior-yacas-process))
               (setq inferior-yacas-process nil)
               (run-hooks 'inferior-yacas-exit-hook))) t t)
  (run-hooks 'inferior-yacas-mode-hook))

;;;; Interacting with the Yacas process

;;;; Starting and stopping

(defun yacas ()
  "Run yacas interactively inside a buffer."
  (interactive)
  (yacas-start)
  (switch-to-buffer (process-buffer inferior-yacas-process)))

(defun yacas-start ()
  "Start a Yacas process."
  (interactive)
  (if (processp inferior-yacas-process)
      (unless (eq (process-status inferior-yacas-process) 'run)
        (delete-process inferior-yacas-process)
        (save-excursion
          (set-buffer "*yacas*")
          (erase-buffer))
        (setq inferior-yacas-process nil)))
  (unless (processp inferior-yacas-process)
    (setq yacas-input-end 0)
    (let ((ybuf (make-comint "yacas" yacas-command nil yacas-args)))
      (save-excursion
        (set-buffer ybuf)
        (setq inferior-yacas-process (get-buffer-process ybuf))
        (while (not (yacas-new-prompt-p))
          (sleep-for 0.100))
        (inferior-yacas-mode)))))

(defun yacas-stop (&optional arg)
  "Kill the currently running Maxima process."
  (interactive "P")
  (if (processp inferior-yacas-process)
      (if arg
	  (progn 
	    (delete-process inferior-yacas-process)
	    (kill-buffer "*yacas*")
	    (setq inferior-yacas-process nil))
	(if (y-or-n-p "Really quit Yacas? ")
	    (progn
	      (delete-process inferior-yacas-process)
	      (kill-buffer "*yacas*")
	      (setq inferior-yacas-process nil))))))

;;;; Sending information to the process

(defun yacas-strip-string (string)
  "Remove any spaces or newlines at the beginning and end of the string"
  (while (or
          (string= "\n" (substring string -1))
          (string= " " (substring string -1)))
    (setq string (substring string 0 -1)))
  (while (or
          (string= "\n" (substring string 0 1))
          (string= " " (substring string 0 1)))
    (setq string (substring string 1)))
  string)

(defun yacas-prompt ()
  "Return the point of the last prompt in the yacas process buffer."
  (save-excursion
    (set-buffer (process-buffer inferior-yacas-process))
    (goto-char (point-max))
    (re-search-backward (concat "^" yacas-input-prompt-string))
    (match-end 0)))

(defun yacas-new-prompt-p ()
  "Check to see if there is a new prompt after the last input."
  (save-excursion
    (set-buffer (process-buffer inferior-yacas-process))
    (goto-char yacas-input-end)
    (or
     (re-search-forward (concat "^" yacas-input-prompt-string) (point-max) t)
     (re-search-forward "Process yacas finished" (point-max) t))))

(defun yacas-finished-p ()
  "Check to see if the Yacas process has halted"
  (not (yacas-running)))

(defun yacas-single-command (string)
  "Send a command to the Yacas process."
  (setq string (yacas-strip-string string))
  (yacas-start)
  (save-excursion
        (set-buffer (process-buffer inferior-yacas-process))
        (goto-char (point-max))
        (insert string)
        (setq yacas-input-end (point))
        (comint-send-input))
  (while (not (yacas-new-prompt-p))
    (sit-for 0.100)))

(defun yacas-send-block (stuff)
  "Send a block of code to Yacas."
  (yacas-start)
  (while (string-match ";" stuff)
    (setq end (1+ (string-match ";" stuff)))
    (yacas-single-command (substring stuff 0 end))
    (setq stuff (substring stuff end))))

;;; Sending information to the process should be done through these
;; next four commands

(defun yacas-string (string)
  "Send a string to the Yacas process."
  (setq inferior-yacas-computing-p t)
  (yacas-send-block string)
  (yacas-display-buffer))

(defun yacas-string-nodisplay (string)
  "Send a string to the Yacas process."
  (setq inferior-yacas-computing-p t)
  (yacas-send-block string))
    
(defun yacas-region (beg end)
  "Send the region to the Yacas process."
  (interactive "r")
  (yacas-string
   (buffer-substring-no-properties beg end)))

(defun yacas-region-nodisplay (beg end)
  "Send the region to the Yacas process,
do not display the yacas buffer."
  (yacas-string-nodisplay
   (buffer-substring-no-properties beg end)))

(defun yacas-reset ()
  "Reset the Yacas session without restarting it."
  (interactive)
  (yacas-string "restart;"))

(defun yacas-eval-string ()
  "Prompt for a string to send to the Yacas buffer."
  (interactive)
  (let ((str (read-string "Yacas: ")))
    (if (not (string-equal str ""))
	(yacas-string str))))

(defun yacas-line ()
  "Run Yacas on the current line."
  (interactive)
  (yacas-start)
  (let ((beg (progn (beginning-of-line) (point)))
	  (end (progn (end-of-line) (point))))
    (yacas-region beg end)))

(defun yacas-buffer ()
  "Run Yacas on the current buffer."
  (interactive)
  (yacas-region (point-min) (point-max)))

(defun yacas-display-buffer ()
  "Display the inferior-yacas-process buffer so the recent output is visible."
  (interactive)
  (let ((origbuffer (current-buffer)))
    (if (not (processp inferior-yacas-process))
	(yacas-start))
    (pop-to-buffer (process-buffer inferior-yacas-process))
    (goto-char (point-max))
;    (recenter (universal-argument))
    (pop-to-buffer origbuffer)))

(defun yacas-toggle-prettyform ()
  "Toggle the form of the output"
  (interactive)
  (if yacas-prettyform
      (progn
	(yacas-string "PrettyPrinter();")
	(setq yacas-prettyform nil))
    (yacas-string "PrettyPrinter(\"PrettyForm\");")
    (setq yacas-prettyform t)))

(add-hook 'yacas-start-shell-hooks 'yacas-toggle-prettyform)

;;; This menu part is stolen, more or less, from w3-menu.el, by William Perry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar yacas-menu-control-menu nil)
(defvar yacas-menu-process-menu nil)
(defvar yacas-menu-formatting-menu nil)
(defvar yacas-menu-motion-menu nil)
(defvar yacas-menu-misc-menu nil)
(defvar yacas-menu-help-menu nil)
(defvar yacas-menu-yacas-menu nil)
(defvar yacas-menu-yacas-menubar nil)

(defvar yacas-use-menus 
  '(ycs control process formatting motion misc nil help)
  "*Non-nil value causes Yacas mode to provide a menu interface.
A value that is a list causes YACAS mode to install its own menubar.
A value of 1 causes YACAS mode to install a \"Yacas\" item in the 
Emacs menubar.

If the value of yacas-use-menus is a list, it should be a list of symbols.
The symbols and the order that they are listed determine what menus
will be in the menubar and how they are ordered.  Valid symbol values
are:

help            -- Help 
control		-- Inserting control constructs
process  	-- Running Yacas
formatting	-- Formatting commands
motion  	-- Moving around
misc		-- Miscellaneous
ycs		-- A toggle button to switch back to normal emacs menus
nil		-- ** special **

If nil appears in the list, it should appear exactly once.  All
menus after nil in the list will be displayed flushright in the
menubar.")

(defun yacas-menu-global-menubar ()
  (if yacas-running-xemacs
      (default-value 'default-menubar)
    (lookup-key (current-global-map) [menu-bar])))

(defconst yacas-menu-control
  (list
   "Control"
   ["Procedure"  yacas-proc t]
   ["New local variable"  yacas-local t]
   ["Function" yacas-function t]
   ["If"  yacas-if t]
   ["Else"  yacas-else t]
   ["For"  yacas-for t]
   ["ForEach"  yacas-foreach t]
   ["While"  yacas-while t]
   ["Until"  yacas-until t])
  "Yacas control construct list.")

(defconst yacas-menu-process
  (list
   "Process"
   ["Start a Yacas process"   yacas-start
                                  (not (processp inferior-yacas-process))]
   ["Run Yacas on buffer"   yacas-buffer]
   ["Run Yacas on region"  yacas-region ]
   ["Run Yacas on line"  yacas-line]
   ["Display the output buffer"  yacas-display-buffer]
   ["Toggle PrettyForm" yacas-toggle-prettyform ]
   ["Reset the Yacas process" yacas-reset]
   ["Send string to Yacas" yacas-eval-string]
   ["Kill Yacas process"  yacas-stop  (processp inferior-yacas-process)])
  "Yacas process commands")

(defconst yacas-menu-formatting
  (list
   "Formatting"
   ["Comment region"  comment-region]
   ["Uncomment region"  yacas-uncomment-region ]
   ["Indent region"  yacas-indent-region ]
   ["Redefine Tab Size"  yacas-tabsize ]
   ["Short comment" yacas-short-comment]
   ["Long comment" yacas-long-comment])
  "Yacas formatting menu")

(defconst yacas-menu-motion
  (list
   "Motion"
   ["Forward to same indent"  yacas-forward-to-same-indent]
   ["Backward to same indent"  yacas-backward-to-same-indent ])
  "Yacas motion commands")
  
(defconst yacas-menu-misc
  (list
   "Misc"
   ["Comment region" comment-region]
   ["Uncomment region" yacas-uncomment-region]
   ["Short comment" yacas-short-comment]
   ["Long comment" yacas-long-comment]
   ["Title"   yacas-title]
   ["Return after title" yacas-goback-after-title]
   ["Modify"  yacas-modify ])
  "Yacas miscellaneous menu.")

(defun yacas-info-help ()
  (interactive)
  (info (concat "(" yacas-info-dir "yacas-notebook)Yacas mode")))


(defconst yacas-menu-help
  (list
   "Help"
   ["Yacas help" yacas-help] ; yacas-have-w3]
   ["Yacas mode help" yacas-info-help]))

(defconst yacas-menu-yacas
  (list
   "Yacas>>"
   ["Toggle Yacas/Emacs menus" yacas-menu-toggle-menubar]))

(defvar yacas-mode-menu-map nil)

(defun yacas-menu-initialize-yacas-mode-menu-map ()
  (if (null yacas-mode-menu-map)
      (let ((map (make-sparse-keymap))
	    (dummy (make-sparse-keymap)))
	(require 'easymenu)
	;; initialize all the yacas-menu-*-menu variables
	;; with the menus.
	(easy-menu-define yacas-menu-control-menu (list dummy) nil
			  yacas-menu-control)
	(easy-menu-define yacas-menu-process-menu (list dummy) nil
			  yacas-menu-process)
	(easy-menu-define yacas-menu-formatting-menu (list dummy) nil
			  yacas-menu-formatting)
	(easy-menu-define yacas-menu-motion-menu (list dummy) nil
			  yacas-menu-motion)
	(easy-menu-define yacas-menu-misc-menu (list dummy) nil
			  yacas-menu-misc)
	(easy-menu-define yacas-menu-yacas-menu (list dummy) nil
			  yacas-menu-yacas)
	(easy-menu-define yacas-menu-help-menu (list dummy) nil
			  yacas-menu-help)
	;; block the global menubar entries in the map so that YACAS
	;; can take over the menubar if necessary.
	(define-key map [rootmenu] (make-sparse-keymap))
	(define-key map [rootmenu yacas] 
	        (cons "Yacas" (make-sparse-keymap "Yacas")))
	(define-key map [rootmenu yacas options] 'undefined)	
	(define-key map [rootmenu yacas search] 'undefined)
	(define-key map [rootmenu yacas buffer] 'undefined)
	(define-key map [rootmenu yacas mule] 'undefined)
	(define-key map [rootmenu yacas tools] 'undefined)
	(define-key map [rootmenu yacas help] 'undefined)
	(define-key map [rootmenu yacas help-menu] 'undefined)
	;; now build YACAS's menu tree.
	(let ((menu-alist
	       '(
		 (ycs
		  (if (eq yacas-use-menus 1)
		      nil
		    (cons "Yacas>>" yacas-menu-yacas-menu)))
		 (control
		  (cons "Control" yacas-menu-control-menu))
		 (process
		  (cons "Process" yacas-menu-process-menu))
		 (shortcuts
		  (cons "Formatting" yacas-menu-formatting-menu))
		 (motion
		  (cons "Motion" yacas-menu-motion-menu))
		 (misc
		  (cons "Misc" yacas-menu-misc-menu))
		 (help 
                  (cons "Help" yacas-menu-help-menu)
                  )))
	      cons
	      (vec (vector 'rootmenu 'yacas nil))
	      ;; menus appear in the opposite order that we
	      ;; define-key them.
	      (menu-list 
	       (if (consp yacas-use-menus)
		   (reverse yacas-use-menus)
		 (list 'help nil 'misc 'motion 'formatting
		       'process 'control 'ycs ))))
	  (while menu-list
	    (if (null (car menu-list))
		nil;; no flushright support in FSF Emacs
	      (aset vec 2 (intern (concat "yacas-menu-"
					  (symbol-name
					   (car menu-list)) "-menu")))
	      (setq cons (assq (car menu-list) menu-alist))
	      (if cons
		  (define-key map vec (eval (car (cdr cons))))))
	    (setq menu-list (cdr menu-list))))
	(setq yacas-mode-menu-map map)
	(run-hooks 'yacas-menu-setup-hook))))

(defun yacas-menu-make-xemacs-menubar ()
  (let ((menu-alist
	 '((ycs    . yacas-menu-yacas)
	   (control  . yacas-menu-control)
	   (process    . yacas-menu-process)
	   (formatting     . yacas-menu-formatting)
	   (motion     . yacas-menu-motion)
	   (misc     . yacas-menu-misc)
	   (help . yacas-menu-help))
	 )
	cons
	(menubar nil)
	(menu-list yacas-use-menus))
    (while menu-list
      (cond
       ((null (car menu-list))
	(setq menubar (cons nil menubar)))
       (t (setq cons (assq (car menu-list) menu-alist))
	  (if cons
	      (setq menubar (cons (symbol-value (cdr cons)) menubar)))))
      (setq menu-list (cdr menu-list)))
    (nreverse menubar)))

(defun yacas-menu-install-menubar ()
  (cond
   (yacas-running-xemacs
    (cond
     ((not (featurep 'menubar)) nil)	; No menus available
     (t
      (setq yacas-menu-yacas-menubar (yacas-menu-make-xemacs-menubar))
      (set-buffer-menubar yacas-menu-yacas-menubar))))
   ((not (fboundp 'yacas-menu-control-menu))
    (yacas-menu-initialize-yacas-mode-menu-map)
    (define-key yacas-mode-map [menu-bar]
      (lookup-key yacas-mode-menu-map [rootmenu yacas])))))

(defun yacas-menu-install-menubar-item ()
  (cond
   (yacas-running-xemacs
    (if (not (featurep 'menubar))
	nil				; No menus available
      (set-buffer-menubar (copy-sequence (yacas-menu-global-menubar)))
      (add-menu nil "Yacas" (cdr yacas-menu-yacas-menubar))))
   ((not (fboundp 'yacas-menu-control-menu))
    (yacas-menu-initialize-yacas-mode-menu-map)
    (define-key yacas-mode-map [menu-bar]
      (lookup-key yacas-mode-menu-map [rootmenu])))))

(defun yacas-menu-install-menus ()
  (cond ((consp yacas-use-menus)
	 (yacas-menu-install-menubar))
	((eq yacas-use-menus 1)
	 (yacas-menu-install-menubar-item))
	(t nil)))

(defun yacas-menu-set-menubar-dirty-flag ()
  (cond (yacas-running-xemacs
	 (set-menubar-dirty-flag))
	(t
	 (force-mode-line-update))))

(defun yacas-menu-toggle-menubar ()
  (interactive)
  (cond
   (yacas-running-xemacs
    (if (null (car (find-menu-item current-menubar '("Yacas>>"))))
	(set-buffer-menubar yacas-menu-yacas-menubar)
      (set-buffer-menubar (copy-sequence (yacas-menu-global-menubar)))
      (condition-case ()
	  (add-menu-button nil ["[Yacas]" yacas-menu-toggle-menubar t] nil)
	(void-function
	 (add-menu-item nil "[Yacas]" 'yacas-menu-toggle-menubar t))))
    (yacas-menu-set-menubar-dirty-flag))
   (t
    (if (not (eq (lookup-key yacas-mode-map [menu-bar])
		 (lookup-key yacas-mode-menu-map [rootmenu yacas])))
	(define-key yacas-mode-map [menu-bar]
	  (lookup-key yacas-mode-menu-map [rootmenu yacas]))
      (define-key yacas-mode-map [menu-bar]
	(make-sparse-keymap))
      (define-key yacas-mode-map [menu-bar yacas]
	(cons "[Yacas]" 'yacas-menu-toggle-menubar)))
    (yacas-menu-set-menubar-dirty-flag))))


;;;;;;;; Menus for the shell mode

(defun inferior-yacas-quit ()
  (interactive)
  (if (yes-or-no-p "Really quit? ")
      (yacas-stop)))

(defvar inferior-yacas-menu-yacas-menu nil)
(defvar inferior-yacas-menu-quit-menu nil)
(defvar inferior-yacas-menu-arithmetic-menu nil)
(defvar inferior-yacas-menu-functions-menu nil)
(defvar inferior-yacas-menu-linalg-menu nil)
(defvar inferior-yacas-menu-calculus-menu nil)
(defvar inferior-yacas-menu-simplify-menu nil)
(defvar inferior-yacas-menu-solve-menu nil)
(defvar inferior-yacas-menu-poly-menu nil)
(defvar inferior-yacas-menu-lists-menu nil)
(defvar inferior-yacas-menu-config-menu nil)
(defvar inferior-yacas-menu-prog-menu nil)
(defvar inferior-yacas-menu-others-menu nil)
(defvar inferior-yacas-menu-help-menu nil)

(defvar inferior-yacas-use-menus 
  '(yacas arithmetic functions linalg calculus simplify solve poly lists
       config progm others nil help)
  "*Non-nil value causes Yacas shell mode to provide a menu interface.
A value that is a list causes Yacas shell to install its own menubar.
A value of 1 causes Yacas shell to install a \"Yacas\" item in the 
Emacs menubar.

If the value of yacas-use-menus is a list, it should be a list of symbols.
The symbols and the order that they are listed determine what menus
will be in the menubar and how they are ordered.  Valid symbol values
are:

help            -- Help
arithmetic	-- Arithmetic functions
functions  	-- Transcendental functions, etc.
linalg            -- Linear Algebra
calculus          -- Calculus
simplify          -- simplification of expressions
solve		-- solve various equations
poly              -- polynomial related functions
lists             -- list operations
config            -- functions for configuring yacas
progm              -- programming constructs
others            -- a hodgepodge of other stuff
yacas		-- A toggle button to switch back to normal emacs menus
nil		-- ** special **

If nil appears in the list, it should appear exactly once.  All
menus after nil in the list will be displayed flushright in the
menubar.")

(defun inferior-yacas-menu-global-menubar ()
  (if yacas-running-xemacs
      (default-value 'default-menubar)
    (lookup-key (current-global-map) [menu-bar])))

(defvar inferior-yacas-mode-menu-map nil)

(defun inferior-yacas-menu-initialize-inferior-yacas-mode-menu-map ()
  (if (null inferior-yacas-mode-menu-map)
      (let ((map (make-sparse-keymap))
	    (dummy (make-sparse-keymap)))
	(require 'easymenu)
	;; initialize all the inferior-yacas-menu-*-menu variables
	;; with the menus.
	(easy-menu-define inferior-yacas-menu-yacas-menu (list dummy) nil
			  inferior-yacas-menu-yacas)
	(easy-menu-define inferior-yacas-menu-quit-menu (list dummy) nil
			  inferior-yacas-menu-quit)
	(easy-menu-define inferior-yacas-menu-arithmetic-menu (list dummy) nil
			  inferior-yacas-menu-arithmetic)
	(easy-menu-define inferior-yacas-menu-functions-menu (list dummy) nil
			  inferior-yacas-menu-functions)
	(easy-menu-define inferior-yacas-menu-linalg-menu (list dummy) nil
			  inferior-yacas-menu-linalg)
	(easy-menu-define inferior-yacas-menu-calculus-menu (list dummy) nil
			  inferior-yacas-menu-calculus)
	(easy-menu-define inferior-yacas-menu-simplify-menu (list dummy) nil
			  inferior-yacas-menu-simplify)
	(easy-menu-define inferior-yacas-menu-solve-menu (list dummy) nil
			  inferior-yacas-menu-solve)
	(easy-menu-define inferior-yacas-menu-poly-menu (list dummy) nil
			  inferior-yacas-menu-poly)
	(easy-menu-define inferior-yacas-menu-lists-menu (list dummy) nil
			  inferior-yacas-menu-lists)
	(easy-menu-define inferior-yacas-menu-config-menu (list dummy) nil
			  inferior-yacas-menu-config)
	(easy-menu-define inferior-yacas-menu-prog-menu (list dummy) nil
			  inferior-yacas-menu-prog)
	(easy-menu-define inferior-yacas-menu-others-menu (list dummy) nil
			  inferior-yacas-menu-others)
	(easy-menu-define inferior-yacas-menu-help-menu (list dummy) nil
			  inferior-yacas-menu-help)
	;; block the global menubar entries in the map so that YACAS
	;; can take over the menubar if necessary.
	(define-key map [rootmenu] (make-sparse-keymap))
	(define-key map [rootmenu inferior-yacas] 
	        (cons "Yacas Shell" (make-sparse-keymap "Yacas Shell")))
	(define-key map [rootmenu inferior-yacas file] 'undefined)	
	(define-key map [rootmenu inferior-yacas files] 'undefined)	
	(define-key map [rootmenu inferior-yacas edit] 'undefined)	
	(define-key map [rootmenu inferior-yacas options] 'undefined)	
	(define-key map [rootmenu inferior-yacas search] 'undefined)
	(define-key map [rootmenu inferior-yacas buffer] 'undefined)
	(define-key map [rootmenu inferior-yacas completion] 'undefined)	
	(define-key map [rootmenu inferior-yacas signals] 'undefined)	
	(define-key map [rootmenu inferior-yacas inout] 'undefined)	
	(define-key map [rootmenu inferior-yacas mule] 'undefined)
	(define-key map [rootmenu inferior-yacas tools] 'undefined)
	(define-key map [rootmenu inferior-yacas help] 'undefined)
	(define-key map [rootmenu inferior-yacas help-menu] 'undefined)
	;; now build YACAS's menu tree.
	(let ((menu-alist
	       '(
		 (arithmetic
		  (cons "Arith" inferior-yacas-menu-arithmetic-menu))
		 (yacas
		    (cons "Yacas>>" inferior-yacas-menu-yacas-menu))
		 (quit
		  (cons "Quit" inferior-yacas-menu-quit-menu))
		 (functions
		  (cons "Functions" inferior-yacas-menu-functions-menu))
		 (linalg
		  (cons "LinAlg" inferior-yacas-menu-linalg-menu))
		 (calculus
		  (cons "Calc" inferior-yacas-menu-calculus-menu))
		 (simplify
		  (cons "Simp" inferior-yacas-menu-simplify-menu))
		 (solve
		  (cons "Solve" inferior-yacas-menu-solve-menu))
		 (poly
		  (cons "Poly" inferior-yacas-menu-poly-menu))
		 (lists
		  (cons "Lists" inferior-yacas-menu-lists-menu))
		 (config
		  (cons "Config" inferior-yacas-menu-config-menu))
		 (progm
		  (cons "Prog" inferior-yacas-menu-prog-menu))
		 (others
		  (cons "Others" inferior-yacas-menu-others-menu))
		 (help
		  (cons "Help" inferior-yacas-menu-help-menu))))
	      cons
	      (vec (vector 'rootmenu 'inferior-yacas nil))
	      ;; menus appear in the opposite order that we
	      ;; define-key them.
	      (menu-list 
	       (if (consp inferior-yacas-use-menus)
		   (reverse inferior-yacas-use-menus)
		 (list nil 'help 'config 'progm 'others 'lists 'poly 
		       'solve 'simplify 'calculus 'linalg 'functions 
		       'arithmetic 'quit))))
	  (while menu-list
	    (if (null (car menu-list))
		nil;; no flushright support in FSF Emacs
	      (aset vec 2 (intern (concat "inferior-yacas-menu-"
					  (symbol-name
					   (car menu-list)) "-menu")))
	      (setq cons (assq (car menu-list) menu-alist))
	      (if cons
		  (define-key map vec (eval (car (cdr cons))))))
	    (setq menu-list (cdr menu-list))))
	(setq inferior-yacas-mode-menu-map map)
	(run-hooks 'inferior-yacas-menu-setup-hook))))

(defun inferior-yacas-menu-make-xemacs-menubar ()
  (let ((menu-alist
	 '((yacas    . inferior-yacas-menu-yacas)
	   (arithmetic  . inferior-yacas-menu-arithmetic)
	   (functions    . inferior-yacas-menu-functions)
	   (linalg     . inferior-yacas-menu-linalg)
	   (calculus     . inferior-yacas-menu-calculus)
	   (simplify     . inferior-yacas-menu-simplify)
	   (solve     . inferior-yacas-menu-solve)
	   (poly     . inferior-yacas-menu-poly)
	   (lists     . inferior-yacas-menu-lists)
	   (config     . inferior-yacas-menu-config)
	   (progm     . inferior-yacas-menu-prog)
	   (others     . inferior-yacas-menu-others)
	   (help . inferior-yacas-menu-help))
	 )
	cons
	(menubar nil)
	(menu-list inferior-yacas-use-menus))
    (while menu-list
      (cond
       ((null (car menu-list))
	());(setq menubar (cons nil menubar)))
       (t (setq cons (assq (car menu-list) menu-alist))
	  (if cons
	      (setq menubar (cons (symbol-value (cdr cons)) menubar)))))
      (setq menu-list (cdr menu-list)))
    (nreverse menubar)))

(defun inferior-yacas-menu-install-menubar ()
  (cond
   (yacas-running-xemacs
    (cond
     ((not (featurep 'menubar)) nil)	; No menus available
     (t
      (setq inferior-yacas-menu-yacas-menubar 
	    (inferior-yacas-menu-make-xemacs-menubar))
      (set-buffer-menubar inferior-yacas-menu-yacas-menubar))))
   ((not (fboundp 'inferior-yacas-menu-arithmetic-menu))
    (inferior-yacas-menu-initialize-inferior-yacas-mode-menu-map)
    (define-key inferior-yacas-mode-map [menu-bar]
      (lookup-key inferior-yacas-mode-menu-map [rootmenu inferior-yacas])))))

(defun inferior-yacas-menu-install-menubar-item ()
  (cond
   (yacas-running-xemacs
    (if (not (featurep 'menubar))
	nil				; No menus available
      (set-buffer-menubar (copy-sequence (inferior-yacas-menu-global-menubar)))
      (add-menu nil "Yacas" (cdr (inferior-yacas-menu-make-xemacs-menubar)))))
   ((not (fboundp 'inferior-yacas-menu-arithmetic-menu))
    (inferior-yacas-menu-initialize-inferior-yacas-mode-menu-map)
    (define-key inferior-yacas-mode-map [menu-bar]
      (lookup-key inferior-yacas-mode-menu-map [rootmenu])))))

(defun inferior-yacas-menu-install-menus ()
  (cond ((consp inferior-yacas-use-menus)
	 (inferior-yacas-menu-install-menubar))
	((eq inferior-yacas-use-menus 1)
	 (inferior-yacas-menu-install-menubar-item))
	(t nil)))

(defun inferior-yacas-menu-set-menubar-dirty-flag ()
  (cond (yacas-running-xemacs
	 (set-menubar-dirty-flag))
	(t
	 (force-mode-line-update))))

(defun inferior-yacas-menu-toggle-menubar ()
  (interactive)
  (cond
   (yacas-running-xemacs
    (if (null (car (find-menu-item current-menubar '("[Yacas>>]"))))
	(set-buffer-menubar inferior-yacas-menu-yacas-menubar)
      (set-buffer-menubar (copy-sequence (inferior-yacas-menu-global-menubar)))
      (condition-case ()
	  (add-menu-button nil ["[Yacas]" 
				inferior-yacas-menu-toggle-menubar t] nil)
	(void-function
	 (add-menu-item nil "[Yacas]" 'inferior-yacas-menu-toggle-menubar t))))
    (inferior-yacas-menu-set-menubar-dirty-flag))
   (t
    (if (not (eq (lookup-key inferior-yacas-mode-map [menu-bar])
		 (lookup-key inferior-yacas-mode-menu-map 
			     [rootmenu inferior-yacas])))
	(define-key inferior-yacas-mode-map [menu-bar]
	  (lookup-key inferior-yacas-mode-menu-map [rootmenu inferior-yacas]))
      (define-key inferior-yacas-mode-map [menu-bar]
	(make-sparse-keymap))
      (define-key inferior-yacas-mode-map [menu-bar inferior-yacas]
	(cons "[Yacas]" 'inferior-yacas-menu-toggle-menubar)))
    (inferior-yacas-menu-set-menubar-dirty-flag))))

