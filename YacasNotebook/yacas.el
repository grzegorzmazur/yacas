;;                 Yacas editing support package

(require 'font-lock)
(require 'yacas-names)
(require 'comint)
(provide 'yacas)


;; First of all, some values that the user might wish to change

(defvar yacas-documentation-directory
  "/usr/share/yacas/documentation/"
  "Path where the yacas documentation is kept.")

(defvar yacas-info-dir "")

(defvar yacas-command "yacas"
  "The command to run Yacas on a file.")

(defvar yacas-args "-p"
  "Arguments passed to yacas-command.")

(defvar yacas-indent 3 
  "*The indentation in yacas-mode")

(defvar yacas-prompt-regexp "\\(In> \\|Out> \\)"
  "A regular expression for the Yacas prompt.")

(defvar yacas-input-prompt-string "In> "
  "A string for the standard Yacas prompt.")

(defvar yacas-output-prompt-string "Out> "
  "A string for the standard Yacas prompt.")

(defvar yacas-show-yacas-buffer t
  "If t, the Yacas buffer will be visible.")

;;;;;;;;;;;;;
(defvar yacas-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(setq yacas-prompt-width (string-width yacas-input-prompt-string))

(setq yacas-process nil)

(defvar yacas-send-filter-active nil
  "Status of Yacas process filter: t if enabled, else nil.")

(setq yacas-use-filter nil)

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
  (modify-syntax-entry ?\\ "\\"  yacas-mode-syntax-table)
  (modify-syntax-entry ?( "()"  yacas-mode-syntax-table)  
  (modify-syntax-entry ?) ")("  yacas-mode-syntax-table)
  (modify-syntax-entry ?[ "(]"  yacas-mode-syntax-table)  
  (modify-syntax-entry ?] ")["  yacas-mode-syntax-table)
  (modify-syntax-entry ?{ "(}"  yacas-mode-syntax-table)  
  (modify-syntax-entry ?} "){"  yacas-mode-syntax-table)
  (modify-syntax-entry ?+ "."   yacas-mode-syntax-table)
  (modify-syntax-entry ?- "."    yacas-mode-syntax-table)
  (modify-syntax-entry ?/  ". 14" yacas-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"   yacas-mode-syntax-table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?= "."    yacas-mode-syntax-table)
  (modify-syntax-entry ?% "."    yacas-mode-syntax-table)
  (modify-syntax-entry ?< "."    yacas-mode-syntax-table)
  (modify-syntax-entry ?> "."    yacas-mode-syntax-table)
  (modify-syntax-entry ?$ "."    yacas-mode-syntax-table)
  (modify-syntax-entry ?| "."    yacas-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    yacas-mode-syntax-table)
  (set-syntax-table yacas-mode-syntax-table))

(defvar yacas-mode-map nil
  "Keymap used in yacas-mode")

(defun yacas-add-keys (map)
  "Add Yacas specific key bindings to the keymap"
    (define-key map "\C-?" 'yacas-untab)
    (define-key map "\C-i" 'yacas-tab)
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
    (define-key map "\C-c/" 'comment-region)
    (define-key map "\C-c\\" 'yacas-uncomment-region)
    (define-key map "\C-c\C-t" 'yacas-title)
    (define-key map "\C-c\C-m" 'yacas-modify)
    ;; Motion commands
    (define-key map "\C-c<" 'yacas-backward-to-same-indent)
    (define-key map "\C-c>" 'yacas-forward-to-same-indent))

(defun yacas-add-process-keys (map)
    ;; The run Yacas commands
    (define-key map "\M-\C-p" 'yacas-start-process)
    (define-key map "\M-\C-r" 'yacas-region)
    (define-key map "\M-\C-b" 'yacas-buffer)
    (define-key map "\M-\C-l" 'yacas-line)
    (define-key map "\M-\C-k" 'yacas-kill-job)
    (define-key map "\M-\C-s" 'yacas-eval-string)
    (define-key map "\C-c=" 'yacas-recenter-output-buffer)
    (define-key map "\C-c\C-h" 'yacas-help)
;    (define-key map "\C-c\C-o" 'yacas-copy-last-output)
;    (define-key map "\C-c\C-s" 'yacas-copy-complete-last-output)
    (define-key map "\M-\C-q" 'yacas-reset))

(if yacas-mode-map ()
  (let ((map (make-sparse-keymap)))
    (yacas-add-keys map)
    (yacas-add-process-keys map)
    (define-key map "\C-c?" 'yacas-info-help)
    (define-key map (kbd "M-TAB") 'yacas-complete)
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
  `(
   ; Constants
    (,yacas-match-constants (0 font-lock-constant-face nil))
   ; Control flow
    (,yacas-match-control  (0 font-lock-keyword-face nil))
   ; Operators
    (,yacas-match-symbol-operators (0 font-lock-keyword-face nil))
   ;Predicates
    (,yacas-match-symbol-predicates (0 font-lock-type-face nil))
    (,yacas-match-predicates (0 font-lock-type-face nil))
   ; Functions 
    (,yacas-match-functions (0 font-lock-function-name-face nil))
   ; Strings 
    ("\\W\\\"\\([^\\\"]*\\)\\\"\\W" (1 font-lock-string-face t))
   ; Comments
    ("/\\*\\(.*\\)\\*/" (0 font-lock-comment-face t))
    ("//.*$" (0 font-lock-comment-face t)))
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
\\[yacas-line] just does the current line.  \\[yacas-kill-job] can
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
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(yacas-font-lock-keywords
	  nil nil ((?_ . "w") (?~ . "w")) beginning-of-defun
	  (font-lock-comment-start-regexp . "/*")
	  (font-lock-comment-end-regexp . "*/")
	  (font-lock-mark-block-function . mark-defun)))
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 72)
  (set-syntax-table yacas-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *\\|// *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'font-lock-defaults)
  (yacas-menu-install-menus)
  (run-hooks 'yacas-mode-hooks))

(setq yacas-have-w3 nil)
(if (or yacas-running-xemacs (require 'w3 "w3" t))
    (progn
      (setq yacas-have-w3 t)
      (defun yacas-help ()
	(interactive)
	(let ((fn (read-string "Help on: ")))
	  (select-window (split-window))
	  (if (string= fn "")
	      (w3-open-local 
	       (concat yacas-documentation-directory "books.html"))
	    (w3-open-local
	     (concat yacas-documentation-directory "ref.html#" fn)))
	  (define-key (current-local-map) "q"
	    '(lambda ()
	       (interactive)
	       (w3-quit)
	       (delete-window)))))))

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

(defun yacas-send-filter (proc string)
  "The filter for a Yacas process.
It will tell when the process has stopped (by looking for the prompt)."
     (let ((old-buffer (current-buffer)))
       (unwind-protect
	   (let (moving)
	     (set-buffer (process-buffer proc))
	     (setq moving (= (point) (process-mark proc)))
	     (save-excursion
	       ;; Insert the text, moving the process-marker.
	       (goto-char (process-mark proc))
	       (insert string)
	       (set-marker (process-mark proc) (point)))
	     (if moving (goto-char (process-mark proc))))
	 (set-buffer old-buffer)
	 (if (not (< (length string) yacas-prompt-width))
	     (if 
		 (string= 
		  (substring string (- yacas-prompt-width)) 
		  yacas-input-prompt-string)
		 (setq yacas-send-filter-active nil))))))

;;; Invoking Yacas in an inferior shell.

(defun yacas (&optional arg)
  "Run yacas in a buffer, without shell. Exiting yacas will kill the buffer.
The buffer is called *Yacas*. The buffer is put in shell-mode with
a yacas-syntax-table."
  (interactive)
  (if (get-buffer "*Yacas*")
      nil
    (yacas-start-shell))
  (switch-to-buffer "*Yacas*")
  (if arg
      nil
    (delete-other-windows)))

(defvar yacas-shell-map nil
  "Keymap used in the *Yacas* buffer")

(defun yacas-start-shell ()
  "Start a Yacas shell."
  (require 'shell)
  (save-excursion
    (setq yacas-process (start-process "yacas" "*Yacas*"
				       yacas-command yacas-args))
    (if yacas-use-filter
	(set-process-filter yacas-process 'yacas-send-filter))
    (set-buffer "*Yacas*")
    (shell-mode)
    (make-local-variable 'comint-process-echoes)
    (setq comint-process-echoes nil)
    (make-local-variable 'comint-scroll-show-maximum-output)
    (setq comint-scroll-show-maximum-output t)
    (make-local-variable 'shell-prompt-pattern)
    (setq shell-prompt-pattern yacas-prompt-regexp)
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp (concat "^" yacas-input-prompt-string))
    (set-syntax-table yacas-mode-syntax-table)
    (setq yacas-shell-map (copy-keymap shell-mode-map))
    (define-key yacas-shell-map "\M-\C-k"    'yacas-kill-job) 
    (define-key yacas-shell-map (kbd "M-TAB") 'yacas-complete)
    (define-key yacas-shell-map "\C-c=" 'yacas-recenter-output-buffer)
    (define-key yacas-shell-map "\C-a" 'comint-bol)
    (define-key yacas-shell-map "\C-c\C-h" 'yacas-help)
    (define-key yacas-shell-map [up] 'comint-previous-input)
    (define-key yacas-shell-map [down] 'comint-next-input)    
    (define-key yacas-shell-map [(control up)] 'previous-line)
    (define-key yacas-shell-map [(control down)] 'next-line)
    (use-local-map yacas-shell-map)
    (make-local-variable 'font-lock-defaults)
    (setq yacas-shell-font-lock-keywords
	  (cons '("\\(In> \\|Out> \\)" 0 font-lock-keyword-face nil)
		yacas-font-lock-keywords))
    (setq yacas-shell-font-lock-keywords
	  (cons '(
"\\(To exit Yacas, enter  Exit(); or quit or Ctrl-c. Type \\?\\? for help.\\|Or type \\?function for help on a function.\\|Type 'restart' to restart Yacas.\\|To see example commands, keep typing Example();\\|Numeric mode:\\)"
0 font-lock-keyword-face nil) yacas-shell-font-lock-keywords))
    (setq font-lock-defaults '(yacas-shell-font-lock-keywords nil))
    (yacas-shell-menu-install-menus)
    (run-hooks 'yacas-start-shell-hooks)))

(defun yacas-wait ()
  "Wait until Yacas is through with the current computation."
  (if yacas-use-filter
      (while yacas-send-filter-active
	(sleep-for 1))))

(defun yacas-start-process ()
  "Start a Yacas process"
  (interactive)
  (setq yacas-send-filter-active t)
  (if (not (processp yacas-process)) 
      (progn
	(yacas-start-shell)
	(yacas-wait)))
;  (yacas-wait)
;  (setq yacas-send-filter-active t)
;  (yacas-string "PrettyPrinter(\"PrettyForm\");")
;  (yacas-wait)
  (if yacas-show-yacas-buffer
      (yacas-recenter-output-buffer nil)))

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
  (if (not (processp yacas-process)) 
	(yacas-start-shell))
  (setq yacas-send-filter-active t)
  (let ((beg (progn (beginning-of-line) (point)))
	  (end (progn (end-of-line) (point))))
    (yacas-region beg end)))

(defun yacas-region (beg end)
  "Run Yacas on the current region."  
  (interactive "r")
  (let (str)
    (setq yacas-send-filter-active t)
    (if (not (processp yacas-process)) 
	(progn
	  (yacas-start-shell)
	  (yacas-wait)
	  ))
    (setq yacas-send-filter-active t)
    (setq str (buffer-substring-no-properties beg end))
    (setq str (yacas-replace-chars-in-string (string-to-char "\n")
				    (string-to-char " ") str))
    (yacas-string (concat "[" str "];"))
    (yacas-wait)
    (sit-for 1)
    (if yacas-show-yacas-buffer
    (yacas-recenter-output-buffer nil))))

(defun yacas-string (yacas-str)
  "Run Yacas on a given string."
  (interactive "r")
  (if (not (processp yacas-process)) 
      (progn
	(yacas-start-shell)
	(yacas-wait)))
  (setq yacas-send-filter-active t)
  (process-send-string yacas-process (concat yacas-str "\n"))
  (sit-for 1)
  (if yacas-show-yacas-buffer
  (yacas-recenter-output-buffer nil)))

(defun yacas-buffer ()
  "Run Yacas on the current buffer."
  (interactive)
  (yacas-region (point-min) (point-max)))

(defun yacas-kill-job ()
  "Kill the currently running Yacas job."
  (interactive)
  (if (processp yacas-process)
      (progn
	(delete-process "yacas")
	(kill-buffer "*Yacas*")
	(setq yacas-process nil))))

(defun yacas-recenter-output-buffer (linenum)
  "Redisplay buffer of Yacas job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((old-buffer (current-buffer))
        (yacas-shell (if  (get-buffer-process (current-buffer))
                         (current-buffer)
                         (get-buffer "*Yacas*"))))
    (if (null yacas-shell)
        (message "No Yacas output buffer")
      (pop-to-buffer yacas-shell)
      (goto-char (point-max))
      (recenter (if linenum
                    (prefix-numeric-value linenum)
                  (/(window-height) 2)))
      (pop-to-buffer old-buffer))))

(defun yacas-copy-last-output ()
  "Copy the last output from Yacas to the kill-ring,
only including what follows the Out> prompt.
If the yacas-prettyform is t, then only copy what's before the
Out> prompt."
  (interactive)
  (if yacas-prettyform
      (yacas-copy-prettyform-output)
    (let ((out-start)
	  (out-end)
	  (old-buffer (current-buffer))
	  (yacas-shell (if  (get-buffer-process (current-buffer))
			   (current-buffer)
                         (get-buffer "*Yacas*"))))
      (yacas-wait)
      (if (null yacas-shell)
	  (message "No Yacas output buffer")
	(pop-to-buffer yacas-shell)
	(save-excursion
	  (end-of-buffer)
	  (re-search-backward yacas-input-prompt-string)
	  (forward-line -1)
	  (end-of-line)
	  (forward-char 1)
	  (setq out-end (point))
	  (re-search-backward yacas-output-prompt-string)
	  (forward-char (string-width yacas-output-prompt-string))
	  (setq out-start (point))
	  (copy-region-as-kill out-start out-end)))
      (pop-to-buffer old-buffer))))

(defun yacas-copy-complete-last-output ()
  "Copy the last output from Yacas to the kill-ring,
including what's before the Out> prompt.  
If the yacas-prettyform it t, then only copy what's before the
Out> prompt."
  (interactive)
  (if yacas-prettyform
      (yacas-copy-prettyform-output)
    (let ((out-start)
	  (out-end)
	  (old-buffer (current-buffer))
	  (yacas-shell (if  (get-buffer-process (current-buffer))
			   (current-buffer)
                         (get-buffer "*Yacas*"))))
      (yacas-wait)
      (if (null yacas-shell)
	  (message "No Yacas output buffer")
	(pop-to-buffer yacas-shell)
	(save-excursion
	  (end-of-buffer)
	  (re-search-backward yacas-input-prompt-string)
	  (forward-line -1)
	  (end-of-line)
	  (forward-char 1)
	  (setq out-end (point))
	  (re-search-backward yacas-input-prompt-string)
	  (if (looking-at ".*Out>.*$")
	      (re-search-forward yacas-input-prompt-string)
	    (forward-line 1)
	    (beginning-of-line))
	  (setq out-start (point))
	  (copy-region-as-kill out-start out-end)))
      (pop-to-buffer old-buffer))))	

(defun yacas-copy-prettyform-output ()
  "Copy the last output from Yacas to the kill-ring."
  (interactive)
  (let ((out-start)
	(out-end)
	(old-buffer (current-buffer))
        (yacas-shell (if  (get-buffer-process (current-buffer))
                         (current-buffer)
		       (get-buffer "*Yacas*"))))
    (yacas-wait)
    (if (null yacas-shell)
        (message "No Yacas output buffer")
      (pop-to-buffer yacas-shell)
      (save-excursion
	(end-of-buffer)
	(search-backward yacas-output-prompt-string)
	        ;; Get rid of blank lines at the end
	(forward-line -1)
	(while (looking-at "^ *$") (forward-line -1)) 	
	(forward-line 1)
	(setq out-end (point))
	(search-backward yacas-input-prompt-string)
	       ;; Get rid of blank lines at the beginning
	(forward-line 1)
        (while (looking-at "^ *$") (forward-line 1))
	(setq out-start (point))
	(copy-region-as-kill out-start out-end)))
    (pop-to-buffer old-buffer)))

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
   ["Start a Yacas process"   yacas-start-process 
                                  (not (processp yacas-process))]
   ["Run Yacas on buffer"   yacas-buffer]
   ["Run Yacas on region"  yacas-region ]
   ["Run Yacas on line"  yacas-line]
   ["Recenter the output buffer"  yacas-recenter-output-buffer]
   ["Toggle PrettyForm" yacas-toggle-prettyform ]
   ["Reset the Yacas process" yacas-reset]
   ["Send string to Yacas" yacas-eval-string]
   ["Kill Yacas process"  yacas-kill-job  (processp yacas-process)])
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
   ["Yacas help" yacas-help yacas-have-w3]
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
		  (if yacas-have-w3
		      (cons "Help" yacas-menu-help-menu)
		    nil))))
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

(defun yacas-fn-isinfinity ()
  (interactive)
  (insert "IsInfinity()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnonzerointeger ()
  (interactive)
  (insert "IsNonZeroInteger()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnotzero ()
  (interactive)
  (insert "IsNotZero()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-ispositiveinteger ()
  (interactive)
  (insert "IsPositiveInteger()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-ispositivenumber ()
  (interactive)
  (insert "IsPositiveNumber()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnegativeinteger ()
  (interactive)
  (insert "IsNegativeInteger()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnegativenumber ()
  (interactive)
  (insert "IsNegativeNumber()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isinteger ()
  (interactive)
  (insert "IsInteger()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isnumber ()
  (interactive)
  (insert "IsNumber()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isodd ()
  (interactive)
  (insert "IsOdd()")
  (forward-char -1)
  (message "Args: integer"))

(defun yacas-fn-iseven ()
  (interactive)
  (insert "IsEven()")
  (forward-char -1)
  (message "Args: integer"))

(defun yacas-fn-fibonacci ()
  (interactive)
  (insert "Fibonacci()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-pi ()
  (interactive)
  (insert "Pi()"))

(defun yacas-fn-conjugate ()
  (interactive)
  (insert "Conjugate()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-denom ()
  (interactive)
  (insert "Denom()")
  (forward-char -1)
  (message "Args: r"))

(defun yacas-fn-numer ()
  (interactive)
  (insert "Numer()")
  (forward-char -1)
  (message "Args: r"))

(defun yacas-fn-isrational ()
  (interactive)
  (insert "IsRational()")
  (forward-char -1)
  (message "Args: r"))

(defun yacas-fn-iszero ()
  (interactive)
  (insert "IsZero()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-max ()
  (interactive)
  (insert "Max()")
  (forward-char -1)
  (message "Args: x, y or a list of numbers"))

(defun yacas-fn-min ()
  (interactive)
  (insert "Min()")
  (forward-char -1)
  (message "Args: x,y or a list of numbers"))

(defun yacas-fn-factorize ()
  (interactive)
  (insert "Factorize()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (if (string= var "")
	(message "Args: list")
      (insert var ", ")
      (setq var (read-string "From: "))
      (insert var ", ")
      (setq var (read-string "To: "))
      (insert var ", "))))

(defun yacas-fn-average ()
  (interactive)
  (insert "Average()")
  (forward-char -1)
  (message "Args: list of numbers"))

(defun yacas-fn-bin ()
  (interactive)
  (insert "Bin()")
  (forward-char -1)
  (message "Args: n, m"))

(defun yacas-fn-im ()
  (interactive)
  (insert "Im()")
  (forward-char -1)
  (message "Args: z"))

(defun yacas-fn-re ()
  (interactive)
  (insert "Re()")
  (forward-char -1)
  (message "Args: z"))

(defun yacas-fn-complex ()
  (interactive)
  (insert "Complex()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-round ()
  (interactive)
  (insert "Round()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-ceil ()
  (interactive)
  (insert "Ceil()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-floor ()
  (interactive)
  (insert "Floor()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-decimal ()
  (interactive)
  (insert "Decimal()")
  (forward-char -1)
  (message "Args: fraction"))

(defun yacas-fn-contfrac ()
  (interactive)
  (insert "ContFrac()")
  (forward-char -1)
  (message "Args: x ;Opt args: maximum depth"))

(defun yacas-fn-padicexpand ()
  (interactive)
  (insert "PAdicExpand()")
  (forward-char -1)
  (message "Args: number, p"))

(defun yacas-fn-factors ()
  (interactive)
  (insert "Factors()")
  (forward-char -1)
  (message "Args: n (integer or polynomial)"))

(defun yacas-fn-factor ()
  (interactive)
  (insert "Factor()")
  (forward-char -1)
  (message "Args: n (integer or polynomial)"))

(defun yacas-fn-isprimepower ()
  (interactive)
  (insert "IsPrimePower()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isprime ()
  (interactive)
  (insert "IsPrime()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-tobase ()
  (interactive)
  (insert "ToBase()")
  (forward-char -1)
  (message "Args: base, number"))

(defun yacas-fn-frombase ()
  (interactive)
  (insert "FromBase()")
  (forward-char -1)
  (message "Args: base, number"))

(defun yacas-fn-lcm ()
  (interactive)
  (insert "Lcm()")
  (forward-char -1)
  (message "Args: m, n (integers or polynomials)"))

(defun yacas-fn-gcd ()
  (interactive)
  (insert "Gcd()")
  (forward-char -1)
  (message "Args: m, n (or a list of numbers or polynomials)"))

(defun yacas-fn-mod ()
  (interactive)
  (insert "Mod()")
  (forward-char -1)
  (message "Args: m, n (integers or polynomials)"))

(defun yacas-fn-div ()
  (interactive)
  (insert "Div()")
  (forward-char -1)
  (message "Args: m, n (integers or polynomials)"))


(defun yacas-fn-limit ()
  (interactive)
  (insert "Limit()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "Value: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-antideriv ()
  (interactive)
  (insert "Integrate()")
  (forward-char -1)
  (message "Args: expression, variable"))

(defun yacas-fn-integrate ()
  (interactive)
  (insert "Integrate()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "From: "))
    (insert var ", ")
    (setq var (read-string "To: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-curl ()
  (interactive)
  (insert "Curl()")
  (forward-char -1)
  (message "Args: vector, basis"))

(defun yacas-fn-diverge ()
  (interactive)
  (insert "Diverge()")
  (forward-char -1)
  (message "Args: vector, basis"))

(defun yacas-fn-d ()
  (interactive)
  (insert "D()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-newton ()
  (interactive)
  (insert "Newton()")
  (forward-char -1)
  (let ((var (read-string "Function: ")))
    (insert var ", ")
    (setq var (read-string "Variable: "))
    (insert var ", ")
    (setq var (read-string "Initial value: "))
    (insert var ", ")
    (setq var (read-string "Accuracy: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-reversepoly ()
  (interactive)
  (insert "ReversePoly()")
  (forward-char -1)
  (let ((var (read-string "Polynomial 1: ")))
    (insert var ", ")
    (setq var (read-string "Polynomial 2: "))
    (insert var ", ")
    (setq var (read-string "New Variable: "))
    (insert var)
    (setq var (read-string "Degree: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-inversetaylor ()
  (interactive)
  (insert "InverseTaylor()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "Centered at: "))
    (insert var ", ")
    (setq var (read-string "Degree: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-taylor ()
  (interactive)
  (insert "Taylor()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "Centered at: "))
    (insert var ", ")
    (setq var (read-string "Order: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-secure ()
  (interactive)
  (insert "Secure()")
  (forward-char -1)
  (message "Args: body"))

(defun yacas-fn-prog ()
  (interactive)
  (insert "Prog()")
  (forward-char -1)
  (message "Args: expressions ..."))

(defun yacas-fn-tracerule ()
  (interactive)
  (insert "TraceRule()")
  (forward-char -1)
  (message "Args: template"))

(defun yacas-fn-traceexp ()
  (interactive)
  (insert "TraceExp()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-tracestack ()
  (interactive)
  (insert "TraceStack()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-localsymbols ()
  (interactive)
  (insert "LocalSymbols()")
  (forward-char -1)
  (message "Args: symbols"))

(defun yacas-fn-apply ()
  (interactive)
  (insert "Apply()")
  (forward-char -1)
  (message "Args: operator, list"))

(defun yacas-fn-eval ()
  (interactive)
  (insert "Eval()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-hold ()
  (interactive)
  (insert "Hold()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-maxevaldepth ()
  (interactive)
  (insert "MaxEvalDepth()")
  (forward-char -1)
  (message "Args: n"))


(defun yacas-fn-fastarcsin ()
  (interactive)
  (insert "FastArcSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastarccos ()
  (interactive)
  (insert "FastArcCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastarctan ()
  (interactive)
  (insert "FastArcTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fasttan ()
  (interactive)
  (insert "FastTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastcos ()
  (interactive)
  (insert "FastCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastsin ()
  (interactive)
  (insert "FastSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastpower ()
  (interactive)
  (insert "FastPower()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-fastlog ()
  (interactive)
  (insert "FastLog()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastexp ()
  (interactive)
  (insert "FastExp()")
  (forward-char -1)
  (message "Args: x"))


(defun yacas-fn-patchload ()
  (interactive)
  (insert "PatchLoad()")
  (forward-char -1)
  (message "Args: filename"))

(defun yacas-fn-findfile ()
  (interactive)
  (insert "FindFile()")
  (forward-char -1)
  (message "Args: name"))

(defun yacas-fn-defload ()
  (interactive)
  (insert "DefLoad()")
  (forward-char -1)
  (message "Args: filename"))

(defun yacas-fn-load ()
  (interactive)
  (insert "Load()")
  (forward-char -1)
  (message "Args: filename"))

(defun yacas-fn-tofile ()
  (interactive)
  (insert "ToFile()")
  (forward-char -1)
  (message "Args: file"))

(defun yacas-fn-readtoken ()
  (interactive)
  (insert "ReadToken()"))

(defun yacas-fn-lispread ()
  (interactive)
  (insert "LispRead()"))

(defun yacas-fn-read ()
  (interactive)
  (insert "Read()"))

(defun yacas-fn-tostring ()
  (interactive)
  (insert "ToString()"))

(defun yacas-fn-fromstring ()
  (interactive)
  (insert "FromString()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-fromfile ()
  (interactive)
  (insert "FromFile()")
  (forward-char -1)
  (message "Args: file"))

(defun yacas-fn-newline ()
  (interactive)
  (insert "NewLine()")
  (forward-char -1)
  (message "Args: number (optional)"))

(defun yacas-fn-space ()
  (interactive)
  (insert "Space()")
  (forward-char -1)
  (message "Args: number (optional)"))

(defun yacas-fn-writestring ()
  (interactive)
  (insert "WriteString()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-write ()
  (interactive)
  (insert "Write()")
  (forward-char -1)
  (message "Args: expressions"))

(defun yacas-fn-prettyform ()
  (interactive)
  (insert "PrettyForm()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-echo ()
  (interactive)
  (insert "Echo()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-fullform ()
  (interactive)
  (insert "FullForm()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-use ()
  (interactive)
  (insert "Use()")
  (forward-char -1)
  (message "Args: file"))


(defun yacas-fn-iszerovector ()
  (interactive)
  (insert "IsZeroVector()")
  (forward-char -1)
  (message "Args: vector"))

(defun yacas-fn-isunitary ()
  (interactive)
  (insert "IsUnitary()")
  (forward-char -1)
  (message "Args: square matrix"))

(defun yacas-fn-ishermitian ()
  (interactive)
  (insert "IsHermitian()")
  (forward-char -1)
  (message "Args: square matrix"))

(defun yacas-fn-eigenvectors ()
  (interactive)
  (insert "EigenVectors()")
  (forward-char -1)
  (message "Args: matrix, eigenvalues"))

(defun yacas-fn-eigenvalues ()
  (interactive)
  (insert "EigenValues()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-characteristicequation ()
  (interactive)
  (insert "CharacteristicEquation()")
  (forward-char -1)
  (message "Args: matrix, variable"))

(defun yacas-fn-solvematrix ()
  (interactive)
  (insert "SolveMatrix()")
  (forward-char -1)
  (message "Args: matrix, vector"))

(defun yacas-fn-minor ()
  (interactive)
  (insert "Minor()")
  (forward-char -1)
  (message "Args: matrix, i, j"))

(defun yacas-fn-cofactor ()
  (interactive)
  (insert "CoFactor()")
  (forward-char -1)
  (message "Args: matrix, i, j"))

(defun yacas-fn-inverse ()
  (interactive)
  (insert "Inverse()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-trace ()
  (interactive)
  (insert "Trace()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-diagonalmatrix ()
  (interactive)
  (insert "DiagonalMatrix()")
  (forward-char -1)
  (message "Args: vector"))

(defun yacas-fn-determinant ()
  (interactive)
  (insert "Determinant()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-tranpose ()
  (interactive)
  (insert "Tranpose()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-zeromatrix ()
  (interactive)
  (insert "ZeroMatrix()")
  (forward-char -1)
  (message "Args: n, m"))

(defun yacas-fn-normalize ()
  (interactive)
  (insert "Normalize()")
  (forward-char -1)
  (message "Args: vector"))

(defun yacas-fn-ismatrix ()
  (interactive)
  (insert "IsMatrix()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-identity ()
  (interactive)
  (insert "Identity()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-basevector ()
  (interactive)
  (insert "BaseVector()")
  (forward-char -1)
  (message "Args: row, n"))

(defun yacas-fn-zerovector ()
  (interactive)
  (insert "ZeroVector()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-crossproduct ()
  (interactive)
  (insert "CrossProduct()")
  (forward-char -1)
  (message "Args: vector1, vector2"))

(defun yacas-fn-inproduct ()
  (interactive)
  (insert "InProduct()")
  (forward-char -1)
  (message "Args: vector1, vector2"))

(defun yacas-fn-commutator ()
  (interactive)
  (insert "Commutator()")
  (forward-char -1)
  (message "Args: a, b"))


(defun yacas-fn-islist ()
  (interactive)
  (insert "IsList()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-select ()
  (interactive)
  (insert "Select()")
  (forward-char -1)
  (message "Args: predicate, list"))

(defun yacas-fn-tableform ()
  (interactive)
  (insert "TableForm()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-table ()
  (interactive)
  (insert "Table()")
  (forward-char -1)
  (message "Args: body, variable, from, to, step"))

(defun yacas-fn-bubblesort ()
  (interactive)
  (insert "BubbleSort()")
  (forward-char -1)
  (message "Args: list, \"compare\""))

(defun yacas-fn-unflatten ()
  (interactive)
  (insert "Unflatten()")
  (forward-char -1)
  (message "Args: list, operator, identity"))

(defun yacas-fn-flatten ()
  (interactive)
  (insert "Flatten()")
  (forward-char -1)
  (message "Args: expression, operator"))

(defun yacas-fn-associndices ()
  (interactive)
  (insert "AssocIndices()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-assoc ()
  (interactive)
  (insert "Assoc()")
  (forward-char -1)
  (message "Args: key, list"))

(defun yacas-fn-partition ()
  (interactive)
  (insert "Partition()")
  (forward-char -1)
  (message "Args: list, n"))

(defun yacas-fn-take ()
  (interactive)
  (insert "Take()")
  (forward-char -1)
  (message "Args: list, integer of pair of integers"))

(defun yacas-fn-drop ()
  (interactive)
  (insert "Drop()")
  (forward-char -1)
  (message "Args: list, integer or pair of integers"))

(defun yacas-fn-filllist ()
  (interactive)
  (insert "FillList()")
  (forward-char -1)
  (message "Args: item, length"))

(defun yacas-fn-difference ()
  (interactive)
  (insert "Difference()")
  (forward-char -1)
  (message "Args: list1, list2"))

(defun yacas-fn-union ()
  (interactive)
  (insert "Union()")
  (forward-char -1)
  (message "Args: list1, list2"))

(defun yacas-fn-intersection ()
  (interactive)
  (insert "Intersection()")
  (forward-char -1)
  (message "Args: list1, list2"))

(defun yacas-fn-count ()
  (interactive)
  (insert "Count()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-swap ()
  (interactive)
  (insert "Swap()")
  (forward-char -1)
  (message "Args: list, index1, index2"))

(defun yacas-fn-popback ()
  (interactive)
  (insert "PopBack()")
  (forward-char -1)
  (message "Args: stack"))

(defun yacas-fn-popfront ()
  (interactive)
  (insert "PopFront()")
  (forward-char -1)
  (message "Args: stack"))

(defun yacas-fn-pop ()
  (interactive)
  (insert "Pop()")
  (forward-char -1)
  (message "Args: stack, index"))

(defun yacas-fn-push ()
  (interactive)
  (insert "Push()")
  (forward-char -1)
  (message "Args: stack, element"))

(defun yacas-fn-removeduplicates ()
  (interactive)
  (insert "RemoveDuplicates()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-destructiveappend ()
  (interactive)
  (insert "DestructiveAppend()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-append ()
  (interactive)
  (insert "Append()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-find ()
  (interactive)
  (insert "Find()")
  (forward-char -1)
  (message "Args: list, item"))

(defun yacas-fn-contains ()
  (interactive)
  (insert "Contains()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-flatcopy ()
  (interactive)
  (insert "FlatCopy()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-destructivereplace ()
  (interactive)
  (insert "DestructiveReplace()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-replace ()
  (interactive)
  (insert "Replace()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-destructivedelete ()
  (interactive)
  (insert "DestructiveDelete()")
  (forward-char -1)
  (message "Args: list, index"))

(defun yacas-fn-destructiveinsert ()
  (interactive)
  (insert "DestructiveInsert()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-insert ()
  (interactive)
  (insert "Insert()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-delete ()
  (interactive)
  (insert "Delete()")
  (forward-char -1)
  (message "Args: list, index"))

(defun yacas-fn-concat ()
  (interactive)
  (insert "Concat()")
  (forward-char -1)
  (message "Args: list1, list2, ..."))

(defun yacas-fn-listify ()
  (interactive)
  (insert "Listify()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-unlist ()
  (interactive)
  (insert "UnList()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-list ()
  (interactive)
  (insert "List()")
  (forward-char -1)
  (message "Args: elements of list"))

(defun yacas-fn-destructivereverse ()
  (interactive)
  (insert "DestructiveReverse()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-nth ()
  (interactive)
  (insert "Nth()")
  (forward-char -1)
  (message "Args: list, index"))

(defun yacas-fn-length ()
  (interactive)
  (insert "Length()")
  (forward-char -1)
  (message "Args: object (list, array or string)"))

(defun yacas-fn-tail ()
  (interactive)
  (insert "Tail()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-head ()
  (interactive)
  (insert "Head()")
  (forward-char -1)
  (message "Args: list"))


(defun yacas-fn-mathdiv ()
  (interactive)
  (insert "MathDiv()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-matharccos ()
  (interactive)
  (insert "MathArcCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-matharcsin ()
  (interactive)
  (insert "MathArcSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-matharctan ()
  (interactive)
  (insert "MathArcTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathtan ()
  (interactive)
  (insert "MathTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathcos ()
  (interactive)
  (insert "MathCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathsin ()
  (interactive)
  (insert "MathSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathpower ()
  (interactive)
  (insert "MathPower()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathlog ()
  (interactive)
  (insert "MathLog()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathexp ()
  (interactive)
  (insert "MathExp()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathmod ()
  (interactive)
  (insert "MathMod()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathabs ()
  (interactive)
  (insert "MathAbs()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathceil ()
  (interactive)
  (insert "MathCeil()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathfloor ()
  (interactive)
  (insert "MathFloor()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathsqrt ()
  (interactive)
  (insert "MathSqrt()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathdivide ()
  (interactive)
  (insert "MathDivide()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathmultiply ()
  (interactive)
  (insert "MathMultiply()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathsubtract ()
  (interactive)
  (insert "MathSubtract()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathadd ()
  (interactive)
  (insert "MathAdd()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathgcd ()
  (interactive)
  (insert "MathGcd()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-mathor ()
  (interactive)
  (insert "MathOr()")
  (forward-char -1)
  (message "Args: booleans"))

(defun yacas-fn-mathnot ()
  (interactive)
  (insert "MathNot()")
  (forward-char -1)
  (message "Args: booleans"))

(defun yacas-fn-mathand ()
  (interactive)
  (insert "MathAnd()")
  (forward-char -1)
  (message "Args: booleans"))


(defun yacas-fn-listfromarray ()
  (interactive)
  (insert "ListFromArray()")
  (forward-char -1)
  (message "Args: array"))

(defun yacas-fn-arraycreatefromlist ()
  (interactive)
  (insert "ArrayCreateFromList()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-arrayset ()
  (interactive)
  (insert "ArraySet()")
  (forward-char -1)
  (message "Args: array, index, element"))

(defun yacas-fn-arrayget ()
  (interactive)
  (insert "ArrayGet()")
  (forward-char -1)
  (message "Args: array, index"))

(defun yacas-fn-arraysize ()
  (interactive)
  (insert "ArraySize()")
  (forward-char -1)
  (message "Args: array"))

(defun yacas-fn-arraycreate ()
  (interactive)
  (insert "ArrayCreate()")
  (forward-char -1)
  (message "Args: size, initial value"))

(defun yacas-fn-generictypename ()
  (interactive)
  (insert "GenericTypeName()")
  (forward-char -1)
  (message "Args: object"))

(defun yacas-fn-isgeneric ()
  (interactive)
  (insert "IsGeneric()")
  (forward-char -1)
  (message "Args: object"))

(defun yacas-fn-shiftright ()
  (interactive)
  (insert "ShiftRight()")
  (forward-char -1)
  (message "Args: number, bits"))

(defun yacas-fn-shiftleft ()
  (interactive)
  (insert "ShiftLeft()")
  (forward-char -1)
  (message "Args: number, bits"))

(defun yacas-fn-greaterthan ()
  (interactive)
  (insert "GreaterThan()")
  (forward-char -1)
  (message "Args: number1, number2"))

(defun yacas-fn-lessthan ()
  (interactive)
  (insert "LessThan()")
  (forward-char -1)
  (message "Args: number1, number2"))

(defun yacas-fn-equals ()
  (interactive)
  (insert "Equals()")
  (forward-char -1)
  (message "Args: a, b"))

(defun yacas-fn-bitxor ()
  (interactive)
  (insert "BitXor()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-bitor ()
  (interactive)
  (insert "BitOr()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-bitand ()
  (interactive)
  (insert "BitAnd()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-macrolocal ()
  (interactive)
  (insert "MacroLocal()")
  (forward-char -1)
  (message "Args: variables"))

(defun yacas-fn-local ()
  (interactive)
  (insert "Local()")
  (forward-char -1)
  (message "Args: variables"))

(defun yacas-fn-unfence ()
  (interactive)
  (insert "UnFence()")
  (forward-char -1)
  (message "Args: \"operator\", arity"))

(defun yacas-fn-tryretract ()
  (interactive)
  (insert "Retract()")
  (forward-char -1)
  (message "Args: \"operator\", arity"))

(defun yacas-fn-holdarg ()
  (interactive)
  (insert "HoldArg()")
  (forward-char -1)
  (message "Args: \"operator\", parameter"))

(defun yacas-fn-rulebase ()
  (interactive)
  (insert "RuleBase()")
  (forward-char -1)
  (message "Args: \"operator\", parameter list"))

(defun yacas-fn-macrorulebase ()
  (interactive)
  (insert "MacroRuleBase()")
  (forward-char -1)
  (message "Args: \"operator\", parameter list"))

(defun yacas-fn-rightprecedence ()
  (interactive)
  (insert "RightPrecedence()")
  (forward-char -1)
  (message "Args: \"operator\", precendence"))

(defun yacas-fn-leftprecedence ()
  (interactive)
  (insert "LeftPrecedence()")
  (forward-char -1)
  (message "Args: \"operator\", precendence"))

(defun yacas-fn-rightassociative ()
  (interactive)
  (insert "RightAssociative()")
  (forward-char -1)
  (message "Args: \"operator\""))

(defun yacas-fn-opprecedence ()
  (interactive)
  (insert "OpPrecedence()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-ispostfix ()
  (interactive)
  (insert "IsPostfix()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-isprefix ()
  (interactive)
  (insert "IsPrefix()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-isinfix ()
  (interactive)
  (insert "IsInfix()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-infix ()
  (interactive)
  (insert "Infix()")
  (forward-char -1)
  (message "Args: \"operator\", precedence"))

(defun yacas-fn-bodied ()
  (interactive)
  (insert "Bodied()")
  (forward-char -1)
  (message "Args: \"operator\", precedence"))

(defun yacas-fn-postfix ()
  (interactive)
  (insert "Postfix()")
  (forward-char -1)
  (message "Args: \"operator\""))

(defun yacas-fn-prefix ()
  (interactive)
  (insert "Prefix()")
  (forward-char -1)
  (message "Args: \"operator\""))

(defun yacas-fn-check ()
  (interactive)
  (insert "Check()")
  (forward-char -1)
  (message "Args: predicate, error"))

(defun yacas-fn-concatstrings ()
  (interactive)
  (insert "ConcatStrings()")
  (forward-char -1)
  (message "Args: string1, string2, ..."))

(defun yacas-fn-string ()
  (interactive)
  (insert "String()")
  (forward-char -1)
  (message "Args: atom"))

(defun yacas-fn-atom ()
  (interactive)
  (insert "Atom()")
  (forward-char -1)
  (message "Args: atom"))

(defun yacas-fn-lazyglobal ()
  (interactive)
  (insert "LazyGlobal()")
  (forward-char -1)
  (message "Args: variable"))

(defun yacas-fn-object ()
  (interactive)
  (insert "Object()")
  (forward-char -1)
  (message "Args: predicate, object"))

(defun yacas-fn-clear ()
  (interactive)
  (insert "Clear()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-macroclear ()
  (interactive)
  (insert "MacroClear()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-set ()
  (interactive)
  (insert "Set()")
  (forward-char -1)
  (message "Args: variable, value"))

(defun yacas-fn-macroset ()
  (interactive)
  (insert "MacroSet()")
  (forward-char -1)
  (message "Args: variable, value"))

(defun yacas-fn-canprove ()
  (interactive)
  (insert "CanProve()")
  (forward-char -1)
  (message "Args: proposition"))

(defun yacas-fn-isconstant ()
  (interactive)
  (insert "IsConstant()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isboolean ()
  (interactive)
  (insert "IsBoolean()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isbound ()
  (interactive)
  (insert "IsBound()")
  (forward-char -1)
  (message "Args: variable"))

(defun yacas-fn-isstring ()
  (interactive)
  (insert "IsString()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isatom ()
  (interactive)
  (insert "IsAtom()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isfunction ()
  (interactive)
  (insert "IsFunction()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isnonobject ()
  (interactive)
  (insert "IsNonObject()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-isfreeof ()
  (interactive)
  (insert "IsFreeOf()")
  (forward-char -1)
  (message "Args: expression, variable or list of variables"))

(defun yacas-fn-withvalue ()
  (interactive)
  (insert "WithValue()")
  (forward-char -1)
  (message "Args: variable, value, expression"))

(defun yacas-fn-subst ()
  (interactive)
  (insert "Subst()")
  (forward-char -1)
  (message "Args: from, to"))

(defun yacas-fn-patchstring ()
  (interactive)
  (insert "PatchString()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-systemcall ()
  (interactive)
  (insert "SystemCall()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-makevector ()
  (interactive)
  (insert "MakeVector()")
  (forward-char -1)
  (message "Args: variable, n"))

(defun yacas-fn-randomintegervector ()
  (interactive)
  (insert "RandomIntegerVector()")
  (forward-char -1)
  (message "Args: nr, from, to"))

(defun yacas-fn-map ()
  (interactive)
  (insert "Map()")
  (forward-char -1)
  (message "Args: \"operator\", list of lists"))

(defun yacas-fn-mapsingle ()
  (interactive)
  (insert "MapSingle()")
  (forward-char -1)
  (message "Args: \"operator\", list"))

(defun yacas-fn-nrargs ()
  (interactive)
  (insert "NrArgs()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-type ()
  (interactive)
  (insert "Type()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-permutations ()
  (interactive)
  (insert "Permutations()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-levicivita ()
  (interactive)
  (insert "LeviCivita()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-varlist ()
  (interactive)
  (insert "VarList()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-random ()
  (interactive)
  (insert "Random()"))

(defun yacas-fn-rationalize ()
  (interactive)
  (insert "Rationalize()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-sum ()
  (interactive)
  (insert "Sum()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (if (string= var "")
	(message "Args: list")
      (insert var ", ")
      (setq var (read-string "From: "))
      (insert var ", ")
      (setq var (read-string "To: "))
      (insert var ", "))))

(defun yacas-fn-pslq ()
  (interactive)
  (insert "Pslq()")
  (forward-char -1)
  (message "Args: xlist, precision"))

(defun yacas-fn-help ()
  (interactive)
  (insert "Help()")
  (forward-char -1)
  (message "Opt arg: String containing the name of a function"))

(defun yacas-fn-prettyprinter ()
  (interactive)
  (insert "PrettyPrinter()")
  (forward-char -1)
  (message "Opt args: \"PrettyForm\" turns on pretty printing."))

(defun yacas-fn-defaultdirectory ()
  (interactive)
  (insert "DefaultDirectory()")
  (forward-char -1)
  (message "Args: Path to where yacas script files reside"))

(defun yacas-fn-n ()
  (interactive)
  (insert "N()")
  (forward-char -1)
  (message "Args: expression ;Opt args: precision"))


(defun yacas-fn-bigoh ()
  (interactive)
  (insert "BigOh()")
  (forward-char -1)
  (message "Args: polynomial, variable, degree"))

(defun yacas-fn-monic ()
  (interactive)
  (insert "Monic()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-leadingcoef ()
  (interactive)
  (insert "LeadingCoef()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-randompoly ()
  (interactive)
  (insert "RandomPoly()")
  (forward-char -1)
  (message "Args: variable, degree, coefmin, coefmax"))

(defun yacas-fn-primitivepart ()
  (interactive)
  (insert "PrimitivePart()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-content ()
  (interactive)
  (insert "Content()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-coef ()
  (interactive)
  (insert "Coef()")
  (forward-char -1)
  (message "Args: expression, variable, order"))

(defun yacas-fn-degree ()
  (interactive)
  (insert "Degree()")
  (forward-char -1)
  (message "Args: expression ;Opt args: variable"))

(defun yacas-fn-expand ()
  (interactive)
  (insert "Expand()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-lagrangeinterpolant ()
  (interactive)
  (insert "LagrangeInterpolant()")
  (forward-char -1)
  (message "Args: xlist, ylist, variable"))


(defun yacas-fn-getprecision ()
  (interactive)
  (insert "GetPrecision()"))

(defun yacas-fn-precision ()
  (interactive)
  (insert "Precision()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-historysize ()
  (interactive)
  (insert "HistorySize()")
  (forward-char -1)
  (message "Args: The number of lines to store in history file"))


(defun yacas-fn-trigsimpcombine ()
  (interactive)
  (insert "TrigSimpCombine()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-eliminate ()
  (interactive)
  (insert "Eliminate()")
  (forward-char -1)
  (message "Args: var, replace, function"))

(defun yacas-fn-radsimp ()
  (interactive)
  (insert "RadSimp()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-simplify ()
  (interactive)
  (insert "Simplify()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-suchthat ()
  (interactive)
  (insert "SuchThat()")
  (forward-char -1)
  (message "Args: expression, variable"))

(defun yacas-fn-psolve ()
  (interactive)
  (insert "PSolve()")
  (forward-char -1)
  (message "Args: expression, variable"))

(defun yacas-fn-solve ()
  (interactive)
  (insert "Solve()")
  (forward-char -1)
  (message "Args: equation, variable"))


(defun yacas-fn-sethelpbrowser ()
  (interactive)
  (insert "SetHelpBrowser()")
  (forward-char -1)
  (message "Args: helpbrowser"))


(defun yacas-fn-sign ()
  (interactive)
  (insert "Sign()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-abs ()
  (interactive)
  (insert "Abs()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-sqrt ()
  (interactive)
  (insert "Sqrt()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-ln ()
  (interactive)
  (insert "Ln()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-exp ()
  (interactive)
  (insert "Exp()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-arctan ()
  (interactive)
  (insert "ArcTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-arccos ()
  (interactive)
  (insert "ArcCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-arcsin ()
  (interactive)
  (insert "ArcSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-tan ()
  (interactive)
  (insert "Tan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-cos ()
  (interactive)
  (insert "Cos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-sin ()
  (interactive)
  (insert "Sin()")
  (forward-char -1)
  (message "Args: x"))


(defconst yacas-shell-menu-arithmetic
  (list
   "Arith"
   ["Average" yacas-fn-average]
   ["Bin" yacas-fn-bin]
   ["Ceil" yacas-fn-ceil]
   ["Complex" yacas-fn-complex]
   ["Conjugate" yacas-fn-conjugate]
   ["ContFrac" yacas-fn-contfrac]
   ["Decimal" yacas-fn-decimal]
   ["Denom" yacas-fn-denom]
   ["Div" yacas-fn-div]
   ["Factor" yacas-fn-factor]
   ["Factorize" yacas-fn-factorize]
   ["Factors" yacas-fn-factors]
   ["Fibonacci" yacas-fn-fibonacci]
   ["Floor" yacas-fn-floor]
   ["FromBase" yacas-fn-frombase]
   ["Gcd" yacas-fn-gcd]
   ["Im" yacas-fn-im]
   ["Lcm" yacas-fn-lcm]
   ["Max" yacas-fn-max]
   ["Min" yacas-fn-min]
   ["Mod" yacas-fn-mod]
   ["Numer" yacas-fn-numer]
   ["PAdicExpand" yacas-fn-padicexpand]
   ["Pi" yacas-fn-pi]
   ["Re" yacas-fn-re]
   ["Round" yacas-fn-round]
   ["Sum" yacas-fn-sum]
   ["ToBase" yacas-fn-tobase]
   (list
    "Predicates"
    ["IsEven" yacas-fn-iseven]
    ["IsInfinity" yacas-fn-isinfinity]
    ["IsInteger" yacas-fn-isinteger]
    ["IsNegativeInteger" yacas-fn-isnegativeinteger]
    ["IsNegativeNumber" yacas-fn-isnegativenumber]
    ["IsNonZeroInteger" yacas-fn-isnonzerointeger]
    ["IsNotZero" yacas-fn-isnotzero]
    ["IsNumber" yacas-fn-isnumber]
    ["IsOdd" yacas-fn-isodd]
    ["IsPositiveInteger" yacas-fn-ispositiveinteger]
    ["IsPositiveNumber" yacas-fn-ispositivenumber]
    ["IsPrime" yacas-fn-isprime]
    ["IsPrimePower" yacas-fn-isprimepower]
    ["IsRational" yacas-fn-isrational]
    ["IsZero" yacas-fn-iszero])))

(defconst yacas-shell-menu-variables
  (list
   "Variables"
   ["Clear" yacas-fn-clear]
   ["LazyGlobal" yacas-fn-lazyglobal]
   ["Local" yacas-fn-local]
   ["MacroClear" yacas-fn-macroclear]
   ["MacroLocal" yacas-fn-macrolocal]
   ["MacroSet" yacas-fn-macroset]
   ["Set" yacas-fn-set]))

(defconst yacas-shell-menu-debug
  (list
   "Debug"
   ["TraceExp" yacas-fn-traceexp]
   ["TraceRule" yacas-fn-tracerule]
   ["TraceStack" yacas-fn-tracestack]))

(defconst yacas-shell-menu-calculus
  (list
   "Calc"
   ["AntiDeriv" yacas-fn-antideriv]
   ["Curl" yacas-fn-curl]
   ["D" yacas-fn-d]
   ["Diverge" yacas-fn-diverge]
   ["Integrate" yacas-fn-integrate]
   ["InverseTaylor" yacas-fn-inversetaylor]
   ["Limit" yacas-fn-limit]
   ["Newton" yacas-fn-newton]
   ["ReversePoly" yacas-fn-reversepoly]
   ["Taylor" yacas-fn-taylor]))

(defconst yacas-shell-menu-control
  (list
   "Control"
   ["Apply" yacas-fn-apply]
   ["Else"  yacas-else]
   ["Eval" yacas-fn-eval]
   ["For"  yacas-for]
   ["ForEach"  yacas-foreach]
   ["Function" yacas-function]
   ["Hold" yacas-fn-hold]
   ["If"  yacas-if]
   ["LocalSymbols" yacas-fn-localsymbols]
   ["New local variable"  yacas-local]
   ["Procedure"  yacas-proc]
   ["Prog" yacas-fn-prog]
   ["Secure" yacas-fn-secure]
   ["Until"  yacas-until]
   ["While"  yacas-while]))

(defconst yacas-shell-menu-fast
  (list
   "Fast..."
   ["FastArcCos" yacas-fn-fastarccos]
   ["FastArcSin" yacas-fn-fastarcsin]
   ["FastArcTan" yacas-fn-fastarctan]
   ["FastCos" yacas-fn-fastcos]
   ["FastExp" yacas-fn-fastexp]
   ["FastLog" yacas-fn-fastlog]
   ["FastPower" yacas-fn-fastpower]
   ["FastSin" yacas-fn-fastsin]
   ["FastTan" yacas-fn-fasttan]))

(defconst yacas-shell-menu-io
  (list
   "I/O"
   ["DefLoad" yacas-fn-defload]
   ["Echo" yacas-fn-echo]
   ["FindFile" yacas-fn-findfile]
   ["FromFile" yacas-fn-fromfile]
   ["FromString" yacas-fn-fromstring]
   ["FullForm" yacas-fn-fullform]
   ["LispRead" yacas-fn-lispread]
   ["Load" yacas-fn-load]
   ["NewLine" yacas-fn-newline]
   ["PatchLoad" yacas-fn-patchload]
   ["PrettyForm" yacas-fn-prettyform]
   ["Read" yacas-fn-read]
   ["ReadToken" yacas-fn-readtoken]
   ["Space" yacas-fn-space]
   ["ToFile" yacas-fn-tofile]
   ["ToString" yacas-fn-tostring]
   ["Use" yacas-fn-use]
   ["Write" yacas-fn-write]
   ["WriteString" yacas-fn-writestring]))

(defconst yacas-shell-menu-linalg
  (list
   "LinAlg"
   ["BaseVector" yacas-fn-basevector]
   ["CharacteristicEquation" yacas-fn-characteristicequation]
   ["CoFactor" yacas-fn-cofactor]
   ["Commutator" yacas-fn-commutator]
   ["CrossProduct" yacas-fn-crossproduct]
   ["Determinant" yacas-fn-determinant]
   ["DiagonalMatrix" yacas-fn-diagonalmatrix]
   ["EigenValues" yacas-fn-eigenvalues]
   ["EigenVectors" yacas-fn-eigenvectors]
   ["Identity" yacas-fn-identity]
   ["InProduct" yacas-fn-inproduct]
   ["Inverse" yacas-fn-inverse]
   ["Minor" yacas-fn-minor]
   ["Normalize" yacas-fn-normalize]
   ["SolveMatrix" yacas-fn-solvematrix]
   ["Trace" yacas-fn-trace]
   ["Tranpose" yacas-fn-tranpose]
   ["ZeroMatrix" yacas-fn-zeromatrix]
   ["ZeroVector" yacas-fn-zerovector]
   (list
    "Predicates"
    ["IsHermitian" yacas-fn-ishermitian]
    ["IsMatrix" yacas-fn-ismatrix]
    ["IsUnitary" yacas-fn-isunitary]
    ["IsZeroVector" yacas-fn-iszerovector])))

(defconst yacas-shell-menu-arrays
  (list
   "Arrays"
   ["ArrayCreate" yacas-fn-arraycreate]
   ["ArrayCreateFromList" yacas-fn-arraycreatefromlist]
   ["ArrayGet" yacas-fn-arrayget]
   ["ArraySet" yacas-fn-arrayset]
   ["ArraySize" yacas-fn-arraysize]))

(defconst yacas-shell-menu-sets
  (list
   "Sets"
   ["Difference" yacas-fn-difference]
   ["Intersection" yacas-fn-intersection]   
   ["Union" yacas-fn-union]))


(defconst yacas-shell-menu-lists
  (list
   "Lists"
   yacas-shell-menu-arrays
   yacas-shell-menu-sets
   ["Append" yacas-fn-append]
   ["Assoc" yacas-fn-assoc]
   ["AssocIndices" yacas-fn-associndices]
   ["BubbleSort" yacas-fn-bubblesort]
   ["Concat" yacas-fn-concat]
   ["Contains" yacas-fn-contains]
   ["Count" yacas-fn-count]
   ["Delete" yacas-fn-delete]
   ["DestructiveAppend" yacas-fn-destructiveappend]
   ["DestructiveDelete" yacas-fn-destructivedelete]
   ["DestructiveInsert" yacas-fn-destructiveinsert]
   ["DestructiveReplace" yacas-fn-destructivereplace]
   ["DestructiveReverse" yacas-fn-destructivereverse]
   ["Drop" yacas-fn-drop]
   ["FillList" yacas-fn-filllist]
   ["Find" yacas-fn-find]
   ["FlatCopy" yacas-fn-flatcopy]
   ["Flatten" yacas-fn-flatten]
   ["Head" yacas-fn-head]
   ["Insert" yacas-fn-insert]
   ["Length" yacas-fn-length]
   ["List" yacas-fn-list]
   ["ListFromArray" yacas-fn-listfromarray]
   ["Listify" yacas-fn-listify]
   ["Nth" yacas-fn-nth]
   ["Partition" yacas-fn-partition]
   ["Pop" yacas-fn-pop]
   ["PopBack" yacas-fn-popback]
   ["PopFront" yacas-fn-popfront]
   ["Push" yacas-fn-push]
   ["RemoveDuplicates" yacas-fn-removeduplicates]
   ["Replace" yacas-fn-replace]
   ["Select" yacas-fn-select]
   ["Swap" yacas-fn-swap]
   ["Table" yacas-fn-table]
   ["TableForm" yacas-fn-tableform]
   ["Tail" yacas-fn-tail]
   ["Take" yacas-fn-take]
   ["UnList" yacas-fn-unlist]
   ["Unflatten" yacas-fn-unflatten]
   (list
    "Predicates"
    ["IsList" yacas-fn-islist])))

(defconst yacas-shell-menu-math
  (list
   "Math..."
   ["MathAbs" yacas-fn-mathabs]
   ["MathAdd" yacas-fn-mathadd]
   ["MathAnd" yacas-fn-mathand]
   ["MathArcCos" yacas-fn-matharccos]
   ["MathArcSin" yacas-fn-matharcsin]
   ["MathArcTan" yacas-fn-matharctan]
   ["MathCeil" yacas-fn-mathceil]
   ["MathCos" yacas-fn-mathcos]
   ["MathDiv" yacas-fn-mathdiv]
   ["MathDivide" yacas-fn-mathdivide]
   ["MathExp" yacas-fn-mathexp]
   ["MathFloor" yacas-fn-mathfloor]
   ["MathGcd" yacas-fn-mathgcd]
   ["MathLog" yacas-fn-mathlog]
   ["MathMod" yacas-fn-mathmod]
   ["MathMultiply" yacas-fn-mathmultiply]
   ["MathNot" yacas-fn-mathnot]
   ["MathOr" yacas-fn-mathor]
   ["MathPower" yacas-fn-mathpower]
   ["MathSin" yacas-fn-mathsin]
   ["MathSqrt" yacas-fn-mathsqrt]
   ["MathSubtract" yacas-fn-mathsubtract]
   ["MathTan" yacas-fn-mathtan]))

(defconst yacas-shell-menu-misc
  (list
   "Misc"
   ["Atom" yacas-fn-atom]
   ["BitAnd" yacas-fn-bitand]
   ["BitOr" yacas-fn-bitor]
   ["BitXor" yacas-fn-bitxor]
   ["Bodied" yacas-fn-bodied]
   ["Check" yacas-fn-check]
   ["ConcatStrings" yacas-fn-concatstrings]
   ["Equals" yacas-fn-equals]
   ["GenericTypeName" yacas-fn-generictypename]
   ["GreaterThan" yacas-fn-greaterthan]
   ["Help" yacas-fn-help]
   ["HoldArg" yacas-fn-holdarg]
   ["Infix" yacas-fn-infix]
   ["LeftPrecedence" yacas-fn-leftprecedence]
   ["LessThan" yacas-fn-lessthan]
   ["LeviCivita" yacas-fn-levicivita]
   ["MacroRuleBase" yacas-fn-Macrorulebase]
   ["MakeVector" yacas-fn-makevector]
   ["Map" yacas-fn-map]
   ["MapSingle" yacas-fn-mapsingle]
   ["N" yacas-fn-n]
   ["NrArgs" yacas-fn-nrargs]
   ["Object" yacas-fn-object]
   ["OpPrecedence" yacas-fn-opprecedence]
   ["PatchString" yacas-fn-patchstring]
   ["Permutations" yacas-fn-permutations]
   ["Postfix" yacas-fn-postfix]
   ["Prefix" yacas-fn-prefix]
   ["Pslq" yacas-fn-pslq]
   ["Random" yacas-fn-random]
   ["RandomIntegerVector" yacas-fn-randomintegervector]
   ["RightAssociative" yacas-fn-rightassociative]
   ["RightPrecedence" yacas-fn-rightprecedence]
   ["RuleBase" yacas-fn-rulebase]
   ["ShiftLeft" yacas-fn-shiftleft]
   ["ShiftRight" yacas-fn-shiftright]
   ["String" yacas-fn-string]
   ["Subst" yacas-fn-subst]
   ["SystemCall" yacas-fn-systemcall]
   ["Retract" yacas-fn-tryretract]
   ["Type" yacas-fn-type]
   ["UnFence" yacas-fn-unfence]
   ["VarList" yacas-fn-varlist]
   ["WithValue" yacas-fn-withvalue]
   (list
    "Predicates"
    ["CanProve" yacas-fn-canprove]
    ["IsAtom" yacas-fn-isatom]
    ["IsBoolean" yacas-fn-isboolean]
    ["IsBound" yacas-fn-isbound]
    ["IsConstant" yacas-fn-isconstant]
    ["IsFreeOf" yacas-fn-isfreeof]
    ["IsFunction" yacas-fn-isfunction]
    ["IsGeneric" yacas-fn-isgeneric]
    ["IsInfix" yacas-fn-isinfix]
    ["IsNonObject" yacas-fn-isnonobject]
    ["IsPostfix" yacas-fn-ispostfix]
    ["IsPrefix" yacas-fn-isprefix]
    ["IsString" yacas-fn-isstring])))

(defconst yacas-shell-menu-poly
  (list
   "Poly"
   ["BigOh" yacas-fn-bigoh]
   ["Coef" yacas-fn-coef]
   ["Content" yacas-fn-content]
   ["Degree" yacas-fn-degree]
   ["Div" yacas-fn-div]
   ["Expand" yacas-fn-expand]
   ["Factor" yacas-fn-factor]
   ["Factors" yacas-fn-factors]
   ["Gcd" yacas-fn-gcd]
   ["LagrangeInterpolant" yacas-fn-lagrangeinterpolant]
   ["Lcm" yacas-fn-lcm]
   ["LeadingCoef" yacas-fn-leadingcoef]
   ["Mod" yacas-fn-mod]
   ["Monic" yacas-fn-monic]
   ["PrimitivePart" yacas-fn-primitivepart]
   ["RandomPoly" yacas-fn-randompoly]))
   
(defconst yacas-shell-menu-config
  (list
   "Config"
   ["DefaultDirectory" yacas-fn-defaultdirectory]
   ["GetPrecision" yacas-fn-getprecision]
   ["HistorySize" yacas-fn-historysize]
   ["MaxEvalDepth" yacas-fn-maxevaldepth]
   ["Precision" yacas-fn-precision]
   ["PrettyPrinter" yacas-fn-prettyprinter]
   ["SetHelpBrowser" yacas-fn-sethelpbrowser]))

(defconst yacas-shell-menu-simplify
  (list
   "Simp"
   ["Eliminate" yacas-fn-eliminate]
   ["RadSimp" yacas-fn-radsimp]
   ["Rationalize" yacas-fn-rationalize]
   ["Simplify" yacas-fn-simplify]
   ["SuchThat" yacas-fn-suchthat]
   ["TrigSimpCombine" yacas-fn-trigsimpcombine]))

(defconst yacas-shell-menu-solve
  (list
   "Solve"
   ["PSolve" yacas-fn-psolve]
   ["Solve" yacas-fn-solve]))

(defun yacas-shell-quit ()
  (interactive)
  (if (yes-or-no-p "Really quit? ")
      (yacas-kill-job)))

(defconst yacas-shell-menu-yacas
  (list
   "Yacas>>"
   ["Quit Yacas" yacas-shell-quit]
   ["Toggle Yacas/Emacs menus" yacas-shell-menu-toggle-menubar]))

(defconst yacas-shell-menu-quit
  (list
   "quit"
   ["Quit Yacas" yacas-shell-quit]))

(defun yacas-shell-help ()
  (interactive)
  (info "emacs"))

(defconst yacas-shell-menu-help
  (list
   "Help"
   ["Yacas help" yacas-help]
   ["Yacas shell help" yacas-shell-help]))

(defconst yacas-shell-menu-functions
  (list
   "Functions"
   ["Abs" yacas-fn-abs]
   ["ArcCos" yacas-fn-arccos]
   ["ArcSin" yacas-fn-arcsin]
   ["ArcTan" yacas-fn-arctan]
   ["Cos" yacas-fn-cos]
   ["Exp" yacas-fn-exp]
   ["Ln" yacas-fn-ln]
   ["Sign" yacas-fn-sign]
   ["Sin" yacas-fn-sin]
   ["Sqrt" yacas-fn-sqrt]
   ["Tan" yacas-fn-tan]))

(defconst yacas-shell-menu-others
  (list
   "Others"
   yacas-shell-menu-fast
   yacas-shell-menu-math
   yacas-shell-menu-io
   yacas-shell-menu-misc))

(defconst yacas-shell-menu-prog
  (list
   "Prog"
   yacas-shell-menu-control
   yacas-shell-menu-variables
   yacas-shell-menu-debug))

(defvar yacas-shell-menu-yacas-menu nil)
(defvar yacas-shell-menu-quit-menu nil)
(defvar yacas-shell-menu-arithmetic-menu nil)
(defvar yacas-shell-menu-functions-menu nil)
(defvar yacas-shell-menu-linalg-menu nil)
(defvar yacas-shell-menu-calculus-menu nil)
(defvar yacas-shell-menu-simplify-menu nil)
(defvar yacas-shell-menu-solve-menu nil)
(defvar yacas-shell-menu-poly-menu nil)
(defvar yacas-shell-menu-lists-menu nil)
(defvar yacas-shell-menu-config-menu nil)
(defvar yacas-shell-menu-prog-menu nil)
(defvar yacas-shell-menu-others-menu nil)
(defvar yacas-shell-menu-help-menu nil)

(defvar yacas-shell-use-menus 
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

(defun yacas-shell-menu-global-menubar ()
  (if yacas-running-xemacs
      (default-value 'default-menubar)
    (lookup-key (current-global-map) [menu-bar])))

(defvar yacas-shell-mode-menu-map nil)

(defun yacas-shell-menu-initialize-yacas-shell-mode-menu-map ()
  (if (null yacas-shell-mode-menu-map)
      (let ((map (make-sparse-keymap))
	    (dummy (make-sparse-keymap)))
	(require 'easymenu)
	;; initialize all the yacas-shell-menu-*-menu variables
	;; with the menus.
	(easy-menu-define yacas-shell-menu-yacas-menu (list dummy) nil
			  yacas-shell-menu-yacas)
	(easy-menu-define yacas-shell-menu-quit-menu (list dummy) nil
			  yacas-shell-menu-quit)
	(easy-menu-define yacas-shell-menu-arithmetic-menu (list dummy) nil
			  yacas-shell-menu-arithmetic)
	(easy-menu-define yacas-shell-menu-functions-menu (list dummy) nil
			  yacas-shell-menu-functions)
	(easy-menu-define yacas-shell-menu-linalg-menu (list dummy) nil
			  yacas-shell-menu-linalg)
	(easy-menu-define yacas-shell-menu-calculus-menu (list dummy) nil
			  yacas-shell-menu-calculus)
	(easy-menu-define yacas-shell-menu-simplify-menu (list dummy) nil
			  yacas-shell-menu-simplify)
	(easy-menu-define yacas-shell-menu-solve-menu (list dummy) nil
			  yacas-shell-menu-solve)
	(easy-menu-define yacas-shell-menu-poly-menu (list dummy) nil
			  yacas-shell-menu-poly)
	(easy-menu-define yacas-shell-menu-lists-menu (list dummy) nil
			  yacas-shell-menu-lists)
	(easy-menu-define yacas-shell-menu-config-menu (list dummy) nil
			  yacas-shell-menu-config)
	(easy-menu-define yacas-shell-menu-prog-menu (list dummy) nil
			  yacas-shell-menu-prog)
	(easy-menu-define yacas-shell-menu-others-menu (list dummy) nil
			  yacas-shell-menu-others)
	(easy-menu-define yacas-shell-menu-help-menu (list dummy) nil
			  yacas-shell-menu-help)
	;; block the global menubar entries in the map so that YACAS
	;; can take over the menubar if necessary.
	(define-key map [rootmenu] (make-sparse-keymap))
	(define-key map [rootmenu yacas-shell] 
	        (cons "Yacas Shell" (make-sparse-keymap "Yacas Shell")))
	(define-key map [rootmenu yacas-shell file] 'undefined)	
	(define-key map [rootmenu yacas-shell files] 'undefined)	
	(define-key map [rootmenu yacas-shell edit] 'undefined)	
	(define-key map [rootmenu yacas-shell options] 'undefined)	
	(define-key map [rootmenu yacas-shell search] 'undefined)
	(define-key map [rootmenu yacas-shell buffer] 'undefined)
	(define-key map [rootmenu yacas-shell completion] 'undefined)	
	(define-key map [rootmenu yacas-shell signals] 'undefined)	
	(define-key map [rootmenu yacas-shell inout] 'undefined)	
	(define-key map [rootmenu yacas-shell mule] 'undefined)
	(define-key map [rootmenu yacas-shell tools] 'undefined)
	(define-key map [rootmenu yacas-shell help] 'undefined)
	(define-key map [rootmenu yacas-shell help-menu] 'undefined)
	;; now build YACAS's menu tree.
	(let ((menu-alist
	       '(
		 (arithmetic
		  (cons "Arith" yacas-shell-menu-arithmetic-menu))
		 (yacas
		    (cons "Yacas>>" yacas-shell-menu-yacas-menu))
		 (quit
		  (cons "Quit" yacas-shell-menu-quit-menu))
		 (functions
		  (cons "Functions" yacas-shell-menu-functions-menu))
		 (linalg
		  (cons "LinAlg" yacas-shell-menu-linalg-menu))
		 (calculus
		  (cons "Calc" yacas-shell-menu-calculus-menu))
		 (simplify
		  (cons "Simp" yacas-shell-menu-simplify-menu))
		 (solve
		  (cons "Solve" yacas-shell-menu-solve-menu))
		 (poly
		  (cons "Poly" yacas-shell-menu-poly-menu))
		 (lists
		  (cons "Lists" yacas-shell-menu-lists-menu))
		 (config
		  (cons "Config" yacas-shell-menu-config-menu))
		 (progm
		  (cons "Prog" yacas-shell-menu-prog-menu))
		 (others
		  (cons "Others" yacas-shell-menu-others-menu))
		 (help
		  (cons "Help" yacas-shell-menu-help-menu))))
	      cons
	      (vec (vector 'rootmenu 'yacas-shell nil))
	      ;; menus appear in the opposite order that we
	      ;; define-key them.
	      (menu-list 
	       (if (consp yacas-shell-use-menus)
		   (reverse yacas-shell-use-menus)
		 (list nil 'help 'config 'progm 'others 'lists 'poly 
		       'solve 'simplify 'calculus 'linalg 'functions 
		       'arithmetic 'quit))))
	  (while menu-list
	    (if (null (car menu-list))
		nil;; no flushright support in FSF Emacs
	      (aset vec 2 (intern (concat "yacas-shell-menu-"
					  (symbol-name
					   (car menu-list)) "-menu")))
	      (setq cons (assq (car menu-list) menu-alist))
	      (if cons
		  (define-key map vec (eval (car (cdr cons))))))
	    (setq menu-list (cdr menu-list))))
	(setq yacas-shell-mode-menu-map map)
	(run-hooks 'yacas-shell-menu-setup-hook))))

(defun yacas-shell-menu-make-xemacs-menubar ()
  (let ((menu-alist
	 '((yacas    . yacas-shell-menu-yacas)
	   (arithmetic  . yacas-shell-menu-arithmetic)
	   (functions    . yacas-shell-menu-functions)
	   (linalg     . yacas-shell-menu-linalg)
	   (calculus     . yacas-shell-menu-calculus)
	   (simplify     . yacas-shell-menu-simplify)
	   (solve     . yacas-shell-menu-solve)
	   (poly     . yacas-shell-menu-poly)
	   (lists     . yacas-shell-menu-lists)
	   (config     . yacas-shell-menu-config)
	   (progm     . yacas-shell-menu-prog)
	   (others     . yacas-shell-menu-others)
	   (help . yacas-shell-menu-help))
	 )
	cons
	(menubar nil)
	(menu-list yacas-shell-use-menus))
    (while menu-list
      (cond
       ((null (car menu-list))
	());(setq menubar (cons nil menubar)))
       (t (setq cons (assq (car menu-list) menu-alist))
	  (if cons
	      (setq menubar (cons (symbol-value (cdr cons)) menubar)))))
      (setq menu-list (cdr menu-list)))
    (nreverse menubar)))

(defun yacas-shell-menu-install-menubar ()
  (cond
   (yacas-running-xemacs
    (cond
     ((not (featurep 'menubar)) nil)	; No menus available
     (t
      (setq yacas-shell-menu-yacas-menubar 
	    (yacas-shell-menu-make-xemacs-menubar))
      (set-buffer-menubar yacas-shell-menu-yacas-menubar))))
   ((not (fboundp 'yacas-shell-menu-arithmetic-menu))
    (yacas-shell-menu-initialize-yacas-shell-mode-menu-map)
    (define-key yacas-shell-map [menu-bar]
      (lookup-key yacas-shell-mode-menu-map [rootmenu yacas-shell])))))

(defun yacas-shell-menu-install-menubar-item ()
  (cond
   (yacas-running-xemacs
    (if (not (featurep 'menubar))
	nil				; No menus available
      (set-buffer-menubar (copy-sequence (yacas-shell-menu-global-menubar)))
      (add-menu nil "Yacas" (cdr (yacas-shell-menu-make-xemacs-menubar)))))
   ((not (fboundp 'yacas-shell-menu-arithmetic-menu))
    (yacas-shell-menu-initialize-yacas-shell-mode-menu-map)
    (define-key yacas-shell-map [menu-bar]
      (lookup-key yacas-shell-mode-menu-map [rootmenu])))))

(defun yacas-shell-menu-install-menus ()
  (cond ((consp yacas-shell-use-menus)
	 (yacas-shell-menu-install-menubar))
	((eq yacas-shell-use-menus 1)
	 (yacas-shell-menu-install-menubar-item))
	(t nil)))

(defun yacas-shell-menu-set-menubar-dirty-flag ()
  (cond (yacas-running-xemacs
	 (set-menubar-dirty-flag))
	(t
	 (force-mode-line-update))))

(defun yacas-shell-menu-toggle-menubar ()
  (interactive)
  (cond
   (yacas-running-xemacs
    (if (null (car (find-menu-item current-menubar '("[Yacas>>]"))))
	(set-buffer-menubar yacas-shell-menu-yacas-menubar)
      (set-buffer-menubar (copy-sequence (yacas-shell-menu-global-menubar)))
      (condition-case ()
	  (add-menu-button nil ["[Yacas]" 
				yacas-shell-menu-toggle-menubar t] nil)
	(void-function
	 (add-menu-item nil "[Yacas]" 'yacas-shell-menu-toggle-menubar t))))
    (yacas-shell-menu-set-menubar-dirty-flag))
   (t
    (if (not (eq (lookup-key yacas-shell-map [menu-bar])
		 (lookup-key yacas-shell-mode-menu-map 
			     [rootmenu yacas-shell])))
	(define-key yacas-shell-map [menu-bar]
	  (lookup-key yacas-shell-mode-menu-map [rootmenu yacas-shell]))
      (define-key yacas-shell-map [menu-bar]
	(make-sparse-keymap))
      (define-key yacas-shell-map [menu-bar yacas-shell]
	(cons "[Yacas]" 'yacas-shell-menu-toggle-menubar)))
    (yacas-shell-menu-set-menubar-dirty-flag))))
