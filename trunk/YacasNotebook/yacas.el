;;                 Yacas editing support package
;; August 2000
;; Jay Belanger (belanger@truman.edu)
;; This is modified from mupad.el, which I wrote, stealing much from
;; Juergen Billing's (bij@mupad.de), Winfried Truemper (winni@xpilot.org)
;; Olivier Ramare (ramare@zeus.univ-lille1.fr) and Bruno Salvy 
;; (Bruno.Salvy@inria.fr) 
;; 
;; To use this, put this file in the emacs load-path and add
;; (autoload 'yacas-mode "yacas" "Yacas editing mode" t)
;; (autoload 'yacas "yacas" "Yacas interaction mode" t)
;; to your .emacs file, or load it in some other way, such as
;; M-x load-file RET path-to-file/yacas.el
;; You can also add
;; (setq auto-mode-alist (cons '("\\.yacas" . yacas-mode) auto-mode-alist))
;; to your .emacs file, to have every file ending in .yacas start 
;; automatically in yacas-mode, or to have the current buffer in yacas-mode,
;; M-x yacas-mode
;; From the mode information:
;; This is a mode intended to support program development in Yacas.
;; The following control constructs can be entered automatically.
;;
;;   C-c C-p  start a function (procedure)    C-c C-L Local
;;   C-c C-i  If                              C-c C-e Else
;;   C-c C-f  For                             C-c C-w While
;;   C-c C-F  ForEach                         C-c C-u Until
;; The local command is used to add new local variables in a procedure.
;; The if, while and until commands will prompt for a predicate.
;; The for command will prompt for start, predicate and increment,
;; the foreach command will prompt for an item and a list.
;;
;; C-c < and C-c > move backward and forward respectively to the next line
;; having the same (or lesser) level of indentation.
;; The variable yacas-indent controls the number of spaces for each 
;; indentation.
;; C-c C-c will comment out a region, and 
;; C-u C-c C-c  will un-comment it out.
;; C-c ] will indent a region to the level of the line before the region.
;; C-c # will add an inline comment.
;; Use C-c C-r to run Yacas on the current region under
;; a special subshell.  C-c C-b does the whole buffer, and
;; C-c C-l just does the current line.  C-c C-k can
;; be used to kill the yacas process.
;; The last output (after the Out> prompt) can be copied onto the kill-ring
;; with C-c C-o, and the last output, from one input prompt to another, 
;; can be copied onto the kill-ring with C-c C-O
;; In the Yacas subshell buffer, the command C-c C-n will move to the next 
;; prompt, and C-c C-p will move to the previous prompt.  C-up and C-down 
;; can be used to scroll through the input history.

(require 'font-lock)
(provide 'yacas)

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

;; First of all, some values that the user might wish to change

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

(setq yacas-prompt-width (string-width yacas-input-prompt-string))

(setq yacas-process nil)

(defvar yacas-send-filter-active nil
  "Status of Yacas process filter: t if enabled, else nil.")

(setq yacas-use-filter nil)

(setq yacas-prettyform nil)

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
  (set-syntax-table yacas-mode-syntax-table)
  )

(defvar yacas-mode-map nil
  "Keymap used in yacas-mode")

(defun yacas-add-keys (map)
  "Add Yacas specific key bindings to the keymap"
    (define-key map "\C-?" 'yacas-untab)
    (define-key map "\C-i" 'yacas-tab)
    ;; The control constructs
    (define-key map "\C-c\C-e" 'yacas-else)
    (define-key map "\C-c\C-f" 'yacas-for)
    (define-key map "\C-c\C-a" 'yacas-foreach)
    (define-key map "\C-c\C-i" 'yacas-if)
    (define-key map "\C-c\C-v" 'yacas-local)
    (define-key map "\C-c\C-m" 'yacas-modify)
    (define-key map "\C-c\C-p" 'yacas-proc)
    (define-key map "\C-c\C-t" 'yacas-title)
    (define-key map "\C-c\C-w" 'yacas-while)
    (define-key map "\C-c\C-u" 'yacas-until)
    (define-key map "\C-c\C-n" 'yacas-function)
    ;; Other formatting commands
    (define-key map "\C-c#" 'yacas-short-comment)
    (define-key map "\C-c]" 'yacas-indent-region)
    ;; Motion commands
    (define-key map "\C-c<" 'yacas-backward-to-same-indent)
    (define-key map "\C-c>" 'yacas-forward-to-same-indent)
    ;; The run Yacas commands
    (define-key map "\C-c\C-y" 'yacas-start-process)
    (define-key map "\C-c\C-r" 'yacas-region)
    (define-key map "\C-c\C-b" 'yacas-buffer)
    (define-key map "\C-c\C-l" 'yacas-line)
    (define-key map "\C-c\C-k" 'yacas-kill-job) ;; Also in yacas-shell
    (define-key map "\C-c\C-o" 'yacas-copy-last-output)
    (define-key map "\C-c\C-s" 'yacas-copy-complete-last-output)
    (define-key map "\C-c\C-t" 'yacas-recenter-output-buffer)
 )

(if yacas-mode-map ()
  (let ((map (make-sparse-keymap)))
    (yacas-add-keys map)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "\C-m" 'yacas-newline)
    (setq yacas-mode-map map)))

;; Font lock 
(defconst yacas-font-lock-keywords (purecopy
  (list
   ; Constants
   '("\\(EndOfFile\\|False\\|Infinity\\|Pi\\|True\\)"
     0 font-lock-constant-face nil)
   ; Control flow
   '("\\(F\\(or\\(Each\\)?\\|unction\\)\\|If\\|Local\\|Prog\\|Until\\|While\\)"
     0 font-lock-keyword-face nil)
   ; Operators
   '("\\(\\+\\+\\|--\\|\\.\\.\\|/@\\|:[ =]\\|<<\\|>>\\|[]!%(-+/@[{}^-]\\)"
     0 font-lock-keyword-face nil)
   ;Predicates
   '("\\(!=\\|<=\\|>=\\|And\\|Equals\\|GreaterThan\\|Is\\(Atom\\|Bo\\(olean\\|und\\)\\|Constant\\|Even\\|F\\(reeOf\\|unction\\)\\|Generic\\|In\\(fi\\(nity\\|x\\)\\|teger\\)\\|List\\|Matrix\\|N\\(egative\\(Integer\\|Number\\)\\|o\\(n\\(Object\\|ZeroInteger\\)\\|tZero\\)\\|umber\\)\\|Odd\\|P\\(os\\(itive\\(Integer\\|Number\\)\\|tfix\\)\\|r\\(efix\\|ime\\(Power\\)?\\)\\)\\|Rational\\|String\\|Zero\\(Vector\\)?\\)\\|LessThan\\|Math\\(And\\|Not\\|Or\\)\\|Not\\|Or\\|[<=>]\\)"
     0 font-lock-type-face nil)
   ; Functions
   '("\\(A\\(bs\\|pp\\(end\\|ly\\)\\|r\\(c\\(Cos\\|Sin\\|Tan\\)\\|ray\\(Create\\(FromList\\)?\\|Get\\|S\\(et\\|ize\\)\\)\\)\\|ssoc\\(Indices\\)?\\|tom\\|verage\\)\\|B\\(aseVector\\|i\\(gOh\\|n\\|t\\(And\\|Or\\|Xor\\)\\)\\|odied\\|ubbleSort\\)\\|C\\(anProve\\|h\\(aracteristicEquation\\|eck\\)\\|lear\\|o\\(Factor\\|ef\\|m\\(mutator\\|plex\\)\\|n\\(cat\\(Strings\\)?\\|jugate\\|t\\(Frac\\|ains\\|ent\\)\\)\\|s\\|unt\\)\\|rossProduct\\|url\\)\\|D\\(e\\(cimal\\|f\\(Load\\|aultDirectory\\)\\|gree\\|lete\\|nom\\|structive\\(Append\\|Delete\\|Insert\\|Re\\(place\\|verse\\)\\)\\|terminant\\)\\|i\\(agonalMatrix\\|fference\\|verge\\|[v]\\)\\|llLoad\\|rop\\)\\|E\\(cho\\|igenV\\(alues\\|ectors\\)\\|liminate\\|val\\|xp\\(and\\)?\\)\\|F\\(actor\\(ize\\|s\\)?\\|i\\(bonacci\\|llList\\|nd\\(File\\)?\\)\\|lat\\(Copy\\|ten\\)\\|rom\\(Base\\|File\\|String\\)\\|ullForm\\)\\|G\\(cd\\|e\\(nericTypeName\\|tPrecision\\)\\)\\|H\\(ead\\|old\\(Arg\\)?\\)\\|I\\(dentity\\|m\\|n\\(Product\\|fix\\|sert\\|te\\(grate\\|rsection\\)\\|verse\\(Taylor\\)?\\)\\)\\|L\\(a\\(grangeInterpolant\\|zyGlobal\\)\\|cm\\|e\\(adingCoef\\|ftPrecedence\\|ngth\\|viCivita\\)\\|i\\(mit\\|s\\(pRead\\|t\\(FromArray\\|ify\\)?\\)\\)\\|n\\|o\\(ad\\|calSymbols\\)\\)\\|M\\(a\\(cro\\(Clear\\|Local\\|Rule\\(Base\\)?\\|Set\\)\\|keVector\\|pSingle\\|th\\(A\\(bs\\|dd\\|rc\\(Cos\\|Sin\\|Tan\\)\\)\\|C\\(eil\\|os\\)\\|Div\\(ide\\)?\\|Exp\\|Floor\\|Gcd\\|Log\\|M\\(od\\(\\)?\\|ultiply\\)\\|Power\\|S\\(in\\|qrt\\|ubtract\\)\\|Tan\\)\\|xEvalDepth\\|[px]\\)\\|in\\(or\\)?\\|o\\(nic\\|[d]\\)\\)\\|N\\(ew\\(Line\\|ton\\)\\|ormalize\\|rArgs\\|th\\|umer\\)\\|O\\(bject\\|pPrecedence\\)\\|P\\(AdicExpand\\|Solve\\(\\)?\\|a\\(rtition\\|tch\\(Load\\|String\\)\\)\\|ermutations\\|o\\(p\\(Back\\|Front\\)?\\|stfix\\)\\|r\\(e\\(cision\\|fix\\|tty\\(Form\\|Printer\\)\\)\\|imitivePart\\)\\|ush\\)\\|R\\(a\\(ndom\\(IntegerVector\\|Poly\\)?\\|tionalize\\(\\)?\\)\\|e\\(ad\\(Token\\)?\\|moveDuplicates\\|place\\|versePoly\\)?\\|ight\\(Associative\\|Precedence\\)\\|ule\\(Base\\)?\\)\\|S\\(e\\(cure\\|lect\\|t\\(HelpBrowser\\)?\\)\\|hift\\(Left\\|Right\\)\\|i\\(mplify\\|n\\)\\|olve\\(Matrix\\)?\\|pace\\|qrt\\|t\\(ring\\|ubApiC\\(F\\(ile\\|unction\\)\\|Include\\|Remark\\|S\\(etEnv\\|hortIntegerConstant\\|t\\(art\\|ruct\\)\\)\\)\\)\\|u\\(bst\\|chThat\\|m\\)\\|wap\\|ystemCall\\)\\|T\\(a\\(ble\\(Form\\)?\\|il\\|ke\\|n\\|ylor\\)\\|o\\(Base\\|File\\|String\\)\\|r\\(a\\(ce\\(Exp\\|Rule\\|Stack\\)?\\|nspose\\)\\|igSimpCombine\\|uncRadian\\|yRetract\\)\\|ype\\)\\|U\\(n\\(F\\(ence\\|latten\\)\\|List\\|ion\\)\\|se\\(\\)?\\)\\|VarList\\|W\\(ithValue\\|rite\\(String\\)?\\)\\|Zero\\(Matrix\\|Vector\\)\\|[DN]\\)"
     0 font-lock-function-name-face nil)
   '("\\W\\\"\\([^\\\"]*\\)\\\"\\W" 
     1 font-lock-string-face t)
   '("/\\*\\(.*\\)\\*/" 
     0 font-lock-comment-face t)
   '("//.*$" 0 font-lock-comment-face t)
   "Additional expressions to highlight in Yacas mode.")))

(defun yacas-mode ()
  "This is a mode intended to support program development in Yacas.
The following control constructs can be entered automatically.

  C-c C-p  start a function (procedure)    C-c C-v Local
  C-c C-i  If                              C-c C-e Else
  C-c C-f  For                             C-c C-w While
  C-c C-a  ForEach                         C-c C-u Until

The local command is used to add new local variables in a procedure.
The if, while and until commands will prompt for a predicate.
The for command will prompt for start, predicate and increment,
the foreach command will prompt for an item and a list.

C-c < and C-c > move backward and forward respectively to the next line
having the same (or lesser) level of indentation.
The variable yacas-indent controls the number of spaces for each indentation.
C-c C-c will comment out a region, and 
C-u C-c C-c  will un-comment it out.
C-c ] will indent a region to the level of the line before the region.
C-c # will add an inline comment.

Use C-c C-r to run Yacas on the current region under
a special subshell.  C-c C-b does the whole buffer, and
C-c C-l just does the current line.  C-c C-k can
be used to kill the yacas process.
The last output (after the Out> prompt) can be copied onto the kill-ring
with C-c C-o, and the last output, from one input prompt to another, 
can be copied onto the kill-ring with C-c C-s

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
  (run-hooks 'yacas-mode-hooks))

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

(defun yacas-title ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
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
    (insert "Created: " (format-time-string "%A, %e %B %Y, %l:%M %p %Z") "\n")
    (insert "Last Update: " (format-time-string "%A, %e %B %Y, %l:%M %p %Z"))
    (insert "\n")
    (insert "++*/\n")
    (forward-line -7)))

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
		 (setq yacas-send-filter-active nil)))))
     )


;;; Invoking Yacas in an inferior shell.

(defun yacas ()
  "Run yacas in a buffer, without shell. Exiting yacas will kill the buffer.
The buffer is called *Yacas*. The buffer is put in shell-mode with
a yacas-syntax-table."
  (interactive)
  (if (get-buffer "*Yacas*")
      nil
    (yacas-start-shell))
  (switch-to-buffer "*Yacas*"))

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
    (define-key yacas-shell-map "\C-c\C-c" nil)
    (define-key yacas-shell-map "\C-c\C-c\C-k"    'yacas-kill-job) 
    (define-key yacas-shell-map "\C-c\C-c\C-t" 'yacas-recenter-output-buffer)
    (define-key yacas-shell-map "\C-a" 'comint-bol)
    (use-local-map yacas-shell-map)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add a menu to the menu-bar (and NOT to the usual menubar).

(require 'easymenu)

(easy-menu-define yacas-mode-menu yacas-mode-map "Yacas mode menu"
  '("Yacas"
    ("Control constructs"
     ["Insert procedure"  yacas-proc t]
     ["Insert new local variable"  yacas-local t]
     ["If"  yacas-if t]
     ["Else"  yacas-else t]
     ["For"  yacas-for t]
     ["While"  yacas-while t]
     ["ForEach"  yacas-foreach t]
     ["Short comment" yacas-short-comment])
    ("Run Yacas"
     ["Start a Yacas process"   yacas-start-process]
     ["Run Yacas on buffer"   yacas-buffer]
     ["Run Yacas on region"  yacas-region ]
     ["Run Yacas on line"  yacas-line]
     ["Recenter the output buffer"  yacas-recenter-output-buffer]
     ["Copy the last output"  yacas-copy-last-output]
     ["Copy the complete last output"  yacas-copy-complete-last-output]
     ["Kill Yacas process"  yacas-kill-job ]
     ["Toggle PrettyForm" yacas-toggle-prettyform ])
    ("Shortcuts"
     ["Comment region"  yacas-comment-region]
     ["Uncomment region"  yacas-uncomment-region ]
     ["Forward to same indent"  yacas-forward-to-same-indent]
     ["Backward to same indent"  yacas-backward-to-same-indent ]
     ["Indent region"  yacas-indent-region ]
     ["Tab"  yacas-tab ]
     ["Untab"  yacas-untab ]
     ["Redefine Tab Size"  yacas-tabsize ])
    ("Miscellaneous"
     ["Title"   yacas-title]
     ["Go back after title" yacas-goback-after-title]
     ["Modify"  yacas-modify ])))

