;; yacas-notebook.el  Mode for interaction with Yacas from TeX buffer
;;; Written 2/12/1991 by Dan Dill dan@chem.bu.edu
;;; Modified 2000-2002 by Jay Belanger
;;; Copyright (C) 1991, 1993 Dan Dill (dan@chem.bu.edu) 1999-2002 Jay Belanger
;;; (belanger@truman.edu)
;;; This is part of Yacas-Notebook
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Commentary
;;
;; `Yacas-Notebook' is a major mode for Emacs that allows the user to
;; write documents while interacting with _Yacas_.
;; yacas-mode is a mode intended to support program development in
;; _Yacas_ by inserting control constructs in the buffer, help in
;; formatting the code (for example, a <RET> will start a new line
;; indented the correct amount)(1), and interacting with a _Yacas_
;; process. 

;; INSTALLATION
;; ============
;; Yacas-Notebook:
;; The `Yacas-Notebook' package consists of the files `yacas.el',
;; `yacas-names.el', `yacas-functions.el', `yacas-notebook.el', 
;; and `yacas-notebook.sty'.
;; To install, place `yacas-notebook.el', `yacas.el', `yacas-names.el',
;; and `yacas-functions.el'  somewhere in the load path for Emacs.
;; If you want to run LaTeX on the resulting document, put
;; `yacas-notebook.sty' somewhere in the TeX inputs path.
;;    To make sure that `yacas-notebook.el' is loaded when necessary, the
;; line
;;      (autoload 'yacas-notebook-mode "yacas-notebook" "Yacas-Notebook" t)
;; can be inserted into your `.emacs' file.  Then typing `M-x
;; yacas-notebook-mode' will start `Yacas-Notebook' mode.  The command
;; `M-x ynb-mark-file-as-yacas-notebook' will put the line
;;      %-*-Yacas-Notebook-*-
;; at the beginning of the file, if it isn't there already, and will ensure
;; that the next time the file is opened, it will be in
;; `yacas-notebook-mode'.  This can be done automatically everytime a file
;; is put in `yacas-notebook-mode' by putting the line
;;      (add-hook 'yacas-notebook-mode-hook 'ynb-mark-file-as-yacas-notebook)
;; somewhere in your `.emacs' file.

;; DESCRIPTION
;; ===========
;; Yacas-Notebook:
;; This is a mode intended to allow the user to write documents that
;; include Yacas code.  The file can be LaTeXed to produce nice 
;; looking output (although that isn't necessary, of course), and so the
;; mode is an extension of TeX-mode (AucTeX, if you use that).
;; The units of Yacas code that are worked with are "cells", which are 
;; delimited by "\yacas" and "\endyacas". The cells can be evaluated 
;; individually, as a group, and the output can be returned as Yacas output 
;; or in TeX form.  Evaluating a cell and returning the output is called 
;; "updating" the cell.  This mode also supports some literate programming 
;; constructs.
;; The commands for working with cells are:
;;  C-c C-o  create a cell         
;;  C-c C-u a update all the cells 
;;  C-c C-u A update all the cells in TeX form 
;;  C-c +     go to the next cell 
;;  C-c -     go to the previous cell
;;  C-c C-u q evaluate all  initialization cells
;;  C-c C-u i update all the initialization cells
;;  C-c C-u I update all the initialization cells in TeX form

;; (With a prefix, C-u C-c C-u a and C-u C-c C-u A will update the cells 
;; without prompting)
;; Single lines can be evaluated:
;;  C-c C-u l replace the current line with Yacas output
;;  C-c C-u L replace the current line with Yacas output 
;; in TeX form.

;; Within a cell, the following commands are available:
;;  C-c C-d  delete the cell's output
;;  C-c C-u c  update a cell 
;;  C-c C-u C update a cell in TeX form
;;  C-c C-q toggle initialization cells
;;  C-c @  assemble a cell which defines a package
;;  C-u C-c @  assemble a cell with references

;; Finally, the command M-x ynb-mark-file-as-yacas-notebook will 
;; insert a 
;; %-*-Yacas-Notebook-*- at the beginning
;; of the file (if there isn't one there already) so the file will begin in
;; yacas-notebook-mode next time it's opened.


(require 'yacas)
(require 'font-lock)

(defgroup ynb nil
  "Yacas Notebook"
  :prefix "ynb-"
  :tag "Yacas Notebook")

(defcustom yacas-documentation-directory
  "/usr/share/yacas/documentation/"
  "Path where the yacas documentation is kept."
  :group 'yacas
  :type 'directory)
  
(defcustom ynb-use-tex 
  'auctex
  "Determines which TeX mode yacas-notebook should use.
Choices are 'auctex, 'tex and nil"
  :group 'ynb
  :type '(choice :menu-tag "TeX style"
                 :tag      "TeX style"
                 (const auctex)
                 (const tex) 
                 (const nil)))
  
(defcustom ynb-abbreviations-allowed nil
  "*If non-nil, then `...' abbreviations are allowed in cell labels 
and references. Note that enabling this options will slow cell and 
package assembly."
  :group 'ynb
  :type 'boolean)

(defcustom ynb-max-references 5
  "*Number of references in a cell below which cell references are fetched
as needed, scanning the entire document for each reference.  At or above this
number, all cells in a document for the given filename are precollated in a
single scan of the document."
  :group 'ynb
  :type 'integer)

(defvar ynb-temp-dir
  "/tmp/"
  "*Directory for temporary files.
Specify \"\" to use the directory of the Yacas-Notebook document buffer.")

(defvar ynb-output-marker "\\output"
  "*Contents of line separating input and output portion of cell.")

(defvar ynb-tex-string
  "TeXForm(%);")

(defvar ynb-dereference-path nil
  "List of buffers referenced in cell assembly.
Used by `ynb-dereference-buffer' to detect self-reference.")

(defvar ynb-error-point nil
  "Buffer position where error detected.")

(defvar ynb-buffer-alist nil
  "Alist of temporary buffers associate with cells `file:part'.
The buffers are used in package and cell assembly.")

(defvar ynb-source-buffer nil
  "Buffer from which ynb-collate-cells works.")

(defvar ynb-cell-output ""
  "The output from sending the cell.")

(defconst ynb-temp-suffix 0
  "Temporary filename suffix.  Incremented by 1 for each filename.")

(defun ynb-make-temp-name ()
  "Return a unique filename."
  (setq ynb-temp-suffix (+ ynb-temp-suffix 1))
  (concat (concat (make-temp-name "#mz") "-")
          ynb-temp-suffix
          ".mu"))

(defun ynb-mark-file-as-yacas-notebook ()
  "Mark the file as an Yacas-Notebook buffer.
The next time the file is loaded, it will then be in Yacas-Notebook mode"
  (interactive)
  (save-excursion
    (goto-line 1)
    (beginning-of-line)
    (if (looking-at ".*-\\*-Yacas-Notebook-\\*-")
	()
      (open-line 1)
      (insert "%-*-Yacas-Notebook-*-"))))

;;; @@ Initialization

(defun ynb-replace-assoc (alist key val)
  "Replace ALIST KEY VALuE, if KEY present, else add KEY VALUE.
Return modified alist."
  (if (assoc key alist)
      (setcdr (assoc key alist) val)
    (setcdr alist (cons (cons key val) (cdr alist))))
  alist)

(defun ynb-toggle-init ()
  "Toggle initialization marker of TYPE cell containing point."
  (interactive)
  (save-excursion
    (re-search-backward "^\\\\yacas")
    (goto-char (match-end 0))
    (if (looking-at "\\[\\* Initialization Cell \\*\\]")
        (delete-region (match-beginning 0) (match-end 0))
      (insert "[* Initialization Cell *]"))))

(defun ynb-package-part ()
  "Toggle initialization marker of TYPE cell containing point."
  (interactive)
  (save-excursion
    (let ((package (read-string "Package: " "...:")))
      (re-search-backward "^\\\\mupad")
      (goto-char (match-end 0))
      (insert (concat "<" package ">")))))

(defun ynb-update-all (arg)
  "Optionally update all cells.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (ynb-update nil nil nil)
    (ynb-update nil (y-or-n-p "Interactive update ? ") nil)))

(defun ynb-tex-update-all (arg)
  "Optionally update all cells and return output in TeX form.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (ynb-update nil nil t)
    (ynb-update nil (y-or-n-p "Interactive update ? ") t)))

(defun ynb-update-init (arg)
  "Optionally update all initialization cells.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (ynb-update "\\[\\* Initialization Cell \\*\\]" nil nil)
    (ynb-update "\\[\\* Initialization Cell \\*\\]" 
			   (y-or-n-p "Interactive update ? ") nil)))

(defun ynb-tex-update-init (arg)
  "Optionally update all initialization cells and return output in TeX form.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (ynb-update "\\[\\* Initialization Cell \\*\\]" nil t)
    (ynb-update "\\[\\* Initialization Cell \\*\\]" 
			   (y-or-n-p "Interactive update ? ") t)))

(defun ynb-create-cell ()
  "Insert cell in buffer."
  (interactive)
  (if (ynb-cell-p)
      (error "Cell already exists")
    (if (not (bolp))
        (progn
          (open-line 1)
          (forward-line 1)))
    (insert "\\yacas\n\n\\endyacas")
    (beginning-of-line)
    (previous-line 1)))

(defun ynb-forward-cell ()
  "Move to next cell."
  (interactive)
    (let ((cur-pos (point))
          (cell-pos (point-max))
          new-pos)
        (setq new-pos (ynb-next-cell-start))
        (if (not (equal new-pos cur-pos))
            (if (> new-pos cell-pos)
                nil
              (setq cell-pos new-pos)))
      (if (equal cell-pos (point-max))
          nil; No more cells
        (goto-char cell-pos))))

(defun ynb-backward-cell ()
  "Move to previous cell."
  (interactive)
    (let ((cur-pos (point))
          (cell-pos (point-min))
          new-pos)
        (setq new-pos (ynb-previous-cell-start))
        (if (not (equal new-pos cur-pos))
            (if (< new-pos cell-pos)
                nil
              (setq cell-pos new-pos)))
      (if (equal cell-pos (point-min))
          nil ; No more cells
        (goto-char cell-pos))))

(defun ynb-update (kind ask tex)
  "Optionally update all KIND cells.
If ASK is non-nil, then ask whether each KIND cell is to be updated,
else update each KIND cell.  If KIND is nil, update all cells.
If TEX is non-nil, then insert \\outputtex instead of \\output."
  (let (bypass display-start display-end cur-pos)
    (save-excursion
      (goto-char (point-min))
      (while (ynb-forward-cell)
        (forward-line -1)
        (if (not (looking-at (concat "^\\\\yacas" kind)))
            (progn
              (forward-line 1) ; Don't want the same cell next time
              nil) ; Wrong kind of cell
          ;; We have a cell of the right kind
          (setq display-start (point))
          (goto-char (ynb-cell-end))
          (forward-line 1) ; We need to include cell trailer in narrowed region
          (end-of-line)    ; ..
          (setq display-end (point))
          (forward-line 0)
          (unwind-protect
              (progn
                (narrow-to-region display-start display-end)
                (goto-char (point-min))
                (recenter 1) ; force display, just in case...
                (forward-line 1)
                (if (and ask (not (y-or-n-p "Update this cell? ")))
                    t
                  (ynb-update-type-cell tex)
))
            (widen) ; If user aborts evaluation at prompt
            ) ; unwind-protect
          ) ; if in a valid cell
        ) ; while still types to check
      ) ; save-excursion
    (widen)
;    (beep)
    (message "Update of cells finished")))

(defun ynb-eval-init ()
  "Evaluate all initialization cells, without returning the output."
  (interactive)
  (let (bypass display-start display-end cur-pos)
    (save-excursion
      (goto-char (point-min))
      (while (ynb-forward-cell)
        (forward-line -1)
        (if (not (looking-at "^\\\\yacas\\[\\* Initialization Cell \\*\\]"))
            (progn
              (forward-line 1) ; Don't want the same cell next time
              nil) ; Wrong kind of cell
          ;; We have a cell of the right kind
          (setq display-start (point))
          (goto-char (ynb-cell-end))
          (forward-line 1) ; We need to include cell trailer in narrowed region
          (end-of-line)    ; ..
          (setq display-end (point))
          (forward-line 0)
          (unwind-protect
              (progn
                (narrow-to-region display-start display-end)
                (goto-char (point-min))
                (recenter 1) ; force display, just in case...
                (forward-line 1)
                (ynb-send-cell))
            (widen) ; If user aborts evaluation at prompt
            ) ; unwind-protect
          ) ; if in a valid cell
        ) ; while still types to check
      ) ; save-excursion
    (widen)
;    (beep)
    (message "Evaluation of initialization cells finished")))

(defun ynb-cell-start ()
  "Return position of start of cell containing point."
  (let ((begin-re "^\\\\yacas"))
  (save-excursion
    (if (not (looking-at begin-re))
        (re-search-backward begin-re))
    (forward-line 1)
    (point))))

(defun ynb-cell-end ()
  "Return position of end of cell containing point."
  (let ((end-re "^\\\\endyacas"))
    (save-excursion
      (re-search-forward end-re)
      (forward-line -1)
      (end-of-line)
      (point))))

(defun ynb-previous-cell-start ()
  "Get start of preceding cell.  If none, return current position."
  (let ((cur-pos (point))
        (start nil)
        (begin-re "^\\\\yacas")
        (end-re "^\\\\endyacas"))
    (save-excursion
      (if (not (re-search-backward end-re (point-min) t))
          cur-pos
        (if (ynb-cell-p)
            (progn
              (re-search-backward begin-re)
              (forward-line 1)
              (point))
          cur-pos)))))
              
(defun ynb-next-cell-start ()
  "Get start of next cell.  If none, return current position."
  (let ((cur-pos (point))
        (start nil)
        (begin-re "^\\\\yacas")
        (end-re "^\\\\endyacas"))
    (save-excursion
      (if (re-search-forward begin-re (point-max) t)
          (progn
            (if (not (ynb-cell-p))
                cur-pos)
            (forward-line 1)
            (point))
        cur-pos))))

(defun ynb-cell-p ()
  "Returns t if point is in a Yacas-Notebook cell, else returns nil."
  (let ((begin-re "^\\\\yacas")
        (end-re "^\\\\endyacas")
        found)
    (catch 'done
      (save-excursion
        (if (re-search-backward begin-re (point-min) t)
            (setq found (point))
          (throw 'done nil))) ; No \yacas
      (save-excursion
        (if (re-search-backward end-re found t)
            (throw 'done nil))) ; Intervening \end{...}
      (save-excursion
        (if (re-search-forward end-re (point-max) t)
            (setq found (point))
          (throw 'done nil))) ; No \endyacas
      (save-excursion
        (if (re-search-forward begin-re found t)
            (throw 'done nil) ; Intervening \begin{...}
          (throw 'done t)))))) ; In a cell
        
(defun ynb-newline ()
  "yacas-newline if in a cell, otherwise newline"
  (interactive)
  (if (ynb-cell-p)
      (yacas-newline)
    (newline)))

(defun ynb-delete-output ()
  "Delete current output (if any).  Assumes point in cell.
Output assumed to follow input, separated by a 
ynb-output-marker line.  Input *may* contain blank lines."
  (interactive)
  (let ((out-start (ynb-output-p)))
    (if out-start
        (delete-region out-start (ynb-cell-end))
      t)))

(defun ynb-output-p ()
  "Return start of output text if present, else return nil.  Assumes
point in cell.  Output assumed to follow input, separated by a
\output."
  (save-excursion
    (goto-char (ynb-cell-start))
    (if (re-search-forward "^\\\\output"
         (ynb-cell-end) t)
        (progn
          (forward-line -1)
          (end-of-line)
          (point))
      nil)))

;;; @@ Yacas-Notebook functions for package assembly

(defun ynb-assemble (arg)
 "Assemble package (see ynb-assemble-package), or, with C-u prefix,
assemble references within a cell (see ynb-assemble-cell)."
  (interactive "P")
  (if arg
      (ynb-assemble-cell)
    (ynb-assemble-package)))

(defun ynb-assemble-cell (&optional delete)
  "Assemble references in cell to file with unique name.  The buffer used to
write the file is not deleted, unless optional DELETE is non-nil.
Return the filename."

  ;; Here is how this function works:

  ;; The text of the cell is written to a buffer with key `file:part'.  Then
  ;; the number of references in the cell is counted.  If the number of
  ;; references in the cell is less than ynb-max-references, 
  ;; then the cell references are resolved by successive calls to 
  ;; ynb-dereference-buffer
  ;; which collates the text for cell references as needed, using
  ;; ynb-collate-cells.  If the number of references is equal to or
  ;; greater than ynb-max-references, then all cells in the document
  ;; correpsonding to the current cell type and filename are collated into
  ;; buffers, using ynb-collate-cells, and then the all cell 
  ;; references are are resolved by successive calls to 
  ;; ynb-dereference-buffer.

  ;; The global`ynb-buffer-alist' associates buffer names with keys.
  ;; Buffer names are unique.  The names of all buffers are constructed with
  ;; `ynb-make-temp-name' and are unique.  All buffers except 
  ;; possibly the cell-buffer are deleted on exit.

  (interactive)
  (let ((home-buffer (current-buffer))
        files parts file part 
        ref-count
        cell-key cell-buffer tmp-alist tmp-buffer)
    (if (not (ynb-cell-p)) (error "Not in a cell"))
    (if (not (ynb-reference-p)) 
	(error "Cell contains no references"))
    (save-excursion
      (goto-char (ynb-cell-start))
      (forward-line -1)
      (if (not (looking-at "^\\\\yacas.*<.*:.*>"))
          (error "Cell is not marked"))

      (setq ynb-error-point (point))
      (if ynb-abbreviations-allowed
          (unwind-protect ; In case filename errors
              ;; This can take some seconds
              (progn
                (message "Getting filenames...")
                (setq files (ynb-get-filenames))
                (message "")
                )
            (goto-char ynb-error-point)))

      (setq file (ynb-get-filename files))
      (if (not file) (error "Ambiguous filename"))

      (if ynb-abbreviations-allowed
          ;; This can take several seconds for a document with many cells
          (progn
            (message "Getting partnames")
            (setq parts (ynb-get-partnames file files))
            (message "")
            ))

      (setq part (ynb-get-partname parts))
      (if  (not part) (error "Ambiguous partname"))) ; save-excursion

    (setq cell-key (concat file ":"))
    (if (not (equal part "")) (setq cell-key (concat cell-key part)))
    (message "Assembling `%s' ..." cell-key) ; (sleep-for 1)
    (setq cell-buffer (ynb-make-temp-name))
    (setq ynb-buffer-alist (list (cons cell-key cell-buffer)))
    (unwind-protect
        (save-excursion
          (ynb-append-cell-to-buffer cell-buffer)
          (setq ynb-source-buffer (current-buffer)) 
                                                          ; Collate from here

          (if (< (ynb-reference-count cell-buffer) 
		 ynb-max-references)
              ;; Build reference buffers as needed
                (while 
		  (ynb-dereference-buffer cell-key files parts nil))
            ;; Prebuild all reference buffers
            (ynb-collate-cells file part files parts nil)
            (while 
              (ynb-dereference-buffer cell-key files parts nil nil))
            )
          (set-buffer cell-buffer)
          (write-file (concat ynb-temp-dir cell-buffer))
          (set-buffer home-buffer))
      ;; unwind-protect forms: deleted cell buffers
      (setq tmp-alist ynb-buffer-alist)
      (while (setq tmp-buffer (cdr (car tmp-alist)))
        (setq tmp-alist (cdr tmp-alist))
        (condition-case nil ; In case buffer not actually created
            (if (and (not delete) (equal tmp-buffer cell-buffer))
                nil ; Don't delete the assembly buffer
              (kill-buffer tmp-buffer))
          (error nil)))
      ) ; unwind-protect
    (message "`%s' assembled in file `%s%s'" 
	     cell-key ynb-temp-dir cell-buffer)
    (concat ynb-temp-dir cell-buffer))) ; done

(defun ynb-assemble-package (&optional file overwrite)
  "Assemble text into a package buffer and write that buffer to a file.
The buffer is *not* deleted.  Return the filename.

Optional arguments (useful for batch processing):

FILE package filename;
OVERWRITE, if not nil package filename buffer will be overwritten 
without asking."

  ;; Here is how this function works:

  ;; The entire buffer is scanned for marked cells matching TYPE and FILE and
  ;; these are collated by `file' and `part' into buffers with keys
  ;; `file:part' and, for `part' = "" (a package cell), into a buffer with key
  ;; `FILE'.

  ;; Once the cell buffers have been created, then all cell references in the
  ;; package buffer, with key `FILE', are replaced by the contents of the
  ;; corresponding buffers with keys `file:part', by successive calls to
  ;; ynb-dereference-buffer.

  ;; The global `ynb-buffer-alist' associates buffer names with keys.
  ;; Buffer names are unique.  The names of all buffers are constructed with
  ;; `ynb-make-temp-name' and are unique.    All buffers
  ;; except the package buffer `FILE' are deleted on exit.

  (interactive)
  (let ((home-buffer (current-buffer))
        files parts prompt
        tmp-buffer tmp-alist file-buffer)

    (if (not file)
        ;; If file has not been specifed, prompt
        (progn
              ;; Get default file from cell label, if any
              (save-excursion
                (goto-char (ynb-cell-start))
                (forward-line -1)
                (if (looking-at "^\\\\yacas.*<.*:.*>")
                    (progn
                      (setq ynb-error-point (point))
                      (unwind-protect ; In case filename errors
                          (if ynb-abbreviations-allowed
                              ;; This can take some seconds
                              (progn
                               (message "Getting filenames...")
                               (if (not (setq files 
					       (ynb-get-filenames)))
                                 (error "No complete package filenames found"))
                               (message "")))
                        (goto-char ynb-error-point))
                      (setq file (ynb-get-filename files)))))
          (setq file (read-from-minibuffer "Package file: " file))
          (if (or (not file) (equal file "")) (error "No file specified"))
          ) ; if file not specified in function call
      ) ; if on file

    (if (not overwrite)
        (if (file-exists-p file)
            (progn
              (setq prompt (concat
                            "Package file `"
                            file
                            "' exists. Overwrite it ? "))
              (if (not (y-or-n-p prompt))
                  (error "Package assembly cancelled")))))
    
    (if (get-buffer file) (kill-buffer file))

    (if ynb-abbreviations-allowed
        ;; This can take several seconds for a document with many cells
        (progn
          (message "Getting partnames...")
          (setq parts (ynb-get-partnames file files))
          (message "")))

    (message "Assembling package `%s' ..." file) ;(sleep-for 1)

    ;; Set where assembly will occur
    (setq file-buffer (ynb-make-temp-name))
    (setq ynb-buffer-alist (list (cons file file-buffer)))

    (unwind-protect ; So buffer can be deleted even if errors or abort
        (progn
          (setq ynb-source-buffer (current-buffer)) 
					; Collate from here
          (ynb-collate-cells file nil files parts nil)
          (or (get-buffer (cdr (assoc file ynb-buffer-alist)))
              (error "No `%s' cell `%s:' found" file))
          
          ;; OK, here we go:  Recursively dereference the cell buffer:
          (while (ynb-dereference-buffer file files parts))

          (set-buffer file-buffer)
          (write-file file)
          (set-buffer home-buffer))
      ;; unwind-protect tail:  Delete part files
      (setq tmp-alist ynb-buffer-alist)
      (while (setq tmp-buffer (cdr (car tmp-alist)))
        (setq tmp-alist (cdr tmp-alist))
        (condition-case nil ; In case buffer not actually created
            (if (equal tmp-buffer file-buffer)
                nil ; Don't delete the package buffer
              (kill-buffer tmp-buffer))
          (error nil)))) ; unwind-protect
    (message "Package `%s' assembled" file)
;    file
    (switch-to-buffer-other-window file))) ; done

(defun ynb-reference-count (buffer)
  "Return the number of references in BUFFER."
  (let ((count 0)
        (home-buffer (current-buffer)))
    (save-excursion
          (set-buffer buffer)
          (goto-char (point-min))
          (while (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (point-max) t)
            (setq count (+ count 1)))
          (set-buffer home-buffer)
          )
    count))

(defun ynb-append-cell-to-buffer (buffer)
  "Append text of cell containing point to BUFFER.
Create BUFFER if it does not exist."
  (if (not (ynb-cell-p))
      (error "Not in a cell.")
    (let ((home-buffer (current-buffer))
          (start (ynb-cell-start))
          end)
      (save-excursion
	(goto-char start)
	(beginning-of-line)
        (while (looking-at "^ *$") (forward-line 1))
	(setq start (point))
	(if (not (setq end (ynb-output-p)))
	    (progn
	      (goto-char (ynb-cell-end))
	      (while (looking-at "^ *$") (forward-line -1))
	      (end-of-line)
	      (setq end (point)))
	  (progn
	    (goto-char end)
	    (while (looking-at "^ *$") (forward-line -1))
	    (end-of-line)
	    (setq end (point))))
        (set-buffer (get-buffer-create buffer))
        (goto-char (point-max))
        (insert-buffer-substring home-buffer start end)
        (insert "\n")))))

(defun ynb-collate-cells (file part files parts &optional single)

  "Assemble cells marked with filename FILE in buffers with keys
`file:part' or, for part = null string (package cells), with key `file'.  The
names of all buffers are constructed with `ynb-make-temp-name' 
and are unique.  If PART is non-nil then do not collate cells with keys 
`FILE:PART' and `FILE' (package cells).  Use FILES and PARTS for name 
completion \(see `ynb-get-filename' and 
`ynb-get-partname'\).  If optional SINGLE is non-nil, then 
collate just cells `FILE:PART' (PART must be non-nil).

The global `ynb-buffer-alist' associates buffer names with keys.  
It must be initialized, typically with the buffer for key `FILE' or 
`FILE:PART', according to whether PART is nil or not."

  (let ((home-buffer (current-buffer))
        this-part this-file key)
    (unwind-protect ; For error location
        (setq ynb-error-point (point)) ; Go here if no error
        (progn
          ;; Scan buffer to construct buffers for all `file:part'
          (save-excursion
            (set-buffer ynb-source-buffer) ; Collate from here
            (goto-char (point-min))
            (while (ynb-forward-cell)
                   ;; We have a cell of the right type
                (forward-line -1) ; Move to \begin{...
                (if (not (looking-at "^\\\\yacas.*<.*:.*>"))
                    (forward-line 1) ; So we go to next cell next time through

                  ;; We have a marked cell
                  (setq this-file (ynb-get-filename files))
                  (cond
                   ((not this-file)
                    (setq ynb-error-point (point))
                    (error "Ambiguous filename"))
                   ((not (equal file this-file))
                    (forward-line 1)) ; So we go to next cell next time through
                   (t

                    ;; We have a cell of the right package filename
                    (setq this-part (ynb-get-partname parts))
                    (cond
                     ((not this-part)
                      (setq ynb-error-point (point))
                      (error "Ambiguous partname"))
                     ((and single (not (equal this-part part)))
                      (forward-line 1))    ; Do only `file:part' for 
                                           ; SINGLE non-nil
                     ((and part (equal this-part ""))
                      (forward-line 1)) ; Cell assembly, ignore package 
                                        ; cell `FILE:'
                     ((and (not single) (equal this-part part))
                      (forward-line 1)) ; Cell assembly, ignore cell 
                                        ; `FILE:PART'
                     (t

                      ;; We have a cell with a valid partname
                      (forward-line 1) ; Move into cell
                      (if (equal this-part "")
                          (setq key file)
                        (setq key (concat file ":" this-part)))
                      (or
                       (assoc key ynb-buffer-alist) ; buffer 
                                                              ;already created
                       (ynb-replace-assoc
                        ynb-buffer-alist
                        key (ynb-make-temp-name)))

                      ;; Append cell contents to its buffer
                      (ynb-append-cell-to-buffer
                       (cdr (assoc key ynb-buffer-alist)))
                      
                      ) ; t on valid partname
                     ) ; cond on partname
                    ) ; t on right filename (package)
                   ) ; cond on filename
                  ) ; if a marked cell
              ) ; while still cells to process
            (set-buffer home-buffer)
            ) ; save excursion
          ) ; progn of unwind-protect body
      
      ;; unwind-protect tail:  Delete part files
      (goto-char ynb-error-point)))) ; done

(defun ynb-dereference-buffer (key files parts &optional noinit)
  "Resolve all references in buffer corresponding to KEY in alist
ynb-buffer-alist, using FILES and PARTS for name completion.  
If optional NOINIT is nil, initialize global variable 
`ynb-dereference-path' with KEY.  If NOINIT is non-nil, 
add KEY to `ynb-dereference-path'. then references are collated 
in buffers and added to ynb-buffer-alist if necessary.  Use 
`ynb-dereference-path' to check for self-reference and
report error if detected,"
  (let ((ref-found nil)
        (home-buffer (current-buffer))
        path-to-here
        ref-indent ref-key ref-buffer
        (key-buffer (cdr (assoc key ynb-buffer-alist)))
        file part
        re-found)
    (or key-buffer (error "No cell `%s'" key))
    (set-buffer key-buffer)
    (goto-char (point-min))
    (if noinit
        t
      (setq noinit t)
      (setq ynb-dereference-path (list key)))
    (setq path-to-here ynb-dereference-path)
    (while (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (point-max) t)
      (setq re-found 1)
      (beginning-of-line)
      (setq ref-indent (ynb-get-reference-indentation))
      (setq file (ynb-get-filename files))
      (setq part (ynb-get-partname parts))
      (setq ref-key (concat file ":" part))
      (if (ynb-string-mem ref-key path-to-here)
            (ynb-dereference-error (cons ref-key path-to-here)))
      (setq ynb-dereference-path (cons ref-key path-to-here))
      (if (not (assoc ref-key ynb-buffer-alist))
          ;; Construct buffer on the fly
          (progn
            (setq ref-buffer (ynb-make-temp-name))
            (ynb-replace-assoc ynb-buffer-alist 
					  ref-key ref-buffer)
            (ynb-collate-cells file part files parts t)
            )
        (setq ref-buffer (cdr (assoc ref-key ynb-buffer-alist)))
        )
      (while (ynb-dereference-buffer ref-key files parts noinit))
      (kill-line 1) ; Remove reference line
      (insert-buffer ref-buffer)
      (let ((indent-start (point))
            indent-end)
        (exchange-point-and-mark)
        (setq indent-end (point))
        (exchange-point-and-mark)
        (if ref-indent (indent-rigidly indent-start indent-end ref-indent))))
    (setq ynb-dereference-path path-to-here)
    (set-buffer home-buffer)
    ref-found))

(defun ynb-dereference-error (path)
  "Report package self-reference error, in PATH"
  (let ((cell (car path))
        (home-buffer (current-buffer))
        to-cell from-cell)
    (setq to-cell cell)
    (with-output-to-temp-buffer "*Help*" (message ""))
    (pop-to-buffer "*Help*")
    (insert "Self-reference detected assembling Yacas/TeX cell\n\n")
    (insert (concat "\t\t" to-cell "\n\n"))
    (insert "Here is how the self-reference happened:\n\n")
    (setq path (reverse path))
    (setq from-cell (car path))
    (insert (concat "\t" from-cell "\n"))
    (while (setq path (cdr path))
      (setq to-cell (car path))
      (if (equal cell to-cell)
          (insert (concat " !!! ->\t   -->\t" to-cell "\n"))
        (insert (concat "\t   -->\t" to-cell "\n")))
      (setq from-cell to-cell)
      )
    (pop-to-buffer home-buffer)
    (error "Self-reference detected")))

(defun ynb-get-reference-indentation ()
  "Return indentation of reference on current line.
Line assumed tabified."
  (let (start end)
    (save-excursion
      (beginning-of-line)
      (setq start (point))
      (search-forward "<")
      (untabify start (point))
      (setq end (point))
      (beginning-of-line)
      (tabify (point) end)
      (- end start 1) ; -1 since search places point after `>'
      )))

(defun ynb-insert-complete-name ()
  "Insert complete name in buffer for cell.
Return t if successful, else nil."
  (interactive)
  (let ((here (point))
        start end name text files parts)
    (save-excursion
      (beginning-of-line)
      (cond
       ((and ; partname
         (or
          (re-search-forward "^\\\\yacas<.*:[^\t]*" here t)
          (re-search-forward "^[ \t]*<.*:[^\t]*" here t))
         (equal here (point)))

        ;; This can take a second or two
        (message "Getting filenames...")
        (if (not (setq files (ynb-get-filenames)))
            (error "No package filenames in document"))
        (message "")

        (search-backward "<")
        (forward-char 1)
        (setq start (point))
        (search-forward ":")
        (forward-char -1)
        (setq text (buffer-substring start (point)))
        (if (not (setq name (ynb-complete-name text files)))
            (error "No matching package filename found"))

        ;; This can take several seconds for a document with many cells
        (message "Getting partnames")
        (setq parts (ynb-get-partnames name files))
        (message "")

        (forward-char 1)
        (setq start (point)) ; New start, for partname deletion
        (setq text (buffer-substring (point) here))
        (if (not (setq name (ynb-complete-name
                             (concat text "...")
                             parts)))
            (error "No matching package partname found"))
        (cond
         ((equal t name) ; Text is complete
          (setq name text)
          )
         ((equal t (try-completion name parts)) ; Completion is exact
          )
         (t ; Else, get completion
          (setq name
                (completing-read
                 "Partname (<space> to see partnames): "
                 parts nil t name))
          )
         ) ; cond: what kind of partname completion was done
        (delete-region start here)
        (insert (concat name ">"))
        ) ; End of partname completion
       ((and ; filename
         (or (re-search-forward "^\\\\yacas<[^ \t]*" here t)
             (re-search-forward "^[ \t]*<[^ \t]*" here t))
         (equal here (point)))

        ;; This can take a second or two
        (message "Getting filenames...")
        (if (not (setq files (ynb-get-filenames)))
            (error "No package filenames in document"))
        (message "")

        (re-search-backward "<")
        (forward-char 1)
        (setq start (point))
        (setq text (buffer-substring start here))
        (if (not (setq name (ynb-complete-name
                             (concat text "...") ; completion form
                             files)))
            (error "No matching package filename found"))
        (cond
         ((equal t name) ; Text is complete
          (setq name text)
          )
         ((equal t (try-completion name files)) ; Completion is exact
          )
         (t ; Else, get completion
          (setq name
                (completing-read
                 "Filename (<space> to see filenames): "
                 files nil t name))
          (if (equal "" name) (error "")) ; No response means no completion
          )
         ) ; cond: what kind of filename completion was done
        (delete-region start here)
        (insert (concat name ":"))
        ) ; End of filename completion
       (t
        ;;(error "Nothing to complete")
        nil ; No error; pass to Yacas for symbol completion
        )
       ) ; cond: what kind of completion to do
      ) ; save-excursion
    (if (not name)
        nil
      (goto-char (+ (point) (length name) 1))
      t)))

(defun ynb-get-filenames ()
  "Return alist of package filenames for cells."
  (let (file files)
    (save-excursion
      (goto-char (point-min))
      (while (ynb-forward-cell)
          (forward-line -1)
          (if (not (looking-at (concat "^\\\\yacas.*<.*>")))
              (forward-line 1) ; Cell not marked.  Get set for next one
            (if (setq file (ynb-get-filename)) ; Only unabbreviated
                                                          ;  names
                (if files
                    (if (assoc file files)
                        nil ; already only
                      (setq files (cons (list file) files))) ; Add to alist
                  (setq files (list (list file))))) ; Start alist
            (forward-line 1)
            ) ; if a marked cell
        ) ; while cell to look at
      ) ; save-excursion
    files)) ; let and done

(defun ynb-complete-name (text alist &optional exact)
  "Get full name corresponding to TEXT.
If text is a string ending in `...',
then the substring preceding the `...' is used with try-completion on ALIST.
An exact match is required if optional EXACT is t.
If text is just `...' and alist is length 1, then the car of its single 
element is returned.
Oherwise nil is returned."
  (let (name try-name)
    (if (not (string-match "\\(\\.\\.\\.$\\)" text))
        (setq name text) ; don't do completion on full names
      (if (and
           (eq 0 (match-beginning 1)) ; just "..."
           (eq 1 (length alist))) ; a single package filename
          (setq name (car (car alist)))
        (setq try-name (substring text 0 (match-beginning 1)))
        (setq name (try-completion try-name alist)))
      (cond
       ((equal t name)
        (setq name try-name))
       ((and
         exact
         (not (equal t (try-completion name alist))))
        (setq name nil)))) ; Not an exact match, so error
    name))

(defun ynb-get-partnames (file files)
  "Return alist of partnames for package FILE, using FILES for
filename completion."
  (let (cell-end cell-file part parts)
    (setq ynb-error-point (point))
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (while (ynb-forward-cell)
              (setq cell-end (ynb-cell-end))
              (forward-line -1)
              (if (not (looking-at
                       "^\\\\yacas.*<[^:].*:.*>"))
                  (forward-line 1) ; Not a marked cell
                (setq cell-file (ynb-get-filename files))
                (if (not (equal file cell-file))
                    (forward-line 1) ; Wrong file
                  (while (and
                          (<= (point) cell-end)
                          (or
                           (re-search-forward
                            "^\\\\yacas.*<[^:].*:.*>" cell-end t)
                           (re-search-forward
                            "^ *\t*<[^:].*:.*>" cell-end t)))
                    (beginning-of-line) ; We have a filename-partname reference
                    (if (not (setq file (ynb-get-filename files)))
                        (progn
                          (setq ynb-error-point (point))
                          (error "Ambiguous filename")))
                    (if (not (equal cell-file file))
                        (progn
                          (setq ynb-error-point (point))
                          (error "Reference must match cell filename: `%s'"
                                 cell-file)))
                    (setq part (ynb-get-partname))
                    (if (not part)
                        nil ; Need full (unabbreviated) parts only, for alist
                      (if parts ; Update alist
                          (if (or
                               (equal part "")
                               (ynb-string-mem part parts))
                              nil; already on list
                            (setq parts (append (list part) parts))) ; Add 
                                                                     ;to alist
                        (if (not (equal part ""))
                            (setq parts (list part)))) ; Create alist
                      ) ; if an unabbreviated part                    
                    (forward-line 1)
                    ) ; while references to process in this cell
                  ) ; if a marked cell of this FILE
                ) ; if a marked cell
            ) ; while cells to process
          ); save-excursion
      (goto-char ynb-error-point) ; unwind-protect form
      ) ; unwind-protect
    (setq parts (mapcar 'list parts)) ; Make list into an alist
    parts))

(defun ynb-get-filename (&optional alist)
  "Get filename in package reference on current line.
If optional ALIST is supplied, use it for name completion.
Return nil if no name or error in name."
  (let ((match-re "\\(<\\)[^:]*\\(:\\)")
        (abbrev-re "\\.\\.\\.")
        beg text)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq text (buffer-substring beg (point)))
      (string-match match-re text)
      (setq text 
	    (substring text (+ 1 (match-beginning 1)) (+ -1 (match-end 2))))
      ) ; save excursion
    (if alist
        (ynb-complete-name text alist t)
      (if (string-match abbrev-re text)
          (if ynb-abbreviations-allowed
              nil
            (setq ynb-error-point (point))
	    (error 
"Set ynb-abbreviations-allowed (M-x set-variable) to use abbreviations")
            )
        text))))

(defun ynb-get-partname (&optional alist)
  "Get partname in package reference on current line.
If optional ALIST is supplied, use it for name completion.
Return nil if no name or error in name."
  (let ((match-re "\\(:\\)\\([^>]*\\)")
        (abbrev-re "\\.\\.\\.")
        beg text)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq text (buffer-substring beg (point)))
      (string-match match-re text)
      (setq text (substring text (+ 1 (match-beginning 1)) (match-end 2)))
      ) ; save excursion
    (if alist
        (ynb-complete-name text alist t)
      (if (string-match abbrev-re text)
          (if ynb-abbreviations-allowed
              nil
            (setq ynb-error-point (point))
 (error "Set ynb-abbreviations-allowed (M-x set-variable) to use abbreviations")
            )
        text))))

(defun ynb-string-mem (element list) ; memq doesn't work for strings
  "Returns t if string ELEMENT is in LIST of strings, else returns nil."
  (let (try)
    (catch 'done
      (while (setq try (car list))
        (setq list (cdr list))
        (if (equal element try)
            (throw 'done t)))
      nil)))

(defun ynb-reference-p ()
  "Return t if cell contains a cell reference, else retrun nil."
  (save-excursion
    (goto-char (ynb-cell-start))
    (if 
       (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (ynb-cell-end) t)
        t
      nil)))

;;; @@ Yacas-Notebook functions for "yacas" cells

(defun ynb-send-cell (&optional tex)
  "Send input to process. Point must be in a cell."
  (interactive)
  (if (not (ynb-cell-p))
      (error "Not in Yacas cell"))
  (let ((home-buffer (current-buffer))
        assembled-file 
        start 
        end
        cell)
    (if (ynb-reference-p)
        (progn
          (widen) ; So cell references will be found
          (setq assembled-file (ynb-assemble-cell t)))
      (save-excursion
	(goto-char (ynb-cell-start))
        ;; Now I want to skip over any blank lines at the beginning of the cell
	(beginning-of-line)
	(while (looking-at "^ *$") (forward-line 1))
	(setq start (point))
        ;; as well as at the end of the cell
	(if (not (setq end (ynb-output-p)))
	    (progn
	      (goto-char (ynb-cell-end))
	      (while (looking-at "^ *$") (forward-line -1))
	      (end-of-line)
	      (setq end (point)))
	  (progn
	    (goto-char end)
	    (while (looking-at "^ *$") (forward-line -1))
	    (end-of-line)
	    (setq end (point))))))
    (if assembled-file
        (progn
          (set-buffer (find-file-noselect assembled-file))
          (setq cell (buffer-substring-no-properties (point-min) (point-max))))
      (setq cell (buffer-substring-no-properties start end)))
    (ynb-send-block cell tex)))

(defun ynb-single-command-get-output (string tex)
  "Send a command to the Yacas process."
  (let ((output)
        (end))
    (setq string (yacas-strip-string string))
    (yacas-start)
    (save-excursion
      (set-buffer (process-buffer inferior-yacas-process))
      (goto-char (point-max))
      (insert string)
      (setq yacas-input-end (point))
      (comint-send-input))
    (while (not (yacas-new-prompt-p))
      (sit-for 0.100))
    (if tex
        (yacas-single-command ynb-tex-string))
    (save-excursion
      (set-buffer (process-buffer inferior-yacas-process))
      (goto-char (point-max))
      (if tex
          (progn
            (re-search-backward yacas-input-prompt-string)
            (re-search-backward "\$\"")
            (setq end (point))
            (goto-char (1+ yacas-input-end))
;            (re-search-backward yacas-output-prompt-string)
            (re-search-forward "\"")
            (forward-char 1)
            (setq output 
                  (concat "$$"
                          (buffer-substring-no-properties (point) end)
                          "$$")))
        (forward-line -1)
        (end-of-line)
        (forward-char 1)
        (setq end (point))
;        (re-search-backward (concat "^" yacas-output-prompt-string))
;        (goto-char (match-end 0))
        (goto-char (1+ yacas-input-end))
        (setq output 
              (concat 
               (make-string (length yacas-output-prompt-string) ?\ )
               (buffer-substring-no-properties (point) end)))
        (setq output (replace-regexp-in-string "Out>" "    " output))))
    (if (and tex (not (string= ynb-cell-output "")))
        (setq ynb-cell-output (concat ynb-cell-output "\n" output))
      (setq ynb-cell-output (concat ynb-cell-output output)))))

(defun ynb-send-block (stuff &optional tex)
  "Send a block of code to Yacas."
  (yacas-start)
  (setq ynb-cell-output "")
  (while (string-match ";" stuff)
    (setq end (1+ (string-match ";" stuff)))
    (ynb-single-command-get-output (substring stuff 0 end) tex)
    (setq stuff (substring stuff end))))

(defun ynb-get-output (tex)
  "Insert last output from Yacas.
Assumes point in cell.  Output inserted at end of cell."
  (goto-char (ynb-cell-end))
  (forward-line 1)		 ; Insert marker before output
  (open-line 1)
  (if tex
      (insert "\\outputtex")					; ..
    (insert "\\output"))
  (forward-line 1)		 ; ..
  (save-excursion
    (insert ynb-cell-output)
    (if tex
	(insert "\n"))))

(defun ynb-replace (tex)
  "Replace output (if any) with last Yacas result. Point must be in a cell.
Output assumed to follow input, separated by a ynb-output-marker 
line."
  (if (not (ynb-cell-p))
      (error "Not in Yacas cell"))
  (save-excursion
    (ynb-delete-output)
    (ynb-get-output tex)))

(defun ynb-update-type-cell (tex)
  "Send input to Yacas and replace output with result.
Point must be in cell.  Output assumed to follow input,
separated by a ynb-output-marker line."
  (if (not (ynb-cell-p))
      (error "Not in Yacas cell"))
  (ynb-send-cell tex)
  (ynb-replace tex))

(defun ynb-update-cell ()
  "Send input to Yacas and replace output with result.
Point must be in cell.  Output assumed to follow input,
separated by a ynb-output-marker line."
  (interactive)
  (ynb-update-type-cell nil))

(defun ynb-tex-update-cell ()
  "Send input to yacas and replace output with the result in TeX form.
Point must be in cell.  Output assumed to follow input,
separated by a ynb-output-marker line."
  (interactive)
  (ynb-update-type-cell t))

(defun ynb-replace-line-with-tex ()
  "Sends the current line to Yacas, and then replaces it with the Yacas
output in TeX form."
  (interactive)
  (interactive)
  (setq ynb-cell-output "")
  (ynb-single-command-get-output 
   (buffer-substring-no-properties (line-beginning-position)
                                   (line-end-position)) t)
  (beginning-of-line)
  (insert "% ")
  (end-of-line)
  (forward-line 1)
  (insert ynb-cell-output)
  (newline))

(defun ynb-replace-line ()
  "Sends the current line to Yacas, and then replaces it with the Yacas
output."
  (interactive)
  (setq ynb-cell-output "")
  (ynb-single-command-get-output 
   (buffer-substring-no-properties (line-beginning-position)
                                   (line-end-position)) nil)
  (beginning-of-line)
  (insert "% ")
  (end-of-line)
  (forward-line 1)
  (insert ynb-cell-output))

;;; @@ The mode

;; First, find out what kind of TeX mode is being used.
(cond
 ((eq ynb-use-tex 'auctex)
  (require 'tex-site)
  ;; I don't think this is the best thing to do...
  (load "latex")
  (setq texmode-map LaTeX-mode-map)
  (defun texmode () (latex-mode)))
 ((eq ynb-use-tex 'tex)
  (require 'tex-mode)
  (setq texmode-map tex-mode-map)
  (defun texmode () (tex-mode)))
 (t
  (autoload 'text-mode "text-mode")
  (setq texmode-map text-mode-map)
  (defun texmode () (text-mode))))

(defvar yacas-notebook-mode-map nil
  "Keymap used in yacas-notebook-mode.")

(if yacas-notebook-mode-map ()
  (let ((map (copy-keymap texmode-map)))
    (define-key map "\C-c+" 'ynb-forward-cell)
    (define-key map "\C-c-" 'ynb-backward-cell)
    (define-key map "\C-c\C-ua" 'ynb-update-all)
    (define-key map "\C-c\C-uA" 'ynb-tex-update-all)
    (define-key map "\C-c\C-uq" 'ynb-eval-init)
    (define-key map "\C-c\C-ui" 'ynb-update-init)
    (define-key map "\C-c\C-uI" 'ynb-tex-update-init)
    (define-key map "\C-c?" 'ynb-info-help)
    (define-key map "\C-c\C-u" nil)
    (define-key map "\C-c\C-o" 'ynb-create-cell)
    (define-key map "\C-c\C-ul" 'ynb-replace-line)
    (define-key map "\C-c\C-uL" 'ynb-replace-line-with-tex)
    (define-key map "\C-c\C-s" 'ynb-send-cell)
    (define-key map "\C-c\C-uc" 'ynb-update-cell)
    (define-key map "\C-c\C-uC" 'ynb-tex-update-cell)
    (define-key map "\C-c\C-d" 'ynb-delete-output)
    (define-key map "\C-c\C-q" 'ynb-toggle-init)
    (define-key map "\C-c\C-x" 'ynb-package-part)
    (define-key map "\C-c@" 'ynb-assemble)
    (define-key map [(control c) (control tab)] 
      'ynb-insert-complete-name)
    (setq yacas-notebook-mode-map map)))

(define-derived-mode yacas-notebook-mode texmode "Yacas-Notebook"
  "This is a mode intended to allow the user to write documents that
include Yacas code.  The file can be LaTeXed to produce nice 
looking output (although that isn't necessary, of course), and so the
mode is an extension of TeX-mode (AucTeX, if you use that) that also
includes all the functionality of yacas-mode.
The units of Yacas code that are worked with are \"cells\", which are 
delimited by \"\\yacas\" and \"\\endyacas\". The cells can be evaluated 
individually, as a group, and the output can be returned as Yacas output 
or in TeX form.  Evaluating a cell and returning the output is called 
\"updating\" the cell.  This mode also supports some literate programming 
constructs.  (See the file \"Yacas-NotebookIntro.tex\" for more 
information.)
The commands for working with cells are:
 \\[ynb-create-cell]  create a cell         
 \\[ynb-update-all] update all the cells 
 \\[ynb-tex-update-all] update all the cells in TeX form 
 \\[ynb-forward-cell] go to the next cell 
 \\[ynb-backward-cell] go to the previous cell
 \\[ynb-eval-init] evaluate all  initialization cells
 \\[ynb-update-init] update all the initialization cells
 \\[ynb-tex-update-init] update all the initialization cells in TeX form

(With a prefix, C-u \\[ynb-update-all] and C-u \\[ynb-tex-update-all] will update the cells 
without prompting)
Single lines can be evaluated:
 \\[ynb-replace-line] replace the current line with Yacas output
 \\[ynb-replace-line-with-tex] replace the current line with Yacas output 
in TeX form.

Within a cell, the following commands are available:\\<ynb-yacas-map>
 \\[ynb-delete-output]  delete the cell's output
 \\[ynb-update-cell]  update a cell 
 \\[ynb-tex-update-cell] update a cell in TeX form
 \\[ynb-toggle-init] toggle initialization cells
 \\[ynb-assemble]  assemble a cell which defines a package
 C-u \\[ynb-assemble]  assemble a cell with references

Finally, the command \\[ynb-mark-file-as-yacas-notebook] will 
insert a 
%-*-Yacas-Notebook-*- at the beginning
of the file (if there isn't one there already) so the file will begin in
yacas-notebook-mode next time it's opened.

\\{yacas-notebook-mode-map}
"
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex)
  (make-local-variable 'ispell-tex-p)
  (setq ispell-tex-p t)
  (setq yacas-use-filter t)
  (use-local-map yacas-notebook-mode-map)
  (if (eq ynb-use-tex 'auctex)
    (progn
      (require 'font-latex)
      (add-hook 'yacas-notebook-mode-hook 'font-latex-setup)))
  (ynb-menu-install-menus)
  (run-hooks 'yacas-notebook-mode-hook))

(if (eq ynb-use-tex 'auctex)
    (put 'latex-mode 'font-lock-defaults 'yacas-notebook-mode))

;; Some more fontlocking
;; First, fontify the \yacas and \endyacas

(if (fboundp 'font-lock-add-keywords)
    (progn
      (defun ynb-font-lock-cell (limit)
	"Used to fontify whatever's between \\yacas and \\endyacas."
	(when (re-search-forward "\\\\yacas" 
				 limit t)
	  (let ((beg (match-end 0)) end)
	    (if (search-forward "\\endyacas"
				limit 'move)
		(setq end (match-beginning 0))
	      (setq end (point)))
	    (store-match-data (list beg end))
	    t)))

      (font-lock-add-keywords 'yacas-notebook-mode 
			      '((ynb-font-lock-cell
				 (0 font-lock-function-name-face append t))))

      (font-lock-add-keywords 'yacas-notebook-mode 
	    '(("\\(\\\\\\(endyacas\\|output\\(tex\\)?\\|yacas\\)\\)"
	       . font-lock-keyword-face)))))


;;; This is stolen, more or less, from w3-menu.el, by William Perry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ynb-running-xemacs 
  (string-match "XEmacs\\|Lucid" emacs-version))

(defvar ynb-menu-yn-menu nil)
(defvar ynb-menu-cell-menu nil)
(defvar ynb-menu-update-menu nil)
(defvar ynb-menu-eval-menu nil)
(defvar ynb-menu-format-menu nil)
(defvar ynb-menu-misc-menu nil)
(defvar ynb-menu-help-menu nil)
(defvar ynb-menu-yacas-notebook-menubar nil)

(defvar ynb-use-menus 
  '(yn cell update process control misc nil help)
  "*Non-nil value causes Yacas-Notebook mode to provide a menu interface.
A value that is a list causes YACAS-NOTEBOOK mode to install its own menubar.
A value of 1 causes YACAS-NOTEBOOK mode to install a \"Yacas-Notebook\" item in the 
Emacs menubar.

If the value of ynb-use-menus is a list, it should be a list of symbols.
The symbols and the order that they are listed determine what menus
will be in the menubar and how they are ordered.  Valid symbol values
are:

cell		-- Cell operations
update  	-- Updating functions
process          -- Running a Yacas process
control          -- Control constructs
misc		-- Miscellaneous
yn		-- A toggle button to switch back to normal emacs menus
help            -- help on yacas functions
nil		-- ** special **

If nil appears in the list, it should appear exactly once.  All
menus after nil in the list will be displayed flushright in the
menubar.")

(defun ynb-menu-global-menubar ()
  (if ynb-running-xemacs
      (default-value 'default-menubar)
    (lookup-key (current-global-map) [menu-bar])))

(defconst ynb-menu-cell
  (list
   "Cells"
   ["Create cell"  ynb-create-cell (not (ynb-cell-p))]
   ["Send cell"  ynb-send-cell (ynb-cell-p)]
   ["Update cell"   ynb-update-cell (ynb-cell-p)]
   ["TeX update cell"  ynb-tex-update-cell (ynb-cell-p)]
   ["Delete output"   ynb-delete-output (ynb-cell-p)]
   ["Toggle initialization"  ynb-toggle-init 
                                                     (ynb-cell-p)]
   ["Mark as package part" ynb-package-part (ynb-cell-p)]
   ["Insert complete name" ynb-insert-complete-name 
                                                      (ynb-cell-p)]
   ["Forward cell"  ynb-forward-cell]
   ["Backwards cell"  ynb-backward-cell]))

(defconst ynb-menu-update
  (list
   "Update"
   ["Update line"   ynb-replace-line (not (ynb-cell-p))]
   ["Update all cells"  ynb-update-all]
   ["Update initialization cells"  ynb-update-init]
   "---"
   ["TeX update line"   ynb-replace-line-with-tex 
                                          (not (ynb-cell-p))]
   ["TeX update all cells"  ynb-tex-update-all]
   ["TeX update initialization cells"  ynb-tex-update-init]))

(defconst ynb-menu-control
  (list
   "Control"
   ["Procedure"  yacas-proc (ynb-cell-p)]
   ["New local variable"  yacas-local (ynb-cell-p)]
   ["Function" yacas-function (ynb-cell-p)]
   ["If"  yacas-if (ynb-cell-p)]
   ["Else"  yacas-else (ynb-cell-p)]
   ["For"  yacas-for (ynb-cell-p)]
   ["ForEach"  yacas-foreach (ynb-cell-p)]
   ["While"  yacas-while (ynb-cell-p)]
   ["Until"  yacas-until (ynb-cell-p)]))

(defconst ynb-menu-process
  (list
    "Process"
    ["Start a Yacas process"   yacas-start
     (not (processp yacas-process))]
    ["Run Yacas on region"  yacas-region]
    ["Run Yacas on initialization cells"  ynb-eval-init]
    ["Send string to Yacas" yacas-eval-string t]
    ["Reset the Yacas process" yacas-reset t]
    ["Kill Yacas process"  yacas-stop (processp yacas-process)]))

(defun ynb-info-help ()
  (interactive)
  (info (concat ynb-info-dir "yacas-notebook")))

(defconst ynb-menu-help
  (list
   "Help"
   ["Yacas help" yacas-help]
   ["Yacas notebook help" ynb-info-help]))


(defconst ynb-menu-yn
  (list
   "Yacas Notebook>>"
   ["Toggle Yacas Notebook/Emacs menus"  ynb-menu-toggle-menubar]))

(defconst ynb-menu-misc
  (list
   "Misc"
   ["Indent region"   yacas-indent-region (ynb-cell-p)]
   ["Short comment"  yacas-short-comment (ynb-cell-p)]
   ["Long comment" yacas-long-comment (ynb-cell-p)]
   ["Mark file as Yacas-Notebook"  ynb-mark-file-as-yacas-notebook]
   (list
    "Web"
    ["Assemble cell"  ynb-assemble-cell 
     (and (ynb-cell-p) (ynb-reference-p))]
    ["Assemble package"  ynb-assemble-package 
     (and (ynb-cell-p) (ynb-reference-p))])))

(defvar yacas-notebook-mode-menu-map nil)

(defun ynb-menu-initialize-yacas-notebook-mode-menu-map ()
  (if (null yacas-notebook-mode-menu-map)
      (let ((map (make-sparse-keymap))
	    (dummy (make-sparse-keymap)))
	(require 'easymenu)
	;; initialize all the ynb-menu-*-menu variables
	;; with the menus.
	(easy-menu-define ynb-menu-help-menu (list dummy) nil
			  ynb-menu-help)
	(easy-menu-define ynb-menu-yn-menu (list dummy) nil
			  ynb-menu-yn)
	(easy-menu-define ynb-menu-cell-menu (list dummy) nil
			  ynb-menu-cell)
	(easy-menu-define ynb-menu-update-menu (list dummy) nil
			  ynb-menu-update)
	(easy-menu-define ynb-menu-control-menu (list dummy) nil
			  ynb-menu-control)
	(easy-menu-define ynb-menu-process-menu (list dummy) nil
			  ynb-menu-process)
	(easy-menu-define ynb-menu-misc-menu (list dummy) nil
			  ynb-menu-misc)
	;; block the global menubar entries in the map so that YACAS-NOTEBOOK
	;; can take over the menubar if necessary.
	(define-key map [rootmenu] (make-sparse-keymap))
	(define-key map [rootmenu yacas-notebook] 
	        (cons "Yacas-Notebook" (make-sparse-keymap "Yacas-Notebook")))
	(define-key map [rootmenu yacas-notebook options] 'undefined)	
	(define-key map [rootmenu yacas-notebook search] 'undefined)
	(define-key map [rootmenu yacas-notebook buffer] 'undefined)
	(define-key map [rootmenu yacas-notebook mule] 'undefined)
	(define-key map [rootmenu yacas-notebook tools] 'undefined)
	(define-key map [rootmenu yacas-notebook help] 'undefined)
	(define-key map [rootmenu yacas-notebook help-menu] 'undefined)
	;; now build YACAS's menu tree.
	(let ((menu-alist
	       '(
		 (yn
		  (cons "Yacas-Notebook>>" ynb-menu-yn-menu))
		 (cell
		  (cons "Cells" ynb-menu-cell-menu))
		 (update
		  (cons "Update" ynb-menu-update-menu))
		 (control
		  (cons "Control" ynb-menu-control-menu))
		 (process
		  (cons "Process" ynb-menu-process-menu))
		 (misc
		  (cons "Misc" ynb-menu-misc-menu))
		 (help
		  (cons "Help" ynb-menu-help-menu))))
	      cons
	      (vec (vector 'rootmenu 'yacas-notebook nil))
	      ;; menus appear in the opposite order that we
	      ;; define-key them.
	      (menu-list 
	       (if (consp ynb-use-menus)
		   (reverse ynb-use-menus)
		 (list 'help nil 'misc 'control 'process
		       'update 'cell))))
	  (while menu-list
	    (if (null (car menu-list))
		nil;; no flushright support in FSF Emacs
	      (aset vec 2 (intern (concat "ynb-menu-"
					  (symbol-name
					   (car menu-list)) "-menu")))
	      (setq cons (assq (car menu-list) menu-alist))
	      (if cons
		  (define-key map vec (eval (car (cdr cons))))))
	    (setq menu-list (cdr menu-list))))
	(setq yacas-notebook-mode-menu-map map)
	(run-hooks 'ynb-menu-setup-hook))))

(defun ynb-menu-make-xemacs-menubar ()
  (let ((menu-alist
	 '((yn . ynb-menu-yn)
	   (cell . ynb-menu-cell)
	   (update   . ynb-menu-update)
	   (control     . ynb-menu-control)
	   (process    . ynb-menu-process)
	   (misc     . ynb-menu-misc)
	   (help . ynb-menu-help)))
	cons
	(menubar nil)
	(menu-list ynb-use-menus))
    (while menu-list
      (cond
       ((null (car menu-list))
	(setq menubar (cons nil menubar)))
       (t (setq cons (assq (car menu-list) menu-alist))
	  (if cons
	      (setq menubar (cons (symbol-value (cdr cons)) menubar)))))
      (setq menu-list (cdr menu-list)))
    (nreverse menubar)))

(defun ynb-menu-install-menubar ()
  (cond
   (ynb-running-xemacs
    (cond
     ((not (featurep 'menubar)) nil)	; No menus available
     (t
      (setq ynb-menu-yacas-notebook-menubar 
                              (ynb-menu-make-xemacs-menubar))
      (set-buffer-menubar ynb-menu-yacas-notebook-menubar))))
   ((not (fboundp 'ynb-menu-cell-menu))
    (ynb-menu-initialize-yacas-notebook-mode-menu-map)
    (define-key yacas-notebook-mode-map [menu-bar]
      (lookup-key yacas-notebook-mode-menu-map [rootmenu yacas-notebook])))))

(defun ynb-menu-install-menubar-item ()
  (cond
   (ynb-running-xemacs
    (if (not (featurep 'menubar))
	nil				; No menus available
      (set-buffer-menubar (copy-sequence (ynb-menu-global-menubar)))
      (add-menu nil "Yacas-Notebook" 
                            (cdr ynb-menu-yacas-notebook-menubar))))
   ((not (fboundp 'ynb-menu-cell-menu))
    (ynb-menu-initialize-yacas-notebook-mode-menu-map)
    (define-key yacas-notebook-mode-map [menu-bar]
      (lookup-key yacas-notebook-mode-menu-map [rootmenu])))))

(defun ynb-menu-install-menus ()
  (cond ((consp ynb-use-menus)
	 (ynb-menu-install-menubar))
	((eq ynb-use-menus 1)
	 (ynb-menu-install-menubar-item))
	(t nil)))

(defun ynb-menu-set-menubar-dirty-flag ()
  (cond (ynb-running-xemacs
	 (set-menubar-dirty-flag))
	(t
	 (force-mode-line-update))))

(defun ynb-menu-toggle-menubar ()
  (interactive)
  (cond
   (ynb-running-xemacs
    (if (null (car (find-menu-item current-menubar '("[Yacas-Notebook>>]"))))
	(set-buffer-menubar ynb-menu-yacas-notebook-menubar)
      (set-buffer-menubar (copy-sequence (ynb-menu-global-menubar)))
      (condition-case ()
	  (add-menu-button nil ["[Yacas-Notebook]" 
                                        ynb-menu-toggle-menubar t] nil)
	(void-function
	 (add-menu-item nil "[Yacas-Notebook]" 
                                         'ynb-menu-toggle-menubar t))))
    (ynb-menu-set-menubar-dirty-flag))
   (t
    (if (not (eq (lookup-key yacas-notebook-mode-map [menu-bar])
		 (lookup-key yacas-notebook-mode-menu-map 
                                                 [rootmenu yacas-notebook])))
	(define-key yacas-notebook-mode-map [menu-bar]
	  (lookup-key yacas-notebook-mode-menu-map [rootmenu yacas-notebook]))
      (define-key yacas-notebook-mode-map [menu-bar]
	(make-sparse-keymap))
      (define-key yacas-notebook-mode-map [menu-bar yacas-notebook]
	(cons "[Yacas-Notebook]" 'ynb-menu-toggle-menubar)))
    (ynb-menu-set-menubar-dirty-flag))))

;;; yacas-notebook.el ends here
