;; yacas-notebook.el  Mode for interaction with Yacas from TeX buffer
;;; Written 2/12/1991 by Dan Dill dan@chem.bu.edu
;;; Modified August 2000 by Jay Belanger
;;; Copyright (C) 1991, 1993 Dan Dill (dan@chem.bu.edu) 1999 Jay Belanger
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

(require 'yacas)
(require 'font-lock)
(require 'font-latex)
(require 'easymenu)

(defvar yacas-notebook-temp-dir
  "/tmp/"
  "*Directory for temporary files.
Specify \"\" to use the directory of the Yacas-Notebook document buffer.")

(defvar yacas-notebook-output-marker "\\output"
  "*Contents of line separating input and output portion of cell.")

(defvar yacas-notebook-abbreviations-allowed nil
  "*If non-nil, then `...' abbreviations are allowed in cell labels 
and references. Note that enabling this options will slow cell and 
package assembly.")

(defvar yacas-notebook-max-references 5
  "*Number of references in a cell below which cell references are fetched
as needed, scanning the entire document for each reference.  At or above this
number, all cells in a document for the given filename are precollated in a
single scan of the document.")

(defvar yacas-notebook-tex-string
  "TeXForm(%);")

(setq yacas-notebook-texform nil)

;;; @@ Data structures

(defvar yacas-notebook-dereference-path nil
  "List of buffers referenced in cell assembly.
Used by `yacas-notebook-dereference-buffer' to detect self-reference.")

(defvar yacas-notebook-error-point nil
  "Buffer position where error detected.")

(defvar yacas-notebook-buffer-alist nil
  "Alist of temporary buffers associate with cells `file:part'.
The buffers are used in package and cell assembly.")

(defvar yacas-notebook-source-buffer nil
  "Buffer from which yacas-notebook-collate-cells works.")

(defconst yacas-notebook-temp-suffix 0
  "Temporary filename suffix.  Incremented by 1 for each filename.")

(defun yacas-notebook-make-temp-name ()
  "Return a unique filename."
  (setq yacas-notebook-temp-suffix (+ yacas-notebook-temp-suffix 1))
  (concat (concat (make-temp-name "#mz") "-")
          yacas-notebook-temp-suffix
          ".mu")
  )

(defun yacas-notebook-mark-file-as-yacas-notebook ()
  "Mark the file as an Yacas-Notebook buffer.
The next time the file is loaded, it will then be in Yacas-Notebook mode"
  (interactive)
  (save-excursion
    (goto-line 1)
    (beginning-of-line)
    (if (looking-at ".*-\\*-Yacas-Notebook-\\*-")
	()
      (open-line 1)
      (insert "%-*-Yacas-Notebook-*-")
      )))

;;; @@ Initialization

(defun yacas-notebook-replace-assoc (alist key val)
  "Replace ALIST KEY VALUE, if KEY present, else add KEY VALUE.
Return modified alist."
  (if (assoc key alist)
      (setcdr (assoc key alist) val)
    (setcdr alist (cons (cons key val) (cdr alist))))
  alist)

(defun yacas-notebook-toggle-init ()
  "Toggle initialization marker of TYPE cell containing point."
  (interactive)
  (save-excursion
    (re-search-backward "^\\\\yacas")
    (goto-char (match-end 0))
    (if (looking-at "\\[\\* Initialization Cell \\*\\]")
        (delete-region (match-beginning 0) (match-end 0))
      (insert "[* Initialization Cell *]")
      )))

(defun yacas-notebook-update-all (arg)
  "Optionally update all cells.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (yacas-notebook-update nil nil nil)
    (yacas-notebook-update nil (y-or-n-p "Interactive update ? ") nil)))

(defun yacas-notebook-tex-update-all (arg)
  "Optionally update all cells and return output in TeX form.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (yacas-notebook-update nil nil t)
    (yacas-notebook-update nil (y-or-n-p "Interactive update ? ") t)))

(defun yacas-notebook-update-init (arg)
  "Optionally update all initialization cells.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (yacas-notebook-update "\\[\\* Initialization Cell \\*\\]" nil nil)
    (yacas-notebook-update "\\[\\* Initialization Cell \\*\\]" 
			   (y-or-n-p "Interactive update ? ") nil)))

(defun yacas-notebook-tex-update-init (arg)
  "Optionally update all initialization cells and return output in TeX form.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (yacas-notebook-update "\\[\\* Initialization Cell \\*\\]" nil t)
    (yacas-notebook-update "\\[\\* Initialization Cell \\*\\]" 
			   (y-or-n-p "Interactive update ? ") t)))

(defun yacas-notebook-create-cell ()
  "Insert cell in buffer."
  (interactive)
  (if (yacas-notebook-cell-p)
      (error "Cell already exists")
    (if (not (bolp))
        (progn
          (open-line 1)
          (forward-line 1)))
    (insert "\\yacas\n\n\\endyacas")
    (beginning-of-line)
    (previous-line 1)))

(defun yacas-notebook-forward-cell ()
  "Move to next cell."
  (interactive)
    (let ((cur-pos (point))
          (cell-pos (point-max))
          new-pos)
        (setq new-pos (yacas-notebook-next-cell-start))
        (if (not (equal new-pos cur-pos))
            (if (> new-pos cell-pos)
                nil
              (setq cell-pos new-pos)))
      (if (equal cell-pos (point-max))
          nil; No more cells
        (goto-char cell-pos)
       )))

(defun yacas-notebook-backward-cell ()
  "Move to previous cell."
  (interactive)
    (let ((cur-pos (point))
          (cell-pos (point-min))
          new-pos)
        (setq new-pos (yacas-notebook-previous-cell-start))
        (if (not (equal new-pos cur-pos))
            (if (< new-pos cell-pos)
                nil
              (setq cell-pos new-pos)))
      (if (equal cell-pos (point-min))
          nil ; No more cells
        (goto-char cell-pos)
       )))

(defun yacas-notebook-update (kind ask tex)
  "Optionally update all KIND cells.
If ASK is non-nil, then ask whether each KIND cell is to be updated,
else update each KIND cell.  If KIND is nil, update all cells.
If TEX is non-nil, then insert \\outputtex instead of \\output."
  (let (bypass display-start display-end cur-pos)
    (save-excursion
      (goto-char (point-min))
      (while (yacas-notebook-forward-cell)
        (forward-line -1)
        (if (not (looking-at (concat "^\\\\yacas" kind)))
            (progn
              (forward-line 1) ; Don't want the same cell next time
              nil) ; Wrong kind of cell
          ;; We have a cell of the right kind
          (setq display-start (point))
          (goto-char (yacas-notebook-cell-end))
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
                  (yacas-notebook-update-type-cell tex)
))
            (widen) ; If user aborts evaluation at prompt
            ) ; unwind-protect
          ) ; if in a valid cell
        ) ; while still types to check
      ) ; save-excursion
    (widen)
;    (beep)
    (message "Update of cells finished")
    ) ; let
  )

(defun yacas-notebook-eval-init ()
  "Evaluate all initialization cells, without returning the output."
  (interactive)
  (let (bypass display-start display-end cur-pos)
    (save-excursion
      (goto-char (point-min))
      (while (yacas-notebook-forward-cell)
        (forward-line -1)
        (if (not (looking-at "^\\\\yacas\\[\\* Initialization Cell \\*\\]"))
            (progn
              (forward-line 1) ; Don't want the same cell next time
              nil) ; Wrong kind of cell
          ;; We have a cell of the right kind
          (setq display-start (point))
          (goto-char (yacas-notebook-cell-end))
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
                (yacas-notebook-send-cell))
            (widen) ; If user aborts evaluation at prompt
            ) ; unwind-protect
          ) ; if in a valid cell
        ) ; while still types to check
      ) ; save-excursion
    (widen)
;    (beep)
    (message "Evaluation of initialization cells finished")
    ) ; let
  )

(defun yacas-notebook-cell-start ()
  "Return position of start of cell containing point."
  (let ((begin-re "^\\\\yacas"))
  (save-excursion
    (if (not (looking-at begin-re))
        (re-search-backward begin-re))
    (forward-line 1)
    (point))))

(defun yacas-notebook-cell-end ()
  "Return position of end of cell containing point."
  (let ((end-re "^\\\\endyacas"))
    (save-excursion
      (re-search-forward end-re)
      (forward-line -1)
      (end-of-line)
      (point))))

(defun yacas-notebook-previous-cell-start ()
  "Get start of preceding cell.  If none, return current position."
  (let ((cur-pos (point))
        (start nil)
        (begin-re "^\\\\yacas")
        (end-re "^\\\\endyacas"))
    (save-excursion
      (if (not (re-search-backward end-re (point-min) t))
          cur-pos
        (if (yacas-notebook-cell-p)
            (progn
              (re-search-backward begin-re)
              (forward-line 1)
              (point))
          cur-pos)))))
              
(defun yacas-notebook-next-cell-start ()
  "Get start of next cell.  If none, return current position."
  (let ((cur-pos (point))
        (start nil)
        (begin-re "^\\\\yacas")
        (end-re "^\\\\endyacas"))
    (save-excursion
      (if (re-search-forward begin-re (point-max) t)
          (progn
            (if (not (yacas-notebook-cell-p))
                cur-pos)
            (forward-line 1)
            (point))
        cur-pos))))

(defun yacas-notebook-cell-p ()
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
        
(defun yacas-notebook-newline ()
  "yacas-newline if in a cell, otherwise newline"
  (interactive)
  (if (yacas-notebook-cell-p)
      (yacas-newline)
    (newline)
))

(defun yacas-notebook-delete-output ()
  "Delete current output (if any).  Assumes point in cell.
Output assumed to follow input, separated by a 
yacas-notebook-output-marker line.  Input *may* contain blank lines."
  (interactive)
  (let ((out-start (yacas-notebook-output-p)))
    (if out-start
        (delete-region out-start (yacas-notebook-cell-end))
      t)))

(defun yacas-notebook-output-p ()
  "Return start of output text if present, else return nil.  Assumes
point in cell.  Output assumed to follow input, separated by a
\output."
  (save-excursion
    (goto-char (yacas-notebook-cell-start))
    (if (re-search-forward "^\\\\output"
         (yacas-notebook-cell-end) t)
        (progn
          (forward-line -1)
          (end-of-line)
          (point))
      nil)))

;;; @@ Yacas-Notebook functions for package assembly

(defun yacas-notebook-assemble (arg)
 "Assemble package (see yacas-notebook-assemble-package), or, with C-u prefix,
assemble references within a cell (see yacas-notebook-assemble-cell)."
  (interactive "P")
  (if arg
      (yacas-notebook-assemble-cell)
    (yacas-notebook-assemble-package)))

(defun yacas-notebook-assemble-cell (&optional delete)
  "Assemble references in cell to file with unique name.  The buffer used to
write the file is not deleted, unless optional DELETE is non-nil.
Return the filename."

  ;; Here is how this function works:

  ;; The text of the cell is written to a buffer with key `file:part'.  Then
  ;; the number of references in the cell is counted.  If the number of
  ;; references in the cell is less than yacas-notebook-max-references, 
  ;; then the cell references are resolved by successive calls to 
  ;; yacas-notebook-dereference-buffer
  ;; which collates the text for cell references as needed, using
  ;; yacas-notebook-collate-cells.  If the number of references is equal to or
  ;; greater than yacas-notebook-max-references, then all cells in the document
  ;; correpsonding to the current cell type and filename are collated into
  ;; buffers, using yacas-notebook-collate-cells, and then the all cell 
  ;; references are are resolved by successive calls to 
  ;; yacas-notebook-dereference-buffer.

  ;; The global`yacas-notebook-buffer-alist' associates buffer names with keys.
  ;; Buffer names are unique.  The names of all buffers are constructed with
  ;; `yacas-notebook-make-temp-name' and are unique.  All buffers except 
  ;; possibly the cell-buffer are deleted on exit.

  (interactive)
  (let ((home-buffer (current-buffer))
        files parts file part 
        ref-count
        cell-key cell-buffer tmp-alist tmp-buffer)
    (if (not (yacas-notebook-cell-p)) (error "Not in a cell"))
    (if (not (yacas-notebook-reference-p)) 
	(error "Cell contains no references"))
    (save-excursion
      (goto-char (yacas-notebook-cell-start))
      (forward-line -1)
      (if (not (looking-at "^\\\\yacas.*<.*:.*>"))
          (error "Cell is not marked"))

      (setq yacas-notebook-error-point (point))
      (if yacas-notebook-abbreviations-allowed
          (unwind-protect ; In case filename errors
              ;; This can take some seconds
              (progn
                (message "Getting filenames...")
                (setq files (yacas-notebook-get-filenames))
                (message "")
                )
            (goto-char yacas-notebook-error-point)))

      (setq file (yacas-notebook-get-filename files))
      (if (not file) (error "Ambiguous filename"))

      (if yacas-notebook-abbreviations-allowed
          ;; This can take several seconds for a document with many cells
          (progn
            (message "Getting partnames")
            (setq parts (yacas-notebook-get-partnames file files))
            (message "")
            ))

      (setq part (yacas-notebook-get-partname parts))
      (if  (not part) (error "Ambiguous partname"))

      ) ; save-excursion

    (setq cell-key (concat file ":"))
    (if (not (equal part "")) (setq cell-key (concat cell-key part)))
    (message "Assembling `%s' ..." cell-key) ; (sleep-for 1)
    (setq cell-buffer (yacas-notebook-make-temp-name))
    (setq yacas-notebook-buffer-alist (list (cons cell-key cell-buffer)))
    (unwind-protect
        (save-excursion
          (yacas-notebook-append-cell-to-buffer cell-buffer)
          (setq yacas-notebook-source-buffer (current-buffer)) 
                                                          ; Collate from here

          (if (< (yacas-notebook-reference-count cell-buffer) 
		 yacas-notebook-max-references)
              ;; Build reference buffers as needed
                (while 
		  (yacas-notebook-dereference-buffer cell-key files parts nil))
            ;; Prebuild all reference buffers
            (yacas-notebook-collate-cells file part files parts nil)
            (while 
              (yacas-notebook-dereference-buffer cell-key files parts nil nil))
            )
          (set-buffer cell-buffer)
          (write-file (concat yacas-notebook-temp-dir cell-buffer))
          (set-buffer home-buffer)
          )
      ;; unwind-protect forms: deleted cell buffers
      (setq tmp-alist yacas-notebook-buffer-alist)
      (while (setq tmp-buffer (cdr (car tmp-alist)))
        (setq tmp-alist (cdr tmp-alist))
        (condition-case nil ; In case buffer not actually created
            (if (and (not delete) (equal tmp-buffer cell-buffer))
                nil ; Don't delete the assembly buffer
              (kill-buffer tmp-buffer))
          (error nil)))
      ) ; unwind-protect
    (message "`%s' assembled in file `%s%s'" 
	     cell-key yacas-notebook-temp-dir cell-buffer)
    (concat yacas-notebook-temp-dir cell-buffer)
    ) ; let
  ) ; done

(defun yacas-notebook-assemble-package (&optional file overwrite)
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
  ;; yacas-notebook-dereference-buffer.

  ;; The global `yacas-notebook-buffer-alist' associates buffer names with keys.
  ;; Buffer names are unique.  The names of all buffers are constructed with
  ;; `yacas-notebook-make-temp-name' and are unique.    All buffers
  ;; except the package buffer `FILE' are deleted on exit.

  (interactive)
  (let ((home-buffer (current-buffer))
        files parts prompt
        tmp-buffer tmp-alist file-buffer
        )

    (if (not file)
        ;; If file has not been specifed, prompt
        (progn
              ;; Get default file from cell label, if any
              (save-excursion
                (goto-char (yacas-notebook-cell-start))
                (forward-line -1)
                (if (looking-at "^\\\\yacas.*<.*:.*>")
                    (progn
                      (setq yacas-notebook-error-point (point))
                      (unwind-protect ; In case filename errors
                          (if yacas-notebook-abbreviations-allowed
                              ;; This can take some seconds
                              (progn
                               (message "Getting filenames...")
                               (if (not (setq files 
					       (yacas-notebook-get-filenames)))
                                 (error "No complete package filenames found"))
                               (message "")
                               ))
                        (goto-char yacas-notebook-error-point))
                      (setq file (yacas-notebook-get-filename files)))))
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

    (if yacas-notebook-abbreviations-allowed
        ;; This can take several seconds for a document with many cells
        (progn
          (message "Getting partnames...")
          (setq parts (yacas-notebook-get-partnames file files))
          (message "")
          ))

    (message "Assembling package `%s' ..." file) ;(sleep-for 1)

    ;; Set where assembly will occur
    (setq file-buffer (yacas-notebook-make-temp-name))
    (setq yacas-notebook-buffer-alist (list (cons file file-buffer)))

    (unwind-protect ; So buffer can be deleted even if errors or abort
        (progn
          (setq yacas-notebook-source-buffer (current-buffer)) 
					; Collate from here
          (yacas-notebook-collate-cells file nil files parts nil)
          (or (get-buffer (cdr (assoc file yacas-notebook-buffer-alist)))
              (error "No `%s' cell `%s:' found" file))
          
          ;; OK, here we go:  Recursively dereference the cell buffer:
          (while (yacas-notebook-dereference-buffer file files parts))

          (set-buffer file-buffer)
          (write-file file)
          (set-buffer home-buffer)
          )
      ;; unwind-protect tail:  Delete part files
      (setq tmp-alist yacas-notebook-buffer-alist)
      (while (setq tmp-buffer (cdr (car tmp-alist)))
        (setq tmp-alist (cdr tmp-alist))
        (condition-case nil ; In case buffer not actually created
            (if (equal tmp-buffer file-buffer)
                nil ; Don't delete the package buffer
              (kill-buffer tmp-buffer))
          (error nil)))
      ) ; unwind-protect
    (message "Package `%s' assembled" file)
;    file
    (switch-to-buffer-other-window file)
    ) ; let
  ) ; done

(defun yacas-notebook-reference-count (buffer)
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
    count
    ))

(defun yacas-notebook-append-cell-to-buffer (buffer)
  "Append text of cell containing point to BUFFER.
Create BUFFER if it does not exist."
  (if (not (yacas-notebook-cell-p))
      (error "Not in a cell.")
    (let ((home-buffer (current-buffer))
          (start (yacas-notebook-cell-start))
          end)
      (save-excursion
	(goto-char start)
	(beginning-of-line)
        (while (looking-at "^ *$") (forward-line 1))
	(setq start (point))
	(if (not (setq end (yacas-notebook-output-p)))
	    (progn
	      (goto-char (yacas-notebook-cell-end))
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
        (insert "\n")
        ))))

(defun yacas-notebook-collate-cells (file part files parts &optional single)

  "Assemble cells marked with filename FILE in buffers with keys
`file:part' or, for part = null string (package cells), with key `file'.  The
names of all buffers are constructed with `yacas-notebook-make-temp-name' 
and are unique.  If PART is non-nil then do not collate cells with keys 
`FILE:PART' and `FILE' (package cells).  Use FILES and PARTS for name 
completion \(see `yacas-notebook-get-filename' and 
`yacas-notebook-get-partname'\).  If optional SINGLE is non-nil, then 
collate just cells `FILE:PART' (PART must be non-nil).

The global `yacas-notebook-buffer-alist' associates buffer names with keys.  
It must be initialized, typically with the buffer for key `FILE' or 
`FILE:PART', according to whether PART is nil or not."

  (let ((home-buffer (current-buffer))
        this-part this-file key
        )
    (unwind-protect ; For error location
        (setq yacas-notebook-error-point (point)) ; Go here if no error
        (progn

          ;; Scan buffer to construct buffers for all `file:part'
          (save-excursion
            (set-buffer yacas-notebook-source-buffer) ; Collate from here
            (goto-char (point-min))
            (while (yacas-notebook-forward-cell)
                   ;; We have a cell of the right type
                (forward-line -1) ; Move to \begin{...
                (if (not (looking-at "^\\\\yacas.*<.*:.*>"))
                    (forward-line 1) ; So we go to next cell next time through

                  ;; We have a marked cell
                  (setq this-file (yacas-notebook-get-filename files))
                  (cond
                   ((not this-file)
                    (setq yacas-notebook-error-point (point))
                    (error "Ambiguous filename"))
                   ((not (equal file this-file))
                    (forward-line 1)) ; So we go to next cell next time through
                   (t

                    ;; We have a cell of the right package filename
                    (setq this-part (yacas-notebook-get-partname parts))
                    (cond
                     ((not this-part)
                      (setq yacas-notebook-error-point (point))
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
                       (assoc key yacas-notebook-buffer-alist) ; buffer 
                                                              ;already created
                       (yacas-notebook-replace-assoc
                        yacas-notebook-buffer-alist
                        key (yacas-notebook-make-temp-name)))

                      ;; Append cell contents to its buffer
                      (yacas-notebook-append-cell-to-buffer
                       (cdr (assoc key yacas-notebook-buffer-alist)))
                      
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
      (goto-char yacas-notebook-error-point)
      ) ; unwind-protect
    ) ; let
  ) ; done

(defun yacas-notebook-dereference-buffer (key files parts &optional noinit)
  "Resolve all references in buffer corresponding to KEY in alist
yacas-notebook-buffer-alist, using FILES and PARTS for name completion.  
If optional NOINIT is nil, initialize global variable 
`yacas-notebook-dereference-path' with KEY.  If NOINIT is non-nil, 
add KEY to `yacas-notebook-dereference-path'. then references are collated 
in buffers and added to yacas-notebook-buffer-alist if necessary.  Use 
`yacas-notebook-dereference-path' to check for self-reference and
report error if detected,"
  (let ((ref-found nil)
        (home-buffer (current-buffer))
        path-to-here
        ref-indent ref-key ref-buffer
        (key-buffer (cdr (assoc key yacas-notebook-buffer-alist)))
        file part
        re-found
        )
    (or key-buffer (error "No cell `%s'" key))
    (set-buffer key-buffer)
    (goto-char (point-min))
    (if noinit
        t
      (setq noinit t)
      (setq yacas-notebook-dereference-path (list key))
      )
    (setq path-to-here yacas-notebook-dereference-path)
    (while (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (point-max) t)
      (setq re-found 1)
      (beginning-of-line)
      (setq ref-indent (yacas-notebook-get-reference-indentation))
      (setq file (yacas-notebook-get-filename files))
      (setq part (yacas-notebook-get-partname parts))
      (setq ref-key (concat file ":" part))
      (if (yacas-notebook-string-mem ref-key path-to-here)
            (yacas-notebook-dereference-error (cons ref-key path-to-here)))
      (setq yacas-notebook-dereference-path (cons ref-key path-to-here))
      (if (not (assoc ref-key yacas-notebook-buffer-alist))
          ;; Construct buffer on the fly
          (progn
            (setq ref-buffer (yacas-notebook-make-temp-name))
            (yacas-notebook-replace-assoc yacas-notebook-buffer-alist 
					  ref-key ref-buffer)
            (yacas-notebook-collate-cells file part files parts t)
            )
        (setq ref-buffer (cdr (assoc ref-key yacas-notebook-buffer-alist)))
        )
      (while (yacas-notebook-dereference-buffer ref-key files parts noinit))
      (kill-line 1) ; Remove reference line
      (insert-buffer ref-buffer)
      (let ((indent-start (point))
            indent-end)
        (exchange-point-and-mark)
        (setq indent-end (point))
        (exchange-point-and-mark)
        (if ref-indent (indent-rigidly indent-start indent-end ref-indent))
        )

      )
    (setq yacas-notebook-dereference-path path-to-here)
    (set-buffer home-buffer)
    ref-found
    ) ; let
  ) ; done

(defun yacas-notebook-dereference-error (path)
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
    (error "Self-reference detected")
    ))

(defun yacas-notebook-get-reference-indentation ()
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

(defun yacas-notebook-insert-complete-name ()
  "Insert complete name in buffer for cell.
Return t if successful, else nil."
  (interactive)
  (let ((here (point))
        start end name text files parts
        )
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
        (if (not (setq files (yacas-notebook-get-filenames)))
            (error "No package filenames in document"))
        (message "")

        (search-backward "<")
        (forward-char 1)
        (setq start (point))
        (search-forward ":")
        (forward-char -1)
        (setq text (buffer-substring start (point)))
        (if (not (setq name (yacas-notebook-complete-name text files)))
            (error "No matching package filename found"))

        ;; This can take several seconds for a document with many cells
        (message "Getting partnames")
        (setq parts (yacas-notebook-get-partnames name files))
        (message "")

        (forward-char 1)
        (setq start (point)) ; New start, for partname deletion
        (setq text (buffer-substring (point) here))
        (if (not (setq name (yacas-notebook-complete-name
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
        (if (not (setq files (yacas-notebook-get-filenames)))
            (error "No package filenames in document"))
        (message "")

        (re-search-backward "<")
        (forward-char 1)
        (setq start (point))
        (setq text (buffer-substring start here))
        (if (not (setq name (yacas-notebook-complete-name
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

(defun yacas-notebook-get-filenames ()
  "Return alist of package filenames for cells."
  (let (file files)
    (save-excursion
      (goto-char (point-min))
      (while (yacas-notebook-forward-cell)
          (forward-line -1)
          (if (not (looking-at (concat "^\\\\yacas.*<.*>")))
              (forward-line 1) ; Cell not marked.  Get set for next one
            (if (setq file (yacas-notebook-get-filename)) ; Only unabbreviated
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
    files
    )) ; let and done

(defun yacas-notebook-complete-name (text alist &optional exact)
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

(defun yacas-notebook-get-partnames (file files)
  "Return alist of partnames for package FILE, using FILES for
filename completion."
  (let (cell-end cell-file part parts)
    (setq yacas-notebook-error-point (point))
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (while (yacas-notebook-forward-cell)
              (setq cell-end (yacas-notebook-cell-end))
              (forward-line -1)
              (if (not (looking-at
                       "^\\\\yacas.*<[^:].*:.*>"))
                  (forward-line 1) ; Not a marked cell
                (setq cell-file (yacas-notebook-get-filename files))
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
                    (if (not (setq file (yacas-notebook-get-filename files)))
                        (progn
                          (setq yacas-notebook-error-point (point))
                          (error "Ambiguous filename")))
                    (if (not (equal cell-file file))
                        (progn
                          (setq yacas-notebook-error-point (point))
                          (error "Reference must match cell filename: `%s'"
                                 cell-file)))
                    (setq part (yacas-notebook-get-partname))
                    (if (not part)
                        nil ; Need full (unabbreviated) parts only, for alist
                      (if parts ; Update alist
                          (if (or
                               (equal part "")
                               (yacas-notebook-string-mem part parts))
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
      (goto-char yacas-notebook-error-point) ; unwind-protect form
      ) ; unwind-protect
    (setq parts (mapcar 'list parts)) ; Make list into an alist
    parts
    ) ; let
  ) ; done

(defun yacas-notebook-get-filename (&optional alist)
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
        (yacas-notebook-complete-name text alist t)
      (if (string-match abbrev-re text)
          (if yacas-notebook-abbreviations-allowed
              nil
            (setq yacas-notebook-error-point (point))
 (error "Set yacas-notebook-abbreviations-allowed (M-x set-variable) to use abbreviations")
            )
        text))))

(defun yacas-notebook-get-partname (&optional alist)
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
        (yacas-notebook-complete-name text alist t)
      (if (string-match abbrev-re text)
          (if yacas-notebook-abbreviations-allowed
              nil
            (setq yacas-notebook-error-point (point))
 (error "Set yacas-notebook-abbreviations-allowed (M-x set-variable) to use abbreviations")
            )
        text))))

(defun yacas-notebook-string-mem (element list) ; memq doesn't work for strings
  "Returns t if string ELEMENT is in LIST of strings, else returns nil."
  (let (try)
    (catch 'done
      (while (setq try (car list))
        (setq list (cdr list))
        (if (equal element try)
            (throw 'done t)))
      nil)))

(defun yacas-notebook-reference-p ()
  "Return t if cell contains a cell reference, else retrun nil."
  (save-excursion
    (goto-char (yacas-notebook-cell-start))
    (if 
       (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (yacas-notebook-cell-end) t)
        t
      nil)))

;;; @@ Yacas-Notebook functions for "yacas" cells

(defun yacas-notebook-send-line ()
  "Send the current line to the yacas process."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq bg (point))
    (end-of-line)
    (setq ed (point))
    (yacas-region bg ed)
    (if yacas-notebook-texform
	(yacas-string yacas-notebook-tex-string))))


(defun yacas-notebook-send-cell ()
  "Send input to process. Point must be in a cell."
  (interactive)
  (if (not (yacas-notebook-cell-p))
      (error "Not in Yacas cell"))
  (let ((home-buffer (current-buffer))
        assembled-file start end)
    (if (yacas-notebook-reference-p)
        (progn
          (widen) ; So cell references will be found
          (setq assembled-file (yacas-notebook-assemble-cell t)))
      (save-excursion
        (goto-char (yacas-notebook-cell-start))
        (setq start (point))
        (if (not (setq end (yacas-notebook-output-p)))
            (setq end (yacas-notebook-cell-end))))
      (progn
	(goto-char (yacas-notebook-cell-start))
        ;; Now I want to skip over any blank lines at the beginning of the cell
	(beginning-of-line)
	(while (looking-at "^ *$") (forward-line 1))
	(setq start (point))
        ;; as well as at the end of the cell
	(if (not (setq end (yacas-notebook-output-p)))
	    (progn
	      (goto-char (yacas-notebook-cell-end))
	      (while (looking-at "^ *$") (forward-line -1))
	      (end-of-line)
	      (setq end (point)))
	  (progn
	    (goto-char end)
	    (while (looking-at "^ *$") (forward-line -1))
	    (end-of-line)
	    (setq end (point))))
	)
      )
    (if assembled-file
	; Loading the assembled file will give the result of True;
	;(yacas-string (concat "Load\(\"" assembled-file "\"\);"))
	(let ((assembled-file-buffer) (assembled-file-buffer-contents))
	  (save-excursion
	    (set-buffer (find-file-noselect assembled-file))
	    (setq assembled-file-buffer-contents 
		  (buffer-substring-no-properties (point-min) (point-max))))
	  (setq assembled-file-buffer-contents 
		(yacas-replace-chars-in-string (string-to-char "\n")
				      (string-to-char " ") 
				      assembled-file-buffer-contents))
	  (yacas-string (concat "[" assembled-file-buffer-contents "];")))
      (yacas-region start end)
      )))


(defun yacas-notebook-copy-last-tex-output ()
  "Copy the last output from Yacas to the kill-ring,
removing the beginning \"$ and ending $\";"
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
	(re-search-backward yacas-input-prompt-string)
	(re-search-backward "\$\"")
	(setq out-end (point))
	(re-search-backward yacas-output-prompt-string)
	(re-search-forward "\"")
	(forward-char 1)
	(setq out-start (point))
	(copy-region-as-kill out-start out-end)))
    (pop-to-buffer old-buffer)))


(defun yacas-notebook-get-output (tex)
  "Insert last output from Yacas.
Assumes point in cell.  Output inserted at end of cell."
  (if tex
      (progn
	(yacas-string yacas-notebook-tex-string)
	(yacas-notebook-copy-last-tex-output)) 
    (yacas-copy-complete-last-output))
  (goto-char (yacas-notebook-cell-end))
  (forward-line 1)		 ; Insert marker before output
  (open-line 1)
  (if tex
      (insert "\\outputtex")					; ..
    (insert "\\output"))
  (forward-line 1)		 ; ..
;  (yank))
  (save-excursion
    (yank)
    (if tex
	(insert "\n"))))

(defun yacas-notebook-put-output ()
  " "
  (interactive)
  (yacas-notebook-delete-output)
  (yacas-notebook-get-output nil))

(defun yacas-notebook-replace (tex)
  "Replace output (if any) with last Yacas result. Point must be in a cell.
Output assumed to follow input, separated by a yacas-notebook-output-marker 
line."
  (if (not (yacas-notebook-cell-p))
      (error "Not in Yacas cell"))
  (save-excursion
    (yacas-notebook-delete-output)
    (yacas-notebook-get-output tex)
    ))

(defun yacas-notebook-update-type-cell (tex)
  "Send input to Yacas and replace output with result.
Point must be in cell.  Output assumed to follow input,
separated by a yacas-notebook-output-marker line."
  (if (not (yacas-notebook-cell-p))
      (error "Not in Yacas cell"))
  (yacas-notebook-send-cell)
  (yacas-notebook-replace tex))

(defun yacas-notebook-update-cell ()
  "Send input to Yacas and replace output with result.
Point must be in cell.  Output assumed to follow input,
separated by a yacas-notebook-output-marker line."
  (interactive)
  (yacas-notebook-update-type-cell nil))

(defun yacas-notebook-tex-update-cell ()
  "Send input to yacas and replace output with the result in TeX form.
Point must be in cell.  Output assumed to follow input,
separated by a yacas-notebook-output-marker line."
  (interactive)
  (setq yacas-notebook-texform t)
  (yacas-notebook-update-type-cell t)
  (setq yacas-notebook-texform nil))

(defun yacas-notebook-replace-line-with-tex ()
  "Sends the current line to Yacas, and then replaces it with the Yacas
output in TeX form."
  (interactive)
  (setq yacas-notebook-texform t)
  (yacas-notebook-replace-line)
  (setq yacas-notebook-texform nil))

(defun yacas-notebook-replace-line ()
  "Sends the current line to Yacas, and then replaces it with the Yacas
output."
  (interactive)
  (yacas-wait)
  (yacas-notebook-send-line)
  (yacas-wait)
  (beginning-of-line)
  (insert "% ")
  (end-of-line)
  (newline)
  (if yacas-notebook-texform
      (yacas-notebook-copy-last-tex-output)
    (yacas-copy-complete-last-output))
  (yank))

;; This next part isn't useful right not, but it may be
;; in the future ...
; (defun yacas-notebook-put-outputgraphics ()
;   "Insert environment outputgraphic. Assumes point in cell."
;   (interactive)
;   (if (not (yacas-notebook-cell-p))
;       (error "Not in Yacas cell"))
;   (yacas-notebook-delete-output)
;   (goto-char (yacas-notebook-cell-end))
;   (forward-line 1)		 ; Insert marker before output
;   (open-line 2)		 ; ..
;   (insert "\\outputgraphics")
;   (forward-line 1)
;   (insert (concat "\\mgraphics{" yacas-notebook-default-graphics-width "}{"))
;   (setq pt (point))
;   (insert "}")
;   (goto-char pt)
; 	)
;
; (defun yacas-notebook-graphics-update ()
;   "Sends the cell to yacas and inserts a graphics environment"
;   (interactive)
;   (yacas-notebook-send-cell)
;   (yacas-notebook-put-outputgraphics)
; )

(defun yacas-notebook-dont-show-yacas-buffer ()
  "Prevents the yacas buffer from automatically popping up."
  (interactive)
        (delete-other-windows)
	(setq pop-up-windows nil)
	(setq yacas-show-yacas-buffer nil))

(defun yacas-notebook-show-yacas-buffer ()
  "Pops the yacas buffer up."
  (interactive)
  (yacas-recenter-output-buffer nil)
  (setq pop-up-windows t)
  (setq yacas-show-yacas-buffer t))

;;; @@ The mode

(define-derived-mode yacas-notebook-mode tex-mode "Yacas-Notebook"
  "This is a mode intended to allow the user to write documents that
include Yacas code.  The file can be LaTeXed to produce nice 
looking output (although that isn't necessary, of course), and so the
mode is an extension of TeX-mode (AucTeX, if you use that) that also
includes all the functionality of yacas-mode.  (\"TeX-command-master\", 
however, has been rebound to \"C-cC-cC-c\".)
The units of Yacas code that are worked with are \"cells\", which are 
delimited by \"\\yacas\" and \"\\endyacas\". The cells can be evaluated 
individually, as a group, and the output can be returned as Yacas output 
or in TeX form.  Evaluating a cell and returning the output is called 
\"updating\" the cell.  This mode also supports some literate programming 
constructs.  (See the file \"Yacas-NotebookIntro.tex\" for more 
information.)
The commands for working with cells are:
  C-c C-c o  create a cell         C-c C-c d  delete the cell's output
  C-c C-c u  update a cell         C-c C-c U  update a cell in TeX form
  C-c C-c a  update all the cells  C-c C-c A  update all the cells in TeX form
  C-c C-c +  go to the next cell   C-c C-c -  go to the previous cell
  C-c C-c q  toggle initialization cells
  C-c C-c Q  evaluate all  initialization cells
  C-c C-c v  update all the initialization cells
  C-c C-c V  update all the initialization cells in TeX form
  C-c C-c =  assemble a cell with references
  C-c C-c @  assemble a cell which defines a package
(With a prefix, C-c C-c a and C-c C-c A will update the cells without
prompting)
Since the Yacas output can be returned to the Yacas-Notebook buffer, by default
the buffer which runs the Yacas process is not shown.  This can be changed.
  C-c C-c B  show the Yacas buffer
  C-c C-c b  don't show the Yacas buffer.
Single lines can be evaluated:
  C-c C-c n replace the current line with Yacas output
  C-c C-c N replace the current line with Yacas output in TeX form.

Finally, the command C-c C-c M will insert a %-*-Yacas-Notebook-*- at the beginning
of the file (if there isn't one there already) so the file will begin in
yacas-notebook-mode next time it's opened.

\\{yacas-notebook-mode-map}
"
  (font-latex-setup)
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex)
  (make-local-variable 'ispell-tex-p)
  (setq ispell-tex-p t)
  (setq yacas-use-filter t)
  (yacas-notebook-dont-show-yacas-buffer)
  (run-hooks 'yacas-notebook-mode-hook))

;; The keymap
(define-key yacas-notebook-mode-map "\C-c\C-c" nil)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-c" 
  'TeX-command-master)
(define-key yacas-notebook-mode-map "\C-c\C-cM"  
  'yacas-notebook-mark-file-as-yacas-notebook)
(define-key yacas-notebook-mode-map "\C-m"  
  'yacas-notebook-newline)
(define-key yacas-notebook-mode-map "\C-c\C-cn" 
  'yacas-notebook-replace-line)
(define-key yacas-notebook-mode-map "\C-c\C-cN" 
  'yacas-notebook-replace-line-with-tex)
(define-key yacas-notebook-mode-map "\C-c\C-co" 
  'yacas-notebook-create-cell)
(define-key yacas-notebook-mode-map "\C-c\C-c-" 
  'yacas-notebook-backward-cell)
(define-key yacas-notebook-mode-map "\C-c\C-c+" 
  'yacas-notebook-forward-cell)
(define-key yacas-notebook-mode-map "\C-c\C-cs" 
  'yacas-notebook-send-cell)
(define-key yacas-notebook-mode-map "\C-c\C-cd" 
  'yacas-notebook-delete-output)
(define-key yacas-notebook-mode-map "\C-c\C-cu" 
  'yacas-notebook-update-cell)
(define-key yacas-notebook-mode-map "\C-c\C-cU" 
  'yacas-notebook-tex-update-cell)
(define-key yacas-notebook-mode-map "\C-c\C-ca" 
  'yacas-notebook-update-all)
(define-key yacas-notebook-mode-map "\C-c\C-cA" 
  'yacas-notebook-tex-update-all)
(define-key yacas-notebook-mode-map "\C-c\C-cb" 
  'yacas-notebook-dont-show-yacas-buffer)
(define-key yacas-notebook-mode-map "\C-c\C-cB" 
  'yacas-notebook-show-yacas-buffer)
(define-key yacas-notebook-mode-map "\C-c\C-cq" 
  'yacas-notebook-toggle-init)
(define-key yacas-notebook-mode-map "\C-c\C-cv" 
  'yacas-notebook-update-init)
(define-key yacas-notebook-mode-map "\C-c\C-cV" 
  'yacas-notebook-tex-update-init)
(define-key yacas-notebook-mode-map "\C-c\C-cQ" 
  'yacas-notebook-eval-init)
;(define-key yacas-notebook-mode-map "\C-c\C-cG" 
;  'yacas-notebook-graphics-update)
;(define-key yacas-notebook-mode-map "\C-c\C-cg" 
;  'yacas-notebook-put-outputgraphics)
;    (define-key yacas-notebook-mode-map "\e\t"  
;  'yacas-notebook-insert-complete-name)
(define-key yacas-notebook-mode-map "\C-c\C-c=" 
  'yacas-notebook-assemble-cell)
(define-key yacas-notebook-mode-map "\C-c\C-c@" 
  'yacas-notebook-assemble-package)
(define-key yacas-notebook-mode-map "\C-c\C-m" 
  'yacas-newline)
(define-key yacas-notebook-mode-map "\C-c\C-?" 
  'yacas-untab)
(define-key yacas-notebook-mode-map "\C-c\C-i" 
  'yacas-tab)
;; The control constructs
(define-key yacas-notebook-mode-map "\C-c\C-c\C-e" 
  'yacas-else)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-f" 
  'yacas-for)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-a" 
  'yacas-foreach)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-i" 
  'yacas-if)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-v" 
  'yacas-local)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-p" 
  'yacas-proc)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-w" 
  'yacas-while)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-u" 
  'yacas-until)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-n" 
  'yacas-function)
;; Other formatting commands
(define-key yacas-notebook-mode-map "\C-c\C-c#" 
  'yacas-short-comment)
(define-key yacas-notebook-mode-map "\C-c\C-c]" 
  'yacas-indent-region)
;; Motion commands
(define-key yacas-notebook-mode-map "\C-c\C-c<" 
  'yacas-backward-to-same-indent)
(define-key yacas-notebook-mode-map "\C-c\C-c>" 
  'yacas-forward-to-same-indent)
;; The run Yacas commands
(define-key yacas-notebook-mode-map "\C-c\C-c\C-y" 
  'yacas-start-process)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-r" 
  'yacas-region)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-b" 
  'yacas-buffer)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-l" 
  'yacas-line)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-k" 
  'yacas-kill-job)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-o" 
  'yacas-copy-last-output)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-s" 
  'yacas-copy-complete-last-output)
(define-key yacas-notebook-mode-map "\C-c\C-c\C-t" 
  'yacas-recenter-output-buffer)

;; Some more fontlocking
;; First, fontify the \yacas and \endyacas

(if (fboundp 'font-lock-add-keywords)
    (progn
      (defun yacas-notebook-font-lock-cell (limit)
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
			      '((yacas-notebook-font-lock-cell
				 (0 font-lock-function-name-face append t))))

      (font-lock-add-keywords 'yacas-notebook-mode 
	    '(("\\(\\\\\\(endyacas\\|output\\(tex\\)?\\|yacas\\)\\)"
	       . font-lock-keyword-face)))))

;; Now, the menu

(easy-menu-define yacas-notebook-mode-menu yacas-notebook-mode-map 
		  "Yacas-Notebook mode menu"
   '("Yacas-Notebook"
    ("Cell operations"
     ["Create cell"  yacas-notebook-create-cell]
     ["Toggle initialization"  yacas-notebook-toggle-init]
     ["Forward cell"  yacas-notebook-forward-cell]
     ["Backwards cell"  yacas-notebook-backward-cell])
    ("Yacas evaluation"
     ["Update line"   yacas-notebook-replace-line]
     ["Update cell"   yacas-notebook-update-cell]
     ["Evaluate initialization cells"  yacas-notebook-eval-init]
     ["Update initialization cells"  yacas-notebook-update-init]
     ["Update all cells"  yacas-notebook-update-all]
     ["Toggle PrettyForm" yacas-toggle-prettyform ])
    ("Yacas TeX evaluation"
     ["Update line"   yacas-notebook-replace-line-with-tex]
     ["Update cell"  yacas-notebook-tex-update-cell]
     ["Update initialization cells"  yacas-notebook-tex-update-init]
     ["Update all cells"  yacas-notebook-tex-update-all])
    ("Yacas evaluate only"
     ["Evaluate region"  yacas-region]
     ["Evaluate cell"  yacas-notebook-send-cell])
    ("Yacas formatting"
     ["Insert procedure"  yacas-proc t]
     ["Insert new local variable"  yacas-local t]
     ["Insert function" yacas-function t]
     ["If"  yacas-if t]
     ["Else"  yacas-else t]
     ["For"  yacas-for t]
     ["ForEach"  yacas-foreach t]
     ["While"  yacas-while t]
     ["Until"  yacas-until t]
     ["Inline comment"  yacas-short-comment]
     ["Indent region"   yacas-indent-region])
    ("Web"
     ["Assemble cell"  yacas-notebook-assemble-cell]
     ["Assemble package"  yacas-notebook-assemble-package])
    ("Miscellaneous"
     ["Start a Yacas process"   yacas-start-process]
     ["Delete output"   yacas-notebook-delete-output]
     ["Show Yacas buffer"  yacas-notebook-show-yacas-buffer]
     ["Dont show Yacas buffer"  yacas-notebook-dont-show-yacas-buffer]
     ["Kill Yacas process"  yacas-kill-job]
     ["Mark file as Yacas-Notebook"  
                            yacas-notebook-mark-file-as-yacas-notebook])))

(provide 'yacas-notebook)
;;; yacas-notebook.el ends here
