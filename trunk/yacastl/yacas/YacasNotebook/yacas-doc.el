;;; yacas-doc.el --- Major modes for writing Yacas documentation

;; Copyright (C) 2002 Jay Belanger

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

;;; Commentary:

;; Quick intro
;;
;; To install, put this file somewhere in your emacs load path.
;; To make sure that `yacas-doc.el' is loaded when necessary,
;; put the line
;;  (autoload 'yacas-doc "yacas-doc" "Yacas Documentation mode" t)
;; in your `.emacs' file.  If you want any file ending in `.ys.txt' to begin
;; in `yacas-doc-mode', for example, put the line
;; (setq auto-mode-alist (cons '("\\.ys.txt" . yacas-doc-mode) auto-mode-alist))
;; to your `.emacs' file.
;;
;; Yacas documentation mode is basically text-mode with support for
;; highlighting the documentation commands.  C-h m will give a brief
;; reminder of what these commands are.

(require 'font-lock)
(provide 'yacas-doc)

(defvar yacas-doc-tab-face 'yacas-doc-tab-face)
(defface yacas-doc-tab-face 
  '((((background light)) (:foreground "Grey"))
    (((background dark)) (:foreground "Grey")))
  "Font Lock mode face used to highlight tabs."
  :group 'yacas-doc)

(defvar yacas-doc-keyword-face 'yacas-doc-keyword-face)
(defface yacas-doc-keyword-face 
  '((((background light)) (:foreground "Magenta" :bold t))
    (((background dark)) (:foreground "LightSteelBlue" :bold t))
    (t (:bold t)))
  "Font Lock mode face used to highlight keywords."
  :group 'yacas-doc)

(defvar yacas-doc-delimiter-face 'yacas-doc-delimiter-face)
(defface yacas-doc-delimiter-face 
  '((((background light)) (:foreground "Blue"))
    (((background dark)) (:foreground "LightBlue"))
    (t (:bold t)))
  "Font Lock mode face used to highlight keywords."
  :group 'yacas-doc)

(defvar yacas-doc-heading-face 'yacas-doc-heading-face)
(defface yacas-doc-heading-face
  '((((background light)) 
     (:foreground "Blue" :bold t ))
    (((background dark)) 
     (:foreground "LightBlue" :bold t ))
    (t (:bold t)))
  "Font Lock mode face used to highlight headings in Yacas doc."
  :group 'yacas-doc)

(defvar yacas-doc-argument-face 'yacas-doc-argument-face)
(defface yacas-doc-argument-face
  '((((background light)) 
     (:foreground "Magenta" :italic t))
    (((background dark)) 
     (:foreground "LightSteelBlue" :italic t))
    (t (:bold t :italic t)))
  "Font Lock mode face used to highlight headings in Yacas doc."
  :group 'yacas-doc)

(defvar yacas-doc-code-face 'yacas-doc-code-face)
(defface yacas-doc-code-face
  '((((background light)) (:foreground "Brown"))
    (((background dark)) (:foreground "burlywood"))
    (t (:bold t :italic t)))
  "Font Lock mode face used to highlight code in Yacas doc."
  :group 'yacas-doc)

(defvar yacas-doc-italic-face 'yacas-doc-italic-face)
(defface yacas-doc-italic-face 
  '((((background light)) (:foreground "Blue" :italic t))
    (((background dark)) (:foreground "LightBlue" :italic t))
    (t (:italic t)))
  "Font Lock mode face used to highlight italic LaTeX."
  :group 'yacas-doc)

(defvar yacas-doc-typewriter-face 'yacas-doc-typewriter-face)
(defface yacas-doc-typewriter-face
  '((((background light)) (:foreground "Blue"))
    (((background dark)) (:foreground "LightBlue"))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight tt font in Yacas doc."
  :group 'yacas-doc)

(defvar yacas-doc-web-link-face 'yacas-doc-web-link-face)
(defface yacas-doc-web-link-face
  '((((background light)) (:foreground "Blue" :italic t))
    (((background dark)) (:foreground "LightBlue" :italic t))
    (t (:italic t)))
  "Font Lock mode face used to highlight web links in Yacas doc."
  :group 'yacas-doc)

(defvar yacas-doc-math-face 'yacas-doc-math-face)
(defface yacas-doc-math-face 
    '((((background light)) (:foreground "Brown" :italic t))
      (((background dark))  (:foreground "burlywood" :italic t))
      (t (:underline t)))
    "Font Lock mode face used to highlight math in Yacas-doc."
    :group 'yacas-doc)

(defvar yacas-doc-math-delimiter-face 'yacas-doc-math-delimiter-face)
(defface yacas-doc-math-delimiter-face 
    '((((background light)) (:foreground "Brown"))
      (((background dark))  (:foreground "burlywood"))
      (t (:underline t)))
    "Font Lock mode face used to highlight math in Yacas-doc."
    :group 'yacas-doc)

(defconst yacas-doc-labels
  (concat "^"
          (regexp-opt (list
                       "*A"
                       "*BOOK"
                       "*EVAL"
                       "*HEAD"
                       "*FOOT"
                       "*INCLUDE"
                       "*BLURB"
                       "*INTRO"
                       "*REM"
                       "*CMD"
                       "*FUNC"
                       "*SEE"
                       "*STD"
                       "*UNIX"
                       "*MSWIN"
                       "*MAC"
                       "*CORE"
                       "*CALL"
                       "*PARMS"
                       "*DESC"
                       "*EG"))))

(defconst yacas-doc-keywords
  (list
   ;; Labels
   (list yacas-doc-labels 0 yacas-doc-keyword-face)
   (list "^\\(\\*\t\\)[^0-9]" 1 yacas-doc-keyword-face)
   (list "^\\(\\*\t[0-9]+.\\)" 1 yacas-doc-keyword-face)
   ;; Delimiters
   (list "<i>\\|</i>\\|{\\|}\\|<\\*\\|\\*>" 0 yacas-doc-delimiter-face)
   ;; Headings
   (list "^\t\t\t\t\\([^\t].*\\)$" 1 yacas-doc-heading-face t) ;; Book headings
   (list "^\t\t\t\\([^\t].*\\)$" 1 yacas-doc-heading-face t) ;; Chapter headings
   (list "^\t\t\\([^\t].*\\)$" 1 yacas-doc-heading-face t) ;; Section heading
   (list "^\t    \\(.*\\)$" 1 yacas-doc-heading-face t) ;; Subsection heading
   ;; Italics
   (list "<i>\\(.*\\)</i>" 1 yacas-doc-italic-face t)
   ;; Typewriter face
   (list "{\\([^}]*\\)}" 1 yacas-doc-typewriter-face t)
   ;; Web lines
   (list "<\\*\\(.*\\)\\*>" 1 yacas-doc-web-link-face)
   ;; Tabs
   (list "\t" 0 yacas-doc-tab-face t)
   ;; Math
   (list "\\$" 0 yacas-doc-math-delimiter-face t)
   (list "\\$\\([^\\$]*\\)\\$" 1 yacas-doc-math-face t)
   (list "\\$\\$\\([^\\$]*\\)\\$\\$" 1 yacas-doc-math-face t)
   ;; Sample code
   (list "^\t\\([^\t].*\\)$" 1 yacas-doc-code-face t)
   ;; Include lines, etc.
   (list (concat yacas-doc-labels "\\(.*\\)$") 1 yacas-doc-argument-face)))

(defvar yacas-doc-tab-display "<t>")
(defvar yacas-doc-mode-map nil)

(if yacas-doc-mode-map nil
  (setq yacas-doc-mode-map (make-sparse-keymap))
  (define-key yacas-doc-mode-map (kbd "TAB") 
                                  (lambda () 
                                    (interactive)
                                    (insert "\t"))))

(define-derived-mode yacas-doc-mode text-mode "Yacas Documentation"
;(defun yacas-doc-mode ()
"A Quick reminder of Yacas Doc markup.

Sample code:
<tab> Code
Items:
*<tab> Item
Enumerated items:
*<tab>[0-9]+. Item
Emphasized text:
<i> Text </i>
Typewriter text:
{Text}
Web hyperlink:
<*Link*>
Math:
$Math$ or $$Math$$

Headings:
<tab><tab><tab><tab> Book heading
<tab><tab><tab> Chapter heading
<tab><tab> Section heading
<tab><spc><spc><spc><spc> Subsection heading
*HEAD Lower level heading

Labels that affect the current line:
*A anchor  (for anchor and index entry)
*BOOK title (start a book, give a title)
*EVAL statement (evaluate inline as a Yacas statement, insert results)
*HEAD heading (low level heading, lower than subsection)
*FOOT text (footnote)
*INCLUDE filename (include another documentation file)

Labels that affect the subsequent paragraph:
*BLURB  (short summary of book, must follow *BOOK label)
*INTRO (chapter introduction)
*REM (comment)

Special labels that accept several arguments:
*CMD command name and one line description
*FUNC command name and one line description
*SEE  see alsos

Special labels without arguments
*STD (standard library)
*UNIX (Unix specific)
*MSWIN (MSWindows specific)
*MAC (MacIntosh specific)
*CORE (core function)
*CALL (calling format)
*PARMS (parameters)
*DESC (description)
*E.G. (examples)
*EG (example)
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map yacas-doc-mode-map)
  (setq major-mode 'yacas-doc-mode)
  (setq mode-name "Yacas Documentation")
  (make-variable-buffer-local 'tab-width)
  (setq tab-width (length yacas-doc-tab-display))
  (if (null buffer-display-table)
      (setq buffer-display-table (copy-sequence standard-display-table)))
  (aset buffer-display-table ?\t (vconcat yacas-doc-tab-display))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(yacas-doc-keywords)))
