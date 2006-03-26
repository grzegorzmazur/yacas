;;; font-latex.el --- LaTeX fontification for Font Lock mode.

;; Copyright (C) 1996 Peter S. Galbraith
 
;; Authors:    Peter S. Galbraith <galbraith@mixing.qc.dfo.ca>
;;             Simon Marshall <Simon.Marshall@esrin.esa.it>
;; Maintainer: Peter S. Galbraith <galbraith@mixing.qc.dfo.ca>
;; Created:    06 July 1996
;; Version:    0.504 (20 Oct 97)
;; Keywords:   LaTeX faces

;; RCS $Id: font-latex.el,v 1.3 2006-03-26 12:49:13 ayalpinkus Exp $
;; Note: RCS version number does not correspond to release number.

;; LCD Archive Entry: (Not yet submitted!)
;; font-latex|Peter Galbraith|galbraith@mixing.qc.dfo.ca|
;; LaTeX fontification for font-lock|
;; 06-Jul-1996|0.01|~/modes/font-latex.el|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;  This package enhances font-lock fontification patterns for LaTeX.

;; New versions of this package (if they exist) may be found at:
;;  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/font-latex.el

;; Description:
;;  This package enhances font-lock fontification patterns for LaTeX.
;;  font-lock mode is a minor mode that causes your comments to be
;;  displayed in one face, strings in another, reserved words in another,
;;  and so on.
;;
;;  Please see the accompanying file font-latex.tex for a demo of what
;;  font-latex is supposed to do at different fontification levels.

;; Installation instructions:
;;
;;  Put this file in your emacs load-path, and byte-compile it:
;;    M-x byte-compile-file
;;  ** It runs faster when you byte-compile it! **
;;
;;  Then all you need to do is add this form to your .emacs file:
;;
;;    (if window-system
;;        (require 'font-latex))
;;
;; Turning on font-latex:
;;
;;   After font-latex is loaded (or `required'), it will be automatically
;;   used whenever you enter `font-lock-mode' on a LaTeX buffer. Therefore,
;;   I direct you to the file font-lock.el that comes with Emacs for more
;;   info.
;;
;; Turning on Font-lock automatically:
;;
;;  If you choose to turn-on font-lock by default using a mode-hook,
;;  there is an order to respect with-respect-to loading font-latex.  
;;  Do either:
;;
;;    (if window-system
;;        (progn
;;          (require 'font-latex)
;;          (add-hook 'latex-mode-hook 'turn-on-font-lock 'append)
;;          (add-hook 'LaTeX-mode-hook 'turn-on-font-lock 'append)))
;;  or
;;    (if window-system
;;        (progn
;;          (add-hook 'latex-mode-hook 'turn-on-font-lock)
;;          (add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
;;          (require 'font-latex)))
;;
;;  It's probably not a bad idea to always append 'turn-on-font-lock
;;  such that it is always sure to be last.
;;
;; Fontification Levels:
;;
;;  There are two levels of fontification, selected by the value of the
;;  font-lock variable font-lock-maximum-decoration.  There are ways
;;  documented in font-latex.el to set this differently for each mode that
;;  uses font-lock, but if you are unsure and are running on a fast enough
;;  machine, try putting this in your ~/.emacs file: 
;;    (setq font-lock-maximum-decoration t) 
;;  It probably best to put it before the (require 'font-latex) statement.
;;
;; Changing colours
;;
;;  Okay, so you hate the colours I picked.  How do you change them you ask?
;;  First, find the font name to change using the command:
;;    M-x list-text-properties-at
;;  Then, suppose you got `font-latex-math-face', edit ~/.Xdefaults and add:
;;    Emacs.font-latex-math-face.attributeForeground: blue
;;  without the semi-colon I'm using here ascomment delimiters, of course.
;;
;; Lazy-lock users:
;;
;;  lazy-lock and font-latex don't work too well together (up to Emacs 19.33
;;  and XEmacs 19.14 anyway).  font-latex uses functions to find text to
;;  fontify that may span more than one line, and this doesn't suit
;;  lazy-lock's search limits too well.  Recent versions of font-latex are
;;  a bit better, and perhaps you can live with the occasional 
;;  mis-fontification.
;;
;; Old hilit19 (and hilit-LaTeX) users:
;;
;;  If you are upgrading from using hilit-LaTeX.el or were using hilit19,
;;  you must disable hilit19 (at least for latex mode) in order to use
;;  font-latex.el.  Here's how:
;;  
;;  - If you don't care to use hilit19 at all, don't `load' or `require' it 
;;    in your ~/.emacs file by removing the "(require 'hilit-LaTeX)" line.
;;  - If you wish to use hilit19 everywhere but in latex mode, add the 
;;    following before your `load' or `require' hilit19:
;;
;;    (setq hilit-mode-enable-list  '(not latex-mode))
;;
;;  You can tell you are using font-latex instead of hilit-LaTeX because:
;;
;;  - colours will be different 
;;  - You'll see a message like `Fontifying font-latex.tex...done' 
;;    instead of `highlighting 1: \(^\|[^\\]\)\(\\[a-zA-Z\\]+\)'
;; ----------------------------------------------------------------------------
;;; Change log:
;; V0.504 20Oct97 Kevin Ruland <kruland@seistl.com> (RCS V1.46)
;;    Fixed the real bug in font-latex-match-command-outside-arguments
;; V0.503 16Oct97 PSG (RCS V1.45)
;;    Patched font-latex-match-command-outside-arguments for allow for
;;    strange interaction with AUC-TeX's LaTeX-environment command.
;; V0.502 07Oct97 (RCS V1.44)
;;    Kevin Ruland <kevin@rodin.wustl.edu> edits font-latex-find-matching-close
;;    PSG: Changed OliveGreen for OliveDrab, found in rgb.txt
;; V0.501 24Sep97 (RCS V1.42)
;;    Kevin Ruland <kevin@rodin.wustl.edu> added font-latex-find-matching-close
;;    used instead of scan-sexp to find arguments containing extra brackets.
;; V0.500 23Sep97 PSG (RCS V1.41)
;;  - Support for Emacs-20 (No customize support yet)
;; V0.403 19Nov96 (RCS V1.37)
;;  - Christoph Wedler <wedler@fmi.uni-passau.de>
;;    XEmacs patch for local math-font 
;;  - Changed scheme for fontification of \section*{...}  
;; V0.402 13Nov96 PSG (RCS V1.35)
;;  - Embeded comments handled.
;;  - Better XEmacs initilisation.
;; V0.401 12Nov96 PSG (RCS V1.34) - Nothing fontified when commented-out. 
;; V0.400 11Nov96 PSG (RCS V1.33) 
;;  - Stab at on-the-fly multiline.
;;  - mono support: <Johannes.Weinert@Informatik.Uni-Oldenburg.DE>
;; V0.314 16Oct96 PSG - Support for dark background removed for XEmacs.
;; V0.313 07Oct96 PSG (RCS V1.31) - Support for dark background.
;; V0.312 26Aug96 PSG (RCS V1.30) - Added font-latex-commented-outp.
;; V0.311 22Aug96 PSG (RCS V1.29) - fixed for XEmacs.
;; V0.310 22Aug96 simon (RCS V1.27)
;;  - make font-latex-setup run font-lock-make-faces before variable trickery.
;;  - set font-latex-string-face to the global value of font-lock-string-face.
;; V0.309 21Aug96 PSG (RCS V1.26)
;;  - new font-latex-math-face done by string syntax.  User may modify it.
;;  - new font-latex-string-face.
;; V0.308 15Aug96 PSG (RCS V1.25) 
;;  - $$...$$ gets font-latex-math-face
;;  - font-latex-match-math-envII fixed.
;; V0.307 14Aug96 PSG (RCS V1.23) - setup okay if loaded in a latex-mode-hook
;; V0.306 14Aug96 PSG (RCS V1.22) - added "item" to font-latex-match-function
;; V0.305 14Aug96 PSG (RCS V1.20) - use keep in font-latex-match-math-envII
;; V0.304 14Aug96 PSG (RCS V1.18) - minor comment edits.
;; V0.303 14Aug96 simon (RCS V1.17)
;;  - rewrote font-latex-match-math-envII like font-latex-match-quotation
;; V0.302 12Aug96 PSG (RCS V1.16)
;;  - (goto-char end) in condition-case error to avoid infinite loops.
;; V0.301 08Aug96 PSG (RCS V1.14)
;;  - Better faces in XEmacs.
;; V0.300 07Aug96 PSG (RCS V1.12)
;;  - Changed font-latex-match-font-inside-braces again for stranded \bf
;;  - "[a-z]+box" changed
;;  - font-latex-match-math-env checks preceding-char for \\[
;;  - use eval-after-compile in font-latex-match-math-envII 
;; V0.201 05Aug96 PSG added \\(display\\)?math to Simon's changes 
;; V0.200 05Aug96 simon: (RCS V1.10)
;;  - fixed font-latex-match-command-outside-arguments
;;  - rewrote font-latex-match-font-outside-braces like above
;;  - rewrote font-latex-match-font-inside-braces like above
;; V0.101 01Aug96 PSG added \\(display\\)?math
;; V0.100 01Aug96 PSG - massive new test version
;; V0.061 23Jul96 PSG
;;  - Removed trailing "\\>" in warning-face regexp (fails with \\ \- \\*)
;; V0.06  23Jul96 PSG
;;  - fixed dobib in font-latex-labels.
;;  - shorter font regexp in levels 3+4.
;;  - removed \item and & from type
;;  - fixed font-latex-math-envII regexp
;; V0.05  22Jul96 PSG
;;  - changed \ref etc to reference-face.
;;  - \\b added in buggy \item[option] regexp (not really fixed).
;;  - font-latex-labels regexp bug
;; V0.041  simon:
;;  - added font-latex-match-command-outside-arguments
;;  - rewrote font-latex-match-quotation and font-latex-bib-highlight-mouse
;;  - rewrote then removed bib-cite functionality.
;;  - general top-level cleanup
;; V0.04 11Jul96 PSG
;;  - added font-lock-comment-start-regexp defined in 19.32
;;  - encoded 8-bit characters to 7-bit.
;; V0.03 10Jul96 PSG
;;  - font-latex-bib-cite-mouse-highlight-p can change after font-lock-defaults
;;    is constructed.
;; V0.02 09Jul96 PSG 
;;  - added font-latex-bib-cite-mouse-highlight-p
;;  - Fixed `overwrite' flags
;; V0.01 06Jul96 Peter S Galbraith - Created
;; ----------------------------------------------------------------------------
;;; Code:
(require 'font-lock)

(defvar font-latex-warning-face			'font-latex-warning-face
  "Face to use for LaTeX major keywords.")
(defvar font-latex-sedate-face			'font-latex-sedate-face
  "Face to use for LaTeX minor keywords.")
(defvar font-latex-italic-face			'font-latex-italic-face
  "Face to use for LaTeX italics.")
(defvar font-latex-bold-face			'font-latex-bold-face
  "Face to use for LaTeX bolds.")
(defvar font-latex-math-face			'font-latex-math-face
  "Face to use for LaTeX math environments.")

;; End-User can stop reading here.

;; Make sure font-latex.el is supported.  I don't claim to have tested this...
(if (if (save-match-data (string-match "Lucid\\|XEmacs" (emacs-version)))
	(and (= emacs-major-version 19) (< emacs-minor-version 14))
      (and (= emacs-major-version 19) (< emacs-minor-version 29)))
    (error "`font-latex' was written for Emacs 19.29/XEmacs 19.14 or later"))

(defvar font-latex-is-XEmacs
  (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))))

(defvar font-latex-is-Emacs20
  (and (not font-latex-is-XEmacs) (= 20 emacs-major-version)))

(defvar font-latex-string-face nil
  "Face to use for strings.  This is set by Font LaTeX.")

(defvar font-lock-comment-start-regexp nil
  "Regexp to match the start of a comment.")

(eval-when-compile
  (require 'cl))

(cond
 (font-latex-is-Emacs20
  (defface font-latex-bold-face
    '((((class grayscale) (background light)) (:foreground "DimGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :bold t))
      (((class color) (background light)) 
       (:foreground "DarkOliveGreen" :bold t ))
      (((class color) (background dark)) (:foreground "OliveDrab" :bold t ))
      (t (:bold t)))
    "Font Lock mode face used to bold LaTeX."
    :group 'font-latex-highlighting-faces)
  
  (defface font-latex-italic-face
    '((((class grayscale) (background light)) 
       (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) 
       (:foreground "LightGray" :italic t))
      (((class color) (background light)) 
       (:foreground "DarkOliveGreen" :italic t ))
      (((class color) (background dark)) 
       (:foreground "OliveDrab" :italic t ))
      (t (:italic t)))
    "Font Lock mode face used to highlight italic LaTeX."
    :group 'font-latex-highlighting-faces)

  (defface font-latex-math-face
    '((((class grayscale) (background light)) 
       (:foreground "DimGray" :underline t))
      (((class grayscale) (background dark)) 
       (:foreground "LightGray" :underline t))
      (((class color) (background light)) (:foreground "green4"))
      (((class color) (background dark))  (:foreground "LightSeaGreen"))
      (t (:underline t)))
    "Font Lock mode face used to highlight math in LaTeX."
    :group 'font-latex-highlighting-faces)

  (defface font-latex-sedate-face
    '((((class grayscale) (background light)) (:foreground "DimGray"))
      (((class grayscale) (background dark))  (:foreground "LightGray"))
      (((class color) (background light)) (:foreground "DimGray"))
      (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
      )
    "Font Lock mode face used to highlight sedate stuff in LaTeX."
    :group 'font-latex-highlighting-faces)

  (copy-face 'font-lock-warning-face 'font-latex-warning-face)
  (copy-face 'font-lock-string-face 'font-latex-string-face))
 ((not font-latex-is-XEmacs)
  ;;; emacs:
  ;; Otherwise I overwrite fock-lock-face-attributes.
  ;; font-lock.el needs a better way to add these faces!        
  (if (not font-lock-face-attributes)
      (font-lock-make-faces))
  (unless (assq 'font-latex-sedate-face font-lock-face-attributes)
    (cond 
     ;; FIXME: Add better conditions for grayscale.
     ((memq font-lock-display-type '(mono monochrome grayscale greyscale
                                     grayshade greyshade))
      (setq font-lock-face-attributes
            (append 
             font-lock-face-attributes
             (list '(font-latex-bold-face nil nil t nil nil)
                   '(font-latex-italic-face nil nil nil t nil)
                   '(font-latex-math-face nil nil nil nil t)
                   '(font-latex-sedate-face nil nil nil t nil)
                   (list
                    'font-latex-warning-face
                    (cdr (assq 'background-color (frame-parameters)))
                    (cdr (assq 'foreground-color (frame-parameters)))
                    nil nil nil)))))
     ((eq font-lock-background-mode 'light) ; light colour background
      (setq font-lock-face-attributes
           (append 
            font-lock-face-attributes
                 ;;;FIXME: These won't follow font-lock-type-face's changes.
                 ;;;       Should I change to a (copy-face) scheme?
            '((font-latex-bold-face "DarkOliveGreen" nil t nil nil)
              (font-latex-italic-face "DarkOliveGreen" nil nil t nil)
              (font-latex-math-face "green4")
              (font-latex-sedate-face "grey50")
              (font-latex-warning-face "red" nil t nil nil)))))
    (t			; dark colour background
     (setq font-lock-face-attributes
           (append 
            font-lock-face-attributes
            '((font-latex-bold-face "OliveDrab" nil t nil nil)
              (font-latex-italic-face "OliveDrab" nil nil t nil)
              (font-latex-math-face "LightSeaGreen")
	      ;; good are > LightSeaGreen, LightCoral, coral, orchid, orange
              (font-latex-sedate-face "grey60")
              (font-latex-warning-face "red" nil t nil nil))))))))
 (t
  ;;; XEmacs:
  (make-face 'font-latex-string-face "Face to use for LaTeX string.")
  (copy-face 'font-lock-string-face 'font-latex-string-face)

  (make-face 'font-latex-bold-face "Face to use for LaTeX bolds.")
  (copy-face 'font-lock-type-face 'font-latex-bold-face)
  (make-face-bold 'font-latex-bold-face)

  (make-face 'font-latex-italic-face "Face to use for LaTeX italics.")
  (copy-face 'font-lock-type-face 'font-latex-italic-face)
  (make-face-italic 'font-latex-italic-face)

  (make-face 'font-latex-math-face "Face to use for LaTeX math.")
  (make-face 'font-latex-sedate-face "Face to use for LaTeX minor keywords.")
  (make-face 'font-latex-warning-face "Face to use for LaTeX major keywords.")
  (make-face-bold 'font-latex-warning-face)
  ;; XEmacs uses a tag-list thingy to determine if we are using color
  ;;  or mono (and I assume a dark background).
  (set-face-foreground 'font-latex-math-face "green4" 'global nil 'append)
  (set-face-foreground 'font-latex-sedate-face "grey50" 'global nil 'append)
  (set-face-foreground 'font-latex-warning-face "red" 'global nil 'append)))

(defun font-latex-setup ()
  "Setup this buffer for LaTeX font-lock.  Usually called from a hook."
  ;; Trickery to make $$ fontification be in `font-latex-math-face' while
  ;; strings get whatever `font-lock-string-face' has been set to.
  (cond
   (font-latex-is-Emacs20
    (make-local-variable 'font-lock-string-face)
    (setq font-lock-string-face font-latex-math-face
	  font-latex-string-face (default-value 'font-lock-string-face))
    ;; Tell Font Lock about the support.
    (make-local-variable 'font-lock-defaults)
    ;; Parentheses () disabled because they should not delimit fontification
    ;; in LaTeX text.
    (setq font-lock-defaults
	  '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
	    nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
	    (font-lock-comment-start-regexp . "%")
	    (font-lock-mark-block-function . mark-paragraph))))
   (font-latex-is-XEmacs
    ;; Cool patch from Christoph Wedler...
    (let (instance)
      (mapcar (function
	       (lambda (property)
		 (setq instance
		       (face-property-instance 'font-latex-math-face property
					       nil 0 t))
		 (if (numberp instance)
		     (setq instance
			   (face-property-instance 'default property nil 0)))
		 (or (numberp instance)
		     (set-face-property 'font-lock-string-face property
					instance (current-buffer)))))
	      (built-in-face-specifiers))))
   (t
    (font-lock-make-faces)
    (make-local-variable 'font-lock-string-face)
    (setq font-lock-string-face font-latex-math-face
	  font-latex-string-face (default-value 'font-lock-string-face))
    ;; Tell Font Lock about the support.
    (make-local-variable 'font-lock-defaults)
    ;; Parentheses () disabled because they should not delimit fontification
    ;; in LaTeX text.
    (setq font-lock-defaults
	  '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
	    nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
	    (font-lock-comment-start-regexp . "%")
	    (font-lock-mark-block-function . mark-paragraph))))))

(when font-latex-is-XEmacs
    (put 'latex-mode 'font-lock-defaults
         '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
           nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
           (font-lock-comment-start-regexp . "%")
           (font-lock-mark-block-function . mark-paragraph)))
    (put 'latex-tex-mode	'font-lock-defaults 'latex-mode)
    (put 'LaTex-tex-mode	'font-lock-defaults 'latex-mode)
    (put 'LaTeX-mode        'font-lock-defaults 'latex-mode)
    (put 'japanese-LaTeX-mode 'font-lock-defaults 'latex-mode)
    (put 'LATeX-MoDe	'font-lock-defaults 'latex-mode)
    (put 'lATEx-mODe	'font-lock-defaults 'latex-mode))

(defconst font-latex-keywords-1
  (list
   ;; FIXME: Maybe I should put this in a function, use override but let
   ;;        the function determine if commented-out.
   (list (concat "\\\\\\(\\(no\\)?pagebreak\\|\\(new\\|clear\\(double\\)?\\)"
		 "page\\|enlargethispage\\|\\(no\\)?linebreak\\|newline\\|"
		 "-\\|\\\\\\(\*\\)?\\|displaybreak\\|allowdisplaybreaks\\)")
	 '(0 font-latex-warning-face))
   '("\\$\\$\\([^$]+\\)\\$\\$" 1 font-latex-math-face)        ;;; $$...$$
   '(font-latex-match-quotation . font-latex-string-face)     ;;; ``...''
   '(font-latex-match-font-outside-braces		      ;;;\textit{text}
     (0 font-lock-keyword-face
        append                         ;Override? [t 'keep 'prepend 'append]
        ;; Can't use prepend because that overwrites syntax fontification
        ;; e.g. comments.
        t)                              ;Laxmatch? if t, do not signal error
     (1 font-latex-italic-face append t)
     (2 font-latex-bold-face append t)
     (3 font-lock-type-face append t))
   '(font-latex-match-font-inside-braces		      ;;;{\it text}
     (0 font-lock-keyword-face append t)
     (1 font-latex-italic-face append t)
     (2 font-latex-bold-face append t)
     (3 font-lock-type-face append t)))
  "Subdued level highlighting for LaTeX modes.")

(defconst font-latex-keywords-2
  (append font-latex-keywords-1
   '((font-latex-match-reference                              ;;;\cite
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;    [opt]
      (2 font-lock-reference-face append t))                 ;;;         {key}
     (font-latex-match-function                               ;;;\section
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-lock-function-name-face append t))             ;;;        {text}
     (font-latex-match-variable
      (0 font-lock-keyword-face nil t)
      (1 font-lock-variable-name-face nil t)
      (2 font-lock-variable-name-face nil t))
     (font-latex-match-math-env 
      (0 font-latex-math-face append t))         	      ;;;\(...\)
     (font-latex-match-math-envII                             ;;;Math environ.
      (0 font-latex-math-face append t))
     ("\\\\[@A-Za-z]+"                                        ;;;Other commands
      (0 font-latex-sedate-face append))))
  "High level highlighting for LaTeX modes.")

(defvar font-latex-keywords font-latex-keywords-1
  "Default expressions to highlight in TeX mode.")


(defun font-latex-match-reference (limit)
  (font-latex-match-command-outside-arguments
   (eval-when-compile
     (concat "\\\\" "\\("
	     (mapconcat 'identity 
	      '("[A-Za-z]*cite[A-Za-z]*" "label" "\\(page\\|v\\|eq\\)?ref"
		"index" "glossary" "\\(footnote\\(mark\\|text\\)?\\)")
	      "\\|")
      "\\)\\>"))
   limit nil nil))

(defun font-latex-match-function (limit)
  "Fontify things like \\section{text}"
  (font-latex-match-command-outside-arguments
   (eval-when-compile
     (concat "\\\\" "\\("
      (mapconcat 'identity 
       ;; \\*? doesn't work with \\> at the end of the regexp.
       ;; Instead, allow `*' for all commands (!)
       '("item" ;;;FIXME: does not have an {arg} so should treated elsewhere.
         "include" "input" "bibliography"
	 "part" "chapter" "\\(sub\\)*section" "\\(sub\\)*paragraph"
	 "begin" "end"
	 "title" "author" "date" "thanks" "address"
	 "pagenumbering"
	 "\\(this\\)?pagestyle"
	 "nofiles" "includeonly"
	 "bibliographystyle" "\\(document\\(style\\|class\\)\\)"
	 "\\(re\\)?new\\(environment\\|command\\|length\\|theorem\\|counter\\)"
	 "usepackage" "caption" "\\(f\\|m\\|s\\)box" "\\(v\\|h\\)space")
       "\\|")
      "\\)\\>"))
   limit nil t))

(defun font-latex-match-variable (limit)
  "Fontify things like \\newcommand{stuff}"
  (font-latex-match-command-outside-arguments
   (eval-when-compile
     (concat "\\\\" "\\("
	     "set\\(length\\|towidth\\|counter\\)\\|"
	     "addto\\(length\\|counter\\)"
             "\\)\\>"))
   limit t nil))

;;
;; font-latex-find-matching-close is a little helper function which
;; is used like scan-sexp.  It skips over matching
;; pairs of '{' and '}'.  As an added benefit, it ignores any characters
;; which occur after the tex comment character %.
(defun font-latex-find-matching-close (closechar)
"*Skip over matching pairs of '{' and '}', ignoring
any characters in comments, until closechar is found.  If the end of file
is reached, return nil."
  (save-excursion
    (save-match-data
      (let ((mycount 1))
	(while (and (> mycount 0)
		    (progn
		      (backward-char 1)
		      (re-search-forward
		       (concat "[^\\\\]["
			       ;; closechar might be ]
			       ;; and therefor must be first in regexp
			       (char-to-string closechar)
			       "{}%]")
		       nil t)))
	  (if (= (preceding-char) ?%) ;; Found a comment
	      (forward-line 1)
	    (setq mycount (if (= (preceding-char) ?{)
			      (+ mycount 1)
			    (- mycount 1)))))
	(if (= mycount 0)
	    (point))))))

;; FIXME: --About font-latex-commented-outp--
;; Fontification is *slower* for affected functions (in particular
;; font-latex-match-function), so it will be worth it to increase
;; performance in the algorithm.
;;  - don't return (store-match-data (list nil nil)) in
;;    font-latex-match-command-outside-arguments, instead skip over
;;    commented-out parts internally.  
;;  - Perhaps handling outlined code is excessive and slows down the 
;;    search too much?
;;  - Is save-match-data expensive? The calling function could store
;;    the match-data before it calls (font-latex-commented-outp) knowing
;;    that is would trash the list.
(defun font-latex-commented-outp ()
  "Return t is comment character is found between bol and point."
  (save-excursion
    (let ((limit (point)))
      (save-match-data
        ;; Handle outlined code
        (re-search-backward "^\\|\C-m" (point-min) t)
        (if (re-search-forward "^%\\|[^\\]%" limit t)
            t
          nil)))))

(defvar font-latex-match-command-cache-state nil
  "Cache state of unterminated match to fontify")
(defvar font-latex-match-command-cache-start nil
  "Cache start of unterminated match to fontify")
(defvar font-latex-match-command-cache-limit nil
  "Cache end of unterminated match to fontify")
(defvar font-latex-match-command-cache-keywords nil
  "Cache keywords of unterminated match to fontify")
(make-variable-buffer-local 'font-latex-match-command-cache-state)
(make-variable-buffer-local 'font-latex-match-command-cache-start)
(make-variable-buffer-local 'font-latex-match-command-cache-limit)
(make-variable-buffer-local 'font-latex-match-command-cache-keywords)

;; FIXME - Note to myself 
;; In call to font-latex-match-command-outside-arguments, I could arrange
;; such that keywords which cannot use [options] have this set to nil.
;; LaTeX code woulldn't fontify if options are used illegally in commands,
;; cuing users in that they are doing something wrong.  (See RCS V1.11 for
;; useopt option)
;;
;; NOTE - Without an override flag, font-lock does not re-fontify the
;;  option `opt' when the `t' is typed-in in "\cite[opt".  The first `o'
;;  was fontified and now has a face, which font-lock-apply-highlight
;;  won't override.  The `p' and `t' get a face as they are typed by 
;;  inheriting from left-stickyness on the `o'.
;;  THEREFORE, I cannot rely on font-lock-apply-highlight to continue 
;;  multi-line incomplete patterns, because the first character of the 
;;  pattern on the first line has a face.  I must use `prepend'.
(defun font-latex-match-command-outside-arguments (keywords limit twoargs 
                                                   asterix)
  "Search for regexp command KEYWORDS[opt]{arg} before LIMIT.
If TWOARG is t, allow two arguments {arg1}{arg2}
If ASTERIX is t, fontify trailing asterix in command.
Sets `match-data' so that:
 subexpression 0 is the keyword, 
 subexpression 1 is the contents of any following [...] forms 
 subexpression 2 is the contents of any following {...} forms.  
Returns nil if none of KEYWORDS is found."
  ;; Prior incomplete match?
  (if font-latex-match-command-cache-state
      (setq font-latex-match-command-cache-state nil) ;Stop now!
    (when (and font-latex-match-command-cache-keywords
               (equal font-latex-match-command-cache-keywords keywords)
               (>= font-latex-match-command-cache-limit (point))
               (<  font-latex-match-command-cache-start (point)))
      (goto-char font-latex-match-command-cache-start)
      (setq font-latex-match-command-cache-state 'stop)) ;Can only do once
    (when (re-search-forward keywords limit t)
      (let ((this-start (match-beginning 0)))
        (cond
         ((font-latex-commented-outp)
          ;; Return a nul match such that we skip over this pattern.
          ;; (Would be better to skip over internally to this function)
          (store-match-data (list nil nil))
          t)
         (t
          (let ((kbeg (match-beginning 0))
                (kend (match-end 0)) 
                sbeg send cbeg cend)
            (goto-char kend)            ;May be moved by asterix
            (if (and asterix (eq (following-char) ?\*))
                (forward-char 1)) 
            (skip-chars-forward " \n\t" limit)
            (setq kend (point))
            (while (eq (following-char) ?\[)
              (setq sbeg (1+ kend))   ;Be shure to set sbeg relative to kend
              (forward-char 1)
              (save-restriction
                ;; Restrict to LIMIT.
                (narrow-to-region (point-min) limit)
                (if (condition-case nil
                        (goto-char (or (font-latex-find-matching-close ?\])
				       (point-max)))
                      (error))
                    (setq send (1- (point)))
                  (setq send (point-max))
                  (goto-char send)
                  (setq font-latex-match-command-cache-state 'stop))))
            (skip-chars-forward " \n\t" limit)
            (when (eq (following-char) ?\{)
              (forward-char 1)
              (setq cbeg (point))
              (save-restriction
                ;; Restrict to LIMIT.
                (narrow-to-region (point-min) limit)
                (if (condition-case nil
                        (goto-char (or (font-latex-find-matching-close ?\})
                                       (point-max)))
                      (error))
                    (setq cend (1- (point)))
                  (setq cend (point-max))
                  (goto-char cend)
                  (setq font-latex-match-command-cache-state 'stop))))
            (when twoargs
              (skip-chars-forward " \n\t" limit)
              (when (eq (following-char) ?\{)
                (forward-char 1)
                (save-restriction
                  ;; Restrict to LIMIT.
                  (narrow-to-region (point-min) limit)
                  (if (condition-case nil
                          (goto-char (or (font-latex-find-matching-close ?\})
                                         (point-max)))
                        (error))
                      (setq cend (1- (point)))
                    (setq cend (point-max))
                    (goto-char cend)
                    (setq font-latex-match-command-cache-state 'stop)))))
            (store-match-data (list kbeg kend sbeg send cbeg cend))
            (when font-latex-match-command-cache-state
              (setq font-latex-match-command-cache-start this-start)
              (setq font-latex-match-command-cache-limit (point))
              (setq font-latex-match-command-cache-keywords keywords))
            t)))))))

(defvar font-latex-match-font-cache-state nil
  "Cache state of unterminated match to fontify")
(defvar font-latex-match-font-cache-start nil
  "Cache start of unterminated match to fontify")
(defvar font-latex-match-font-cache-limit nil
  "Cache end of unterminated match to fontify")
(defvar font-latex-match-font-cache-keywords nil
  "Cache keywords of unterminated match to fontify")
(make-variable-buffer-local 'font-latex-match-font-cache-state)
(make-variable-buffer-local 'font-latex-match-font-cache-start)
(make-variable-buffer-local 'font-latex-match-font-cache-limit)
(make-variable-buffer-local 'font-latex-match-font-cache-keywords)

(defun font-latex-match-font-outside-braces (limit)
  "Search for font-changing command like \textbf{fubar} before LIMIT.  
Sets `match-data' so that:
 subexpression 0 is the keyword, 
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (if font-latex-match-font-cache-state
      (setq font-latex-match-font-cache-state nil) ;Stop now!
    (when (and font-latex-match-font-cache-keywords
               (equal font-latex-match-font-cache-keywords keywords)
               (>= font-latex-match-font-cache-limit (point))
               (<  font-latex-match-font-cache-start (point)))
      (goto-char font-latex-match-font-cache-start)
      (setq font-latex-match-font-cache-state 'stop)) ;Can only do once
    (when (re-search-forward
           (eval-when-compile
             (concat "\\\\" "\\("
		     "\\(emph\\)\\|"			      ;;; 2 - italic
		     "\\(text\\("
		                "\\(it\\|sl\\)\\|"	      ;;; 5 - italic
		                "\\(md\\|rm\\|sf\\|tt\\)\\|"  ;;; 6 - type
		                "\\(bf\\|sc\\|up\\)"	      ;;; 7 - bold
		     "\\)\\)\\|"
		     "\\(boldsymbol\\|pmb\\)"		      ;;; 8 - bold
		     "\\)" "{"))
           limit t)
      (cond
       ((font-latex-commented-outp)
        ;; Return a nul match such that we skip over this pattern.
        ;; (Would be better to skip over internally to this function)
        ;; Using `prepend' won't help here, because the problem is that
        ;; scan-sexp *fails* to find a commented-out matching bracket!
        (store-match-data (list nil nil))
        t)
       (t
        (let ((kbeg (match-beginning 0)) (kend (match-end 1)) 
              (beg  (match-end 0)) end itbeg itend bfbeg bfend ttbeg ttend)
          (goto-char kend)
          (save-restriction
            ;; Restrict to LIMIT.
            (narrow-to-region (point-min) limit)
	    (re-search-forward "{" nil t)
            (if (condition-case nil
                    (goto-char (or (font-latex-find-matching-close ?\})
				   (point-max)))
                  (error))
                (setq end (1- (point)))
              (setq end (point-max))
              (goto-char end)
              (setq font-latex-match-font-cache-state 'stop)))
          (cond ((or (match-beginning 2) (match-beginning 5))
                 (setq itbeg beg
                       itend end))
                ((match-beginning 6)
                 (setq ttbeg beg
                       ttend end))
                (t
                 (setq bfbeg beg
                       bfend end)))
          (store-match-data 
           (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend))
          (when font-latex-match-font-cache-state
            (setq font-latex-match-font-cache-start kbeg)
            (setq font-latex-match-font-cache-limit (point))
            (setq font-latex-match-font-cache-keywords keywords))
          ;; Start the subsequent search immediately after this keyword.
          (goto-char kend)
          t))))))

(defvar font-latex-match-infont-cache-state nil
  "Cache state of unterminated match to fontify")
(defvar font-latex-match-infont-cache-start nil
  "Cache start of unterminated match to fontify")
(defvar font-latex-match-infont-cache-limit nil
  "Cache end of unterminated match to fontify")
(defvar font-latex-match-infont-cache-keywords nil
  "Cache keywords of unterminated match to fontify")
(make-variable-buffer-local 'font-latex-match-infont-cache-state)
(make-variable-buffer-local 'font-latex-match-infont-cache-start)
(make-variable-buffer-local 'font-latex-match-infont-cache-limit)
(make-variable-buffer-local 'font-latex-match-infont-cache-keywords)

(defun font-latex-match-font-inside-braces (limit)
  "Search for font-changing command like {\bf fubar} before LIMIT.  
Sets `match-data' so that:
 subexpression 0 is the keyword. 
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (if font-latex-match-infont-cache-state
      (setq font-latex-match-infont-cache-state nil) ;Stop now!
    (when (and font-latex-match-infont-cache-keywords
               (equal font-latex-match-infont-cache-keywords keywords)
               (>= font-latex-match-infont-cache-limit (point))
               (<  font-latex-match-infont-cache-start (point)))
      (goto-char font-latex-match-infont-cache-start)
      (setq font-latex-match-infont-cache-state 'stop)) ;Can only do once
    (when (re-search-forward
           (eval-when-compile
	     (concat "\\\\" "\\("
                                                              ;;; 2 - italic
	             "\\(em\\|it\\(shape\\)?\\|sl\\(shape\\)?\\)\\|"
	                                                      ;;; 5 - bold
	             "\\(bf\\(series\\)?\\|upshape\\|sc\\(shape\\)?\\)\\|"
	             "mdseries\\|tt\\(family\\)?\\|"
                     "sf\\(family\\)?\\|rm\\(family\\)?\\|"
                     "tiny\\|scriptsize\\|footnotesize\\|"
                     "small\\|normalsize\\|large\\|Large\\|LARGE\\|huge\\|Huge"
	             "\\)\\>[ \t]*"))
           limit t)
      (cond
       ((font-latex-commented-outp)
        ;; Return a nul match such that we skip over this pattern.
        ;; (Would be better to skip over internally to this function)
        ;; Using `prepend' won't help here, because the problem is that
        ;; scan-sexp *fails* to find a commented-out matching bracket!
        (store-match-data (list nil nil))
        t)
       (t
        (let ((kbeg (match-beginning 0)) (kend (match-end 1)) 
              (beg  (match-end 0)) end itbeg itend bfbeg bfend ttbeg ttend)
          (goto-char (match-beginning 0))
          (cond 
           ((not (eq (preceding-char) ?\{))
            ;; Fontify only the keyword as bf/it/type (no argument found).
            (cond ((match-beginning 2) (setq itbeg kbeg itend kend))
                  ((match-beginning 5) (setq bfbeg kbeg bfend kend))
                  (t                   (setq ttbeg kbeg ttend kend)))
            (goto-char (match-end 0))
            (store-match-data 
             (list nil nil itbeg itend bfbeg bfend ttbeg ttend))
            t)
           (t
            (save-restriction
              ;; Restrict to LIMIT.
              (narrow-to-region (point-min) limit)
              (if (condition-case nil
                      (goto-char (or (font-latex-find-matching-close ?\})
				     (point-max)))
                    (error))
                  (setq end (1- (point)))
                (setq end (point-max))
                (goto-char end)
                (setq font-latex-match-infont-cache-state 'stop)))
            (cond ((match-beginning 2) (setq itbeg beg itend end))
                  ((match-beginning 5) (setq bfbeg beg bfend end))
                  (t             	   (setq ttbeg beg ttend end)))
            (store-match-data 
             (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend))
            (when font-latex-match-infont-cache-state
              (setq font-latex-match-infont-cache-start kbeg)
              (setq font-latex-match-infont-cache-limit (point))
              (setq font-latex-match-infont-cache-keywords keywords))
            ;; Start the subsequent search immediately after this keyword.
            (goto-char kend)))))))))

;;; FIXME: Add caches for math-env, math-envII and quotations.
(defun font-latex-match-math-env (limit)
  "Used for patterns like:
\\( F = ma \\)
\\ [ F = ma \\] but not \\\\ [len]"
  (when (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
    (goto-char (match-beginning 0))
    (if (eq (preceding-char) ?\\)       ; \\[ is not a math environment
        (progn 
          (goto-char (match-end 0))
          (store-match-data (list nil nil)) 
          t)
      (let ((b1start (point)))
        (search-forward (cond ((match-beginning 1) "\\)")
                              (t                   "\\]"))
                        limit 'move)
        (let ((b2end (or (match-end 0) (point))))
          (store-match-data (list b1start b2end))
          t)))))

(defun font-latex-match-math-envII (limit)
  "Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation}
The \\begin{equation} and \\end{equation are not fontified here."
  (when (re-search-forward 
         (eval-when-compile 
           (concat "\\\\begin{\\(\\(display\\)?math\\|equation\\|eqnarray"
                   "\\|gather\\|multline\\|align\\|x*alignat"
                   "\\)\\*?}"))
         limit t)
    (let ((beg (match-end 0)) end)
      (if (search-forward (concat "\\end{" (buffer-substring 
                                            (match-beginning 1)(match-end 0)))
                          limit 'move)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun font-latex-match-quotation (limit)
  "Used for patterns like:
``this is a normal quote'' and these are multilingual quoted strings:
\"< french \"> and \"`german\"' quotes, << french >> and 8-bit french."
  (when (re-search-forward
	 (eval-when-compile
	   (concat "\\(``\\)\\|\\(\"<\\)\\|\\(\"`\\)\\|\\(<<\\)\\|"
		   "\\(" (char-to-string 171) "\\)")) ; An 8-bit "<<"
	 limit t)
    (let ((beg (match-beginning 0)))
      (search-forward
       (cond ((match-beginning 1) "''")
	     ((match-beginning 2) "\">")
	     ((match-beginning 3) "\"'")
	     ((match-beginning 4) ">>")
	     ((match-beginning 5) (eval-when-compile (char-to-string 187))))
       limit 'move)
      (store-match-data (list beg (point)))
      t)))

;; Install ourselves

(add-hook 'LaTeX-mode-hook 'font-latex-setup)
(add-hook 'latex-mode-hook 'font-latex-setup)
;; If font-latex is loaded using a latex-mode-hook, then the add-hook above
;; won't be called this time around.  Check for this now:
(if (eq major-mode 'latex-mode)
    (font-latex-setup))

;; Provide ourselves:

(provide 'font-latex)

;;; font-latex.el ends here
