;;;; toy-parser.lisp

(in-package #:toy-parser)

;; The original code
(defparameter *additive-color-graph*
  '((red   (red white)   (green yellow) (blue magenta))
    (green (red yellow)  (green white)  (blue cyan))
    (blue  (red magenta) (green cyan)   (blue white))))

(defun symbolic-color-add (a b)
  (cadr (assoc a (cdr (assoc b *additive-color-graph*)))))

(symbolic-color-add 'red 'green)

;; now, the color mix definition parser

(defrule color-name (+ (alpha-char-p character))
  (:lambda (name)
    ;; CL symbols default to upper case.
    (intern (string-upcase (coerce name 'string)) :toy-parser)))

(defrule indent (+ (or #\space #\tab #\newline #\linefeed)))
(defrule indent? (* indent))

(defrule color-mix (and indent? #\+ color-name #\Space color-name)
  (:destructure (i p color-added s color-obtained)
    (declare (ignore i p s))		; indent? + #\Space
    (list color-added color-obtained)))

(defrule color-mixes (+ color-mix))

(defrule color (and indent? "color" #\Space color-name color-mixes)
  (:destructure (i c s name mixes)
    (declare (ignore i c s))		; indent, "color" and #\Space
    (cons name mixes)))

(defrule colors (+ color))

(defrule mix (and colors indent? "mix " color-name " and " color-name)
  (:destructure (graph i m c1 a c2)
    (declare (ignore i m a))		; mix, and
    `(lambda ()
       (let ((*additive-color-graph* ',graph))
	 (symbolic-color-add ',c1 ',c2)))))

;; the text to parse, and the parsing

(defvar *colors-program* "
color red
  +red white
  +green yellow
  +blue magenta

color green
  +red yellow
  +green white
  +blue cyan

color blue
  +red magenta
  +green cyan
  +blue white

mix red and green"
  "Our color mixing definitions")

(defun parse-program (&optional (program *colors-program*))
  "Parse given PROGRAM."
  (parse 'mix program))

(defun mix-colors (&optional (program *colors-program*))
  "Parse, compile then run given color mixing program."
  (funcall (compile nil (parse 'mix program))))
