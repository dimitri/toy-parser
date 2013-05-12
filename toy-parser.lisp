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

;;; now, let's invent some "syntax" for the dataset and program:
(defvar *colors-program*
  "color red   +red white    +green yellow  +blue magenta
   color green +red yellow   +green white   +blue cyan
   color blue  +red magenta  +green cyan    +blue white

mix red and green"
  "Our color mixing definitions")

;;; The Parser.

(defrule whitespace (or #\space #\tab #\newline #\linefeed))
(defrule whitespaces (* whitespace))

;;; keywords: we ignore whitespaces, and the return value
(defrule kw-color (and whitespaces (~ "color")))
(defrule kw-mix (and whitespaces (~ "mix")))
(defrule kw-and (and whitespaces (~ "and")))

;;; parse string: "color red"
(defrule color-name (and whitespaces (+ (alpha-char-p character)))
  (:destructure (ws name)
    (declare (ignore ws))		; ignore whitespaces
    ;; CL symbols default to upper case.
    (intern (string-upcase (coerce name 'string)) :toy-parser)))

;;; parse string "+ red white"
(defrule color-mix (and whitespaces "+" color-name color-name)
  (:destructure (ws plus color-added color-obtained)
    (declare (ignore ws plus))		; ignore whitespaces and keywords
    (list color-added color-obtained)))

(defrule color-mixes (+ color-mix))

;;; a whole color "section", with all the mixes definitions
(defrule color (and kw-color color-name color-mixes)
  (:destructure (c name mixes)
    (declare (ignore c))		; ignore keywords
    (cons name mixes)))

(defrule colors (+ color))

;;; mix red and green
(defrule mix-two-colors (and kw-mix color-name kw-and color-name)
  (:destructure (mix c1 and c2)
    (declare (ignore mix and))		;ignore keywords
    (list c1 c2)))

(defrule program (and colors mix-two-colors)
  (:destructure (graph (c1 c2))
    `(lambda ()
       (let ((*additive-color-graph* ',graph))
	 (symbolic-color-add ',c1 ',c2)))))

;;; Now we can run the parser
(defun parse-program (&optional (program *colors-program*))
  "Parse given PROGRAM."
  (parse 'program program))

(defun mix-colors (&optional (program *colors-program*))
  "Parse, compile then run given color mixing program."
  (funcall (compile nil (parse-program))))
