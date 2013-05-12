# Toy Parser

Toy parser to show up how writing a parser easily gives a compiler in common
lisp. The following took about an hour to write, from getting the idea to
running the code and writing down that documentation file.

TL;DR: Have a look a Common Lisp someday. What about today?

## Additive Color Graph

Example courtesy of
[Pascal Bourguignon](http://informatimago.free.fr/i/index.html), see
[https://groups.google.com/forum/?fromgroups=#!topic/comp.lang.lisp/JJxTBqf7scU]

~~~lisp
;; This is symbolic computation:

(defparameter *additive-color-graph*
    '((red   (red white)   (green yellow) (blue magenta))
      (green (red yellow)  (green white)  (blue cyan))
      (blue  (red magenta) (green cyan)   (blue white))))


(defun symbolic-color-add (a b)
  (cadr (assoc a (cdr (assoc b *additive-color-graph*)))))

(symbolic-color-add 'red 'green)
;; --> yellow
~~~

## Language to parse

Let's invent a minimalist language that we have to parse, for the sake of
the example:

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

	mix red and green

## Compiler

Given the previous program text, our objective is to get the following
program to run:

    TOY-PARSER> (pprint (parse-program))
    
    (LAMBDA NIL
      (LET ((*ADDITIVE-COLOR-GRAPH*
             '((RED (RED WHITE) (GREEN YELLOW) (BLUE MAGENTA))
               (GREEN (RED YELLOW) (GREEN WHITE) (BLUE CYAN))
               (BLUE (RED MAGENTA) (GREEN CYAN) (BLUE WHITE)))))
        (SYMBOLIC-COLOR-ADD 'RED 'GREEN)))
    ; No value

And of course it should give the following answer when run:

    TOY-PARSER> (mix-colors)
    YELLOW

## Is it a parser or a compiler

One hour later, now that the program actually runs, the main entry point
looks like this:

~~~lisp
(defun mix-colors (&optional (program *colors-program*))
  "Parse, compile then run given color mixing program."
  (funcall (compile nil (parse 'mix program))))  
~~~

Let's do some basic measurement, keeping in mind that we're actually parsing
then compiling and only then running the program at each iteration:

    TOY-PARSER> (time (loop repeat 1000 do (mix-colors)))
    (LOOP REPEAT 1000 DO (MIX-COLORS))
    took 753,883 microseconds (0.753883 seconds) to run.
          80,704 microseconds (0.080704 seconds, 10.71%) of which was spent in GC.
    During that period, and with 4 available CPU cores,
         689,943 microseconds (0.689943 seconds) were spent in user mode
          48,780 microseconds (0.048780 seconds) were spent in system mode
     74,895,744 bytes of memory allocated.
    NIL

That result is to be read as an average of `754 microseconds` to parse then
compile then run the proposed code. My *ahah* moment comes from the fact
that we are actually compiling down to machine code, here.

Note that we could also pre-compile the code, then run it as much as we
want, of course.

    TOY-PARSER> (let ((f (compile nil (parse-program)))) (funcall f))
    YELLOW

Let's measure how much faster that runs when just executing the `funcall` in
a loop:

    TOY-PARSER> (time (loop with f = (compile nil (parse-program))
		                  repeat 1000 do (funcall f)))
    (LOOP WITH F = (COMPILE NIL (PARSE-PROGRAM)) REPEAT 1000 DO (FUNCALL F))
    took 989 microseconds (0.000989 seconds) to run.
    During that period, and with 4 available CPU cores,
         972 microseconds (0.000972 seconds) were spent in user mode
          22 microseconds (0.000022 seconds) were spent in system mode
     74,768 bytes of memory allocated.
    NIL

Oh. Let's then even factor out the *compilation* in the *timing*:

    TOY-PARSER> (let ((f (compile nil (parse-program))))
	              (time (loop repeat 1000 do (funcall f))))
    (LOOP REPEAT 1000 DO (FUNCALL F))
    took 62 microseconds (0.000062 seconds) to run.
    During that period, and with 4 available CPU cores,
         63 microseconds (0.000063 seconds) were spent in user mode
          9 microseconds (0.000009 seconds) were spent in system mode
    NIL

That tends to confirm that we're actually running *compiled* code here. And
we can even *disassemble* that *compiled* code and have a look at it:

    TOY-PARSER> (disassemble (compile nil (parse-program)))
    L0
             (leaq (@ (:^ L0) (% rip)) (% fn))       ;     [0]
             (pushq (% rbp))                         ;     [7]
             (movq (% rsp) (% rbp))                  ;     [8]
             (movq (@ '((RED (RED WHITE) (GREEN YELLOW) (BLUE MAGENTA)) (GREEN (RED YELLOW) (GREEN WHITE) (BLUE CYAN)) (BLUE (RED MAGENTA) (GREEN CYAN) (BLUE WHITE))) (% fn)) (% arg_z)) ;    [11]
             (movq (@ '*ADDITIVE-COLOR-GRAPH* (% fn)) (% arg_y)) ;    [18]
             (leaq (@ (:^ L45) (% fn)) (% temp2))    ;    [25]
             (nop)                                   ;    [32]
             (nop)                                   ;    [35]
             (jmpq (@ .SPBIND))                      ;    [38]
    L45
             (leaq (@ (:^ L0) (% rip)) (% fn))       ;    [45]
             (leaq (@ (:^ L101) (% fn)) (% temp0))   ;    [52]
             (pushq (% temp0))                       ;    [59]
             (movq (@ 'RED (% fn)) (% arg_y))        ;    [60]
             (movq (@ 'GREEN (% fn)) (% arg_z))      ;    [67]
             (movl ($ 16) (% nargs))                 ;    [74]
             (movq (@ 'SYMBOLIC-COLOR-ADD (% fn)) (% temp0)) ;    [79]
             (pushq (@ #x12FB8))                     ;    [86]
             (jmpq (@ 10 (% temp0)))                 ;    [93]
    L101
             (leaq (@ (:^ L0) (% rip)) (% fn))       ;   [101]
             (nop)                                   ;   [108]
             (callq (@ .SPUNBIND))                   ;   [110]
             (leaq (@ (:^ L0) (% rip)) (% fn))       ;   [117]
             (jmpq (@ .SPNVALRET))                   ;   [124]
    NIL

Can your programming language of choice make it that easy?
