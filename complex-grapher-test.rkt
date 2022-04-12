#lang racket

;;File:
;;  complex-grapher-test.rkt
;;Summary:
;;  A file to test out the complex-grapher.rkt module
;;Author:
;;  Cassandra Rudig
;;Created: 2/26/22
;;Last Updated: 3/12/22

;;--------------------------------------------------------------------
;; Requirements §1
;;--------------------------------------------------------------------

(require "complex-grapher.rkt")
(require racket/gui/base)

;;--------------------------------------------------------------------
;; Miscellaneous Procedures §2
;;--------------------------------------------------------------------

;A convenient alias
;See make-rectangular in the official docs for details
{define complex make-rectangular}

;Randomly generates a complex number in the given rectangular range
;(random-complex [rmin -1] [rmax 1] [imin -1] [imax 1])
;  → (and/c complex? {lambda (z) (<= rmin (real-part z) rmax)}
;                    {lambda (z) (<= imin (imag-part z) imax)})
;  rmin: real?
;  rmax: (and/c real? (>=/c rmin))
;  imin: real?
;  imax: (and/c real? (>=/c imin))
{define random-complex
  {lambda ([rmin -1] [rmax 1] [imin -1] [imax 1])
    (complex (+ rmin (* (- rmax rmin) (random)))
             (+ imin (* (- imax imin) (random))))}}

;Creates a polynomial procedure in which each supplied argument is a zero of the function
;(zeroes . zrs) → {-> complex? complex?}
;  zrs: (listof complex?)
{define zeroes
  {lambda zrs
    {lambda (x)
      (apply * (map {lambda (zr) (- x zr)} zrs))}}}

;Defines a function with the specified number of zeroes and poles,
;  where item is a quoted expression which resolves to a complex number upon evaluation
;  Bands can be marked with the #:bands and #:alt-bands keyword arguments:
;    data1 and data2 are pairs of nonnegative integers, in which
;      the car of the pair is the number of bands to draw around a zero or pole, and
;      the cdr of the pair is the precision for drawing the bands (bigger numbers = thinner bands)
;(zero-pole zrs poles [item '(random-complex)] [#:bands [data1 #f] #:alt-bands [data2 #f]])
;  → {-> complex? complex?}
;  zrs: nonnegative-integer?
;  poles: nonnegative-integer?
;  item: (and/c (or/c symbol? list?) {lambda (x) (complex? (eval x))})
;  data1: (or/c #f (cons/c nonnegative-integer? nonnegative-integer?))
;  data2: (or/c #f (cons/c nonnegative-integer? nonnegative-integer?))
{define zero-pole
  {lambda (zrs poles [item '(random-complex)] #:bands [data1 #f] #:alt-bands [data2 #f])
    {define func (combine /
                          (apply zeroes (map eval (make-list zrs item)))
                          (apply zeroes (map eval (make-list poles item))))}
    {cond [(nor data1 data2) func]
          [(and data1 (not data2))
           {define freq1 (car data1)} {define tol1 (cdr data1)}
           {lambda (z)
             {define res (func z)}
             {if (near-multiple? (angle res) (/ (* 2 pi) freq1) (expt 2 (- tol1)))
                 0 res}}]
          [(and data1 data2)
           {define freq1 (car data1)} {define freq2 (car data2)}
           {define tol1 (cdr data1)} {define tol2 (cdr data2)}
           {lambda (z)
             {define res (func z)} {define angle-res (angle res)}
             {cond [(near-multiple? angle-res (/ (* 2 pi) freq1) (expt 2 (- tol1))) 0]
                   [(near-multiple? angle-res (/ (* 2 pi) freq2) (expt 2 (- tol2))) +inf.0]
                   [else res]}}]
          [else (error "#:alt-bands keyword requires #:bands keyword")]
          }}}

;If no procedures are supplied, draws the mandelbrot set to complex-canvas with preset window settings
;  If another canvas is supplied, produces the mandelbrot procedure for use in a method call
;(mandelbrot canvas) → {-> complex? complex?}
;  canvas: (is-a?/c canvas%)
;(mandelbrot) → void?
{define mandelbrot
  {case-lambda [(canvas)
                {lambda (x)
                  {define factor (send canvas get-scale-factor)}
                  {define dist (dist-est x (* 2 (round (/ (- 52 (log (/ factor 2) 2)) 2))))}
                  {if (zero? dist) 0 (/ (* 2 (expt dist 1/4+1/4i)) (expt factor 1/4))}}]
               [() (send complex-canvas on-paint (mandelbrot complex-canvas) (/ 2.47 768) -0.765 0)]}}

;Creates a procedure that acts as a polynomial function with the arguments used as the coefficients
;  The right most argument always represents the coefficient of the term with degree 0
;  Each argument is the coefficient of the term 1 degree greater than the argument to its right
;(polynomial . coefficients) → {-> complex? complex?}
;  coefficients: (listof complex?)
{define polynomial
  {lambda coefficients
    (eval (list 'lambda '(x)
                (cons '+ (filter identity (map {lambda (c d) {cond [(zero? c) #f]
                                                                   [(zero? d) c]
                                                                   [{and (= 1 d) (= 1 c)} `x]
                                                                   [(= 1 d) `(* ,c x)]
                                                                   [(= 1 c) `(expt x ,d)]
                                                                   [else `(* ,c (expt x ,d))]}}
                                               coefficients
                                               (range (sub1 (length coefficients)) -1 -1))))))}}

;Creates a procedure that calculates the derivative of the polynomial function
;  with the arguments used as coefficients
;(polyprime . coefficients) → {-> complex? complex?}
;  coefficients: (listof complex?)
{define polyprime
  {lambda coefficients
    (apply polynomial (reverse (cdr (map * (reverse coefficients) (range (length coefficients))))))}}

;Creates a procedure that executes a single step of newton's method for finding zeroes of a function
;  The coefficients determine the polynomial to search for zeroes. See the polynomial procedure
;(newton-step . coefficients) → {-> complex? complex?}
;  coefficients: (listof complex?)
{define newton-step
  {lambda coefficients
    {define polynom (apply polynomial coefficients)}
    {define polyprim (apply polyprime coefficients)}
    {lambda (x) (- x (/ (polynom x) (polyprim x)))}}}

;Estimates the integral of proc using left-endpoint approximation to the specified precision
;  The bounds of the integral are low and high
;(integrate low high proc [precision 32]) → complex?
;  low: real?
;  high: (and/c real? (>=/c low))
;  proc: {-> complex? complex?}
;  precision: positive-integer?
{define integrate
  {lambda (low high proc [precision 32])
    {define increment (/ (- high low) precision)}
    {define mag (magnitude increment)}
    {let kernel ([i low] [sum 0])
      {if (> (magnitude (- high i)) mag)
          (kernel (+ i increment) (+ sum (* (proc i) increment))) sum}}}}

;Same as erf, but can take complex inputs, and has relatively limited precision
;  The precision argument is used for the integral approximation
;(erfz z [precision 32]) → complex?
;  z: complex?
;  precision: positive-integer?
{define erfz
  {lambda (z [precision 32])
    (* (/ 2 (sqrt pi)) (integrate 0 z {lambda (z) (exp (- (sqr z)))} precision))}}

;Defines a procedure using a formula-style format
;  The arguments of the procedure are placed between the f: and the ~,
;    as an unparenthesized sequence of identifiers
;  The body is placed after the ~, without enclosing parentheses
;(f: args ~ form)
;  args = id ...+
;       | id
;
;  form = expr ...
;
;  expr = id
;       | body
{define-syntax f:
  {syntax-rules (~)
    [(_ arg ...+ ~ . stuff)
     (eval '{lambda (arg ...+) stuff})]
    [(_ arg ~ . stuff)
     (eval '{lambda (arg) stuff})]}}
;See the following examples:
;(f: x ~ sqr x) → #<procedure>
;((f: x ~ sqr x) 5) → 25
;((f: x ~ lambda (y) (+ y x)) 3) → #<procedure>
;(((f: x ~ lambda (y) (+ y x)) 3) 4) → 7

;Returns the result of processing arg with each of the procedures in procs from left to right
;  The output of one procedure is used for the next
;  This is inspired by the %>% (pipeline) operator in R
;(>> arg . procs) → any/c
;  arg: any/c
;  procs: (cons/c (procedure-arity-includes 1) (listof procedure?))
{define >>
  {lambda (arg . procs)
    {let kernel ([res arg] [rest procs])
      {if (null? rest) res
          (kernel ((car rest) res) (cdr rest))}}}}

;Calls the on-paint method for complex-canvas with the given arguments, for convenience
;  Also adjusts scale so that dividing by frame width is unnecessary
;(graph eq) → void?
;(graph eq scale) → void?
;(graph eq scale x y) → void?
;  eq: (-> complex? complex?)
;  scale: complex?
;  x: real?
;  y: real?
{define graph
  {case-lambda [(eq)
                {send complex-canvas on-paint eq}]
               [(eq scale)
                {send complex-canvas on-paint eq (/ scale {send complex-frame get-width})}]
               [(eq scale x y)
                {send complex-canvas on-paint eq (/ scale {send complex-frame get-width}) x y}]}}

;Calls the set-resolution method for complex frame with the given resolution
;(set-res [res 6]) → void?
;  res: (and/c dimension-integer? positive?)
{define set-res
  {lambda ([res 6])
    {send complex-frame set-resolution res}}}

;Shows the window and canvas
;(show) → void?
{define show
  {lambda ()
    {send complex-frame show #t}}}

;Hides the window and canvas
;(hide) → void?
{define hide
  {lambda ()
    {send complex-frame show #f}}}


;Resets the canvas to the default equation and window settings
;(reset) → void?
{define reset
  {lambda ()
    {send complex-canvas on-paint identity (/ 4 768) 0 0}}}

;Determine if x is within tolerance distance of any multiple mult
;(near-multiple? x [mult (/ (* 2 pi) 3)] [tolerance 0.015625]) → boolean?
;  x: real?
;  mult: real?
;  tolerance: real?
{define near-multiple?
  {lambda (x [mult (/ (* 2 pi) 3)] [tolerance 0.015625])
    (or (< (mod-extended x mult) tolerance)
        (< (mod-extended (- x) mult) tolerance))}}

;Get the counter from the current timer, for use in automatically-updating equations
;(counter) → nonnegative-integer?
{define counter
  {lambda ()
    {send complex-timer get-counter}}}

;Sets the timer's redraw, determining whether the canvas will automatically update every interval
;(redraw [bool #t]) → void?
;  bool: boolean?
{define redraw
  {lambda ([bool #t])
    {send complex-timer set-redraw bool}}}

{define help
  {lambda ()
    (display "
(help) : show this dialogue
(graph eq) : graph the complex-valued procedure eq
(graph eq scale) : graph with scale being the horizontal span of the window in units
(graph eq scale x y): graph with x and y being the real and imaginary offsets from the origin
(set-res res) : set the resolution to the given number of pixels (lower values draw more points)
(show) : show the window and canvas
(hide) : hide the window and canvas
(reset) : reset the canvas to default
(mandelbrot) : draw the mandelbrot set
(polynomial . coefficients) : create a polynomial procedure
(zero-pole m n) : create a function with m zeroes and n poles
(complex x y) : create the complex number x+yi
(random-complex) : create a random complex number in the unit square
(f: args ~ form) : define a formula
(>> arg . procs) : pipeline arg through procs left to right")}}

;;--------------------------------------------------------------------
;; Object Definitions §3
;;--------------------------------------------------------------------

{define complex-frame (new complex-frame%
                           [label "Complex Equation Grapher"]
                           [width 768] [height 768]
                           [style '(float)]
                           [x 200] [y 70])}

{define complex-canvas (new complex-canvas% [parent complex-frame])}

{define complex-dc (send complex-canvas get-dc)}

;Use if you want the canvas to automatically update every interval
;(counter) gets the current counter, which can be used in equations
{define complex-timer (new complex-timer%
                           [notify-callback {lambda ()
                                              {define redraw (send complex-timer get-redraw)}
                                              (send complex-timer increment-counter)
                                              {if (> (send complex-canvas get-draws)
                                                     {if redraw +nan.0 0})
                                                  (void)
                                                  (send complex-canvas refresh)}}]
                       
                           ;Making this value too small will break things!
                           ;  interval is in ms
                           [interval 1000])}

;;--------------------------------------------------------------------
;; Drawing Calls 4
;;--------------------------------------------------------------------

(send complex-dc set-pen (make-color 255 255 255) 0 'transparent)
(send complex-dc set-font (make-font #:size 12 #:face "Courier"))
(send complex-frame show #t)

;;--------------------------------------------------------------------
;; Extra Notes and Temporary Stuff 5
;;--------------------------------------------------------------------

#|
Change the resolution:
(send complex-frame set-resolution `res`)
  Higher values of res = more detail but slower
  Default res is 6

Graph the complex-valued function proc:
(graph proc)

Graph proc and manually change the window settings:
(graph proc scale x-offset y-offset)
  The scale is the horizontal distance in units across the window
  An x-offset of 3 and a y offset of -4 will put the center of the graph at 3-4i

Many ways to express the same function:
(graph {lambda (x) (sinh (cos x))})
(graph (f: x ~ >> x cos sinh))
(graph (compose sinh cos))
(graph (f: x ~ sinh (cos x))
(graph {lambda (x) (>> x cos sinh)})

Graph a function with a zero and a pole, each at random locations
(graph (zero-pole 1 1))

Zero-pole function with bands:
(graph (zero-pole 1 1 #:bands '(3 . 5) #:alt-bands '(6 . 5)))

Complex step-like function:
(graph {lambda (x)
           {define mag (magnitude x)}
           (* (/ x mag) (- mag (expt 2 (floor (log mag 2)))))} 8 0 0)

Iris function:
(graph {f: x ~ * x (expt (magnitude x) 16)})

Error function:
(graph (f: z ~ erfz z 32))

Logarithmic cosine:
{define zlog {lambda (z) (* (log (magnitude z)) (/ z (magnitude z)))}}
(graph (f: x ~ zlog (cos x)))

Target:
(graph {lambda (x) (* (/ x (magnitude x)) (sin (* 10 (magnitude x))))})

Split Rainbow:
(graph (f: x ~ make-polar (real-part x) (imag-part x)))

Checkerboard:
(graph (f: x ~ * (* +i (sub1 (* 2 (mod-extended (imag-part x) 1)))
                 (sub1 (* 2(mod-extended (real-part x) 1))))))
|#
