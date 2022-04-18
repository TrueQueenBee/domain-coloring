#lang racket

;;File:
;;  complex-grapher.rkt
;;Summary:
;;  A complex equation grapher using two-dimensional position for inputs and colors for outputs
;;Author:
;;  Cassandra Rudig
;;Created: 2/25/22
;;Last Updated: 4/17/22

;;--------------------------------------------------------------------
;; Requirements §1
;;--------------------------------------------------------------------

{require colors}
{require racket/class}
{require racket/gui/base}
{require {rename-in racket/list
                    (cartesian-product cartesian-product*)}}

;;--------------------------------------------------------------------
;; Provisions §2
;;--------------------------------------------------------------------

{provide {contract-out
          [sgn/c {-> real? flat-contract?}]
          [arity-overlaps? {-> procedure-arity? procedure-arity? boolean?}]
          [homogenous? {->* (list?) ({-> any/c any/c any/c}) boolean?}]
          [cartesian-product {-> list? list? (listof pair?)}]
          [sigmoid {-> real? (and/c real? (between/c 0 1))}]
          [mod-extended {->i ([r1 real?] [r2 real?])
                             [result (r1 r2) (and/c real?
                                                    (or/c (between/c r1 (* -inf.0 (sgn r1)))
                                                          (between/c (* -inf.0 (sgn r1)) r1))
                                                    (sgn/c r2))]}]
          [num->byte {-> real? byte?}]
          [dist-est {-> complex? nonnegative-integer? (and/c inexact-real? (or/c zero? positive?))}]
          [combine {->i ([super-proc procedure?])
                        #:rest [procs (super-proc)
                                      {lambda (lst)
                                        {and (homogenous? (map procedure-arity lst) arity-overlaps?)
                                             (procedure-arity-includes? super-proc (length lst))}}]
                        [result (procs)
                                {lambda (res)
                                  (arity-includes? (procedure-arity res)
                                                   (procedure-arity (car procs)))}]}]
          [complex-timer% (and/c (subclass?/c timer%)
                                 {class/c [get-counter {->m nonnegative-integer?}]
                                          [increment-counter {->m void?}]
                                          [get-redraw {->m boolean?}]
                                          [set-redraw {->m boolean? void?}]})]

          [complex-frame% (and/c (subclass?/c frame%)
                                 (implementation?/c top-level-window<%>)
                                 {class/c [get-resolution
                                           {->m (and/c dimension-integer? positive?)}]
                                          [set-resolution
                                           {->m (and/c dimension-integer? positive?) void?}]})]

          [complex-canvas% (and/c (subclass?/c canvas%)
                                  (implementation?/c canvas<%>)
                                  {class/c [get-scale-factor {->m complex?}]
                                           [get-draws {->m exact-nonnegative-integer?}]
                                           [on-paint {->*m () ({-> complex? complex?}
                                                               complex? real? real?) void?}]
                                           [color {->m real? real? (is-a?/c color%)}]
                                           [on-event {->m (is-a?/c mouse-event%) void?}]})]
          }}

;;--------------------------------------------------------------------
;; Miscellaneous Procedures §3
;;--------------------------------------------------------------------

;Produces a flat contract that requires the input to match the sign of x
;(sgn/c x) → flat-contract?
;  x: real?
{define sgn/c
  {lambda (x)
    (or/c (between/c 0 (* x +inf.0)) (between/c (* x +inf.0) 0))}}

;Returns #t if procedures with arity a accept some number of arguments
;  that is also accepted by procedures with arity b
;(arity-overlaps? a b) → boolean?
;  a: procedure-arity?
;  b: procedure-arity?
{define arity-overlaps?
  {lambda (a b)
    {or (arity-includes? a b) (arity-includes? b a)}}}

;Returns #t if every element in lst is the same according to comp
;  every adjacent pair of elements a and b must return #t for (comp a b)
;(homogenous? lst [comp equal?]) → boolean?
;  lst: list?
;  comp: {-> any/c any/c any/c}
{define homogenous?
  {lambda (lst [comp equal?])
    {let kernel ([rest lst])
      {if (< (length rest) 2) #t
          {and (comp (car rest) (cadr rest))
               (kernel (cdr rest))}}}}}

;Given two lists, returns their cartesian product, as a list
;(cartesian-product lst1 lst2) → (listof pair?)
;  lst1: list?
;  lst2: list?
;The built-in cartesian-product has been renamed to cartesian-product*
{define cartesian-product
  {lambda (lst1 lst2)
    {let kernel ([rest1 lst1]
                 [rest2 lst2]
                 [cur null])
      {cond [(empty? rest2) cur]
            [(empty? rest1) (kernel lst1 (cdr rest2) cur)]
            [else (kernel (cdr rest1) rest2 (cons (cons (car rest1) (car rest2)) cur))]}}}}

;Returns the sigmoid function of n, to squeeze any real number between 0 and 1
;(sigmoid n) → (and/c real? (between/c 0 1))
;  n: real?
{define sigmoid
  {lambda (n)
    (/ 1 (add1 (exp (- n))))}}

;Extends the modulo operation to the real numbers
;The output has the same sign as r2
;(mod-extended r1 r2) → (and/c real? (or/c (between/c r1 (* -inf.0 (sgn r1)))
;                                          (between/c (* -inf.0 (sgn r1)) r1)) (sgn/c r2))
;  r1: real?
;  r2: real?
{define mod-extended
  {lambda (r1 r2)
    (- r1 (* r2 (floor (/ r1 r2))))}}

;Takes a number, multiplies by 256 and returns the closest byte
;(num->byte num) → byte?
;  num: real?
{define num->byte
  {lambda (num)
    {if (equal? num +nan.0) 255
        (min (floor (inexact->exact (* (max num 0) 256))) 255)}}}

;Given a complex number, approximates to some precision the exterior distance from the number
;  to the boundary of the Mandelbrot set; points in the set are colored black
;(dist-est c max-iterations) → (and/c inexact-real? (or/c zero? positive?))
;  c: complex?
;  max-iterations: nonnegative-integer?
{define dist-est
  {lambda (c max-iterations)
    {if (zero? c) 0
        {let kernel ([z 0]
                     [iteration 0]
                     [derivative 1]
                     [last 0])
          {define poly (+ c (sqr z))}
          {define mag (magnitude poly)}
          {define div (add1 (* 2 z derivative))}
          {define result (max 0 (* 1/2 mag (log mag) (/ 1 (magnitude div))))}
          {if (or (= iteration max-iterations) (nan? result))
              last
              (kernel poly (add1 iteration) div result)}}}}}

;Creates a procedure which takes as many arguments as each of the procedures in procs takes,
;  then applies super-proc to their results
;(combine super-proc . procs) → {lambda (res) (arity-includes? (procedure-arity res)
;                                                              (procedure-arity (car procs)))}
;  super-proc: {lambda (sp) (procedure-arity-include? sp (length procs))}
;  procs: {lambda (lst) (homogenous? (map procedure-arity lst) arity-overlaps?)}
{define combine
  {lambda (super-proc . procs)
    {lambda args
      {let ([results (map {lambda (proc) (apply proc args)} procs)])
        (apply super-proc results)}}}}

;;--------------------------------------------------------------------
;; Class Definitions §4
;;--------------------------------------------------------------------

;;complex-timer%: class
;;  superclass: timer%
;;  extends: no interfaces used
;;  purpose: adds additional functionality to the timer
{define complex-timer%
  {class timer%
    (super-new)

    ;counter: nonnegative-integer?
    ;counts how many times the object's increment-counter method has been called
    ;intended for use with the timer's notify-callback method
    {define counter 0}

    ;Returns the value of counter
    ;(get-counter) → nonnegative-integer?
    {define/public get-counter
      {lambda () counter}}

    ;(increment-counter) → void?
    ;[Side Effect]: increments the object's counter by 1
    {define/public increment-counter
      {lambda () (set! counter (add1 counter))}}

    ;redraw: boolean?
    ;used to tell the canvas whether to redraw or not
    {define redraw #f}

    ;Returns the value of redraw
    ;(get-redraw) → boolean?
    {define/public get-redraw
      {lambda () redraw}}

    ;(set-redraw bool) → void?
    ;  bool: boolean?
    ;[Side Effect]: sets redraw to bool
    {define/public set-redraw
      {lambda (bool) (set! redraw bool)}}}}

;;complex-frame%: class
;;  superclass: frame%
;;  extends: top-level-window<%>
;;  purpose: allows the frame to store resolution information
{define complex-frame%
  {class frame%
    (super-new)

    ;resolution: (and/c dimension-integer? positive?)
    ;determines how many pixels long the squares are that the canvas colors as a single unit
    ;if not specified, resolution is 6 (canvas colors 6x6 squares of pixels)
    (init-field [resolution 6])

    ;Returns the current resolution
    ;(get-resolution) → (and/c dimension-integer? positive?)
    {define/public get-resolution
      {lambda () resolution}}

    ;(set-resolution res) → void?
    ;  res: (and/c dimension-integer? positive?)
    ;[Side Effect]: sets resolution to res
    {define/public set-resolution
      {lambda (res) (set! resolution res)}}
    }}

;;complex-canvas%: class
;;  superclass: canvas%
;;  extends: canvas<%>
;;  purpose: adds a lot of additional functionality to the canvas class
{define complex-canvas%
  {class canvas%
    (super-new)

    ;black: (equal?/c (make-color 0 0 0))
    ;constant for the color black
    {define black (make-color 0 0 0)}

    ;dc: (is-a?/c dc<%>)
    ;the drawing context for the canvas
    {define dc (send this get-dc)}

    ;parent: (is-a?/c frame%)
    ;the parent frame for the canvas
    {define parent (send this get-parent)}

    ;width: (or/c dimension-integer? #f)
    ;the width of the parent frame, in pixels
    {define width (send parent get-width)}

    ;height: (or/c dimension-integer? #f)
    ;the height of the parent frame, in pixels
    {define height (send parent get-height)}

    ;resolution: (and/c dimension-integer? positive?)
    ;the resolution of the parent frame, in pixels
    {define resolution (send parent get-resolution)}

    ;equation: (-> complex? complex?)
    ;  the complex-valued equation to be used by the canvas's on-paint method
    ;scale: complex?
    ;  the value to multiply the complex inputs by in order to bring them into a desirable range
    ;x-offset: real?
    ;  the amount to shift the real boundary of the window by (effected by scale)
    ;y-offset: real?
    ;  the amount to shift the imaginary boundary of the window by (effected by scale)
    (init-field [equation identity]
                [scale (/ 4 (send parent get-width))]
                [x-offset 0] [y-offset 0])

    ;Returns the the distance in units between the real boundaries of the window
    ;  non-zero imaginary parts in the scale factor mean the horizontal boundaries of the window
    ;  are not parallel to the real axis
    ;(get-scale-factor) → complex?
    {define/public get-scale-factor {lambda () (* width scale)}}
    
    ;draws: exact-nonnegative-integer?
    ;the number of times the canvas has been updated
    {define draws 0}

    ;Returns the number of times the canvas has been updated
    ;(get-draws) → exact-nonnegative-integer?
    {define/public get-draws {lambda () draws}}

    ;Updates the canvas
    ;  called whenever the window is resized or at every timer interval if redraw is #t
    ;(on-paint [new-eq equation] [new-scale scale] [new-x x-offset] [new-y y-offset]) → void?
    ;  new-eq: (-> complex? complex?)
    ;  new-scale: complex?
    ;  new-x: real?
    ;  new-y: real?
    {define/override on-paint
      {lambda ([new-eq equation] [new-scale scale] [new-x x-offset] [new-y y-offset])
        
        ;Update canvas fields based on arguments and the parent frame
        (set! equation new-eq)(set! scale new-scale)(set! x-offset new-x)(set! y-offset new-y)
        (set! width (send parent get-width))
        (set! height (send parent get-height))
        (set! resolution (send parent get-resolution))

        ;Generate a list of all units of the frame to color based on the current resolution
        {define block-list (cartesian-product (range 0 width resolution) (range 0 height resolution))}
        ;Get a list of center points of each unit using the canvas's coordinate system
        {define point-list (map {lambda (p) (block->point p)}
                                block-list)}

        ;Iterate through the blocks and their correspond points
        ;  color converts the points to complex numbers,
        ;    processes them with equation, and outputs a color
        ;  the blocks are used for location data
        {for ([p point-list]
              [b block-list])
          (send dc set-brush (color (car p) (cdr p)) 'solid)
          (send dc draw-rectangle (car b) (cdr b) resolution resolution)}      

        ;Increment draws
        (set! draws (add1 draws))}}

    ;Takes a pair of numbers corresponding to the frame's coordinate system,
    ;  and converts to a pair of numbers corresponding to the canvas's coordinate system
    ;(block->point p) → (cons/c (and/c real? exact?) (and/c real? exact?))
    ;  p: (cons/c (and/c integer? (between/c 0 width))
    ;             (and/c integer? (between/c 0 height))
    {define/private block->point
      {lambda (p)
        (cons (+ (/ (- resolution width) 2) (car p))
              (+ (/ (- resolution height) 2) (cdr p)))}}

    ;Given two real numbers, creates a complex number transformed by scale and the offsets
    ;(scaled-number x y) → complex?
    ;  x: real?
    ;  y: real?
    {define/private scaled-number
      {lambda (x y)
        (+ (make-rectangular x-offset y-offset) (* scale (make-rectangular x y)))}}
    
    ;Given two real numbers, creates a complex number,
    ;  processes it with equation and then outputs a color
    ;The hue of the color corresponds to the angle of the output
    ;The lightness of the color corresponds to the magnitude of the output
    ;(color p) → (is-a?/c color%)
    ;  x: real?
    ;  y: real?
    {define/public color
      {lambda (x y)
        
        ;Get the input, process it with equation, and apply the color-constant
        {define output (equation (scaled-number x y))}

        {if (zero? output) black ;Prevent (angle 0) which is an error
            
            ;Magnitude used as lightness (L)
            ;Amplitude used as hue (H)
            ;Saturation is always 1
            (hsl->color (hsl ({lambda (x) (- x (floor x))} (/ (angle output) (* 2 pi))) 1
                             ({lambda (x) (/ x (add1 x))} (magnitude output))))}}}

    ;(on-event event) → void?
    ;  event: (is-a?/c mouse-event%)
    ;[Side Effect]: whenever the mouse left-clicks on the window,
    ;                 zoom in to that part of the graph by a factor of 2
    {define/override on-event
      {lambda (event)
        {when (send event button-down? 'left)
          {define p (block->point (cons (send event get-x) (send event get-y)))}
          {define c (scaled-number (car p) (cdr p))}
          (send this on-paint equation (/ scale 2)
                (real-part c)
                (imag-part c))}
        {when (send event button-down? 'right)
          {define p (block->point (cons (send event get-x) (send event get-y)))}
          {define c (scaled-number (car p) (cdr p))}
          (send this on-paint equation (* scale 2)
                (real-part c)
                (imag-part c))}}}
    #|
    {define command ""}
    
    {define/override on-char
      {lambda (event)
        {define code (send event get-key-code)}
        {when (and (char? code) (not (equal? #\backspace code))) (set! command (~a command code))
          (send dc draw-text command 100 700)}
        {when (equal? #\backspace code)
          (set! command (substring command 0 (sub1 (string-length command))))
          (on-paint) (send dc draw-text command 100 700)}}}
    |#
    }}
