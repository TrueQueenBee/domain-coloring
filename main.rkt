#lang racket

(require "complex-grapher.rkt"
         "complex-grapher-test.rkt")
         
(provide sgn/c
         arity-overlaps?
         homogenous?
         sigmoid
         (rename-out [cartesian-product cartesian-product-2]
                     [num->byte number->byte]
                     [dist-est mandelbrot-distance-estimate]
                     [polyprime polynomial-derivative])
         mod-extended
         combine
         complex-timer%
         complex-frame%
         complex-canvas%
         complex
         random-complex
         zeroes
         zero-pole
         mandelbrot
         polynomial
         newton-step
         integrate
         erfz
         f:
         >>
         graph
         set-res
         show
         reset
         near-multiple?
         counter
         redraw
         help)
