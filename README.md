# Complex-Analysis

To use, open DrRacket and run the complex-grapher-test.rkt file in the same folder as complex-grapher.rkt. This program requires you to install the colors package, which you can do from the command line with `raco pkg install colors`.

You can also run the complex equation grapher in the racket run-time system:
- See your current directory with `,pwd`
- Navigate to the containing folder with `,cd <path>`
- Run the command `,enter "complex-grapher-test.rkt"`

From there, the following commands may be used to control the calculator:

```
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
(>> arg . procs) : pipeline arg through procs left to right
```

The window can be manually resized by dragging the border and the calculator will automatically update.\
Left clicking anywhere on the window will cause the calculator to zoom in to double the magnification and recenter the graph at the clicked point.\
Right clicking the window will cause the calculator to zoom out to half the magnification and recenter the graph at the clicked point.

Additional explanation and details on implementation can be found in the comments in the source files.

