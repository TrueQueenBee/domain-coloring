#lang scribble/manual

@title{Domain Coloring}
@author{@(author+email "Cassandra Rudig" "cassrudig@protonmail.com")

@defmodule[domain-coloring #:use-sources (domain-coloring)]

The domain-coloring library provides an interactive visual tool for
@hyperlink["https://en.wikipedia.org/wiki/Complex_analysis" "complex analysis"]
and a number of procedures and utilities for manipulating the display.
This is done using the technique of @hyperlink["https://en.wikipedia.org/wiki/Domain_coloring" "domain coloring"],
which works by assigning a color to each point on the complex plane to ease the representation and interpretation
of four-dimensional functions.
