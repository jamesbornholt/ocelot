---
title: Ocelot
summary: Ocelot is an embedding of relational logic (like Alloy) in the Rosette solver-aided langage.
---

# Ocelot
<h2 class="subhead">A Solver-Aided Relational Logic DSL</h2>

<img class="logo" src="img/ocelot.jpg" />

Ocelot provides an embedding of relational logic (like [Alloy](http://alloy.mit.edu))
in the [Rosette](http://emina.github.io/rosette/) solver-aided programming language.
The embedding allows both solving and verification of relational specifications,
as well as *synthesis* of relational specifications,
and integration with other Rosette constraints.

Ocelot's synthesis support is the key to our [MemSynth](http://memsynth.uwplse.org) project
for synthesizing memory consistency model specifications.

Ocelot is developed at the [University of Washington](http://cs.washington.edu)
by [James Bornholt](https://homes.cs.washington.edu/~bornholt/)
and [Emina Torlak](https://homes.cs.washington.edu/~emina/).

### Getting started

Ocelot is available through the [Racket](https://racket-lang.org) package manager:

    raco pkg install ocelot

The [documentation](https://docs.racket-lang.org/ocelot/) provides a quick start guide to constructing Ocelot programs.
Ocelot is similar to [Alloy](http://alloy.mit.edu),
and so many of the principles and examples from that language
will translate well.

### Get the code

Ocelot is available [on GitHub](https://github.com/jamesbornholt/ocelot).
