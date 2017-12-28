# ccat

A tool that dumps the given source file and syntax-colors
it using a Joe syntax file (JSF).

The JSF handling code is mostly copied from
the editor I wrote for DOS, and because that one
has to be compilable with pre-standard Borland C++,
the include files are written around its limitations.

Therefore the code does not look very pretty,
and it should not be taken as a model of how to do C++.

The tool `ccat` was written only for the purposes of this video,
and therefore it makes the following assumptions:

* If a filename is passed on commandline and it is not “-”, the data is read from that file and is colored using `ctcode2.jsf`.
* Otherwise the data is read from stdin and is colored using `c.jsf`.
