#!/bin/sh
# The compilation of these modules is only useful for editing benchmarks
# with an editor that uses the files to provide some IDE functionality.
rm -f list/*.c*
ocamlc list/ConsList.ml
ocamlc list/ConcatList.ml -I ./list
rm -f a.out
