#!/bin/sh
# The compilation of these modules is only useful for editing benchmarks
# with an editor that uses the files to provide some IDE functionality.

rm -f list/*.c*
rm -f incomplete/list/*.c*
rm -f incomplete/bst/*.c*

ocamlc list/ConsList.ml
ocamlc list/ConcatList.ml -I ./list
ocamlc incomplete/list/ConsList.ml
ocamlc incomplete/list/ConcatList.ml -I ./list
ocamlc incomplete/bst/bst.ml
rm -f a.out
