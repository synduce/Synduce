#!/bin/sh
# Tested on ubuntu 20.04
sudo apt install z3 cvc4 python3
opam init
eval $(opam env)
opam install core
opam install . --deps-only
make
./atropos -h
