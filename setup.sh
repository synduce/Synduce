#!/bin/sh
# Tested on ubuntu 20.04
sudo apt install z3 python3
echo "Installing CVC4 1.8 in $HOME/.local/bin"
wget https://github.com/CVC4/CVC4/releases/download/1.8/cvc4-1.8-x86_64-linux-opt
sudo chmod u+x cvc4-1.8-x86_64-linux-opt
mkdir -p $HOME/.local/bin
mv cvc4-1.8-x86_64-linux-opt $HOME/.local/bin/cvc4
echo "Installing Ocaml components..."
sudo apt install opam
opam init
eval $(opam env)
opam install switch 4.11.1
opam switch 4.11.1
opam install core
opam install . --deps-only
echo "Compiling the tool..."
make
echo "Calling the tool, should print help message..."
./atropos -h
