#!/bin/bash

msg_fail () {
    echo -e "\033[31m FAILED : $1\033[0m";
}
msg_success () {
    echo -e "\033[32m OK : $1\033[0m"
}
contact () {
    echo -e "\033[46m Please report error to victorn@cs.toronto.edu\033[0m";
    exit 0
}
sep () {
    echo ""
    echo -e "\033[44m $1 \033[0m"
}

# Tested on Ubuntu 20.04

mkdir -p $HOME/.local/bin
# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
    echo "Please add $HOME/.local/bin to your path (e.g. add it in .profile)"
fi

sep "Installing packages z3 and python3"
sudo apt update
sudo apt install z3 python3

sep "Installing CVC4"
CVC4_VERSION=$(cvc4 --version | sed -n 's/^.*CVC4 version \([0-9]*.[0-9]*\).*$/\1/p')
if [ -z $CVC4_VERSION ]
then
    msg_fail "CVC4 not installed !"
    sep
    echo "Installing CVC4 1.8 in $HOME/.local/bin"
    wget https://github.com/CVC4/CVC4/releases/download/1.8/cvc4-1.8-x86_64-linux-opt
    sudo chmod u+x cvc4-1.8-x86_64-linux-opt
    mv cvc4-1.8-x86_64-linux-opt $HOME/.local/bin/cvc4
else
    if [ $(bc <<< "$CVC4_VERSION >= 1.8") ]
    then
        msg_success "CVC4 version $CVC4_VERSION is already installed."
    else
        msg_fail "CVC4 version $CVC4_VERSION is installed, need CVC4 version > 1.8."
        msg_fail "Please update or uninstall CVC4 and run this script again."
        exit 1;
    fi
fi

# 2 - Ocaml componenets
sep "Checking Opam and Ocaml."

# Check if OPAM is installed
OPAM_VERSION=$(opam --version)
if [[ -z $OPAM_VERSION ]]
then
    msg_fail "Opam not installed ! Trying to install opam..."
    sudo apt-get install opam
    eval $(opam config env)
    opam init
    opam install depext
    opam config setup -a
    if [ $? -eq 0 ]; then
	    msg_success "Opam installed"
	    msg_success "If the script fails, check Opam is configured : opam config setup -a"
    fi
else
    msg_success "opam $OPAM_VERSION is installed."
fi

eval $(opam env)


# Ocaml version (and if Ocaml is present)
OCAML_VERSION=$(ocaml -vnum)
if [ -z $OCAML_VERSION ]
then
    opam install switch 4.11.1
    opam switch 4.11.1
else
    if [ $OCAML_VERSION \< "4.08.0" ]
    then
        msg_fail "OCaml $OCAML_VERSION is installed, but we need Ocaml > 4.08.0"
        msg_fail "Installing new switch..."
        opam install switch 4.11.1
        opam switch 4.11.1
    else
        msg_success "Ocaml $OCAML_VERSION is installed."
    fi
fi

sep "Installing Ocaml dependencies"
opam update
opam install core
opam install . --deps-only

sep "Compiling the tool..."
make

sep "Create link atropos to _build/default/bin/Atropos.exe"
ln -s _build/default/bin/Atropos.exe atropos

sep "Calling the tool, should print help message..."
./atropos -h

sep "Please update your environment (source $HOME/.profile) before running the tool."