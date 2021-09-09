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

OS=$(uname)
case $OS in
    'Darwin')
        msg_success "Installation on macOS using brew."
        OS='Mac'
        PKG_INSTALL="brew install"
        PKG_UDPATE="brew update"
        ;;
    'Linux')
        msg_success "Installation on Linux using apt."
        OS='Linux'
        PKG_INSTALL="sudo apt install -y"
        PKG_UPDATE="sudo apt update"
        ;;
    *)
        msg_fail "Unknown OS - please install components manually."
        exit
esac


# Tested on Ubuntu 20.04, 21.04

mkdir -p $HOME/.local/bin
# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
    echo "Please add $HOME/.local/bin to your path (e.g. add it in .profile)"
fi

sep "Updating package lists"
eval "$PKG_UPDATE"

sep "Installing python3 ..."
PYTHON3_VERSION=$(python3 --version)
if [[ -z PYTHON3_VERSION ]]
then
    eval "$PKG_INSTALL python3"
else
    msg_success "$PYTHON3_VERSION is already installed."
fi


sep "Installing z3 ..."
Z3_VERSION=$(z3 --version | sed -n 's/^.*Z3 version \([0-9]*.[0-9]*\).[0-9]*.*$/\1/p')
if [[ -z $Z3_VERSION ]]
then
    msg_fail "Z3 not installed !"
    sep
    echo "Installing Z3 using your package manager."
    $PKG_INSTALL z3
else
    if [ $(bc <<< "$Z3_VERSION >= 4.8") ]
    then
        msg_success "Z3 version $Z3_VERSION is already installed."
    else
        msg_fail "Z3 version $Z3_VERSION is installed, need Z3 version > 4.8."
        msg_fail "Please update or uninstall Z3 and run this script again."
        exit 1;
    fi
fi

sep "Installing CVC4"
CVC4_VERSION=$(cvc4 --version | sed -n 's/^.*CVC4 version \([0-9]*.[0-9]*\).*$/\1/p')
if [[ -z $CVC4_VERSION ]]
then
    msg_fail "CVC4 not installed !"
    case $OS in
        'Mac')
            msg_fail "Skipping installing CVC4 on Mac."
            ;;
        'Linux')
            echo "Installing CVC4 1.8 in $HOME/.local/bin"
            wget -q https://github.com/CVC4/CVC4/releases/download/1.8/cvc4-1.8-x86_64-linux-opt
            sudo chmod u+x cvc4-1.8-x86_64-linux-opt
            mv cvc4-1.8-x86_64-linux-opt $HOME/.local/bin/cvc4
            ;;
        *)
            msg_fail "Don't know how to install CVC4 on this computer.";
            exit 1;
            ;;
    esac
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

if [ $OS == 'Mac' ]
then
    sep "Checking CVC5 installation..."
    CVC5_VERSION=$(cvc5 --version | sed -n 's/^.*cvc5 version \([0-9]*.[0-9]*\).*$/\1/p')
    if [[ -z $CVC5_VERSION ]]
    then
        msg_fail "Please install CVC5 manually."
        exit;
    else
        msg_success "CVC5 is installed."
    fi
fi


# 2 - Ocaml componenets
sep "Checking Opam and Ocaml."

# Check if OPAM is installed
OPAM_VERSION=$(opam --version)
if [[ -z $OPAM_VERSION ]]
then
    msg_fail "Opam not installed ! Trying to install opam..."
    $PKG_INSTALL opam
    eval $(opam config env)
    opam init -a
    opam install -y depext
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
    opam install switch 4.12.0
    opam switch 4.12.0
else
    if [ $OCAML_VERSION \< "4.08.0" ]
    then
        msg_fail "OCaml $OCAML_VERSION is installed, but we need Ocaml >= 4.09.0"
        msg_fail "Installing new switch..."
        opam install switch 4.12.0
        opam switch 4.12.0
    else
        msg_success "Ocaml $OCAML_VERSION is installed."
    fi
fi

sep "Installing Ocaml dependencies"
opam update
opam install -y core
opam install -y . --deps-only

sep "Compiling the tool..."
make

if [ -e Synduce ]
then
    msg_success "Synduce should link to _build/default/bin/Synduce.exe"
else
    sep "Create link Synduce to _build/default/bin/Synduce.exe"
    ln -s _build/default/bin/Synduce.exe Synduce
fi
# Initialize submodules
git submodule init
git submodule update

# Running tests
sep "Calling the tool, should print help message..."
./Synduce -h
echo "Running the tool on the benchmarks should take 10 to 40 minutes."
echo "Would you like to run Synduce on the benchmarks [y/N] ?"
read RUNTESTS
case $RUNTESTS in
    'y')
    sep "Running tests: should take about 10mins..."
    ./test/runtests.sh
    ;;
    *)
    echo "Skipping tests."
esac
sep "  Please update your environment (source $HOME/.profile) before running the tool."

