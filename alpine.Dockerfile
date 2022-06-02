FROM ocamlpro/ocaml:4.12
CMD ["sh"]
# Install z3 and some utilities
RUN sudo apk add bash wget z3
# Install cvc4
RUN wget -q https://github.com/CVC4/CVC4/releases/download/1.8/cvc4-1.8-x86_64-linux-opt && \
    sudo chmod a+x cvc4-1.8-x86_64-linux-opt && \
    sudo mv cvc4-1.8-x86_64-linux-opt /bin/cvc4
COPY --chown=ocaml:ocaml . /home/opam/synduce
# Install Ocaml dependencies
RUN cd /home/opam/synduce && opam switch create . --deps ocaml-system
# Build and run an example.
RUN cd /home/opam/synduce && \
    opam pin syguslib-utils -y
    opam install . --deps-only -y
    eval $(opam env) && \
    dune build bin/Synduce.exe && \
    ./_build/default/bin/Synduce.exe benchmarks/list/sum.ml
