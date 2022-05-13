FROM ocaml/opam:debian-11-ocaml-4.12
ENV DEBIAN_FRONTEND=noninteractive
CMD ["bash"]
# ocaml sets us with user "opam"
USER root
ENV HOME=/root
# Install system packages
RUN apt-get update && apt-get install -y git z3 wget
# Install cvc4
RUN wget -q https://github.com/CVC4/CVC4/releases/download/1.8/cvc4-1.8-x86_64-linux-opt && \
    sudo chmod a+x cvc4-1.8-x86_64-linux-opt && \
    mv cvc4-1.8-x86_64-linux-opt /bin/cvc4
# Install opam components; need to be opam user.
USER opam
ENV HOME=/home/opam
RUN opam install core -y
COPY --chown=opam:opam . /home/opam/synduce
RUN ls /home/opam/synduce
RUN cd /home/opam/synduce && \
    opam pin syguslib-utils -y
    opam install . --deps-only -y && \
    eval $(opam env) && \
    dune build bin/Synduce.exe && \
    ./_build/default/bin/Synduce.exe benchmarks/list/sum.ml -i
