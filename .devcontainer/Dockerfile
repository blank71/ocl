FROM ocaml/opam:fedora-41-ocaml-5.2@sha256:cf783a5bb72326cec8dcf17a3a3c32d4f23a339e09e947edca245daa86f80deb

RUN opam install dune menhir core ocaml-lsp-server ocamlformat alcotest

RUN echo "eval \$(opam env)" >> /home/opam/.bashrc 
