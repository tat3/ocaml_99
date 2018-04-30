99 Problems in OCaml
============
.. image:: https://travis-ci.org/tat3/ocaml_99.svg?branch=master
    :target: https://travis-ci.org/tat3/ocaml_99
    :alt: Build Status

A practice for OCaml

Contents
------------

* src/questions.ml -> main codes
* tests/example_test.ml -> test codes

Requirements
------------

* OCaml
* OPAM
* OASIS
* OUnit

Usage
------------

.. code-block:: bash

    $ oasis setup
    $ make configure CONFIGUREFLAGS=--enable-tests
    $ make test
