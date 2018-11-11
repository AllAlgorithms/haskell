#!/bin/bash

build_dir="./build"

build() {
  mkdir -p "$build_dir"
  ghc \
    -Wall -Werror \
    -odir "$build_dir" \
    -hidir "$build_dir" \
    -o "$build_dir"/GliderTest \
    GliderTest.hs \
    *.hs
}

run_test() { "$build_dir"/GliderTest; }

build && test

