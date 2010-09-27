#!/usr/bin/env bash

for a in *.asm; do
  ./assemble.sh $a
done

