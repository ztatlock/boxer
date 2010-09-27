#!/usr/bin/env bash

# an asm is just a card, but potentially with comments
# asm to card conversion = take first 10 chars of each line

asm=$1
crd="$(basename $asm .asm).card"

sed 's/^\(..........\).*$/\1/' $asm > $crd

