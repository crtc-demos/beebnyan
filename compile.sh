#!/bin/sh
set -x
set -e
../../code/pasta/pasta -a beebnyan.s -o nyancat
cp nyancat nyancat.inf "$OUTPUTDISK"
cp nyansqz nyansqz.inf "$OUTPUTDISK"
cp song song.inf "$OUTPUTDISK"
cp '!boot' '!boot.inf' "$OUTPUTDISK"
