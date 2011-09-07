#!/bin/sh
rm -f output.asc
for x in `seq 2 13`; do
  ./ttxconv texts/text$x tmp
  cat tmp >> output.asc
done
