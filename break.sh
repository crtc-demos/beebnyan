#!/bin/sh
beatlength=$(echo "10 k 60 71 / p" | dc)
echo "beat length: $beatlength"
barlength=$(echo "10 k $beatlength 4 * p" | dc)
echo "bar length: $barlength"
sox out.wav start.wav trim 0 3.98
#for x in `seq 0 62`; do
#  sox out.wav bar$(printf "%.2d" $x).wav trim $(echo "10 k $barlength $x * 3.98 + p" | dc) $barlength
#done
RATE=6250
sox mono.wav --no-dither -c 1 -r 6250 --endian little -2 -u -t raw loop.raw trim 3.98 $(echo "10 k $barlength 8 * p" | dc)
sox mono.wav --no-dither -c 1 -r 6250 --endian little -2 -s -t raw sloop.raw trim 3.98 $(echo "10 k $barlength 8 * p" | dc)

