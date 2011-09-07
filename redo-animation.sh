#!/bin/sh
cp ~/.wine/drive_c/Program\ Files/Cebra\ Teletext/text{2,3,4,5,6,7,8,9,10,11,12,13} texts
./ttxconv.sh
../../code/mediaplayer/teletext/squeeze output.asc nyansqz
