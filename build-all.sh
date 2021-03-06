set -x

cd $(dirname "$0")

BINS=`pwd`/bin

if ! [ -d "$BINS" ]; then
  mkdir -p "$BINS"
fi

if [ ! -x "$BINS/pasta" ]; then
  if [ -x "$(which pasta)" ]; then
    PASTA=$(which pasta)
  else
    PASTA=/home/jules/code/pasta/pasta
  fi

  cp "$PASTA" "$BINS"
fi

if [ ! -x "$BINS/adfs" ]; then
  pushd adfs
  ./compile.sh
  popd
  cp adfs/adfs "$BINS"
fi

if [ ! -x "$BINS/bbcim" ]; then
  pushd bbcim
  ./mkbbcim
  popd
  cp bbcim/bbcim "$BINS"
fi

if [ ! -x "$BINS/pasta" ] || [ ! -x "$BINS/bbcim" ]; then
  echo 'Missing binary for pasta or bbcim! Whoops.'
  exit 1
fi

PATH="$BINS:$PATH"
OUTPUTDISK=$(readlink -f tmpdisk)

mkdir -p "$OUTPUTDISK"
pushd "$OUTPUTDISK"
rm -f *
popd

export OUTPUTDISK

set -e

./compile.sh

bbcim -new demodisk.ssd
pushd tmpdisk
bbcim -a ../demodisk.ssd *
popd
