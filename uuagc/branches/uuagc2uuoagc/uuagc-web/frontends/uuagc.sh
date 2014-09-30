#! /bin/sh -e

# Todo: make this script work when files contain spaces
# I don't know how to do this in a portable way for now,
# so better don't put spaces in your filenames.

set -e

# ugly parameter test
if test "$#" -lt 2; then
  echo "usage: $0 <host:port> (<file> | [ag-opt])+" 1>&2
  exit 1
fi

# first argument *must* be the host:port combination
addr="$1"
shift


# Scan commandline arguments for
# input files, output file, and other options

inputfiles=
outputfile=
prevOpt=
opts=
verbose=
for arg in "$@"; do
  # test if the argument is an option (starts with "-")
  if test "x$arg" != "x${arg#-}"; then
    prevOpt="$arg"
    opts="$arg $opts"
    if test "x$arg" = "x-v" -o "x$arg" = "x--verbose"; then
      verbose=1
    fi
  else
    ext="${arg##*.}"

    if test "x$prevOpt" = "x-o" -o "x$prevOpt" = "x--output"; then
      # found an output file
      outputfile="$arg"
    elif test "x$ext" = xag -o "x$ext" = xlag; then
      # looks like an input file based on the extension
      # inputfiles="${inputfiles[@]}" # "$arg"
      inputfiles="$inputfiles $arg"
    else
      # assume that its an argument to an option
      opts="$arg $opts"
    fi

    prevOpt=
  fi
done

if test -z "$inputfiles"; then
  echo "error: no .ag input files given" 1>&2
  exit 1
fi

if test -z "$outputfile"; then
  for candidate in $inputfiles; do
    outputfile=$(echo "${candidate%.*}.hs" | tr -d ' ')
    break
  done
fi


# start preparing the actual input file
# this is the concatenation of the
# ag files so that includes are
# before their includers.
# don't create cyclic includes or you'll
# end up with a very big file...

inputfile=$(mktemp)

append()
{
  local file=$(echo "$1" | tr -d ' ')
  local dir="$(dirname $file)/"

  if test -n "$verbose"; then
    echo "including file: $file"
  fi

  includes="$(grep -iE '^[[:space:]]*INCLUDE[[:space:]]+".+"[[:space:]]*$' $file | sed -r 's/^[[:space:]]*INCLUDE[[:space:]]+"(.+)"[[:space:]]*$/\1/')"
  if test -n "$includes"; then
    appendall "$dir" $includes
  fi

  # put a line directive on top so that we can get error messages in
  # terms of the actual source locations
  echo "{- LINE 1 \"$file\" -}" >> $inputfile

  # then the actual contents
  cat $file >> $inputfile

  # since we concatenate files, this means that when in the original file an unexpected end of file
  # would be encountered, here it would continue with the next file. This is rather undesirable. This
  # hack should help a bit.
  echo "{- LINE 9999999 \"$file\" -}" >> $inputfile
  echo "{}"                           >> $inputfile  # two consequtive {}'s can only occur at toplevel
  echo "{}"                           >> $inputfile  # so this terminates incompete nested parse processes
}

appendall()
{
  local prefix=$1
  shift

  for include in "$@"; do
    append "${prefix}${include}"
  done
}

appendall "" $inputfiles


# use curl to upload the source file to the server

# temp files for the resulting headers and content
headers=$(mktemp)
content=$(mktemp)

if test -n "$verbose"; then
  curl -D $headers -F "opts=$opts" -F "src=@$inputfile" "http://$addr/compile" --progress-bar -o $content
else
  curl -D $headers -F "opts=$opts" -F "src=@$inputfile" "http://$addr/compile" -s -S -o $content
fi

# check if the returned contents were actually error messages
if grep -q 'uuagc error' "$headers"; then
  cat "$content" 1>&2
else
  cp "$content" "$outputfile"
  if test -n "$verbose"; then
    echo "generated: $outputfile"
  fi
fi

# cleanup temporary files
rm -f "$headers" "$content" "$inputfile"
