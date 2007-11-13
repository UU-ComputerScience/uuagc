
DIR=`dirname $0`
cd $DIR

PATH=/usr/local/bin:$PATH

# Temporary location
DEST1=/home/alexey/hut
HOST=losser.labs.cs.uu.nl

INTERVAL=$1

function cp-pkg {
  PKG=$1
  ./mk-src.sh $PKG $INTERVAL || return 30
  
  # Extract names, attention it requires a unique glob match,
  # so that there is only one uuagc and one uulib.
  NAME=$PKG*
  
  # Copy snapshots 
  scp $NAME $HOST:$DEST1
  
  # The most recent tarballs
  scp $NAME $HOST:$DEST1/$PKG-latest-src.tar.gz
  
  # Copy to ultimate destination
  ssh -t $HOST "exec $DEST1/copy-to-nix.sh"
  
  rm $NAME
}

cp-pkg uulib
cp-pkg uuagc
