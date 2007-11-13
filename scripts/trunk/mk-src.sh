PACKAGE=$1
# Time interval (hour granularity) over which we do not do snapshots
if test "d$2" != d; then
  INTERVAL=$(($2 * 60 * 60))
else
# Default interval is huge (~100yrs) so it always works
  INTERVAL=$((100 * 365 * 24 * 60 * 60)) 
fi

rm -rf trunk
svn co https://svn.cs.uu.nl:12443/repos/uust-repo/$PACKAGE/trunk/
# Adding revision information
cat >> trunk/README <<EOF

Revision information (for bug reporting)
----------------------------------------

EOF
svn info trunk/ >> trunk/README

# Check if we do the snapshot
PKGDATE=`grep 'Last Changed Date'  trunk/README   | sed 's/.*\(20..-..-..\).*/\1/'`
PKGSECS=`date +%s -d $PKGDATE`
NOW=`date +%s`
echo "Last modification to $PACKAGE: $PKGDATE"
# Is the last change within the snapshot inverval?
# if not, abort. (crap shell programming)
test $(($NOW - $PKGSECS)) -le $INTERVAL || echo "Change is not recent, aborting snapshot"
test $(($NOW - $PKGSECS)) -le $INTERVAL || exit 30

(cd trunk && autoreconf && ./configure && make dist)

# Copy snapshots
cp trunk/$PACKAGE-20* .

# This part removes the date from the directory name
# Requires unique globbing!
SRC=`echo $PACKAGE-20*`
DST=$PACKAGE
DIR=`basename $SRC -src.tar.gz`

tar xzvf $SRC
mv $DIR $DST
rm $SRC
tar czvf $SRC $DST
rm -rf $DST


