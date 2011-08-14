# builds uuagc with the uuagc compiler in PATH

echo "** Running autoconf"
autoconf

echo "** Producing derived files"
./configure
make derived

# echo "** Bootstrapping with existing uuagc compiler"
# ./configure
# make build
# make clean
#
# echo "** Using new uuagc compiler to compile ag files"
# ./configure --with-ag=`pwd`/dist/build/uuagc/uuagc
# make derived
