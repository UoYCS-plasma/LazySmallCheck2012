[ $# -ne 1 ] && echo "performance.sh: Expecting one argument representing core." && exit
[ ! -f "suite/Performance.hs" ] && echo "performance.sh: Wrong directory!" && exit

mkdir -p suite/performance/Results

echo "Enter a description of this test set: "
read DESC
NAME=`date +%y%m%d-%H%M`

cabal configure --enable-benchmarks
cabal build

echo $NAME $HOSTNAME $DESC >> "suite/performance/Results/descs.txt"
DIR="suite/performance/Results/$NAME"
mkdir $DIR

for i in `seq 1 $1`
do
   echo
   echo
   echo
   echo "performance.sh: Benchmark -N$i:"
   cabal bench --benchmark-options="-s 20 -u $DIR/N$i.csv LSC2012 +RTS -N$i"
done

echo "performance.sh: Benchmarks complete."