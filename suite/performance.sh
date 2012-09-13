[ ! -f "suite/Performance.hs" ] && echo "performance.sh: Wrong directory!" && exit

mkdir -p suite/performance/Results

echo "Enter a description of this test set: "
read DESC
NAME=`date +%y%m%d-%H%M`

cabal clean
cabal configure --enable-benchmarks -O2
cabal build

echo $NAME $HOSTNAME "SEQ" $DESC >> "suite/performance/Results/descs.txt"
DIR="suite/performance/Results/seq"
mkdir -p $DIR

echo "performance.sh: Benchmarks:"
cabal bench --benchmark-options="-s 20 -u $DIR/$NAME.csv $1"

echo "performance.sh: Benchmarks complete."
echo "performance.sh: Output in $DIR/$NAME.csv"