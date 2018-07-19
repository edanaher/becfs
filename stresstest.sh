
fusermount -u test
set -e
mkdir -p test
result/bin/becfs test
cd test

depth=${1:-3}
branch=${2:-3}

function rundir() {
  local curdepth=$1
  local path=$2
  local letter=$(printf "\x$(printf %x $((96+$curdepth)))")
  local i
  for i in `seq 1 $branch`; do
    newpath=$path$letter$i
    if [ $depth == $curdepth ]; then
      echo $newpath > $newpath
    else
      mkdir $newpath
      rundir $(($curdepth+1)) $newpath/
    fi
  done
}

rundir 1
