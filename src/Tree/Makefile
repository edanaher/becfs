all: ListCoq.hs MonoBtreeCoq.hs

ListCoq.hs : List.v Util.vo
	coqc List.v
	sed -e '3i import qualified Data.Bits' -e '3i import qualified Data.Char' -e '3i import Debug.Trace (trace)' ListCoq.hs -i

MonoBtreeCoq.hs : MonoBtree.v Util.vo
	coqc MonoBtree.v
	sed -e '3i import qualified Data.Bits' -e '3i import qualified Data.Char' -e '3i import Debug.Trace (trace)' MonoBtreeCoq.hs -i

Util.vo : Util.v
	coqc Util.v

clean:
	rm -f *.glob *.vo .*.aux *.o *.hi
	rm -f ListCoq.hs MonoBtreeCoq.hs
