all: ListCoq.hs

ListCoq.hs : List.v Util.vo
	coqc List.v
	sed -e '3i import qualified Data.Bits' -e '3i import qualified Data.Char' ListCoq.hs -i

Util.vo : Util.v
	coqc Util.v

clean:
	rm *.glob *.vo ListCoq.hs .*.aux
