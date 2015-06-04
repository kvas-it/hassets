.PHONY: clean all

MAIN=Hassets.hs
SOURCES=Asset.hs AssetHolder.hs ${MAIN}
INTERMEDIATE=*.hi *.o
TARGET=Hassets

all: ${TARGET}

${TARGET}: ${SOURCES}
	ghc -o ${TARGET} ${MAIN}

clean:
	rm -f ${INTERMEDIATE} ${TARGET}
