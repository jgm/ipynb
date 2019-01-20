test:
	cabal v2-test

# install haskell-ci from Hackage:
.travis.yml: ipynb.cabal
	make-travis-yml --local-ghc-options="-Wall -Werror" -o .travis.yml $<

format:
	stylish-haskell -i src/Data/Ipynb.hs test/roundtrip.hs

.PHONY: test format
