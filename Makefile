test:
	cabal v2-test

validate-circleci:
	circleci config validate

run-circleci:
	circleci local execute --job cabal-ghc_8_6_3

# install haskell-ci from Hackage:
.travis.yml: ipynb.cabal
	make-travis-yml --local-ghc-options="-Wall -Werror" -o .travis.yml $<

format:
	stylish-haskell -i src/Data/Ipynb.hs test/roundtrip.hs

.PHONY: test format validate-circleci run-circleci
