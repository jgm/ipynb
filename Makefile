test:
	cabal v2-test

# install haskell-ci from Hackage:
.travis.yml: ipynb.cabal
	make-travis-yml -o .travis.yml $<

.PHONY: test
