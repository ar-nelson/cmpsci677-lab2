all: build

build: configure
	cabal build
	cp dist/build/cmpsci677-lab2/cmpsci677-lab2 ./smarthome

configure: cabal.sandbox.config
	cabal configure

cabal.sandbox.config: You_Need_To_Install_The_Haskell_Platform
	cabal sandbox init
	cabal install --only-dependencies

clean: You_Need_To_Install_The_Haskell_Platform
	rm -rf .cabal-sandbox
	rm -rf cabal.sandbox.config
	cabal clean

You_Need_To_Install_The_Haskell_Platform:
	which cabal
