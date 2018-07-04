APT_CMD=$(which apt)
BREW_CMD=$(which brew)

if [[ ! -z $BREW_CMD ]]; then
 	echo "Using brew to install dependencies"
 	sudo brew install cabal-install
elif [[ ! -z $APT_CMD ]]; then
	echo "Using apt to install dependencies"
	sudo apt-get update -y
	sudo apt-get install cabal-install freeglut3 freeglut3-dev -y
else
    echo "Error: Can't detect package installer"
    exit 1;
fi

cabal update
cabal install .