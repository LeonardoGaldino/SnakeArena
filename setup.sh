#!/bin/sh
sudo apt-get update -y
sudo apt-get install cabal-install freeglut3 freeglut3-dev -y
cabal install fungen .