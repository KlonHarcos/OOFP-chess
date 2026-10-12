#!/bin/bash

cd Stockfish/src/
make -j profile-build
cd ../..
mv Stockfish/src/stockfish src/stockfish_executable