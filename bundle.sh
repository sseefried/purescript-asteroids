#!/bin/bash

echo "Bundling...this may take a few minutes"
time psc-bundle output/*/{index,foreign}.js --module Asteroids.Main > html/asteroids.js