#!/usr/bin/env bash

rm -f vendor/*

touch vendor/ramjet-cli-ipr
touch vendor/sherlock-cli
touch vendor/nomossa
touch vendor/pathfinder

echo "Vendor directory reset"
ls -lh vendor/
