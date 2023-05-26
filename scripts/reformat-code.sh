#!/bin/bash -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

find src/ app/ test/ gen-scenario/ -name "*.hs" | xargs fourmolu --mode=inplace