#!/usr/bin/env bash

set -euo pipefail

lint=$(dirname "$0")/lint.sh

for file in $(git ls-files | grep '\.hs$'); do
  $lint "$file"
done
