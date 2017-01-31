#!/usr/bin/env bash

set -euo pipefail

lint=$(dirname "$0")/lint.sh

for file in $(git diff-index --cached --name-only HEAD | grep '\.hs$'); do
  lint "$file"
done
