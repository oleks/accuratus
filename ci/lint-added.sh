#!/usr/bin/env bash

set -euo pipefail

for file in $(git diff-index --cached --name-only HEAD | grep '\.hs$'); do
  echo "Checking $file with hlint:"
  if ! hlint "$file"; then
    exit 1
  fi
done
exit 0
