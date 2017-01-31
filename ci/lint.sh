#!/usr/bin/env sh

set -euo pipefail

for file in $(git ls-files | grep '\.hs$'); do
  echo "Checking $file with hlint:"
  if ! hlint "$file"; then
    exit 1
  fi
done
exit 0
