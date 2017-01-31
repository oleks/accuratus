#!/usr/bin/env bash

set -euo pipefail

file=$1

echo "Checking $file with hlint:"
if ! hlint "$file"; then
  exit 1
fi

exit 0
