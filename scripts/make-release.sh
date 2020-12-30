#!/usr/bin/env bash
set -euo pipefail

if [[ -n $(git status -s) ]]; then
    echo "Working directory has changes. Commit them first."
    exit 1
fi

VERSION="$1"

echo "Bumping version to $VERSION ..."

find . -name 'package.yaml' -type f -exec sed -i '' "s/^version:.*/version:             $VERSION/g" {} +

git add .
git commit -m "version bump"

stack upload stripe-hs
stack upload stripe-servant

git tag -a "$VERSION" -m "Hackage version $VERSION"
git push
git push --tags
