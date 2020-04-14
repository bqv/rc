#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl.bin git.out nix jq.out findutils.out gnused.out

set -euo pipefail
IFS=$'\n\t'

# set -x

REPO=jevolk/charybdis

REV=$(curl -Ls api.github.com/repos/$REPO/git/ref/heads/master | jq .object.sha -r)
echo "Revision: ${REV}" >&2

HASH=$(nix-prefetch-url --unpack github.com/$REPO/archive/$REV.tar.gz 2>/dev/null | xargs nix to-sri --type sha256)
echo "Hash: ${HASH}" >&2

DATE=$(curl -Ls api.github.com/repos/$REPO/commits/$REV | jq '.commit.committer.date|fromdate|strftime("%Y.%m.%d")' -r)
echo "On: ${DATE}" >&2

sed -i '/^    rev = /s%".*"%"'$REV'"%' default.nix
sed -i '/^    hash = /s%".*"%"'$HASH'"%' default.nix

echo 'git commit -am "construct: bump to '$REV' ('$DATE')"'
