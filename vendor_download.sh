#!/usr/bin/env bash
#
# Requires environment variables:
#   GITHUB_TOKEN    A token with access to the fossas/basis repository
#
# Requires binary dependencies in $PATH:
#   jq              Parse and manipulate json structures.
#   curl            Download data over HTTP(s)
#   upx             Compress binaries
#   nomossa         (MacOS builds only)
#

set -e

if [ -z "$GITHUB_TOKEN" ]; then
  echo "Provide your GITHUB_TOKEN in the environment"
  exit 1
fi

rm -f vendor/*

ASSET_POSTFIX=""
PATHFINDER_PATH=""
NOMOSSA_PATH=""
case "$(uname -s)" in
# case "Linux" in
  Darwin)
    ASSET_POSTFIX="darwin"
    NOMOSSA_PATH=$(which nomossa | xargs echo -n)
    PATHFINDER_PATH=$(find . -type f -path '*osx/*/pathfinder/pathfinder' | xargs echo -n)
    ;;

  Linux)
    ASSET_POSTFIX="linux"
    PATHFINDER_PATH=$(find . -type f -path '*linux/*/pathfinder/pathfinder' | xargs echo -n)
    ;;
  
  *)
    echo "Supported hosts are MacOS and Linux"
    exit 1
    ;;
esac

if [ $ASSET_POSTFIX == "darwin" ] && [ $NOMOSSA_PATH == "" ]; then
  echo "Binary 'nomossa' not found in PATH"
  exit 1
fi

NOMOSSA_OUTPUT="vendor/nomossa"
PATHFINDER_OUTPUT="vendor/pathfinder"

if [ ! -z $NOMOSSA_PATH ]; then
  echo "Copying '$NOMOSSA_PATH' to '$NOMOSSA_OUTPUT'"
  cp $NOMOSSA_PATH $NOMOSSA_OUTPUT
fi

echo "Copying '$PATHFINDER_PATH' to '$PATHFINDER_OUTPUT'"
cp $PATHFINDER_PATH $PATHFINDER_OUTPUT

TAG="latest"
echo "Downloading asset information from latest tag for architecture '$ASSET_POSTFIX'"

FILTER=".name == \"ramjet-cli-ipr-$ASSET_POSTFIX\" or .name == \"sherlock-cli-$ASSET_POSTFIX\""
if [ $ASSET_POSTFIX == "linux" ]; then
  FILTER="$FILTER or .name == \"nomossa\""
fi

curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/vnd.github.v3.raw" -s api.github.com/repos/fossas/basis/releases/latest | jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" | while read ASSET; do
  URL="$(echo $ASSET | jq -c -r '.url')"
  NAME="$(echo $ASSET | jq -c -r '.name')"
  OUTPUT=vendor/${NAME%"-$ASSET_POSTFIX"}

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s $URL > $OUTPUT
done

echo "Compressing binaries"
upx vendor/*

echo "Marking binaries executable"
chmod +x vendor/*

echo "Vendored binaries are ready for use"
ls -lh vendor/
