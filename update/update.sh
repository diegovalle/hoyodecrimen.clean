#!/bin/bash
## Run as
#"  ./update.sh update.sh $SQLALCHEMY_DATABASE_URI $CACHE_SECRET

set -euo pipefail #exit on error, undefined and prevent pipeline errors
IFS=$'\n\t'
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

psql -d "$1" --set=temp="$DIR/../clean-data" -f "$DIR"/update.sql
psql -d "$1" -c "VACUUM ANALYZE crime_latlong;"
psql -d "$1" -c "VACUUM ANALYZE cuadrantes;"
psql -d "$1" -c "VACUUM ANALYZE pgj;"

curl -X POST -d "CACHE_SECRET=$2" https://hoyodecrimen.com/clear-cache
(xargs -I % wget -q --show-progress --wait=14 --tries=3 -O /dev/null https://hoyodecrimen.com% < "$DIR"/urllist.txt) || true
