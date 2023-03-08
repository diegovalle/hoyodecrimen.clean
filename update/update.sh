#!/bin/bash
#  ./update.sh postgres://deploy:xxxxx@hoyodecrimen.com:44765/apihoyodecrimen xxxx

set -euo pipefail #exit on error, undefined and prevent pipeline errors
IFS=$'\n\t'
#URL="https://data.diegovalle.net/hoyodecrimen"
#TMPDIR="$RUNNER_TEMP"


#wget -N $URL/crime-lat-long-pgj.csv.zip $URL/cuadrantes-pgj.csv.zip $URL/pgj.csv.zip -P "$TMPDIR"
#(cd "$TMPDIR" && unzip -o -d "$TMPDIR/$(date +%Y-%m)" -j '*csv.zip')
#cp "$TMPDIR"/"$(date +%Y-%m)"/*.csv "$TMPDIR"

psql -d "$1" --set=temp="$GITHUB_WORKSPACE/clean-data" -f "$GITHUB_WORKSPACE"/update/update.sql
psql -d "$1" -c "VACUUM ANALYZE crime_latlong;"
psql -d "$1" -c "VACUUM ANALYZE cuadrantes;"
psql -d "$1" -c "VACUUM ANALYZE pgj;"

curl -X POST -d "CACHE_SECRET=$2" https://hoyodecrimen.com/clear-cache
(xargs -I % wget -q --show-progress --wait=14 --tries=3 -O /dev/null https://hoyodecrimen.com% < "$GITHUB_WORKSPACE"/update/urllist.txt) || true
