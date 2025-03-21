#!/bin/bash
## Run as
#"  ./update.sh $SQLALCHEMY_DATABASE_URI $CACHE_SECRET

set -euo pipefail #exit on error, undefined and prevent pipeline errors
IFS=$'\n\t'
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

psql -d "$1" --set=dir="$DIR/../clean-data" -v ON_ERROR_STOP=on -f "$DIR"/update.sql
psql -d "$1" -c "VACUUM ANALYZE crime_latlong;"
psql -d "$1" -c "VACUUM ANALYZE cuadrantes;"
psql -d "$1" -c "VACUUM ANALYZE pgj;"

curl -X POST -F "CACHE_SECRET=$2" --header "fly-prefer-region: dfw" https://api.hoyodecrimen.com/clear-cache
curl -X POST -F "CACHE_SECRET=$2" --header "fly-prefer-region: iad" https://api.hoyodecrimen.com/clear-cache
(xargs -I % wget --header="fly-prefer-region: dfw" -q --show-progress --wait=14 --tries=3 -O /dev/null https://api.hoyodecrimen.com% < "$DIR"/urllist.txt) || true
(xargs -I % wget --header="fly-prefer-region: iad" -q --show-progress --wait=14 --tries=3 -O /dev/null https://api.hoyodecrimen.com% < "$DIR"/urllist.txt) || true


# pgsql2shp -f cuadrantes.shp -g geom -u USER -P PASSWORD -h localhost dbname "select * from cuadrantes_poly"
# shp2pgsql -I -d -s 4326  cuadrantes_poly > cuadrantes_poly_2023.sql
