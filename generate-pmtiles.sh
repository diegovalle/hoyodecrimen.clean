#!/usr/bin/env bash
# Exit on error, undefined and prevent pipeline errors,
# use '|| true' on commands that intentionally exit non-zero
set -euo pipefail
if [[ "${TRACE-0}" == "1" ]]; then set -o xtrace; fi
IFS=$'\n\t'
# The directory from which the script is running
readonly LOCAL_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./script.sh arg1 arg2

What the bash script does.

'
    exit
fi

main() {
    local VAR=123
    ogr2ogr -f GeoJSON clean-data/crime-lat-long-pgj.geojson clean-data/crime-lat-long-pgj.csv -oo X_POSSIBLE_NAMES=long  -oo Y_POSSIBLE_NAMES=lat -oo KEEP_GEOM_COLUMNS=NO 
    tippecanoe --force  -Z1 -z12 -o clean-data/mbtiles/crime-lat-long-pgj.mbtiles -l crime-lat-long-pgj -n "crime-lat-long-pgj" -j '{ "crime-lat-long-pgj": [ "all", [ "==", "crime", "HOMICIDIO DOLOSO" ] ] }' clean-data/crime-lat-long-pgj.geojson  --cluster-distance=9
}

main "$@"
