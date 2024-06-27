#!/usr/bin/env bash
# Exit on error, undefined and prevent pipeline errors,
# use '|| true' on commands that intentionally exit non-zero
set -euox pipefail
IFS=$'\n\t'
# The directory from which the script is running
readonly LOCAL_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Generate zoom 13 to 15 (we can overzoom the map if necessary) and use a cluster distance of 3 pixels
# for zooms 13-14 and no clustering for zoom 15
# tippecanoe --force -r1 -Z13 -z14 \
#            -o "$LOCAL_DIR"/clean-data/json/homicides1.mbtiles \
#            -l crime-lat-long-pgj \
#            -n "crime-lat-long-pgj" \
#            "$LOCAL_DIR"/clean-data/json/homicides.geojson \
#            --cluster-distance=2
# tippecanoe --force -r1 -z15 \
#            -o "$LOCAL_DIR"/clean-data/json/homicides2.mbtiles \
#            -l crime-lat-long-pgj \
#            -n "crime-lat-long-pgj" \
#            "$LOCAL_DIR"/clean-data/json/homicides.geojson
# tile-join --force -o "$LOCAL_DIR"/clean-data/json/"homicides.mbtiles"  \
#           "$LOCAL_DIR"/clean-data/json/homicides1.mbtiles  \
#           "$LOCAL_DIR"/clean-data/json/homicides2.mbtiles
# pmtiles convert "$LOCAL_DIR"/clean-data/json/homicides.mbtiles \
#         "$LOCAL_DIR"/clean-data/json/homicides.pmtiles
tippecanoe --force -r1 -z12 -Z12 \
           -o "$LOCAL_DIR"/clean-data/json/homicides.mbtiles \
           -l crime-lat-long-pgj \
           -n "crime-lat-long-pgj" \
           "$LOCAL_DIR"/clean-data/json/homicides.geojson
pmtiles convert "$LOCAL_DIR"/clean-data/json/homicides.mbtiles \
        "$LOCAL_DIR"/clean-data/json/homicides.pmtiles


tippecanoe --force -r1 -z12 -Z12 \
           -o "$LOCAL_DIR"/clean-data/json/selected-crimes.mbtiles \
           -l crime-lat-long-pgj \
           -n "crime-lat-long-pgj" \
           "$LOCAL_DIR"/clean-data/json/selected-crimes.geojson
pmtiles convert "$LOCAL_DIR"/clean-data/json/selected-crimes.mbtiles \
        "$LOCAL_DIR"/clean-data/json/selected-crimes.pmtiles

#rm -rf "$LOCAL_DIR"/clean-data/json/*.mbtiles
