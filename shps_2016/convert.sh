topojson --id-property=id \
	-s 1e-10 \
        -o topojson/cuadrantes-interactive.json \
        --properties id=Nomenclatu,sector=Sector_hoy\
        -- sectores=cuadrantes.shp

topojson \
	--width 960 \
	--height 800 \
	--margin 0 \
	--id-property=id  \
        -s 1e-10 \
	--projection 'd3.geo.mercator()' \
	-o topojson/cuadrantes.json \
	--properties sector=Sector_hoy,id=Nomenclatu \
	cuadrantes=cuadrantes.shp

topojson --id-property=sector \
	-s 1e-10 \
        -o topojson/sectores-interactive.json \
        --properties sector\
        -- sectores=sectores.shp

topojson \
	--width 960 \
	--height 800 \
	--margin 0\
	--id-property=sector  \
	-s .9 \
	--projection 'd3.geo.mercator()' \
	-o topojson/sectores.json \
        --properties sector\
	-- sectores=sectores.shp


ogr2ogr -f "GeoJSON" geojson/cuadrantes.geojson cuadrantes.shp -sql "SELECT Nomenclatu AS cuadrante, Deleg AS delegacion, Zona AS zona, Sector AS sector1, Sector2 AS sector2, Sector_hoy as sector, from cuadrantes"
ogr2ogr -f "GeoJSON" geojson/sectores.geojson sectores.shp

shp2pgsql -s 4326 -W "latin1" -I -D cuadrantes.shp cuadrantes_poly > sql/cuadrantes_poly.sql
#scp cuadrantes_poly.sql 543fe7165973cae5d30000c1@apihoyodecrimen-valle.rhcloud.com:app-root/data/
