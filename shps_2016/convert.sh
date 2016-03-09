topojson --id-property=Nomenclatu \
	-s 1e-10 \
        -o topojson/cuadrantes-interactive.json \
        --properties cuadrante=Nomenclatu,sector=Sector_hoy\
        -- cuadrantes=cuadrantes.shp

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


ogr2ogr -f "GeoJSON" geojson/cuadrantes.json cuadrantes.shp -sql "SELECT Nomenclatu AS cuadrante, Deleg AS municipio, Zona AS zona, Sector_hoy as sector, cvegeo AS cve_mun from cuadrantes left join '../clean-data/municipios.csv'.municipios on cuadrantes.Deleg = municipios.municipio"
ogr2ogr -f "GeoJSON" geojson/sectores.json sectores.shp -sql "SELECT sector AS sector, cvegeo AS cve_mun, municipio from sectores left join '../clean-data/municipios.csv'.municipios on sectores.sector = municipios.sector" 



shp2pgsql -s 4326 -W "latin1" -I -D cuadrantes_sql.shp cuadrantes_poly > sql/cuadrantes_poly.sql
#scp cuadrantes_poly.sql 543fe7165973cae5d30000c1@apihoyodecrimen-valle.rhcloud.com:app-root/data/
