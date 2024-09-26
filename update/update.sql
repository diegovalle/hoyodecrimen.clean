-- psql -d apihoyodecrimen -f update.sql
\set cuadrantes :dir '/cuadrantes-pgj.csv'
\set pgj :dir '/pgj.csv'
\set crimelatlongpgj :dir '/crime-lat-long-pgj.csv'

\set smoothgamhomicides :dir '/json/smooth-map-colonias-hom.json'
\set crime_trends :dir '/json/crime_trends.json'


BEGIN;
-- LOCK TABLE 'table' IN SHARE ROW EXCLUSIVE mode;
-- cuadrantes
--DROP TABLE IF EXISTS cuadrantes_new;
CREATE TABLE cuadrantes_new (
  cuadrante varchar (20),
  crime varchar (60),
  date varchar (10),
  count int,
  year int,
  sector varchar(60),
  population integer,
  PRIMARY KEY(cuadrante, sector, crime, date)
);
\set command1 '\\copy cuadrantes_new (cuadrante,crime,date,count,year,sector,population) from ' :'cuadrantes' ' with delimiter as '','' NULL AS ''NA'' CSV HEADER ENCODING ''UTF8''; '
:command1
ALTER TABLE cuadrantes_new drop constraint if exists cuadrantes_new_pkey;
ALTER TABLE cuadrantes_new drop constraint if exists cuadrantes_new_pkey1;
-- create indexes
CREATE INDEX ON cuadrantes_new (crime ASC NULLS LAST);
--CREATE INDEX ON cuadrantes_new (cuadrante);
CREATE INDEX ON cuadrantes_new (date DESC NULLS LAST);
--CREATE INDEX ON cuadrantes_new (cuadrante, crime, date);
--CREATE INDEX ON cuadrantes_new ((upper(crime)), date, sector);
--CREATE INDEX ON cuadrantes_new ((upper(crime)), date, cuadrante);
CREATE INDEX ON cuadrantes_new USING btree ((upper(cuadrante)), crime);
CREATE INDEX ON "cuadrantes_new" ((upper(crime)),(upper(sector)),"date");
--CREATE INDEX ON "cuadrantes_new" ("sector","crime","date");
CREATE INDEX ON "cuadrantes_new" ((upper(crime)),(upper(cuadrante)),"date");
--CREATE INDEX ON "cuadrantes_new" ((upper(crime)),(upper(cuadrante)),"crime","date"); --~101MB
CREATE INDEX ON "cuadrantes_new" ((upper(crime)),"date");

-- pgj
--DROP TABLE IF EXISTS pgj_new;
CREATE TABLE pgj_new (
  crime varchar (60),
  date varchar (10),
  count int,
  PRIMARY KEY(crime, date)
);
\set command2 '\\copy pgj_new (crime, date, count) from ' :'pgj' ' with delimiter as '','' NULL AS ''NA'' CSV HEADER;'
:command2
--CREATE INDEX ON pgj_new (crime);
-- crime_latlong
CREATE TABLE IF NOT EXISTS crime_latlong (
        cuadrante varchar (20),
	    crime varchar (60),
	    date  varchar (10),
	    hour  varchar (10),
	    year  varchar (10),
	    month  varchar (10),
        latitude double precision,
        longitude double precision,
        id integer,
        geom geometry,
        PRIMARY KEY(id)
        );
TRUNCATE TABLE crime_latlong;
ALTER TABLE crime_latlong ADD COLUMN IF NOT EXISTS sector VARCHAR(20);
ALTER TABLE crime_latlong ADD COLUMN IF NOT EXISTS hex_idx smallint;
\set command3 '\\copy crime_latlong (cuadrante,sector,hex_idx,crime,date,hour,year,month,latitude,longitude,id) from ' :'crimelatlongpgj' ' with delimiter as '','' NULL AS ''NA'' CSV HEADER;'
:command3
UPDATE crime_latlong
SET geom = ST_GeomFromText(
    'POINT(' || longitude || ' ' || latitude || ')',
    4326
  );
ALTER TABLE crime_latlong ADD COLUMN IF NOT EXISTS hour_int INTEGER; --
update crime_latlong set hour_int =  substring(hour, 1, 2)::int;
create index IF NOT EXISTS hour_int_idx on crime_latlong (hour_int);
-- The ALTER TABLE ... RENAME TO command takes an Access Exclusive lock on 'table'
ALTER TABLE cuadrantes
  RENAME TO cuadrantes_old;
ALTER TABLE cuadrantes_new
  RENAME TO cuadrantes;
DROP TABLE cuadrantes_old;
ALTER TABLE pgj
  RENAME TO pgj_old;
ALTER TABLE pgj_new
  RENAME TO pgj;
DROP TABLE pgj_old;

-- store json files
CREATE TABLE  IF NOT EXISTS  json_files (name VARCHAR(25) PRIMARY KEY, data BYTEA);

INSERT INTO json_files (name) VALUES ('smoothgamhomicides')
ON CONFLICT (name) DO NOTHING;
INSERT INTO json_files (name) VALUES ('crime_trends')
ON CONFLICT (name) DO NOTHING;

\set lo1 '\\lo_import ' :'smoothgamhomicides'
:lo1
update json_files set data = lo_get(:LASTOID) where name = 'smoothgamhomicides';

\set lo2 '\\lo_import ' :'crime_trends' ';'
:lo2
update json_files set data = lo_get(:LASTOID) where name = 'crime_trends';
CREATE UNIQUE INDEX IF NOT EXISTS  filenames ON json_files(name);

COMMIT;


-- To update the cuadrantes:
-- table must have id (the cuadrante code) , sector and geom columns
-- ogr2ogr -f "PostgreSQL" -t_srs "EPSG:4326" "PG:host= port= user= dbname= password=" cuadrantes_population_2023.gpkg  -nlt PROMOTE_TO_MULTI -nln cuadrantes_poly -sql "select geom, cuadrante as id, sector from cuadrantes_population_2023__count" -overwrite
-- CREATE SEQUENCE public.cuadrantes_poly_gid_seq
--   START WITH 1
--   INCREMENT BY 1
--   NO MINVALUE
--   NO MAXVALUE
--   CACHE 1;

