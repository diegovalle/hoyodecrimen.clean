print("Cleaning SESNSP data from elcri.men")

url_estados <- Sys.getenv("FUERO_ESTADOS_URL")
url_estados="https://data.diegovalle.net/elcrimen/nm-fuero-comun-estados.csv.gz"

tmp_pgj <-  tempfile("cuads", fileext = ".csv.gz")
download.file(url_estados, tmp_pgj, quiet = TRUE)
pgj <- read.csv(tmp_pgj, stringsAsFactors = FALSE)


tmp_pgj_victimas <-  tempfile("victimas", fileext = ".csv.gz")
#url_victimas= <- Sys.getenv("VICIMAS_URL")
url_victimas="https://data.diegovalle.net/elcrimen/nm-estatal-victimas.csv.gz"
download.file(url_victimas, tmp_pgj_victimas, quiet = TRUE)
pgj_victimas <- read.csv(tmp_pgj_victimas, stringsAsFactors = FALSE)

pgj_victimas <- pgj_victimas %>%
  filter(state_code == 9 & date >= "2019-01")
pgj_victimas$crime <- NA
pgj_victimas[pgj_victimas$subtipo == "HOMICIDIO DOLOSO", ]$crime <-  "HOMICIDIO DOLOSO"
pgj_victimas[pgj_victimas$subtipo == "FEMINICIDIO", ]$crime <- "HOMICIDIO DOLOSO"
pgj_victimas <- filter(pgj_victimas, !is.na(crime))
pgj_victimas <- pgj_victimas %>%
  group_by(crime, date) %>%
  summarise(count = sum(count), population = sum(population, na.rm = TRUE)/6) %>%
  mutate(date = str_c(date, "-01"))
pgj_victimas$population <- NULL


pgj <- pgj %>%
  filter(state_code == 9 & date >= "2019-01")

pgj$crime <- NA
pgj[pgj$subtipo == "LESIONES DOLOSAS" &
      pgj$modalidad == "CON ARMA DE FUEGO", ]$crime  <-
  "LESIONES POR ARMA DE FUEGO"
pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR" &
      pgj$modalidad == "ROBO DE COCHE DE 4 RUEDAS CON VIOLENCIA", ]$crime  <-
  "ROBO DE VEHICULO AUTOMOTOR C.V."
# pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR - MOTOCICLETA" &
#       pgj$modalidad == "ROBO DE MOTOCICLETA CON VIOLENCIA", ]$crime  <-
#   "ROBO DE VEHICULO AUTOMOTOR C.V."
pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR" &
      pgj$modalidad == "ROBO DE COCHE DE 4 RUEDAS SIN VIOLENCIA", ]$crime  <-
  "ROBO DE VEHICULO AUTOMOTOR S.V."
# pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR - MOTOCICLETA" &
#       pgj$modalidad == "ROBO DE MOTOCICLETA SIN VIOLENCIA", ]$crime  <-
#   "ROBO DE VEHICULO AUTOMOTOR S.V."

# url <- "https://elcri.men/elcrimen-json/states2.json"
# ll <- jsonlite::fromJSON(url)
# df <- as.data.frame(ll["rvsv"])
# df <- subset(df, rvsv.s == 9)


pgj <- pgj %>%
  group_by(crime, date) %>%
  summarise(count = sum(count), population = population[1]) %>%
  mutate(date = str_c(date, "-01")) %>%
  na.omit()
pgj$population <- NULL
pgj$count[pgj$crime == "ROBO DE VEHICULO AUTOMOTOR S.V." &
            pgj$date >= "2026-01-01" ] <- NA
pgj$count[pgj$crime == "ROBO DE VEHICULO AUTOMOTOR C.V." &
            pgj$date >= "2026-01-01" ] <- NA
write.csv(rbind(pgj, pgj_victimas), "clean-data/pgj.csv", row.names = FALSE)
