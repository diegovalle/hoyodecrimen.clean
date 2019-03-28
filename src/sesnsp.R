

tmp_pgj <-  tempfile("cuads", fileext = ".csv.gz")
download.file ("https://elcri.men/data/nm-fuero-comun-estados.csv.gz",
               tmp_pgj)
pgj <- read.csv(tmp_pgj, stringsAsFactors = FALSE)
pgj <- pgj %>%
  filter(state_code == 9 & date >= "2016-01")

pgj$crime <- NA
pgj[pgj$subtipo == "HOMICIDIO DOLOSO", ]$crime <-  "HOMICIDIO DOLOSO"
pgj[pgj$subtipo == "FEMINICIDIO", ]$crime <- "HOMICIDIO DOLOSO"
pgj[pgj$subtipo == "LESIONES DOLOSAS" &
      pgj$modalidad == "CON ARMA DE FUEGO", ]$crime  <-
  "LESIONES POR ARMA DE FUEGO"
pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR" &
      pgj$modalidad == "ROBO DE COCHE DE 4 RUEDAS CON VIOLENCIA", ]$crime  <-
  "ROBO DE VEHICULO AUTOMOTOR C.V."
pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR" &
      pgj$modalidad == "ROBO DE MOTOCICLETA CON VIOLENCIA", ]$crime  <-
  "ROBO DE VEHICULO AUTOMOTOR C.V."
pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR" &
      pgj$modalidad == "ROBO DE COCHE DE 4 RUEDAS SIN VIOLENCIA", ]$crime  <-
  "ROBO DE VEHICULO AUTOMOTOR S.V."
pgj[pgj$subtipo == "ROBO DE VEHÍCULO AUTOMOTOR" &
      pgj$modalidad == "ROBO DE MOTOCICLETA SIN VIOLENCIA", ]$crime  <-
  "ROBO DE VEHICULO AUTOMOTOR S.V."


pgj <- pgj %>%
  group_by(crime, date) %>%
  summarise(count = sum(count), population = population[1]) %>%
  mutate(date = str_c(date, "-01")) %>%
  na.omit()
pgj$population <- NULL
write.csv(pgj, "clean-data/pgj.csv", row.names = FALSE)
