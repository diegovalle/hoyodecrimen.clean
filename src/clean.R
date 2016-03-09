mcrime <- local({
  getNames <- function(fileName, n){
    #crimes <- c()
    cuadrantes <- data.frame()
    for(i in 1:n) {
      print(i)
      
      df = read.csv(file.path("sspdf-data", fileName), header = FALSE,
                    stringsAsFactors = FALSE)
      #df$V1 <- gsub("\\s+", " ", str_trim(df$V1))
      #df[str_length(df$V1) >= 9,]$V1
      #df$crime <- NA
      
      
      
      #df <- filter(df, str_length(df$V1) < 9)
     
      names(df)[1]  <- "crime"
      #names(df)[2] <- "crime"
      names(df)[2:13] <- as.character(as.yearmon(seq(as.Date("2013-01-01"),
                                  as.Date("2013-12-01"), 
                                  "month")))
      #names(df)[15] <- "Total.2013"
      names(df)[14:25] <- as.character(as.yearmon(seq(as.Date("2014-01-01"),
                                                     as.Date("2014-12-01"), 
                                                     "month")))
      #names(df)[28] <- "Total.2014"
      names(df)[26:37] <- as.character(as.yearmon(seq(as.Date("2015-01-01"),
                                                      as.Date("2015-12-01"), 
                                                      "month")))
      names(df)[38:38] <- as.character(as.yearmon(seq(as.Date("2016-01-01"),
                                                      as.Date(lastGood), 
                                                      "month")))
      #names(df)[34] <- "Total.2015"
      #names(df)[35] <- "Total"
      
      df$Total.2013 <- NULL
      df$Total.2014 <- NULL
      df$Total.2015 <- NULL
      df$Total <- NULL
      
      #crime_names  <- na.omit(unique(df$crime))
      cuadrante <- as.character(df$crime[1])
      df$cuadrante <- NA
      for(i in 1:nrow(df)) {
        if(str_length(df$crime[i]) < 9 | df$crime[i] == "(en blanco)" | df$crime[i] == "(EN BLANCO)")
          cuadrante <- as.character(df$crime[i])
        df$cuadrante[i] <- cuadrante
      }
      
      df <- filter(df, str_length(df$crime) >= 9 & df$crime != "(en blanco)")
      
      df$cuadrante <- str_replace(df$cuadrante, "(en blanco)", "NO ESPECIFICADO")
      df$cuadrante <- gsub("\\s+", " ", str_trim(df$cuadrante))
      #df$crime <- df$cuadrante
      df$crime <- toupper(df$crime)
      df$cuadrante <- toupper(df$cuadrante)
      df <- subset(df, !crime %in% "TOTAL GENERAL") 
      unique(df$crime)
      unique(df$cuadrante)
      
      stopifnot(length(unique(df$cuadrante)) == 847 | length(unique(df$cuadrante)) == 848)
      
      df$crime <- str_replace_all(df$crime, "C/V", "C.V.")
      df$crime <- str_replace_all(df$crime, "S/V", "S.V.")
      df$crime <- str_replace_all(df$crime, "ROBO DE VEHICULO C.V.", 
                                  "ROBO DE VEHICULO AUTOMOTOR C.V.")
      df$crime <- str_replace_all(df$crime, "ROBO DE VEHICULO S.V.", 
                                  "ROBO DE VEHICULO AUTOMOTOR S.V.")
      df$crime <- str_replace_all(df$crime, "ROBO A REPARTIDOR C.V.EH", 
                                  "ROBO A REPARTIDOR C.V.")
      df$crime <- str_replace_all(df$crime, "TRANSEUNTE EN VIA PUBLICA C.V.", 
                                  "ROBO A TRANSEUNTE EN VIA PUBLICA C.V.")
      df$crime <- str_replace_all(df$crime, "ROBO A TRANSEUNTE SIN VIOLENCIA", 
                                  "ROBO A TRANSEUNTE EN VIA PUBLICA S.V.")
      df$crime <- str_replace_all(df$crime, "ROBO A CUENTAHABIENTE SALIENDO DE CAJERO Y/O SUC. BANCARIA C.V.",
                                  "ROBO A CUENTAHABIENTE O SUC. BANCARIA C.V.")
      #df <- df[!is.na(df$crime),]
      
      
     
      
      df$crime[str_detect(df$crime, "[-\\(\\)]+")] <- NA
      df$cuadrante[!str_detect(df$cuadrante, "[-\\(\\)]+")] <- NA
      
      
      
      df$cuadrante <- str_replace_all(df$cuadrante, "NE", "NO ESPECIFICADO")
      df <- df[!is.na(df$crime),]
      
      mcrime <- gather(as.data.frame(df), date, count, -cuadrante, -crime)
      mcrime$date <- as.yearmon(mcrime$date)
      
      expand <- expand.grid(crime = unique(mcrime$crime),
                            date = unique(mcrime$date),
                            cuadrante = unique(mcrime$cuadrante))
      
      mcrime <- merge(mcrime, expand, all = TRUE)
   
      
#       expand <- expand.grid(crime = unique(mcrime$crime),
#                             variable = unique(mcrime$variable),
#                             cuadrante = unique(mcrime$cuadrante))
#       
#       mcrime <- merge(mcrime, expand, all = TRUE)
      mcrime$year <- year(mcrime$date)
      
      mcrime$count <- as.numeric(mcrime$count)
      mcrime$count[is.na(mcrime$count)] <- 0
      names(mcrime) <- c("crime", "cuadrante", "date", "count", "year")
      mcrime$date <- as.Date(mcrime$date)
      
      cuadrantes <- rbind(cuadrantes, mcrime)
#       cuadrantes <- subset(cuadrentes, !crime %in% c("ROBO A PASAJERO A BORDO DE MICROBUS C.V.",
#                                                      "ROBO A TRANSPORTISTA C.V.",
#                                                      "ROBO A TRANSPORTISTA S.V.",
#                                                      "ROBO A PASAJERO A BORDO DE MICROBUS  S.V.",
#                                                      "ROBO A PASAJERO AL INTERIOR DEL METRO C.V.",
#                                                      "ROBO A PASAJERO AL INTERIOR DEL METRO S.V.") )
    }
    return(cuadrantes)
  }
  
  crime <- rbind(getNames("RESP SIeIP 065416.csv", 1))
  
  
#   crime$year <- year(as.yearmon(crime$date))
#   crime$date <- as.Date(as.yearmon(crime$date))
#   crime <- crime[,c("cuadrante", "crime", "date", "count", "year")]
#   
#   # Preserve NA's 
#   crime <- subset(crime, !cuadrante %in% "TOTAL")  
#   
#   crime$cuadrante <- as.character(crime$cuadrante)
#   #crime[which(crime$cuadrante == "No específica"),]$cuadrante <- "NO ESPECIFICADO"
#   
#   crime[which(crime$crime == "ROBO DE VEHICULO C.V."),]$crime <- "ROBO DE VEHICULO AUTOMOTOR C.V."
#   crime[which(crime$crime == "ROBO DE VEHICULO S.V."),]$crime <- "ROBO DE VEHICULO AUTOMOTOR S.V."
#   crime[which(crime$crime == "ROBO A REPARTIDOR C.V.EH"),]$crime <- "ROBO A REPARTIDOR C.V." 
#   
#   
#   crime$count <- str_replace_all(crime$count, ",", "")
#   crime$count <- as.numeric(crime$count)
#   mcrime <- crime
  filter(crime, date <= lastGood)
})
## Clean the sspdf crime data from the pdf turned into 3 excel files

# mcrime <- local({
#   readCSV <- function(fileName, skipRows, removeCols, lastDate ) {
#     
#     df <- read.csv(file.path("sspdf-data",  fileName))
#     
#     names(df) <- c("crime", 
#                    as.character(as.yearmon(seq(as.Date("2013-01-01"), 
#                                                as.Date(lastGood), 
#                                                "month"))))
#     return(df)
#   }
#   
#   
#   crime <- readCSV("FOL. 026215 (INF. CUADRANTES).csv", 0, 0, lastDate)
#   
#   
#   crime$cuadrante <- NA
#   crime <- filter(crime, !str_detect(crime$crime, "Total"))
#   crime <- filter(crime, crime != "Robo a transportista C/V y S/V",
#                   #crime != "Robo a pasajero al interior del Metro C/V y S/V",
#                   #crime != "Robo a pasajero a bordo de taxi C/V",
#                   crime != "Robo a cuentahabiente saliendo de cajero y/o suc. bancaria C/V")
#   crime$crime <- str_replace(crime$crime, "ROBO A TRANSEUNTE EN VIA PUBLICA C.V. Y S.V.",
#                              "ROBO A TRANSEUNTE C.V. Y S.V.")
#   crime_names  <- unique(crime[str_length(crime$crime) >= 9, ]$crime)
#   
#   
#   for(i in 1:nrow(crime)) {
#     if(!crime$crime[i] %in% crime_names)
#       cuadrante <- as.character(crime$crime[i])
#     crime$cuadrante[i] <- cuadrante
#   }
#   totals <- crime[nrow(crime),]
#   # 
#   # test_that("Total matches sum", {
#   #   expect_that(sum(as.numeric(crime$Total.General[1:(nrow(crime)-1)])), 
#   #               equals(crime$Total.General[nrow(crime)])
#   # })
#   
#   
# #   crime$Total13 <- NULL
# #   crime$Total14 <- NULL
# #   crime$Total.General <- NULL
#   crime <- subset(crime, crime %in% crime_names)
#   mcrime <- melt(crime, id = c("cuadrante", "crime"))
#   mcrime$variable <- as.yearmon(mcrime$variable)
#   
#   expand <- expand.grid(crime = unique(mcrime$crime),
#                         variable = unique(mcrime$variable),
#                         cuadrante = unique(mcrime$cuadrante))
#   
#   mcrime <- merge(mcrime, expand, all = TRUE)
#   mcrime$year <- year(mcrime$variable)
#   mcrime$value <- str_replace(mcrime$value, "", "0")
#   mcrime$value <- as.numeric(mcrime$value)
#   mcrime[is.na(mcrime)] <- 0
#   
#   mcrime$crime <- str_replace(mcrime$crime, "C/V", "C.V.")
#   mcrime$crime <- str_replace(mcrime$crime, "S/V", "S.V.")
#   mcrime$crime <- toupper(mcrime$crime)
#   
#   mcrime
# })
