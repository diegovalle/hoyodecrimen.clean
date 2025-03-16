df <- local( {
  # URL of the file to download
  file_url <-  "https://transparencia.cdmx.gob.mx/storage/app/uploads/public/67d/1d4/692/67d1d4692c2fa424655113.xlsx"
  # Create temporary file path
  temp_file <- file.path(tempdir(), basename(file_url))
  
  # Check if file already exists
  if (!file.exists(temp_file)) {
    # Download the file if it doesn't exist
    message("Downloading file...")
    download.file(file_url, destfile = temp_file, mode = "wb")
  } else {
    message("File already exists in temporary directory. Using cached version.")
  }
  
  meses <- c(
    "Enero", "Febrero", "Marzo", "Abril",
    "Mayo", "Junio", "Julio", "Agosto",
    "Septiembre", "Octubre", "Noviembre", "Diciembre"
  )
  
  # Assign month numbers as names
  names(meses) <- 1:12
  
  si <- readxl::read_excel(temp_file, skip = 4, sheet = "CARPETAS")  |>
    filter(!is.na(ID_AP)) |>
    rename(
      Longitud= `COORD. X`,
      Latitud = `COORD. Y`,
      Delito = `MODALIDAD - DELITO`,
      #Categoría.de.delito = categoria_delito,
      #Delito = DELITO,
      fecha_hechos = `FECHA DE LOS HECHOS`,
      #Año = anio_hecho,
      hora_hechos = `HORA DE LOS HECHOS`,
      #Mes = mes_hecho
    ) |>
    type_convert(col_types = cols(hora_hechos = col_time(format = ""))) |>
    mutate(fecha_hechos = as.Date(fecha_hechos, format = "%d/%m/%Y")) |>
    mutate(Delito = toupper(Delito)) |>
    mutate(Mes = month(fecha_hechos)) |>
    #filter(fecha_hechos < "2025-02-01") |>
    mutate(Mes = meses[Mes])
  
  delitos <- df |> select(Delito, Categoría.de.delito) |> unique()
  si <- left_join(si, delitos, by = c("Delito" = "Delito"))
  
  si <- filter(si ,fecha_hechos > max(df$fecha_hechos, na.rm = TRUE))
  si$hora_hechos <- hms::as_hms(si$hora_hechos)
  bind_rows(df, si)|>
    filter(fecha_hechos < "2025-02-01")
})

