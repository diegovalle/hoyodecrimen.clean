

clean_fgj_victimas <- function(file_url1, file_url2, dest_carpetas, dest_victimas, delitos) {

  temp_file1 <- file.path(tempdir(),
                          dest_carpetas)
  
  
  download.file(paste0("https://www.fgjcdmx.gob.mx", file_url1), 
                destfile = temp_file1, mode = "wb")
  
  
  temp_file2 <- file.path(tempdir(),
                          dest_victimas)
  
  
  download.file(paste0("https://www.fgjcdmx.gob.mx", file_url2), 
                destfile = temp_file2, mode = "wb")
  
  
  meses <- c(
    "Enero", "Febrero", "Marzo", "Abril",
    "Mayo", "Junio", "Julio", "Agosto",
    "Septiembre", "Octubre", "Noviembre", "Diciembre"
  )
  
  # Assign month numbers as names
  names(meses) <- 1:12
  carpetas <- readxl::read_excel(temp_file1, skip = 1, 
                           sheet = 1)  |>
    filter(!is.na(ID)) |>
    rename(
      Longitud= `COORD X`,
      Latitud = `COORD Y`,
      Delito = `MODALIDAD - DELITO`,
      #Categoría.de.delito = categoria_delito,
      #Delito = DELITO,
      fecha_hechos = `FECHA DE LOS HECHOS`,
      #Año = anio_hecho,
      hora_hechos = `HORA DE LOS HECHOS`,
      ID_AP="ID_CI"
      #Mes = mes_hecho
    ) |>
    type_convert(col_types = cols(hora_hechos = col_time(format = ""))) |>
    mutate(fecha_hechos = as.Date(fecha_hechos, format = "%d/%m/%Y")) |>
    mutate(Delito = toupper(Delito)) |>
    mutate(Mes = month(fecha_hechos)) |>
    #filter(fecha_hechos < "2025-02-01") |>
    mutate(Mes = meses[Mes])
  
  victimas <- readxl::read_excel(temp_file2, skip = 1, 
                                 sheet = 1)  |>
    #filter(!is.na(ID)) |>
    rename_at(vars(matches("^ID_CI$")), function(x) "ID_AP") |>
    select(ID_AP)
  

  stopifnot(setdiff(carpetas$ID_CI, victimas$ID_AP) == numeric(0))
  si <- left_join(victimas, carpetas, by = c("ID_AP" ="ID_AP"))
  
  delitos <- df |> select(Delito, Categoría.de.delito) |> unique()
  si <- left_join(si, delitos, by = c("Delito" = "Delito"))
  
  si$fecha_inicio2 <- as.Date(si$`FECHA DE INICIO`, format = "%d/%m/%Y")
  
  si$hora_hechos <- hms::as_hms(si$hora_hechos) 
  si
}

get_victimas <- function(delitos, min_date) {
  page <- readLines("https://www.fgjcdmx.gob.mx/transparencia/incidencia-delictiva",
                    warn = FALSE)
  page <- paste0(page, collapse = "")
  
  links <- str_extract_all(page, '(?<=href=")/storage.*?\\.xlsx')
  links[[1]][str_detect(links[[1]], '1A')]
  
  links <- extract_links(page)
  years <- as.integer(str_extract(links, "([0-9]{4})\\/[^2]", group = 1))
  names(links) <- years
  links["2021"]
  
  victimas <- data.frame()
  i <- 1
  while(i < length(links)) {
    year_file <- str_extract(links[i], "([0-9]{4})\\/[^2]", group = 1)
    year_file_carpetas <- paste0(year_file, "carpetas", ".xlsx")
    year_file_victimas <- paste0(year_file, "victimas", ".xlsx")
    victimas <- rbind(victimas,
                        clean_fgj_victimas(links[i],
                                           links[i + 1],
                                           year_file_carpetas,
                                           year_file_victimas,
                                           delitos))
    i <- i + 2
    
  }
  victimas <- filter(victimas, fecha_inicio2 >= min_date)
  victimas$fecha_inicio2 <- NULL
  victimas$Latitud <- as.numeric(victimas$Latitud)
  victimas$Longitud <- as.numeric(victimas$Longitud)
  victimas 
}



download_victimas_files  <- function(list_url, base_url, min_date) {
  # Read the text file containing the list of Excel filenames
  
  response <- httr::GET(list_url)
  
  if (httr::status_code(response) != 200) {
    stop("Failed to download file list. HTTP status: ", status_code(response))
  }
  
  # Parse the content and split by newlines
  file_list <- httr::content(response, "text", encoding = "UTF-8")
  file_names <- trimws(unlist(strsplit(file_list, "\n")))
  
  # Remove empty lines
  file_names <- file_names[file_names != ""]
  file_names_victimas <- file_names[str_detect(file_names, "victimas")]
  file_names_carpetas <- file_names[str_detect(file_names, "carpetas")]
  
  cat("Found", length(file_names), "files to process:\n")
  print(file_names)
  
  # Process each Excel file
  results <- list()
  
  for (i in 1:length(file_names_victimas)) {
    # Construct full URL for the Excel file
    temp_file_carpetas <- tempfile(fileext = ".xlsx")
    full_url1 <- paste0(base_url, "/", file_names_carpetas[i])
    cat("\nDownloading:", full_url1, "\n")
    download.file(full_url1, 
                  destfile = temp_file_carpetas, mode = "wb")
    
    
    temp_file_victimas <- tempfile(fileext = ".xlsx")
    full_url2 <- paste0(base_url, "/", file_names_victimas[i])
    cat("\nDownloading:", full_url2, "\n")
    
    download.file(full_url2, 
                  destfile = temp_file_victimas, mode = "wb")
    
    # Process the file
    result <- process_excel_victimas_file(temp_file_carpetas, temp_file_victimas, min_date)
    
    # Store result with filename as key
    results[[file_names_carpetas[i]]] <- result
    
    # Clean up temporary file
    unlink(temp_file)
    
  }
  
  cat("\n=== Processing Complete ===\n")
  cat("Successfully processed", length(results), "out of", length(file_names), "files\n")
  
  # Combine all data frames using rbind
  if (length(results) > 0) {
    combined_data <- do.call(rbind, results)
    cat("Combined data dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")
    return(combined_data)
  } else {
    cat("No data to combine\n")
    return(NULL)
  }
  
  
}

process_excel_victimas_file <- function(temp_file_carpetas, 
                                        temp_file_victimas,
                                        min_date) {
 
  meses <- c(
    "Enero", "Febrero", "Marzo", "Abril",
    "Mayo", "Junio", "Julio", "Agosto",
    "Septiembre", "Octubre", "Noviembre", "Diciembre"
  )
  
  # Assign month numbers as names
  names(meses) <- 1:12
  print(temp_file_carpetas)
  carpetas <- readxl::read_excel(temp_file_carpetas, skip = 1, 
                                 sheet = 1)  |>
    filter(!is.na(ID)) |>
    rename(
      Longitud= `COORD X`,
      Latitud = `COORD Y`,
      Delito = `MODALIDAD - DELITO`,
      #Categoría.de.delito = categoria_delito,
      #Delito = DELITO,
      fecha_hechos = `FECHA DE LOS HECHOS`,
      #Año = anio_hecho,
      hora_hechos = `HORA DE LOS HECHOS`,
      ID_AP="ID_CI"
      #Mes = mes_hecho
    ) |>
    type_convert(col_types = cols(hora_hechos = col_time(format = ""))) |>
    mutate(fecha_hechos = as.Date(fecha_hechos, format = "%d/%m/%Y")) |>
    mutate(Delito = toupper(Delito)) |>
    mutate(Mes = month(fecha_hechos)) |>
    #filter(fecha_hechos < "2025-02-01") |>
    mutate(Mes = meses[Mes])
  
  victimas <- readxl::read_excel(temp_file_victimas, skip = 1, 
                                 sheet = 1)  |>
    #filter(!is.na(ID)) |>
    rename_at(vars(matches("^ID_CI$")), function(x) "ID_AP") |>
    select(ID_AP)
  
  
  stopifnot(setdiff(carpetas$ID_CI, victimas$ID_AP) == numeric(0))
  si <- left_join(victimas, carpetas, by = c("ID_AP" ="ID_AP"))
  
  delitos <- df |> select(Delito, Categoría.de.delito) |> unique()
  si <- left_join(si, delitos, by = c("Delito" = "Delito"))
  
  si$fecha_inicio2 <- as.Date(si$`FECHA DE INICIO`, format = "%d/%m/%Y")
  
  si$hora_hechos <- hms::as_hms(si$hora_hechos) 
  si <- filter(si, fecha_inicio2 >= min_date)
  si$fecha_inicio2 <- NULL
  si$Latitud <- as.numeric(si$Latitud)
  si$Longitud <- as.numeric(si$Longitud)
  return(si)
}
