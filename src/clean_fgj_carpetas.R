

extract_links <- function(page) {
  # Parse the text as HTML
  html <- rvest::read_html(page)
  
  # Extract all <a> tags
  links <- rvest::html_elements(html, "a")
  
  # Filter based on link text and case-insensitive href match
  filtered <- links[
    str_detect(rvest::html_text(links), "Carpetas iniciadas|Víctimas") &
      str_detect(rvest::html_attr(links, "href"), "(?i)1a|1b")
  ]
  
  # Return hrefs
  rvest::html_attr(filtered, "href")
}

clean_fgj_carpetas <- function(file_url, dest, delitos) {
  
  temp_file <- file.path(tempdir(),
                        dest)
  
  
  download.file(paste0("https://www.fgjcdmx.gob.mx", file_url), 
                destfile = temp_file, mode = "wb")
  
  
  meses <- c(
    "Enero", "Febrero", "Marzo", "Abril",
    "Mayo", "Junio", "Julio", "Agosto",
    "Septiembre", "Octubre", "Noviembre", "Diciembre"
  )
  
  # Assign month numbers as names
  names(meses) <- 1:12
  si <- readxl::read_excel(temp_file, skip = 1, 
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
      #Mes = mes_hecho
    ) |>
    type_convert(col_types = cols(hora_hechos = col_time(format = ""))) |>
    mutate(fecha_hechos = as.Date(fecha_hechos, format = "%d/%m/%Y")) |>
    mutate(Delito = toupper(Delito)) |>
    mutate(Mes = month(fecha_hechos)) |>
    #filter(fecha_hechos < "2025-02-01") |>
    mutate(Mes = meses[Mes])
  
  si <- left_join(si, delitos, by = c("Delito" = "Delito"))
  
  si$fecha_inicio2 <- as.Date(si$`FECHA DE INICIO`, format = "%d/%m/%Y")
  
  si$hora_hechos <- hms::as_hms(si$hora_hechos) 
  si
}

get_carpetas <- function(delitos, min_date) {
  page <- readLines("https://www.fgjcdmx.gob.mx/transparencia/incidencia-delictiva",
                    warn = FALSE)
  page <- paste0(page, collapse = "")
  
  links <- str_extract_all(page, '(?<=href=")/storage.*?\\.xlsx')
  links[[1]][str_detect(links[[1]], '1A')]
  
  links <- extract_links(page)
  years <- as.integer(str_extract(links, "([0-9]{4})\\/[^2]", group = 1))
  names(links) <- years
  
  
  carpetas <- data.frame()
  for (link in links) {
    year_file <- str_extract(link, paste0("([0-9]{4})\\/[^2]", ".xlsx"), group = 1)
    if (str_detect(link, "1A"))
      carpetas <- rbind(carpetas,
                        clean_fgj_carpetas(link, 
                                           year_file, 
                                           delitos))
    
  }
  carpetas <- filter(carpetas, fecha_inicio2 >= min_date)
  carpetas$fecha_inicio2 <- NULL
  carpetas$Latitud <- as.numeric(carpetas$Latitud)
  carpetas$Longitud <- as.numeric(carpetas$Longitud)
  carpetas 
}

