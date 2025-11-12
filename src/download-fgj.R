

url_exists <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  result <- list(exists = status == HTTP_STATUS_OK, status = status)
  return(result)
}
