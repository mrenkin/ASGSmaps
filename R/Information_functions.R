
#' Show Geographical Services Available
#'
#' @return A character vector of geographic services available
#' @export
#'
#' @examples
#' available_services()
available_services <- function() {

  # build base url
  base_url <- "https://geo.abs.gov.au/arcgis/rest/services"
  # read html
  geo_html <- rvest::read_html(base_url)
  # find html element
  geo_elements <- rvest::html_elements(geo_html, "li")

  # check that geo_elements is not empty
  if (length(geo_elements) == 0) {
    stop("Elements not found")
  }

  # retrieve text
  services <- rvest::html_text(geo_elements)

  # check that the text is not empty
  if (length(services) == 0) {
    stop("Information not found")
  }

  return(setdiff(services, "CACHE"))
}


#' Show Geographical Standards Available
#'
#' @param service A character vector from the list of
#'    \code{\link[ASGSmaps]{available_services}}
#'
#' @return A list of geographic standards available
#' @export
#'
#' @examples
#' available_standards(service = "ASGS2021")
#'
available_standards <- function(service) {

  # check that the service supplied exists
  if (!service %in% available_services()) {
    stop("ASGS Service not found. Please check that the service supplied exists. See available_services()")
  }

  # build base url
  base_url <- paste0("https://geo.abs.gov.au/arcgis/rest/services", "/", service)
  # read html
  geo_html <- rvest::read_html(base_url)
  # find elements
  geo_elements <- rvest::html_elements(geo_html, "li")

    # check that geo_elements is not empty
  if (length(geo_elements) == 0) {
    stop("Elements not found")
  }

  # retrieve text
  geographies <- rvest::html_text(geo_elements)

  # check that the text is not empty
  if (length(geographies) == 0) {
    stop("Information not found")
  }

  # clean text
  asgs_services <- gsub(paste0(service, "/"), "", gsub(" \\(MapServer\\).*", "", geographies))

  return(asgs_services)
}



#' Show Descriptions of Geographical Standards
#'
#' @param service A string from the list of
#'    \code{\link[ASGSmaps:available_services]{available_services}}
#'
#' @param standard A string from the list of
#'    \code{\link[ASGSmaps:available_standards]{available_standards}}
#'
#' @return A string containing a description of the standard
#' @export
#'
#' @examples
#' standard_description(
#'   service = "ASGS2021",
#'   standard = "LGA"
#' )
standard_description <- function(service, standard) {
  # check that the service supplied exists
  if (!service %in% available_services()) {
    stop("ASGS Service not found. Please check that the service supplied exists. See available_services()")
  }
  # check that the standard supplied exists
  if (!standard %in% available_standards(service)) {
    stop("ASGS Standard not found. Please check that the standard supplied exists. See available_standards()")
  }

  # build base url
  base_url <- paste0("https://geo.abs.gov.au/arcgis/rest/services", "/", service)
  # read html
  info_html <- rvest::read_html(paste0(base_url, "/", toupper(standard), "/MapServer"))
  # find elements
  info_elements <- rvest::html_elements(info_html, "body")

  # check that geo_elements is not empty
  if (length(info_elements) == 0) {
    stop("Elements not found")
  }

  # retrieve text
  info <- rvest::html_text(info_elements)

  # check that the text is not empty
  if (length(info) == 0) {
    stop("Information not found")
  }

  # clean text
  asgs_info <- trimws(
    regmatches(
      x = info,
      m = gregexpr(
        pattern = "(?<=Service Description: )((.|\n)*)(?=Map Name:)",
        text = info,
        perl = TRUE
      )
    )
  )

  return(asgs_info)
}



#' Show Search fields for Geographical Standards
#'
#' @param service A character vector with one element from the list of
#'    \code{\link[ASGSmaps:available_services]{available_services}}
#'
#' @param standard A character vector with one element from the list of
#'    \code{\link[ASGSmaps:available_standards]{available_standards}}
#'
#' @return A list of fields available to search in the query
#' @export
#'
#' @examples
#' standard_fields(
#'   service = "ASGS2021",
#'   standard = "LGA"
#' )
standard_fields <- function(service, standard) {

  # check that the service supplied exists
  if (!service %in% available_services()) {
    stop("ASGS Service not found. Please check that the service supplied exists. See available_services()")
  }
  # check that the standard supplied exists
  if (!standard %in% available_standards(service)) {
    stop("ASGS Standard not found. Please check that the standard supplied exists. See available_standards()")
  }

  # build base url
  base_url <- paste0("https://geo.abs.gov.au/arcgis/rest/services", "/", service)

  # read html
  fields_html <- try(rvest::read_html(paste0(base_url, "/", toupper(standard), "/MapServer/0")), silent = TRUE)
  # find elements
  fields_elements <- rvest::html_elements(fields_html, "body")

  # check that geo_elements is not empty
  if (length(fields_elements) == 0) {
    stop("Elements not found")
  }

  # retrieve text
  fields <- rvest::html_text(fields_elements)

  # check that the text is not empty
  if (length(fields) == 0) {
    stop("Information not found")
  }


  # clean text
  asgs_fields <- trimws(
    regmatches(
      x = fields,
      m = gregexpr(
        pattern = "(?<=Fields: )((.|\n)*)(?=Supported Operations:)",
        text = fields,
        perl = TRUE
      )
    )
  )

  asgs_fields <- gsub(("\r|\n"), " ", asgs_fields, perl = TRUE)
  asgs_fields <- gsub("  ", " ", asgs_fields, perl = TRUE)
  asgs_fields <- gsub("\\( ", "", asgs_fields, perl = TRUE)
  asgs_fields <- unlist(lapply(strsplit(asgs_fields, split = ")"), trimws))
  asgs_fields <- paste("field: ", asgs_fields, sep = "")
  asgs_fields <- gsub(" type:", ", type:", asgs_fields)

  return(asgs_fields)
}



#' Show Values in Specified Standard Fields
#'
#' @param service A character vector with one element from the list of
#'    \code{\link[ASGSmaps:available_services]{available_services}}
#'
#' @param standard A character vector with one element from the list of
#'    \code{\link[ASGSmaps:available_standards]{available_standards}}
#'
#' @param field A character vector with one element from the list of
#'    \code{\link[ASGSmaps:available_standards]{standard_fields}}
#'
#' @param unique Boolean operator either TRUE or FALSE
#'
#' @return A data frame of the values in each field queried.
#' @export
#'
#' @examples
#' # get all of the values in the field 'STATE_NAME_2021' in the 'STE' standard
#' # in the 'ASGS2021' service
#'
#' values <- field_values(
#'   service = "ASGS2021",
#'   standard = "STE",
#'   field = "STATE_NAME_2021"
#' )
#'
#' # get all of the values in the fields 'STATE_NAME_2021' and 'STATE_CODE_2021'
#' # in the 'STE' standard in the 'ASGS2021' service
#'
#' values <- field_values(
#'   service = "ASGS2021",
#'   standard = "STE",
#'   field = "STATE_NAME_2021, STATE_CODE_2021"
#' )
#'
#' # get all of the values in all of the fields in the 'STE' standard in the
#' # 'ASGS2021' service
#'
#' values <- field_values(
#'   service = "ASGS2021",
#'   standard = "STE",
#'   field = "*"
#' )


field_values <- function(service = NULL, standard = NULL, field = NULL, unique = TRUE) {
  # check that service has been defined and is a character vector of length 1
  if (!is.character(service) || length(service) != 1) {
    stop("! `service` must be a character vector of length 1.\n  X Use available_services() to get a list of ASGS services currently available.")
  }
  # check that the service supplied exists
  if (!service %in% available_services()) {
    stop("ASGS Service not found. Please check that the service supplied exists. See available_services()")
  }

  # parse the service URL
  url <- httr::parse_url(paste0("https://geo.abs.gov.au/arcgis/rest/services", "/", service))

  # check that standard has been defined and is a character vector of length 1
  if (!is.character(standard) || length(standard) != 1) {
    stop("! `standard` must be a character vector of length 1.\n  X Use available_standards() to get a list of standards within a specified ASGS service.")
  }
  # check that the standard supplied exists
  if (!standard %in% available_standards(service)) {
    stop("ASGS Standard not found. Please check that the standard supplied exists. See available_standards()")
  }

  # build the standard URL
  url$path <- paste(url$path, paste(toupper(standard), "MapServer/0/query", sep = "/"), sep = "/")


  # check that field has been defined and is a character vector of length 1
  if (!is.character(field) || length(field) != 1) {
    stop("! `field` must be a character vector of length 1.\n  X Use standard_fields() to get a list of fields currently available from a specified standard within a specified ASGS service.")
  }

  # check that the field supplied exists
  if (field != "*") {
    if (!is.element(TRUE, stringr::str_detect(standard_fields(service, standard), paste(trimws(stringr::str_split(field, ',', simplify = TRUE)), collapse = "|")))) {
      stop("ASGS Standard Field not found. Please check that the field supplied exists. See standard_fields()")
    }
  }

  # build a query
  url$query <- list(
    where = "1=1",
    outfields = field,
    returnCountOnly = TRUE, # get the nuber of records only
    f = "pjson"
  )

  # check if the URL is valid
  if (httr::status_code(httr::GET(url)) == 404) {
    stop("! Invalid URL.")
  } # close if

  # make the API request
  count <- httr::GET(url)

  # check if the response is successful
  if (httr::http_error(count)) {
    status_code <- httr::status_code(count)
    stop(paste0("! API request failed with status code ", status_code, "."))
  } # close if

  # get result count
  count_value <- as.numeric(gsub("\\D", "",httr::content(count, as = 'text')))
  # calculate number of pages required
  pages <- floor(count_value/2000)
  # create list of offset values
  offsets <- seq(from = 0, to = pages*2000, by = 2000)

  # create a function to use in the map function
  get_values <- function(offset) {
  # build the query
      url$query <- list(
        where = "1=1",
        outfields = field,
        returnGeometry = "false",
        resultOffset = offset, # this will input the offset values created above
        f = "pjson"
      )

      # check if the URL is valid
      if (httr::status_code(httr::GET(url)) == 404) {
        stop("! Invalid URL.")
      } # close if

      # make the API request
      response <- httr::GET(url, httr::verbose())

      # convert to sf
      response <- sf::st_read(response,
                              quiet = TRUE)
  }

  # use offset values in the function get values
  results <- purrr::map(offsets, get_values)
  # combine records
  values_df <- dplyr::bind_rows(results)
    # result all results
    return(values_df)
} # close function




