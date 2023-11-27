
#' API wrapper to access the Australian Statistical Geography Standard from the
#' Australian Bureau of Statistics.
#'
#' @param service A string specifying the name of the map service to be queried.
#' Should match a single element from the list of \code{\link[ASGSmaps:available_services]{available_services}}.
#' @param standard A string matching a single element from the list of available_standards.
#' Should match a single element from the list of \code{\link[ASGSmaps:available_standards]{available_standards}}.
#' @param where SQL query for a string contained in one of the \code{\link[ASGSmaps:standard_fields]{standard_fields}}.
#' Any legal SQL WHERE clause operating on the fields in the layer is allowed.
#' @param text A string to search for in the layer. Example: text = "New South Wales"
#' @param objectIds A comma-separated list of object IDs. Example: objectIds = "1, 2, 3"
#' @param time A time extent for the query. Time value is specified in number of milliseconds since January 1, 1970 (Unix epoch time).
#' @param geometry A geometry for the query.
#' Example: geometry = "{xmin: 144.3577 ymin: -34.57995 xmax: 146.4928 ymax: -32.67125}"
#' @param geometryType The type of geometry specified by the geometry parameter. Possible values: "esriGeometryPoint", "esriGeometryMultipoint", "esriGeometryPolyline", "esriGeometryPolygon", "esriGeometryEnvelope".
#' @param inSR The spatial reference of the input geometry.
#' @param spatialRel The spatial relationship to be applied on the input geometry. Possible values: "esriSpatialRelIntersects", "esriSpatialRelContains", "esriSpatialRelCrosses", "esriSpatialRelEnvelopeIntersects", "esriSpatialRelIndexIntersects", "esriSpatialRelOverlaps", "esriSpatialRelTouches", "esriSpatialRelWithin", "esriSpatialRelRelation".
#' @param distance The buffer distance for the input geometry. The distance value and units must be specified with the units parameter. Example: distance = 100
#' @param units The unit of measurement for the distance parameter. Possible values: "esriSRUnit_Meter", "esriSRUnit_StatuteMile", "esriSRUnit_NauticalMile", "esriSRUnit_Kilometer", "esriSRUnit_USNauticalMile".
#' @param relationParam The relationship parameter for the spatial relationship. Used only when the spatialRel parameter is set to "esriSpatialRelRelation".
#' @param outfields A comma-separated list of attribute fields to be included in the response. See \code{\link[ASGSmaps:standard_fields]{standard_fields}}.
#' @param returnGeometry Whether to include the geometry in the response.
#' @param returnTrueCurves Whether to return true curves in the output geometry.
#' @param maxAllowableOffset The maximum allowable offset for the output geometry.
#' @param geometryPrecision The number of decimal places in the output geometry.
#' @param outSR The spatial reference of the output geometry.
#' @param havingClause The having clause to be used in the query.
#' @param returnIdsOnly Whether to return only the object IDs. Default: FALSE. .
#' @param returnCountOnly Whether to return only the count of features that satisfy the query.
#' @param orderByFields A comma-separated list of fields to order by.
#' @param groupByFieldsForStatistics A comma-separated list of fields to group by in the statistics.
#' @param outStatistics The statistics to be calculated for the query.
#' @param returnZ Whether to include z-values in the output geometry.
#' @param returnM Whether to include m-values in the output geometry.
#' @param gdbVersion The geodatabase version to use in the query.
#' @param historicMoment The date and time to query the data as of.
#' @param returnDistinctValues Whether to return distinct values in the response.
#' @param resultOffset The number of features to skip in the response.
#' @param resultRecordCount The maximum number of features to return in the response.
#' @param returnExtentOnly Whether to return only the bounding box of the query geometry.
#' @param datumTransformation The datum transformation to use in the query.
#' @param parameterValues The values to be used for query parameters.
#' @param rangeValues The range values for the query.
#' @param quantizationParameters The quantization parameters to be used for the query.
#' @param featureEncoding The feature encoding to be used in the response. Possible values: "esriDefault", "esriJson", "geoJson".
#' @param f The format of the response. Possible values: "html", "json", "geojson", "kmz", "pbf".
#'
#' @details More information about Request Parameters can be found at
#'     \href{https://developers.arcgis.com/rest/services-reference/enterprise/query-map-service-layer-.htm}{ArcGIS REST APIs}
#'
#' @return A simple features dataframe.#'
#' @export
#'
#' @examples
#' # get map data for two local government areas "Bland Shire Council" and
#' # "Carrathool Shire Council" from the 2022 update of ASGS Edition 3 service and
#' # the "LGA" standard
#'
#' map <- get_map_data(
#'   service = "ASGS2022",
#'   standard = "LGA",
#'   where = "LGA_NAME_2022 IN ('Bland', 'Carrathool')"
#' )
#'
#' plot(map$geometry)
#'
#'
#' # get all map data from the 2021 update of ASGS Edition 3 service and
#' # the "STE" standard.
#'
#' map <- get_map_data(
#'   service = "ASGS2021",
#'   standard = "STE",
#'   where = "1=1"
#' )
#'
#' plot(map$geometry)
#'

get_map_data <- function(service,
                         standard,
                         where = NULL,
                         text = NULL,
                         objectIds = NULL,
                         time = NULL,
                         geometry = NULL,
                         geometryType = "esriGeometryEnvelope", # default value
                         inSR = NULL,
                         spatialRel = "esriSpatialRelIntersects", # default value
                         distance = NULL,
                         units = "esriSRUnit_Foot", # default value
                         relationParam = NULL,
                         outfields = "*", # get all fields
                         returnGeometry = "true", # default value
                         returnTrueCurves = "false", # default value
                         maxAllowableOffset = NULL,
                         geometryPrecision = NULL,
                         outSR = NULL,
                         havingClause = NULL,
                         returnIdsOnly = "false", # default value
                         returnCountOnly = "false", # default value
                         orderByFields = NULL,
                         groupByFieldsForStatistics = NULL,
                         outStatistics = NULL,
                         returnZ = "false", # default value
                         returnM = "false", # default value
                         gdbVersion = NULL,
                         historicMoment = NULL,
                         returnDistinctValues = "false", # default value
                         resultOffset = NULL,
                         resultRecordCount = NULL,
                         returnExtentOnly = "false", # default value
                         datumTransformation = NULL,
                         parameterValues = NULL,
                         rangeValues = NULL,
                         quantizationParameters = NULL,
                         featureEncoding = "esriDefaultNULL", # default value
                         f = "geojson"
) {


  if (is.null(service) || !is.character(service) || length(service) != 1) {
    stop("! `service` must be a character vector of length 1.\n  X Use available_services() to get a list of ASGS services currently available.")
  }

  if (is.null(standard) || !is.character(standard) || length(standard) != 1) {
    stop("! `standard` must be a character vector of length 1.\n  X Use available_standards() to get a list of standards within a specified ASGS service.")
  }

  if (!is.null(where) && (!is.character(where) || length(where) != 1)) {
    stop("! `where` must be a legal SQL clause.\n  X Use standard_fields() to get a list of fields available within a specified standard.")
  }

  url <- httr::parse_url(paste0("https://geo.abs.gov.au/arcgis/rest/services", "/", service))
  url$path <- paste(url$path, paste(toupper(standard), "MapServer/0/query", sep = "/"), sep = "/")

  # build the query
  url$query <- list(
    where = where,
    text = text,
    objectIds = objectIds,
    time = time,
    geometry = geometry,
    geometryType = geometryType,
    inSR = inSR,
    spatialRel = spatialRel,
    distance = distance,
    units = units,
    relationParam = relationParam,
    outfields = outfields,
    returnGeometry = returnGeometry,
    returnTrueCurves = returnTrueCurves,
    maxAllowableOffset = maxAllowableOffset,
    geometryPrecision = geometryPrecision,
    outSR = outSR,
    havingClause = havingClause,
    returnIdsOnly = returnIdsOnly,
    returnCountOnly = returnCountOnly,
    orderByFields = orderByFields,
    groupByFieldsForStatistics = groupByFieldsForStatistics,
    outStatistics = outStatistics,
    returnZ = returnZ,
    returnM = returnM,
    gdbVersion = gdbVersion,
    historicMoment = historicMoment,
    returnDistinctValues = returnDistinctValues,
    resultOffset = resultOffset,
    resultRecordCount = resultRecordCount,
    returnExtentOnly = returnExtentOnly,
    datumTransformation = datumTransformation,
    parameterValues = parameterValues,
    rangeValues = rangeValues,
    quantizationParameters = quantizationParameters,
    featureEncoding = featureEncoding,
    f = f
  )


  # check if the URL is valid
  if (httr::status_code(httr::GET(url)) == 404) {
    stop("! Invalid URL.")
  }


  # make the API request
  response <- httr::GET(url, httr::verbose())

  # check if the response is successful
  if (httr::http_error(response)) {
    status_code <- httr::status_code(response)
    stop(paste0("! API request failed with status code ", status_code, "."))
  }

  map <- sf::st_read(response,
    quiet = TRUE
  )

  return(map)
}


