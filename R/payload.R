# The JSON payload: PainDraw drawing schema 1.0 (urn:paindraw:drawing:1.0)

pd_schema_version <- "1.0"

#' Build the payload for one drawing
#'
#' @param id Drawing ID (already checked with `pd_check_id()`).
#' @param background Entry from `pd_backgrounds()`.
#' @param strokes Validated strokes from `pd_validate_strokes()`.
#' @param version App version, e.g. "0.1.0" (must be major.minor.patch).
#' @param time Save time.
#' @return A named list with keys in schema order.
pd_build_payload <- function(
  id,
  background,
  strokes,
  version,
  time = Sys.time()
) {
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", version)) {
    stop("App version must be major.minor.patch, not '", version, "'.")
  }
  list(
    schema_version = pd_schema_version,
    module_version = version,
    app = pd_config$app_name,
    drawing_id = id,
    timestamp = pd_format_ts(time),
    width = as.integer(background$w),
    height = as.integer(background$h),
    units = "px",
    template_file = background$file,
    template_md5 = tolower(background$md5),
    strokes = strokes
  )
}

#' UTC timestamp, YYYY-MM-DDTHH:MM:SSZ
pd_format_ts <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Serialise a payload to JSON, ending in "\r\n" like PainDrawEM
pd_payload_json <- function(payload) {
  json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  paste0(json, "\r\n")
}
