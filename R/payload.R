# The PainDrawEM JSON payload (see redcap_save_record() in PainDraw.php)

#' Build the payload for one drawing
#'
#' @param id Drawing ID (already checked with `pd_check_id()`).
#' @param background Entry from `pd_backgrounds()`.
#' @param strokes Validated strokes from `pd_validate_strokes()`.
#' @param version App version, e.g. "0.1.0".
#' @param time Save time.
#' @return A named list with keys in PainDrawEM order.
pd_build_payload <- function(
  id,
  background,
  strokes,
  version,
  time = Sys.time()
) {
  list(
    v = paste0("paindraw_shiny_v", version),
    id = id,
    w = as.integer(background$w),
    h = as.integer(background$h),
    units = "px",
    tpl = background$file,
    tpl_md5 = background$md5,
    view = NULL,
    app = pd_config$app_name,
    ts = pd_format_ts(time),
    crc = NULL,
    s = strokes
  )
}

#' UTC timestamp as PHP gmdate("Y-m-d\TH:i:s\Z")
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
