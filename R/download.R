# Building the downloadable files

pd_download_kinds <- c("json", "png", "zip")

#' Why the current drawing can't be downloaded (or NULL if it can)
#'
#' @param state From `pd_canvas_state()`.
#' @param id_check From `pd_check_id()`.
pd_download_problem <- function(state, id_check) {
  if (!is.null(state$error)) {
    return(pd_tr("invalid_drawing", state$error))
  }
  if (state$n == 0) {
    return(pd_tr("no_strokes"))
  }
  if (!id_check$ok) {
    return(id_check$message)
  }
  NULL
}

#' Build one download
#'
#' @param kind "json", "png" or "zip" (JSON + PNG).
#' @param id Checked drawing ID.
#' @param background Entry from `pd_backgrounds()`.
#' @param state From `pd_canvas_state()`; must include `png` unless kind is "json".
#' @param version App version.
#' @return list(filename, bytes)
pd_make_download <- function(
  kind,
  id,
  background,
  state,
  version,
  time = Sys.time()
) {
  kind <- match.arg(kind, pd_download_kinds)
  stem <- pd_file_stem(id)

  json <- function() {
    payload <- pd_build_payload(id, background, state$strokes, version, time)
    charToRaw(enc2utf8(pd_payload_json(payload)))
  }
  png <- function() {
    pd_png_from_data_url(
      state$png,
      background$w,
      background$h,
      author = paste0("paindraw_shiny_v", version),
      draw_id = id
    )
  }

  bytes <- switch(kind, json = json(), png = png(), zip = {
    dir <- tempfile("pd_zip_")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    files <- paste0(stem, c(".json", ".png"))
    writeBin(json(), file.path(dir, files[1]))
    writeBin(png(), file.path(dir, files[2]))
    zipfile <- file.path(dir, "out.zip")
    zip::zip(zipfile, files = files, root = dir)
    readBin(zipfile, "raw", file.size(zipfile))
  })
  list(filename = paste0(stem, ".", kind), bytes = bytes)
}
