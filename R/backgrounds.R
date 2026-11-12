# Built-in background images

#' Registry of built-in backgrounds
#'
#' One entry per `bg_*.png` in `dir`, named by file name without extension.
#' `w`/`h` are the canvas size (native size, clamped as PainDrawEM does) and
#' `md5` is computed on the exact bytes that are served to the browser.
pd_backgrounds <- function(
  dir = "www/backgrounds",
  url_prefix = "backgrounds"
) {
  files <- sort(list.files(dir, pattern = "^bg_.*\\.png$", full.names = TRUE))
  if (length(files) == 0) {
    stop("No backgrounds found in ", dir, call. = FALSE)
  }
  bgs <- lapply(files, function(f) {
    bytes <- readBin(f, "raw", file.size(f))
    native <- pd_png_dims(bytes)
    canvas <- pd_canvas_size(native[["width"]], native[["height"]])
    list(
      key = tools::file_path_sans_ext(basename(f)),
      file = basename(f),
      label = pd_background_label(basename(f)),
      url = paste0(url_prefix, "/", basename(f)),
      md5 = unname(tools::md5sum(f)),
      native_w = native[["width"]],
      native_h = native[["height"]],
      w = canvas[["w"]],
      h = canvas[["h"]]
    )
  })
  names(bgs) <- vapply(bgs, `[[`, "", "key")
  bgs
}

#' Canvas size from the native image size (PainDrawEM rule)
#'
#' Native size; if wider than `max_w`, scale down to `max_w` keeping the
#' aspect ratio. `floor(x + 0.5)` mirrors PHP's round() (half away from zero).
pd_canvas_size <- function(
  native_w,
  native_h,
  max_w = pd_config$max_canvas_width
) {
  w <- native_w
  h <- native_h
  if (w > max_w) {
    h <- floor(max_w * native_h / native_w + 0.5)
    w <- max_w
  }
  c(w = as.integer(w), h = as.integer(h))
}

#' Display name from a file name: "bg_lower_arm_palmar.png" -> "Lower arm (palmar)"
pd_background_label <- function(file) {
  qualifiers <- c(
    "palmar",
    "dorsal",
    "grey",
    "gray",
    "anterior",
    "posterior",
    "left",
    "right",
    "front",
    "back",
    "lateral",
    "medial"
  )
  vapply(
    file,
    function(f) {
      words <- strsplit(
        sub("^bg_", "", tools::file_path_sans_ext(basename(f))),
        "_"
      )[[1]]
      words <- words[nzchar(words)]
      qual <- NULL
      if (length(words) > 1 && words[length(words)] %in% qualifiers) {
        qual <- words[length(words)]
        words <- words[-length(words)]
      }
      label <- paste(words, collapse = " ")
      label <- paste0(toupper(substr(label, 1, 1)), substring(label, 2))
      if (!is.null(qual)) {
        label <- paste0(label, " (", qual, ")")
      }
      label
    },
    character(1),
    USE.NAMES = FALSE
  )
}
