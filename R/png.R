# PNG helpers

#' Width and height from PNG bytes (read from the IHDR chunk, no decoding)
pd_png_dims <- function(bytes) {
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (
    length(bytes) < 24 ||
      !identical(bytes[1:8], signature) ||
      !identical(rawToChar(bytes[13:16]), "IHDR")
  ) {
    stop("Not a PNG file.", call. = FALSE)
  }
  be32 <- function(b) sum(as.numeric(b) * 256^(3:0))
  c(width = be32(bytes[17:20]), height = be32(bytes[21:24]))
}

#' Final PNG bytes from the canvas data URL
#'
#' Checks that the image is a PNG of the expected size, then adds the text
#' properties PainDrawEM adds with Imagick (`Author`, `Draw_id`).
pd_png_from_data_url <- function(data_url, w, h, author, draw_id) {
  prefix <- "data:image/png;base64,"
  if (!pd_is_string(data_url) || !startsWith(data_url, prefix)) {
    stop("Invalid image data.", call. = FALSE)
  }
  b64 <- gsub("\\s+", "", substring(data_url, nchar(prefix) + 1))
  bytes <- tryCatch(base64enc::base64decode(b64), error = function(e) raw())
  dims <- tryCatch(pd_png_dims(bytes), error = function(e) {
    c(width = NA, height = NA)
  })
  if (!identical(unname(dims), as.numeric(c(w, h)))) {
    stop("Image data has the wrong format or size.", call. = FALSE)
  }
  img <- png::readPNG(bytes)
  png::writePNG(img, text = c(Author = author, Draw_id = draw_id))
}
