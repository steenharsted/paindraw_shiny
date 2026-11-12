png_data_url <- function(w, h) {
  img <- array(0.5, c(h, w, 4))
  paste0("data:image/png;base64,", base64enc::base64encode(png::writePNG(img)))
}

test_that("PNG gets Author and Draw_id text and keeps its size", {
  out <- pd_png_from_data_url(
    png_data_url(4, 3),
    4,
    3,
    "paindraw_shiny_v0.1.0",
    "P01"
  )
  img <- png::readPNG(out, info = TRUE)
  expect_equal(dim(img)[1:2], c(3, 4))
  expect_equal(
    attr(img, "info")$text,
    c(Author = "paindraw_shiny_v0.1.0", Draw_id = "P01")
  )
})

test_that("wrong size or non-PNG data is rejected", {
  expect_error(
    pd_png_from_data_url(png_data_url(4, 3), 5, 3, "a", "b"),
    "wrong format"
  )
  expect_error(
    pd_png_from_data_url("data:image/png;base64,AAAA", 4, 3, "a", "b"),
    "wrong format"
  )
  expect_error(
    pd_png_from_data_url("data:image/svg+xml;base64,AAAA", 4, 3, "a", "b"),
    "Invalid"
  )
})
