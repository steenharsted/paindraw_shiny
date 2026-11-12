download_state <- function() {
  png <- png::writePNG(array(0.5, c(200, 100, 4)))
  list(
    n = 1L,
    strokes = pd_validate_strokes(list(raw_stroke()), 100, 200),
    strokes_json = "[]",
    png = paste0("data:image/png;base64,", base64enc::base64encode(png)),
    error = NULL
  )
}

test_that("download problems are reported in order", {
  ok_id <- pd_check_id("1")
  expect_null(pd_download_problem(download_state(), ok_id))
  expect_match(
    pd_download_problem(list(n = 0L, error = NULL), ok_id),
    "Draw something"
  )
  expect_match(pd_download_problem(list(n = 1L, error = "boom"), ok_id), "boom")
  expect_match(pd_download_problem(download_state(), pd_check_id("")), "Enter")
})

test_that("json, png and zip downloads are built", {
  bg <- test_background()
  json <- pd_make_download("json", "a b", bg, download_state(), "0.1.0")
  expect_equal(json$filename, "a_b.json")
  expect_equal(jsonlite::fromJSON(rawToChar(json$bytes))$id, "a b")

  png <- pd_make_download("png", "a b", bg, download_state(), "0.1.0")
  expect_equal(png$filename, "a_b.png")
  expect_equal(
    attr(png::readPNG(png$bytes, info = TRUE), "info")$text[["Draw_id"]],
    "a b"
  )

  zip <- pd_make_download("zip", "a b", bg, download_state(), "0.1.0")
  expect_equal(zip$filename, "a_b.zip")
  f <- tempfile(fileext = ".zip")
  on.exit(unlink(f))
  writeBin(zip$bytes, f)
  expect_setequal(zip::zip_list(f)$filename, c("a_b.json", "a_b.png"))
})

test_that("png download fails without valid image data", {
  st <- download_state()
  st$png <- NULL
  expect_error(
    pd_make_download("png", "1", test_background(), st, "0.1.0"),
    "Invalid image"
  )
})
