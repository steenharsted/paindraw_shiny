reference_file <- function() {
  f <- system.file("extdata", "9_draw_zealand.json", package = "paindrawr")
  if (!nzchar(f)) {
    testthat::skip("paindrawr is not installed")
  }
  f
}

make_json <- function(strokes = list(raw_stroke()), id = "P01") {
  valid <- pd_validate_strokes(strokes, 100, 200)
  payload <- pd_build_payload(
    id,
    test_background(),
    valid,
    "0.1.0",
    time = as.POSIXct("2026-11-12 10:11:12", tz = "UTC")
  )
  pd_payload_json(payload)
}

test_that("top-level keys and stroke keys match PainDrawEM order", {
  ref <- jsonlite::fromJSON(reference_file(), simplifyVector = FALSE)
  ours <- jsonlite::fromJSON(make_json(), simplifyVector = FALSE)
  expect_identical(names(ours), names(ref))
  expect_identical(names(ours$s[[1]]), names(ref$s[[1]]))
})

test_that("values and types are as PainDrawEM writes them", {
  json <- make_json()
  expect_true(endsWith(json, "}\r\n"))
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(x$v, "paindraw_shiny_v0.1.0")
  expect_equal(x$app, "paindraw_shiny")
  expect_equal(x$ts, "2026-11-12T10:11:12Z")
  expect_null(x$view)
  expect_null(x$crc)
  expect_true(is.integer(x$w) && is.integer(x$h))
  expect_match(json, '"view":null', fixed = TRUE)
  expect_match(json, '"crc":null', fixed = TRUE)
  expect_match(json, '"lineWidth":3,', fixed = TRUE)
  expect_match(json, '"alpha":0.1,', fixed = TRUE)
  expect_match(json, '"p":[[10,20],[11,21]]', fixed = TRUE)
})

test_that("a single point serialises as [[x,y]] and pen spray fields as null", {
  json <- make_json(list(raw_stroke(tool = "pen", p = list(list(5L, 6L)))))
  expect_match(json, '"sprayDensity":null,"sprayRadius":null', fixed = TRUE)
  expect_match(json, '"p":[[5,6]]', fixed = TRUE)
})

test_that("special characters in the ID are escaped, not broken", {
  json <- make_json(id = 'a"b\\c/æ')
  expect_equal(jsonlite::fromJSON(json)$id, 'a"b\\c/æ')
})

test_that("output round-trips through paindrawr", {
  reference_file() # skip if paindrawr is missing
  strokes <- list(
    raw_stroke(),
    raw_stroke(
      strokeId = 2L,
      tool = "pen",
      drawInputType = "touch",
      alpha = 1,
      p = list(list(50L, 60L))
    ),
    raw_stroke(strokeId = 3L, p = list(list(1L, 2L), list(3L, 4L)))
  )
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  writeBin(charToRaw(make_json(strokes)), f)

  imported <- paindrawr::pdr_import_json(f)
  expect_equal(imported$id, "P01")
  d <- imported$pdr_data[[1]]
  expect_equal(d$.width, 100L)
  expect_equal(d$.height, 200L)
  expect_equal(d$.strokes$.tool, c("spray", "pen", "spray"))
  expect_equal(d$.strokes$.point_density, c(15L, NA, 15L))
  expect_equal(nrow(d$.points), 5)
  expect_equal(d$.points$.x, c(10, 11, 50, 1, 3))
  expect_equal(d$.points$.y, 200 - c(20, 21, 60, 2, 4)) # y is flipped

  p <- paindrawr::pdr_plot_xy(imported)
  expect_s3_class(p, "ggplot")
})
