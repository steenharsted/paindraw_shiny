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

test_that("the app's schema version is the schema file's", {
  expect_identical(pd_schema_version, schema$properties$schema_version$const)
  expect_identical(schema$`$id`, "urn:paindraw:drawing:1.0")
})

test_that("top-level keys and stroke keys are those of schema 1.0, in order", {
  x <- jsonlite::fromJSON(make_json(), simplifyVector = FALSE)
  expect_identical(names(x), schema_top_keys)
  expect_identical(names(x$strokes[[1]]), schema_stroke_keys)
})

test_that("values and types follow schema 1.0", {
  json <- make_json()
  expect_true(endsWith(json, "}\r\n"))
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_identical(x$schema_version, "1.0")
  expect_identical(x$module_version, "0.1.0")
  expect_identical(x$app, "paindraw_shiny")
  expect_identical(x$drawing_id, "P01")
  expect_identical(x$timestamp, "2026-11-12T10:11:12Z")
  expect_identical(x$width, 100L)
  expect_identical(x$height, 200L)
  expect_identical(x$units, "px")
  expect_identical(x$template_file, "bg_test.png")
  expect_match(x$template_md5, "^[0-9a-f]{32}$")
  expect_match(json, '"schema_version":"1.0",', fixed = TRUE)
  expect_match(json, '"line_width":3,', fixed = TRUE)
  expect_match(json, '"alpha":0.1,', fixed = TRUE)
  expect_match(json, '"points":[[10,20],[11,21]]', fixed = TRUE)
  expect_schema_1_0(json)
})

test_that("a single point serialises as [[x,y]] and pen spray fields as null", {
  json <- make_json(list(raw_stroke(tool = "pen", points = list(list(5L, 6L)))))
  expect_match(json, '"spray_density":null,"spray_radius":null', fixed = TRUE)
  expect_match(json, '"points":[[5,6]]', fixed = TRUE)
  expect_schema_1_0(json)
})

test_that("special characters in the ID are escaped, not broken", {
  json <- make_json(id = 'a"b\\c/æ')
  expect_equal(jsonlite::fromJSON(json)$drawing_id, 'a"b\\c/æ')
})

test_that("module_version must be major.minor.patch", {
  bg <- test_background()
  expect_error(pd_build_payload("1", bg, list(), "0.1.0.9000"), "major")
  expect_error(pd_build_payload("1", bg, list(), "0.1"), "major")
})

test_that("an empty drawing serialises strokes as []", {
  payload <- pd_build_payload("1", test_background(), list(), "0.1.0")
  json <- pd_payload_json(payload)
  expect_match(json, '"strokes":[]', fixed = TRUE)
  expect_schema_1_0(json)
})

test_that("mixed strokes parse into a data frame with integer point matrices", {
  strokes <- list(
    raw_stroke(),
    raw_stroke(
      stroke_id = 2L,
      tool = "pen",
      draw_input_type = "touch",
      alpha = 1,
      points = list(list(50L, 60L))
    ),
    raw_stroke(stroke_id = 3L, points = list(list(1L, 2L), list(3L, 4L)))
  )
  json <- make_json(strokes)
  expect_schema_1_0(json)

  # What a reader using jsonlite's default simplification gets
  s <- jsonlite::fromJSON(json)$strokes
  expect_s3_class(s, "data.frame")
  expect_identical(names(s), schema_stroke_keys)
  expect_identical(s$stroke_id, 1:3)
  expect_identical(s$spray_density, c(15L, NA, 15L))
  expect_type(s$points, "list")
  for (m in s$points) {
    expect_true(is.matrix(m) && is.integer(m) && ncol(m) == 2)
  }
  expect_identical(s$points[[2]], matrix(c(50L, 60L), ncol = 2))
})
