# Source the app's R/ files (as Shiny does when it starts the app)
app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(
  file.path(app_dir, "R"),
  pattern = "\\.[Rr]$",
  full.names = TRUE
)) {
  source(f, local = environment())
}

# A minimal valid stroke as the browser sends it (after jsonlite parsing)
raw_stroke <- function(...) {
  stroke <- list(
    stroke_id = 1L,
    draw_input_type = "mouse",
    tool = "spray",
    color = "#fc3d03",
    line_width = 3L,
    spray_density = 15L,
    spray_radius = 5L,
    alpha = 0.1,
    scale_factor = 0.5,
    device_pixel_ratio = 2,
    points = list(list(10L, 20L), list(11L, 21L))
  )
  # Replace top-level keys only (modifyList() would merge `points` recursively)
  changes <- list(...)
  stroke[names(changes)] <- changes
  stroke
}

# PainDraw drawing schema 1.0 (urn:paindraw:drawing:1.0). The required keys,
# in the order the schema lists them, are read from the schema file itself.
schema <- jsonlite::fromJSON(
  file.path(app_dir, "schema", "paindraw_schema_1.0.json"),
  simplifyVector = FALSE
)
schema_top_keys <- unlist(schema$required)
schema_stroke_keys <- unlist(schema$`$defs`$stroke$required)

# Hand-written check of the schema 1.0 rules on a JSON string. Stand-in for a
# real JSON Schema validator; keep the rules in sync with the schema file.
expect_schema_1_0 <- function(json) {
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  is_str <- function(v) is.character(v) && length(v) == 1
  is_int <- function(v) is.integer(v) && length(v) == 1
  is_num <- function(v) is.numeric(v) && length(v) == 1

  testthat::expect_true(all(schema_top_keys %in% names(x)))
  testthat::expect_identical(x$schema_version, "1.0")
  testthat::expect_true(
    is_str(x$module_version) &&
      grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", x$module_version)
  )
  testthat::expect_true(is_str(x$app) && nzchar(x$app))
  testthat::expect_true(is_str(x$drawing_id) && nzchar(x$drawing_id))
  testthat::expect_true(
    is_str(x$timestamp) &&
      grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?(Z|[+-][0-9]{2}:?[0-9]{2})?$",
        x$timestamp
      )
  )
  testthat::expect_true(is_int(x$width) && x$width >= 1)
  testthat::expect_true(is_int(x$height) && x$height >= 1)
  testthat::expect_identical(x$units, "px")
  testthat::expect_true(is_str(x$template_file) && nzchar(x$template_file))
  testthat::expect_true(
    is_str(x$template_md5) && grepl("^[0-9a-f]{32}$", x$template_md5)
  )
  # An array: unnamed list (an empty object {} would also parse as list())
  testthat::expect_true(is.list(x$strokes) && is.null(names(x$strokes)))
  testthat::expect_match(json, '"strokes":\\[', all = FALSE)

  for (s in x$strokes) {
    testthat::expect_true(all(schema_stroke_keys %in% names(s)))
    testthat::expect_true(is_int(s$stroke_id) && s$stroke_id >= 1)
    testthat::expect_true(s$draw_input_type %in% c("mouse", "touch"))
    testthat::expect_true(s$tool %in% c("spray", "pen"))
    testthat::expect_true(
      is_str(s$color) && grepl("^#[0-9A-Fa-f]{6}$", s$color)
    )
    testthat::expect_true(is_int(s$line_width) && s$line_width >= 0)
    if (s$tool == "spray") {
      testthat::expect_true(is_int(s$spray_density) && s$spray_density >= 0)
      testthat::expect_true(is_int(s$spray_radius) && s$spray_radius >= 0)
    } else {
      # Present as JSON null (key kept, value NULL)
      testthat::expect_true("spray_density" %in% names(s))
      testthat::expect_null(s$spray_density)
      testthat::expect_null(s$spray_radius)
    }
    testthat::expect_true(is_num(s$alpha) && s$alpha >= 0 && s$alpha <= 1)
    testthat::expect_true(is_num(s$scale_factor) && s$scale_factor > 0)
    testthat::expect_true(
      is_num(s$device_pixel_ratio) && s$device_pixel_ratio > 0
    )
    testthat::expect_true(is.list(s$points) && length(s$points) >= 1)
    for (pt in s$points) {
      testthat::expect_true(
        length(pt) == 2 && is.integer(pt[[1]]) && is.integer(pt[[2]])
      )
    }
  }
  invisible(x)
}

test_background <- function() {
  list(
    file = "bg_test.png",
    md5 = "0123456789abcdef0123456789abcdef",
    w = 100L,
    h = 200L
  )
}
