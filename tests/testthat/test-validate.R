expect_invalid <- function(stroke, pattern = NULL) {
  expect_error(
    pd_validate_strokes(list(stroke), 100, 200),
    pattern,
    class = "pd_validation_error"
  )
}

test_that("a valid spray stroke passes with proper types", {
  out <- pd_validate_strokes(list(raw_stroke()), 100, 200)
  s <- out[[1]]
  expect_named(
    s,
    c(
      "strokeId",
      "drawInputType",
      "tool",
      "color",
      "lineWidth",
      "sprayDensity",
      "sprayRadius",
      "alpha",
      "scaleFactor",
      "dpr",
      "p"
    )
  )
  expect_identical(s$strokeId, 1L)
  expect_identical(s$p, list(c(10L, 20L), c(11L, 21L)))
})

test_that("pen strokes get NULL spray settings", {
  out <- pd_validate_strokes(
    list(raw_stroke(tool = "pen", sprayDensity = 99L)),
    100,
    200
  )
  expect_true("sprayDensity" %in% names(out[[1]]))
  expect_null(out[[1]]$sprayDensity)
  expect_null(out[[1]]$sprayRadius)
})

test_that("each validateJsonInput() check rejects bad input", {
  expect_invalid(raw_stroke(strokeId = -1L), "strokeId")
  expect_invalid(raw_stroke(strokeId = 1.5), "strokeId")
  expect_invalid(raw_stroke(drawInputType = "pen"), "drawInputType")
  expect_invalid(raw_stroke(tool = "eraser"), "tool")
  expect_invalid(raw_stroke(color = "#abc"), "color")
  expect_invalid(raw_stroke(color = "red"), "color")
  expect_invalid(raw_stroke(lineWidth = "3"), "lineWidth")
  expect_invalid(raw_stroke(sprayDensity = NULL), "sprayDensity")
  expect_invalid(raw_stroke(sprayRadius = -2L), "sprayRadius")
  expect_invalid(raw_stroke(alpha = 1.5), "alpha")
  expect_invalid(raw_stroke(alpha = 0.123), "alpha")
  expect_invalid(raw_stroke(scaleFactor = -1), "scaleFactor")
  expect_invalid(raw_stroke(dpr = "2"), "dpr")
  expect_invalid(raw_stroke(p = list()), "'p'")
  expect_invalid(raw_stroke(p = list(list(1L, 2L, 3L))), "'p'")
  expect_invalid(raw_stroke(p = list(list(1.5, 2L))), "'p'")
  expect_invalid(raw_stroke(p = list(list(100L, 2L))), "outside")
  expect_invalid(raw_stroke(p = list(list(-1L, 2L))), "outside")
  expect_invalid(list(1, 2), "not an object")
})

test_that("alpha with up to 2 decimals is accepted", {
  for (a in c(0, 0.05, 0.1, 0.67, 1)) {
    expect_equal(
      pd_validate_strokes(list(raw_stroke(alpha = a)), 100, 200)[[1]]$alpha,
      a
    )
  }
})

test_that("size limits are enforced", {
  expect_error(
    pd_validate_strokes(rep(list(raw_stroke()), 3), 100, 200, max_strokes = 2),
    "Too many strokes",
    class = "pd_validation_error"
  )
  expect_error(
    pd_validate_strokes(rep(list(raw_stroke()), 3), 100, 200, max_points = 5),
    "Too many points",
    class = "pd_validation_error"
  )
  expect_error(pd_parse_strokes(strrep("x", 11), max_chars = 10), "too large")
  expect_error(
    pd_parse_strokes("[{"),
    "not valid JSON",
    class = "pd_validation_error"
  )
})

test_that("pd_canvas_state ignores stale tokens and validates the rest", {
  bg <- test_background()
  json <- jsonlite::toJSON(list(raw_stroke()), auto_unbox = TRUE, null = "null")
  raw <- list(
    token = 3L,
    strokes = as.character(json),
    png = "data:image/png;base64,AAAA"
  )

  expect_equal(pd_canvas_state(NULL, 3L, bg)$n, 0L)
  expect_equal(pd_canvas_state(raw, 4L, bg)$n, 0L)

  st <- pd_canvas_state(raw, 3L, bg)
  expect_null(st$error)
  expect_equal(st$n, 1L)
  expect_null(st$png) # PNG only when asked for

  st <- pd_canvas_state(raw, 3L, bg, need_png = TRUE)
  expect_equal(st$png, raw$png)

  bad_png <- modifyList(raw, list(png = "javascript:alert(1)"))
  expect_null(pd_canvas_state(bad_png, 3L, bg)$error)
  expect_match(
    pd_canvas_state(bad_png, 3L, bg, need_png = TRUE)$error,
    "image data"
  )
  expect_match(
    pd_canvas_state(raw[-3], 3L, bg, need_png = TRUE)$error,
    "image data"
  )

  st <- pd_canvas_state(modifyList(raw, list(strokes = "[1]")), 3L, bg)
  expect_match(st$error, "not an object")
})
