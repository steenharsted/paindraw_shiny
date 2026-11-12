test_that("labels are derived from file names", {
  expect_equal(
    pd_background_label(c(
      "bg_lower_arm_palmar.png",
      "bg_full_body.png",
      "bg_spiral_grey.png",
      "bg_zealand.png"
    )),
    c("Lower arm (palmar)", "Full body", "Spiral (grey)", "Zealand")
  )
})

test_that("canvas width is clamped to 796 px as in PainDrawEM", {
  expect_equal(pd_canvas_size(705, 1102), c(w = 705L, h = 1102L))
  expect_equal(pd_canvas_size(992, 1586), c(w = 796L, h = 1273L)) # 1272.66
  expect_equal(pd_canvas_size(1592, 1001), c(w = 796L, h = 501L)) # 500.5 rounds up, like PHP
})

test_that("the registry has dims and md5 of the served files", {
  bgs <- pd_backgrounds(file.path(app_dir, "www", "backgrounds"))
  expect_true("bg_zealand" %in% names(bgs))
  z <- bgs$bg_zealand
  expect_equal(z$file, "bg_zealand.png")
  expect_equal(c(z$w, z$h), c(705L, 1102L))
  # Same bytes as zealand.png used in the paindrawr reference JSON
  expect_equal(z$md5, "bfea4dfa78c3f7fbe68cd312450d6cef")
  expect_equal(z$url, "backgrounds/bg_zealand.png")
  for (bg in bgs) {
    expect_lte(bg$w, 796L)
  }
})

test_that("pd_png_dims rejects non-PNG bytes", {
  expect_error(
    pd_png_dims(charToRaw("GIF89a not a png at all......")),
    "Not a PNG"
  )
})

test_that("tool settings are clamped and converted", {
  s <- pd_tool_settings("spray", "#FC3D03", 3, 5, 15, 90)
  expect_equal(
    s,
    list(
      tool = "spray",
      color = "#fc3d03",
      lineWidth = 3L,
      sprayDensity = 15L,
      sprayRadius = 5L,
      alpha = 0.1
    )
  )
  s <- pd_tool_settings("eraser", "#abc", 999, -1, NULL, 33)
  expect_equal(s$tool, "spray")
  expect_equal(s$color, "#aabbcc")
  expect_equal(s$lineWidth, 20L)
  expect_equal(s$sprayRadius, 1L)
  expect_equal(s$sprayDensity, 15L)
  expect_equal(s$alpha, 0.67)
  expect_equal(pd_tool_settings("pen", "red", 3, 5, 15, 0)$color, "#fc3d03")
})

test_that("an invalid transparency falls back to the tool's default", {
  expect_equal(pd_default_transparency("spray"), 90L)
  expect_equal(pd_default_transparency("pen"), 0L)
  expect_equal(pd_tool_settings("spray", "#fc3d03", 3, 5, 15, NULL)$alpha, 0.1)
  expect_equal(pd_tool_settings("pen", "#fc3d03", 3, 5, 15, NULL)$alpha, 1)
  expect_equal(pd_tool_settings("pen", "#fc3d03", 3, 5, 15, 50)$alpha, 0.5)
})
