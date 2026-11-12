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
    strokeId = 1L,
    drawInputType = "mouse",
    tool = "spray",
    color = "#fc3d03",
    lineWidth = 3L,
    sprayDensity = 15L,
    sprayRadius = 5L,
    alpha = 0.1,
    scaleFactor = 0.5,
    dpr = 2,
    p = list(list(10L, 20L), list(11L, 21L))
  )
  # Replace top-level keys only (modifyList() would merge `p` recursively)
  changes <- list(...)
  stroke[names(changes)] <- changes
  stroke
}

test_background <- function() {
  list(
    file = "bg_test.png",
    md5 = "0123456789abcdef0123456789abcdef",
    w = 100L,
    h = 200L
  )
}
