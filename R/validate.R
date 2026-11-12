# Validation of stroke data sent by the browser.
# R port of PainDrawEM's validateJsonInput() (PainDraw.php), plus limits and a
# bounds check of our own. The browser is untrusted: validate everything.

pd_abort <- function(...) {
  stop(structure(
    class = c("pd_validation_error", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

pd_is_count <- function(x) {
  is.numeric(x) &&
    length(x) == 1 &&
    is.finite(x) &&
    x >= 0 &&
    x == floor(x) &&
    x <= .Machine$integer.max
}

pd_is_int <- function(x) {
  is.numeric(x) &&
    length(x) == 1 &&
    is.finite(x) &&
    x == floor(x) &&
    abs(x) <= .Machine$integer.max
}

pd_is_nonneg_number <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0
}

# PHP: ^(0(\.[0-9]{1,2})?|1(\.0{1,2})?)$ , i.e. 0-1 with at most 2 decimals
pd_is_alpha <- function(x) {
  pd_is_nonneg_number(x) && x <= 1 && abs(x * 100 - round(x * 100)) < 1e-8
}

pd_is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

#' Parse the strokes JSON string from the browser
pd_parse_strokes <- function(
  json,
  max_chars = pd_config$max_strokes_json_chars
) {
  if (!pd_is_string(json)) {
    pd_abort("Stroke data is not a string.")
  }
  if (nchar(json, type = "bytes") > max_chars) {
    pd_abort("Stroke data is too large.")
  }
  tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(e) pd_abort("Stroke data is not valid JSON.")
  )
}

#' Validate strokes
#'
#' @param strokes List of strokes as parsed by `pd_parse_strokes()`.
#' @param w,h Canvas size; points must lie inside the canvas.
#' @return A cleaned list of strokes with keys in PainDrawEM order and proper
#'   types (`sprayDensity`/`sprayRadius` are `NULL` for the pen).
#'   Signals a `pd_validation_error` on invalid input.
pd_validate_strokes <- function(
  strokes,
  w,
  h,
  max_strokes = pd_config$max_strokes,
  max_points = pd_config$max_points
) {
  if (!is.list(strokes) || (length(strokes) > 0 && !is.null(names(strokes)))) {
    pd_abort("Strokes must be an array.")
  }
  if (length(strokes) > max_strokes) {
    pd_abort("Too many strokes.")
  }

  total_points <- 0
  out <- vector("list", length(strokes))
  for (i in seq_along(strokes)) {
    d <- strokes[[i]]
    bad <- function(what) {
      pd_abort("stroke ", i, ": invalid or missing '", what, "'.")
    }
    if (!is.list(d) || is.null(names(d))) {
      pd_abort("stroke ", i, ": not an object.")
    }

    if (!pd_is_count(d[["strokeId"]])) {
      bad("strokeId")
    }
    if (
      !pd_is_string(d[["drawInputType"]]) ||
        !d[["drawInputType"]] %in% c("mouse", "touch")
    ) {
      bad("drawInputType")
    }
    if (!pd_is_string(d[["tool"]]) || !d[["tool"]] %in% c("spray", "pen")) {
      bad("tool")
    }
    if (
      !pd_is_string(d[["color"]]) || !grepl("^#[A-Fa-f0-9]{6}$", d[["color"]])
    ) {
      bad("color")
    }
    if (!pd_is_count(d[["lineWidth"]])) {
      bad("lineWidth")
    }

    spray <- d[["tool"]] == "spray"
    if (spray) {
      if (!pd_is_count(d[["sprayDensity"]])) {
        bad("sprayDensity")
      }
      if (!pd_is_count(d[["sprayRadius"]])) bad("sprayRadius")
    }
    if (!pd_is_alpha(d[["alpha"]])) {
      bad("alpha")
    }
    if (!pd_is_nonneg_number(d[["scaleFactor"]])) {
      bad("scaleFactor")
    }
    # PHP checks scaleFactor here by mistake; we check dpr as intended
    if (!pd_is_nonneg_number(d[["dpr"]])) {
      bad("dpr")
    }

    p <- d[["p"]]
    if (!is.list(p) || length(p) == 0) {
      bad("p")
    }
    total_points <- total_points + length(p)
    if (total_points > max_points) {
      pd_abort("Too many points.")
    }
    p <- lapply(p, function(pair) {
      xy <- if (is.list(pair)) pair else as.list(pair)
      if (length(xy) != 2 || !pd_is_int(xy[[1]]) || !pd_is_int(xy[[2]])) {
        bad("p")
      }
      xy <- as.integer(c(xy[[1]], xy[[2]]))
      if (xy[1] < 0 || xy[2] < 0 || xy[1] >= w || xy[2] >= h) {
        pd_abort("stroke ", i, ": point outside the canvas.")
      }
      xy
    })

    out[[i]] <- list(
      strokeId = as.integer(d[["strokeId"]]),
      drawInputType = d[["drawInputType"]],
      tool = d[["tool"]],
      color = d[["color"]],
      lineWidth = as.integer(d[["lineWidth"]]),
      sprayDensity = if (spray) as.integer(d[["sprayDensity"]]) else NULL,
      sprayRadius = if (spray) as.integer(d[["sprayRadius"]]) else NULL,
      alpha = as.numeric(d[["alpha"]]),
      scaleFactor = as.numeric(d[["scaleFactor"]]),
      dpr = as.numeric(d[["dpr"]]),
      p = p
    )
  }
  out
}

#' Turn the raw canvas state from the browser into validated data
#'
#' @param raw `input$canvas_state` or `input$canvas_request`: list with
#'   `token`, `strokes` (JSON string) and, for requests, `png` (data URL).
#' @param token The token the server expects. A state with another token
#'   belongs to an earlier background/drawing and is ignored.
#' @param background Entry from `pd_backgrounds()`.
#' @param need_png Must `raw` contain the PNG data URL?
#' @return list(n, strokes, strokes_json, png, error)
pd_canvas_state <- function(raw, token, background, need_png = FALSE) {
  state <- function(
    n = 0L,
    strokes = list(),
    strokes_json = NULL,
    png = NULL,
    error = NULL
  ) {
    list(
      n = n,
      strokes = strokes,
      strokes_json = strokes_json,
      png = png,
      error = error
    )
  }
  if (!is.list(raw)) {
    return(state())
  }
  raw_token <- suppressWarnings(as.integer(raw[["token"]]))
  if (length(raw_token) != 1 || !identical(raw_token, as.integer(token))) {
    return(state())
  }

  res <- tryCatch(
    pd_validate_strokes(
      pd_parse_strokes(raw[["strokes"]]),
      background$w,
      background$h
    ),
    pd_validation_error = function(e) e
  )
  if (inherits(res, "error")) {
    return(state(error = conditionMessage(res)))
  }
  if (length(res) == 0) {
    return(state())
  }

  png <- NULL
  if (need_png) {
    png <- raw[["png"]]
    if (
      !pd_is_string(png) ||
        nchar(png, type = "bytes") > pd_config$max_png_chars ||
        !startsWith(png, "data:image/png;base64,")
    ) {
      return(state(error = "the image data is missing or invalid."))
    }
  }
  state(
    n = length(res),
    strokes = res,
    strokes_json = raw[["strokes"]],
    png = png
  )
}
