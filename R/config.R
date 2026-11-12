# App-wide constants. Immutable, so safe to share between sessions.

pd_config <- list(
  app_name = "paindraw_shiny",
  # PainDrawEM clamps the canvas width to this (redcap_module_save_configuration)
  max_canvas_width = 796L,
  default_background = "bg_full_body",
  # Header logo, relative to www/. Set to NULL to show no logo.
  logo = "img/paindraw-shiny-logo.png",
  max_id_chars = 100L,
  # Limits on what a (possibly hostile) browser may send
  max_strokes = 5000L,
  max_points = 500000L,
  max_strokes_json_chars = 20e6,
  max_png_chars = 40e6
)

# PainDrawEM defaults (parseModuleSettings)
pd_tool_defaults <- list(
  tool = "spray",
  color = "#fc3d03",
  line_width = 3L,
  spray_radius = 5L,
  spray_density = 15L,
  transparency = 90L,
  # Not PainDrawEM: the pen gets its own transparency, opaque by default.
  # (A semi-transparent pen rendered as in PainDrawEM turns opaque while drawn.)
  pen_transparency = 0L
)

pd_tool_ranges <- list(
  line_width = c(1L, 20L),
  spray_radius = c(1L, 30L),
  spray_density = c(1L, 50L),
  transparency = c(0L, 100L)
)

#' Read the app version from DESCRIPTION (single source of truth)
pd_app_version <- function(path = "DESCRIPTION") {
  unname(read.dcf(path, fields = "Version")[1, 1])
}

#' Tool settings as sent to the browser
#'
#' Inputs come from the client and are untrusted, so they are clamped to the
#' allowed ranges and fall back to the defaults.
pd_tool_settings <- function(
  tool,
  color,
  line_width,
  spray_radius,
  spray_density,
  transparency
) {
  clamp <- function(x, name, default = pd_tool_defaults[[name]]) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1 || !is.finite(x)) {
      x <- default
    }
    range <- pd_tool_ranges[[name]]
    as.integer(min(max(round(x), range[1]), range[2]))
  }
  tool <- if (identical(tool, "pen")) "pen" else "spray"
  list(
    tool = tool,
    color = pd_normalise_color(color),
    lineWidth = clamp(line_width, "line_width"),
    sprayDensity = clamp(spray_density, "spray_density"),
    sprayRadius = clamp(spray_radius, "spray_radius"),
    alpha = round(
      1 -
        clamp(transparency, "transparency", pd_default_transparency(tool)) /
          100,
      2
    )
  )
}

#' Default transparency (%) for a tool
pd_default_transparency <- function(tool) {
  if (identical(tool, "pen")) {
    pd_tool_defaults$pen_transparency
  } else {
    pd_tool_defaults$transparency
  }
}

#' Normalise a colour to 6-digit lower-case hex
#'
#' PainDrawEM's validator only accepts `#rrggbb`, so `#rgb` is expanded.
#' Anything else gives the default colour.
pd_normalise_color <- function(color) {
  if (!is.character(color) || length(color) != 1 || is.na(color)) {
    return(pd_tool_defaults$color)
  }
  color <- tolower(trimws(color))
  if (grepl("^#[0-9a-f]{3}$", color)) {
    digits <- strsplit(substring(color, 2), "")[[1]]
    color <- paste0("#", paste0(digits, digits, collapse = ""))
  }
  if (grepl("^#[0-9a-f]{6}$", color)) color else pd_tool_defaults$color
}
