# All user-facing text. A translation is a change to this file only.

pd_strings <- list(
  app_title = "Pain drawing",

  # Sidebar
  background_label = "Background image",
  tool_label = "Tool",
  tool_spray = "Spray",
  tool_pen = "Pen",
  color_label = "Colour",
  line_width_label = "Line width / dot size (px)",
  spray_radius_label = "Spray radius (px)",
  spray_density_label = "Spray density (dots per point)",
  transparency_label = "Transparency",
  tools_help = "Changes apply to the next stroke. Spray and pen each keep their own transparency.",

  # Canvas
  canvas_title = "Drawing",
  canvas_aria = "Drawing canvas",
  canvas_help = "Draw with a mouse, finger or stylus.",
  undo = "Undo",
  clear = "Clear",
  clear_confirm = "Clear the whole drawing? This cannot be undone.",
  bg_load_error = "The background image could not be loaded.",

  # Save card
  save_title = "Save",
  id_label = "Drawing ID",
  id_help = paste(
    "The ID is stored in the files and used as the file name.",
    "Each drawing in this session needs its own ID.",
    "Nothing is stored on the server, so IDs from earlier sessions",
    "cannot be checked."
  ),
  dl_json = "Data (JSON)",
  dl_png = "Image (PNG)",
  dl_zip = "Both (zip)",
  new_drawing = "New drawing",
  status_strokes = "%d stroke(s), not downloaded yet.",
  status_saved = "Downloaded as '%s'. Press 'New drawing' to start the next one.",

  # Problems
  no_strokes = "Draw something before downloading.",
  invalid_drawing = "The drawing could not be validated: %s",
  id_empty = "Enter a drawing ID.",
  id_invalid = "The drawing ID contains invalid characters.",
  id_too_long = "The drawing ID can have at most %d characters.",
  id_used = "The ID '%s' is already used for another drawing in this session.",
  id_file_clash = "The ID '%s' gives the same file name as '%s', used earlier in this session.",

  # Footer (%s = app version)
  footer = paste(
    "paindraw_shiny v%s by Steen Harsted and S\u00f8ren O'Neill.",
    "Free software under the GNU GPL (version 3 or later).",
    "If you use it in research, please cite paindraw_shiny and paindrawr.",
    "Nothing you draw is stored on the server."
  ),

  # Dialogues
  confirm_bg_title = "Change background?",
  confirm_bg_body = "Changing the background clears the current drawing.",
  confirm_new_title = "Start a new drawing?",
  confirm_new_body = "The current drawing has not been downloaded and will be lost.",
  btn_cancel = "Cancel",
  btn_continue = "Continue"
)

#' Look up a UI string; extra arguments are passed to sprintf()
pd_tr <- function(key, ...) {
  txt <- pd_strings[[key]]
  if (is.null(txt)) {
    stop("Unknown UI string: ", key, call. = FALSE)
  }
  if (...length() > 0) sprintf(txt, ...) else txt
}
