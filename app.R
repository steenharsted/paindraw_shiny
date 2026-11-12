# paindraw_shiny: collect pain drawings in the PainDrawEM data format.
# Shiny sources the files in R/ automatically before this file.

library(shiny)
library(bslib)

app_version <- pd_app_version("DESCRIPTION")
backgrounds <- pd_backgrounds("www/backgrounds")
default_background <- if (
  pd_config$default_background %in% names(backgrounds)
) {
  pd_config$default_background
} else {
  names(backgrounds)[1]
}

pd_color_input <- function(id, label, value) {
  div(
    class = "form-group shiny-input-container",
    tags$label(class = "control-label", `for` = id, label),
    tags$input(
      id = id,
      type = "color",
      value = value,
      class = "pd-color form-control form-control-color"
    )
  )
}

pd_confirm_modal <- function(title, body, ok_id, cancel_id) {
  modalDialog(
    title = title,
    body,
    easyClose = FALSE,
    footer = tagList(
      actionButton(cancel_id, pd_tr("btn_cancel")),
      actionButton(ok_id, pd_tr("btn_continue"), class = "btn-danger")
    )
  )
}

download_ids <- c("dl_json", "dl_png", "dl_zip")

ui <- page_sidebar(
  # The logo replaces the text title; without a logo, the text is shown
  title = if (is.null(pd_config$logo)) {
    pd_tr("app_title")
  } else {
    tags$span(
      class = "pd-title",
      tags$img(
        src = pd_config$logo,
        class = "pd-logo",
        alt = pd_tr("app_title")
      )
    )
  },
  window_title = pd_tr("app_title"),
  fillable = FALSE,
  tags$head(tags$link(rel = "stylesheet", href = "css/app.css")),
  sidebar = sidebar(
    width = 300,
    selectInput(
      "background",
      pd_tr("background_label"),
      choices = stats::setNames(
        names(backgrounds),
        vapply(backgrounds, `[[`, "", "label")
      ),
      selected = default_background
    ),
    radioButtons(
      "tool",
      pd_tr("tool_label"),
      inline = TRUE,
      choices = stats::setNames(
        c("spray", "pen"),
        c(pd_tr("tool_spray"), pd_tr("tool_pen"))
      ),
      selected = pd_tool_defaults$tool
    ),
    pd_color_input("color", pd_tr("color_label"), pd_tool_defaults$color),
    sliderInput(
      "line_width",
      pd_tr("line_width_label"),
      min = pd_tool_ranges$line_width[1],
      max = pd_tool_ranges$line_width[2],
      value = pd_tool_defaults$line_width,
      step = 1
    ),
    conditionalPanel(
      "input.tool == 'spray'",
      sliderInput(
        "spray_radius",
        pd_tr("spray_radius_label"),
        min = pd_tool_ranges$spray_radius[1],
        max = pd_tool_ranges$spray_radius[2],
        value = pd_tool_defaults$spray_radius,
        step = 1
      ),
      sliderInput(
        "spray_density",
        pd_tr("spray_density_label"),
        min = pd_tool_ranges$spray_density[1],
        max = pd_tool_ranges$spray_density[2],
        value = pd_tool_defaults$spray_density,
        step = 1
      )
    ),
    sliderInput(
      "transparency",
      pd_tr("transparency_label"),
      min = pd_tool_ranges$transparency[1],
      max = pd_tool_ranges$transparency[2],
      value = pd_tool_defaults$transparency,
      step = 1,
      post = " %"
    ),
    helpText(pd_tr("tools_help"))
  ),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(8, 4)),
    card(
      card_header(pd_tr("canvas_title")),
      card_body(mod_canvas_ui("pad")),
      card_footer(class = "small text-muted", pd_tr("canvas_help"))
    ),
    card(
      card_header(pd_tr("save_title")),
      textInput("drawing_id", pd_tr("id_label"), value = "1"),
      helpText(pd_tr("id_help")),
      uiOutput("save_status"),
      div(
        class = "pd-downloads",
        mod_canvas_download_button("pad", "dl_json", "json", pd_tr("dl_json")),
        mod_canvas_download_button("pad", "dl_png", "png", pd_tr("dl_png")),
        mod_canvas_download_button(
          "pad",
          "dl_zip",
          "zip",
          pd_tr("dl_zip"),
          class = "btn-outline-primary"
        ),
        # Hidden; clicked by the JS once the requested file has been built
        downloadLink("dl_file", label = NULL, style = "display: none;")
      ),
      actionButton("new_drawing", pd_tr("new_drawing"), class = "btn-secondary")
    )
  ),
  tags$footer(class = "small text-muted mt-3", pd_tr("footer", app_version)),
  # At the end of the page, so Shiny's own JavaScript is loaded first
  tags$script(src = "js/paindraw_shiny.js")
)

server <- function(input, output, session) {
  # Background, with confirmation when it would clear a drawing ----
  active_bg <- reactiveVal(default_background)
  background <- reactive(backgrounds[[active_bg()]])

  # Spray and pen each remember their own transparency. The slider shows the
  # value of the active tool; `settings` reads the remembered value, so a tool
  # switch never sends the other tool's transparency to the browser.
  transparency <- reactiveVal(c(
    spray = pd_default_transparency("spray"),
    pen = pd_default_transparency("pen")
  ))
  active_tool <- reactive(if (identical(input$tool, "pen")) "pen" else "spray")

  observeEvent(input$transparency, {
    tr <- transparency()
    tr[[active_tool()]] <- input$transparency
    transparency(tr)
  })
  observeEvent(
    input$tool,
    updateSliderInput(
      session,
      "transparency",
      value = transparency()[[active_tool()]]
    ),
    ignoreInit = TRUE
  )

  settings <- reactive(pd_tool_settings(
    active_tool(),
    input$color,
    input$line_width,
    input$spray_radius,
    input$spray_density,
    transparency()[[active_tool()]]
  ))

  canvas <- mod_canvas_server("pad", background, settings)
  state <- canvas$state

  observeEvent(
    input$background,
    {
      key <- input$background
      if (!key %in% names(backgrounds) || identical(key, active_bg())) {
        return()
      }
      if (state()$n == 0) {
        active_bg(key)
      } else {
        showModal(pd_confirm_modal(
          pd_tr("confirm_bg_title"),
          pd_tr("confirm_bg_body"),
          "bg_ok",
          "bg_cancel"
        ))
      }
    },
    ignoreInit = TRUE
  )
  observeEvent(input$bg_ok, {
    removeModal()
    if (input$background %in% names(backgrounds)) active_bg(input$background)
  })
  observeEvent(input$bg_cancel, {
    removeModal()
    updateSelectInput(session, "background", selected = active_bg())
  })

  # Drawing IDs ----
  # `drawing` numbers the drawings in this session; `used_ids` maps each
  # downloaded ID to the drawing it was used for.
  drawing <- reactiveVal(1L)
  used_ids <- reactiveVal(stats::setNames(integer(), character()))
  # ID and strokes JSON of the last download of the current drawing
  last_download <- reactiveVal(NULL)

  id_check <- reactive(pd_check_id(input$drawing_id, used_ids(), drawing()))

  # The drawing as it is now has been downloaded (under any ID) ...
  drawing_downloaded <- reactive({
    !is.null(last_download()) &&
      identical(state()$strokes_json, last_download()$strokes_json)
  })
  # ... and under the ID that is entered now
  is_saved <- reactive({
    drawing_downloaded() && identical(id_check()$id, last_download()$id)
  })

  problem <- reactive(pd_download_problem(state(), id_check()))

  observe({
    session$sendCustomMessage(
      "pd-enable",
      list(ids = download_ids, enabled = is.null(problem()))
    )
  })

  output$save_status <- renderUI({
    msg <- problem()
    if (!is.null(msg)) {
      p(class = "small text-danger", msg)
    } else if (is_saved()) {
      p(class = "small text-success", pd_tr("status_saved", id_check()$id))
    } else {
      p(class = "small text-muted", pd_tr("status_strokes", state()$n))
    }
  })

  # Downloads ----
  # A click sends the drawing (with the PNG if needed) as canvas$request().
  # We build the file, keep it in `pending`, and let the browser fetch it
  # through the hidden download link.
  pending <- reactiveVal(NULL)

  observeEvent(canvas$request(), {
    rq <- canvas$request()
    chk <- id_check()
    msg <- pd_download_problem(rq$state, chk)
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return()
    }
    file <- tryCatch(
      pd_make_download(rq$kind, chk$id, background(), rq$state, app_version),
      error = function(e) e
    )
    if (inherits(file, "error")) {
      showNotification(
        pd_tr("invalid_drawing", conditionMessage(file)),
        type = "error"
      )
      return()
    }
    pending(file)
    used <- used_ids()
    used[chk$id] <- drawing()
    used_ids(used)
    last_download(list(id = chk$id, strokes_json = rq$state$strokes_json))
    session$sendCustomMessage("pd-download", list(id = "dl_file"))
  })

  output$dl_file <- downloadHandler(
    filename = function() pending()$filename,
    content = function(file) writeBin(pending()$bytes, file)
  )
  # The link is hidden, but its URL must still be set
  outputOptions(output, "dl_file", suspendWhenHidden = FALSE)

  # New drawing ----
  start_new_drawing <- function() {
    canvas$clear()
    drawing(drawing() + 1L)
    last_download(NULL)
    pending(NULL)
    updateTextInput(
      session,
      "drawing_id",
      value = pd_next_id(input$drawing_id, names(used_ids()))
    )
  }
  observeEvent(input$new_drawing, {
    if (state()$n > 0 && !drawing_downloaded()) {
      showModal(pd_confirm_modal(
        pd_tr("confirm_new_title"),
        pd_tr("confirm_new_body"),
        "new_ok",
        "new_cancel"
      ))
    } else {
      start_new_drawing()
    }
  })
  observeEvent(input$new_ok, {
    removeModal()
    start_new_drawing()
  })
  observeEvent(input$new_cancel, removeModal())
}

shinyApp(ui, server)
