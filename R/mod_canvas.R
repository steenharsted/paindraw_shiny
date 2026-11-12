# Shiny module: the drawing canvas and its bridge to www/js/paindraw_shiny.js

mod_canvas_ui <- function(id) {
  ns <- NS(id)
  canvas_id <- ns("canvas")
  action_button <- function(action, label, ...) {
    tags$button(
      type = "button",
      class = "btn btn-outline-secondary btn-sm",
      `data-pd-action` = action,
      `data-pd-canvas` = canvas_id,
      disabled = NA,
      label,
      ...
    )
  }
  tagList(
    tags$canvas(
      id = canvas_id,
      class = "pd-canvas",
      width = 1,
      height = 1,
      role = "img",
      `aria-label` = pd_tr("canvas_aria")
    ),
    div(
      class = "pd-canvas-actions",
      action_button("undo", pd_tr("undo")),
      action_button(
        "clear",
        pd_tr("clear"),
        `data-pd-confirm` = pd_tr("clear_confirm")
      )
    )
  )
}

#' A download button for the canvas `id`; clicking it sends a request to R
#' (see `request` in `mod_canvas_server()`). Disabled until enabled by R.
mod_canvas_download_button <- function(
  id,
  button_id,
  kind,
  label,
  class = "btn-primary"
) {
  tags$button(
    type = "button",
    id = button_id,
    class = paste("btn", class),
    `data-pd-download` = kind,
    `data-pd-canvas-id` = NS(id)("canvas"),
    disabled = NA,
    icon("download"),
    label
  )
}

#' @param background Reactive: entry from `pd_backgrounds()`.
#' @param settings Reactive: tool settings from `pd_tool_settings()`.
#' @return list(state = reactive validated canvas state, request = reactive
#'   download request, clear = function, canvas_id)
mod_canvas_server <- function(id, background, settings) {
  moduleServer(id, function(input, output, session) {
    canvas_id <- session$ns("canvas")

    # Increased whenever the canvas is reset. The browser echoes it back, so
    # a state from before the reset is never mistaken for the current one.
    token <- reactiveVal(0L)
    next_token <- function() {
      value <- isolate(token()) + 1L
      token(value)
      value
    }

    observe({
      session$sendCustomMessage(
        "pd-settings",
        list(id = canvas_id, settings = settings())
      )
    })

    observe({
      bg <- background()
      session$sendCustomMessage(
        "pd-background",
        list(
          id = canvas_id,
          token = next_token(),
          url = bg$url,
          w = bg$w,
          h = bg$h,
          error_text = pd_tr("bg_load_error")
        )
      )
    })

    # Sent after every stroke: strokes only, no PNG
    state <- reactive(pd_canvas_state(
      input$canvas_state,
      token(),
      background()
    ))

    # Sent when a download button is clicked: strokes, plus the PNG for
    # "png"/"zip". A new list each time, so observers fire on every click.
    request <- reactiveVal(NULL)
    observeEvent(input$canvas_request, {
      raw <- input$canvas_request
      kind <- raw[["kind"]]
      if (!pd_is_string(kind) || !kind %in% pd_download_kinds) {
        return()
      }
      request(list(
        kind = kind,
        state = pd_canvas_state(
          raw,
          token(),
          background(),
          need_png = kind != "json"
        ),
        nonce = raw[["nonce"]]
      ))
    })

    clear <- function() {
      session$sendCustomMessage(
        "pd-clear",
        list(id = canvas_id, token = next_token())
      )
    }

    list(state = state, request = request, clear = clear, canvas_id = canvas_id)
  })
}
