# maketimeliner — Shiny editor for multi-stream timelines.
# Every table (events, links, timelines, streams, windows, overlay, phases,
# legend, settings) is editable here AND importable/exportable as CSV, using
# the same column names as read_timeline_csvs() / write_timeline_csvs().

library(shiny)
library(maketimeliner)

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
TC <- maketimeliner:::.table_cols

# All tables are held as character data.frames; the package coerces on render.
as_chr_df <- function(df, table) {
  df <- maketimeliner:::.conform(df, table)
  df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  df[, TC[[table]], drop = FALSE]
}
blank_row <- function(table) {
  r <- as.data.frame(as.list(stats::setNames(rep("", length(TC[[table]])), TC[[table]])),
                     stringsAsFactors = FALSE, check.names = FALSE)
  r
}
mat_resize <- function(m, ids) {
  n <- length(ids)
  out <- matrix("", n, n, dimnames = list(ids, ids))
  if (!is.null(m) && length(m)) {
    r <- intersect(rownames(m), ids); c <- intersect(colnames(m), ids)
    out[r, c] <- m[r, c, drop = FALSE]
  }
  out
}
mat_to_long <- function(m) {
  if (is.null(m) || !any(nzchar(m)))
    return(data.frame(from = character(0), to = character(0), value = character(0),
                      stringsAsFactors = FALSE))
  idx <- which(m != "", arr.ind = TRUE)
  data.frame(from = rownames(m)[idx[, 1]], to = colnames(m)[idx[, 2]],
             value = m[idx], stringsAsFactors = FALSE)
}

COLOUR_COLS <- c("colour", "stroke", "fontcolour")

# ── CSS ───────────────────────────────────────────────────────────────────────
APP_CSS <- "
body{background:#f0f2f5;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
.app-header{padding:14px 0 10px;border-bottom:1px solid #e2e8f0;margin-bottom:14px}
.app-title{font-size:20px;font-weight:700;color:#2d3748;display:inline}
.app-badge{font-size:11px;font-weight:600;background:#2b8a6e;color:#fff;border-radius:10px;padding:2px 8px;margin-left:8px;vertical-align:middle}
.panel-box{background:#fff;border-radius:8px;border:1px solid #e2e8f0;padding:14px;margin-bottom:12px}
.panel-box h5{font-size:11px;font-weight:700;color:#718096;text-transform:uppercase;letter-spacing:.06em;margin:0 0 10px;padding-bottom:6px;border-bottom:1px solid #edf2f7}
.help{font-size:12px;color:#718096;margin-bottom:8px;line-height:1.5}
.btn-row .btn,.btn-row .shiny-download-link{margin:0 4px 6px 0}
.btn-xs{padding:2px 8px;font-size:12px}
.err{color:#c53030;font-size:12px;white-space:pre-wrap}
.warn{color:#b7791f;font-size:12px;white-space:pre-wrap}
.nav-tabs>li>a{padding:6px 9px;font-size:12.5px}
table.dataTable td{font-size:12px;padding:3px 6px !important}
#preview iframe{width:100%;height:760px;border:1px solid #e2e8f0;border-radius:6px;background:#fff}
.settings-grid .form-group{margin-bottom:6px}
.settings-grid label{font-size:11.5px;color:#4a5568;font-weight:600}
.settings-grid .shiny-input-container{width:100% !important}
"

# ── Generic editable-table module ───────────────────────────────────────────
tableUI <- function(id, title, help) {
  ns <- NS(id)
  div(class = "panel-box",
    h5(title), div(class = "help", HTML(help)),
    div(class = "btn-row",
      actionButton(ns("add"), "+ Row", class = "btn-xs"),
      actionButton(ns("dup"), "Duplicate selected", class = "btn-xs"),
      actionButton(ns("del"), "Delete selected", class = "btn-xs"),
      downloadButton(ns("dl"), "CSV", class = "btn-xs")),
    fileInput(ns("csv"), NULL, accept = ".csv", buttonLabel = "Import CSV…",
              placeholder = "replace this table from CSV"),
    DT::DTOutput(ns("tbl")))
}

tableServer <- function(id, rv, key, table, file_stub, on_change = function() NULL) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      rv$ver[[key]]
      df <- isolate(rv[[key]])
      ci <- which(names(df) %in% COLOUR_COLS) - 1L
      js <- sprintf("function(row, data){ [%s].forEach(function(i){ var v = data[i];
        if (v) { $('td:eq(' + i + ')', row).css({'background-color': v, 'color': '#000'}); } }); }",
        paste(ci, collapse = ","))
      DT::datatable(df, rownames = FALSE, editable = list(target = "cell"),
                    selection = "multiple",
                    options = list(dom = "tp", pageLength = 50, scrollX = TRUE,
                                   ordering = FALSE, rowCallback = DT::JS(js)))
    }, server = FALSE)
    observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      df <- rv[[key]]
      df[info$row, info$col + 1L] <- as.character(info$value)
      rv[[key]] <- df
      on_change()
    })
    bump <- function() { rv$ver[[key]] <- (rv$ver[[key]] %||% 0) + 1; on_change() }
    observeEvent(input$add, { rv[[key]] <- rbind(rv[[key]], blank_row(table)); bump() })
    observeEvent(input$dup, {
      s <- input$tbl_rows_selected; req(length(s))
      rv[[key]] <- rbind(rv[[key]], rv[[key]][s, , drop = FALSE]); bump() })
    observeEvent(input$del, {
      s <- input$tbl_rows_selected; req(length(s))
      rv[[key]] <- rv[[key]][-s, , drop = FALSE]; bump() })
    observeEvent(input$csv, {
      df <- tryCatch(maketimeliner:::.read_csv(input$csv$datapath),
                     error = function(e) { showNotification(e$message, type = "error"); NULL })
      req(df)
      rv[[key]] <- as_chr_df(df, table); bump()
      showNotification(paste("Imported", nrow(df), "rows into", title_of(key)))
    })
    output$dl <- downloadHandler(
      filename = function() paste0(rv$prefix, "_", file_stub, ".csv"),
      content  = function(f) utils::write.csv(rv[[key]], f, row.names = FALSE))
  })
}
title_of <- function(key) gsub("_", " ", key)

# ── Matrix (links) module: list view + matrix view, both editable ─────────────
matrixUI <- function(id, title, help, value_label = "Code(s)") {
  ns <- NS(id)
  div(class = "panel-box",
    h5(title), div(class = "help", HTML(help)),
    fluidRow(
      column(4, selectInput(ns("from"), "From", choices = NULL)),
      column(4, selectInput(ns("to"), "To", choices = NULL)),
      column(4, textInput(ns("val"), value_label, ""))),
    div(class = "btn-row",
      actionButton(ns("set"), "Set link", class = "btn-xs"),
      actionButton(ns("del"), "Delete selected links", class = "btn-xs"),
      downloadButton(ns("dl"), "Matrix CSV", class = "btn-xs")),
    fileInput(ns("csv"), NULL, accept = ".csv", buttonLabel = "Import matrix CSV…",
              placeholder = "first column = from-ids; headers = to-ids"),
    tabsetPanel(type = "pills",
      tabPanel("List", DT::DTOutput(ns("long"))),
      tabPanel("Matrix", div(class = "help", "Edit cells directly (blank or 0 = no link)."),
               DT::DTOutput(ns("mat")))))
}

matrixServer <- function(id, rv, key, file_stub) {
  moduleServer(id, function(input, output, session) {
    observe({
      ids <- rv$nodes$id[nzchar(rv$nodes$id)]
      updateSelectInput(session, "from", choices = ids, selected = isolate(input$from))
      updateSelectInput(session, "to", choices = ids, selected = isolate(input$to))
    })
    m <- reactive({ mat_resize(rv[[key]], unique(rv$nodes$id[nzchar(rv$nodes$id)])) })
    bump <- function() rv$ver[[key]] <- (rv$ver[[key]] %||% 0) + 1
    output$long <- DT::renderDT({
      rv$ver[[key]]; df <- mat_to_long(isolate(m()))
      DT::datatable(df, rownames = FALSE, editable = list(target = "cell", disable = list(columns = c(0, 1))),
                    options = list(dom = "tp", pageLength = 50, ordering = FALSE))
    }, server = FALSE)
    observeEvent(input$long_cell_edit, {
      info <- input$long_cell_edit; mm <- m(); df <- mat_to_long(mm)
      for (k in seq_len(nrow(info))) if (info$col[k] == 2L)
        mm[df$from[info$row[k]], df$to[info$row[k]]] <- trimws(as.character(info$value[k]))
      rv[[key]] <- mm
    })
    output$mat <- DT::renderDT({
      rv$ver[[key]]; mm <- isolate(m())
      DT::datatable(mm, rownames = TRUE, editable = list(target = "cell", disable = list(columns = 0)),
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    }, server = FALSE)
    observeEvent(input$mat_cell_edit, {
      info <- input$mat_cell_edit; mm <- m()
      v <- trimws(as.character(info$value)); if (v == "0") v <- ""
      mm[info$row, info$col] <- v
      rv[[key]] <- mm
    })
    observeEvent(input$set, {
      req(input$from, input$to); mm <- m()
      v <- trimws(input$val); if (v == "0") v <- ""
      mm[input$from, input$to] <- v
      rv[[key]] <- mm; bump()
    })
    observeEvent(input$del, {
      s <- input$long_rows_selected; req(length(s)); mm <- m(); df <- mat_to_long(mm)
      for (k in s) mm[df$from[k], df$to[k]] <- ""
      rv[[key]] <- mm; bump()
    })
    observeEvent(input$csv, {
      mm <- tryCatch(maketimeliner:::.read_matrix_csv(input$csv$datapath),
                     error = function(e) { showNotification(e$message, type = "error"); NULL })
      req(mm); mm[mm == "0"] <- ""
      rv[[key]] <- mm; bump()
      showNotification("Matrix imported")
    })
    output$dl <- downloadHandler(
      filename = function() paste0(rv$prefix, "_", file_stub, ".csv"),
      content  = function(f) {
        mm <- m(); if (!grepl("labels", file_stub)) mm[mm == ""] <- "0"
        df <- data.frame(id = rownames(mm), mm, check.names = FALSE, stringsAsFactors = FALSE)
        names(df)[1] <- ""
        utils::write.csv(df, f, row.names = FALSE)
      })
  })
}

# ── Settings UI built from timeline_defaults() ──────────────────────────────
SETTING_GROUPS <- list(
  "Page & axis" = c("title", "time_scale", "date_format", "axis_label", "show_axis",
                    "show_grid", "plot_width", "padding", "background", "font_family",
                    "show_legend", "legend_title", "tooltip_image_width",
                    "embed_images", "image_max_mb"),
  "Streams" = c("stream_height", "stream_gap", "stream_label_width", "stream_fontsize",
                "stream_colours", "stream_alpha"),
  "Events (nodes)" = c("node_shape", "node_colour", "node_alpha", "node_stroke",
                       "node_stroke_width", "node_linetype", "node_width", "node_height",
                       "fontsize", "fontcolour", "show_node_labels", "label_position",
                       "min_node_sep"),
  "Timelines (links)" = c("edge_palette", "edge_width", "edge_alpha", "edge_linetype",
                          "curve", "curve_tension", "bundle_gap", "timeline_labels",
                          "edge_label_fontsize"),
  "Overlay links" = c("overlay_edge_colour", "overlay_edge_width", "overlay_edge_alpha",
                      "overlay_edge_linetype", "overlay_curvature"),
  "Windows" = c("window_colour", "window_alpha", "window_stroke", "window_stroke_width",
                "window_linetype"))

setting_input <- function(k, v) {
  id <- paste0("set_", k); ch <- maketimeliner:::.setting_choices[[k]]
  if (!is.null(ch)) selectInput(id, k, choices = ch, selected = v)
  else if (is.logical(v)) checkboxInput(id, k, value = isTRUE(v))
  else if (is.numeric(v)) numericInput(id, k, value = v)
  else textInput(id, k, value = v)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML(APP_CSS))),
  div(class = "app-header",
      span(class = "app-title", "maketimeliner"),
      span(class = "app-badge", paste0("v", utils::packageVersion("maketimeliner"))),
      span(style = "margin-left:12px;color:#718096;font-size:13px",
           "Non-linear multi-stream timelines → interactive HTML / SVG")),
  fluidRow(
    column(5,
      tabsetPanel(id = "tabs",
        tabPanel("Import",
          div(class = "panel-box", h5("Import a CSV set"),
            div(class = "help", HTML(paste(
              "Select several CSVs at once; files are recognised by suffix",
              "(<code>_node_props</code>, <code>_adj</code>, <code>_edge_props</code>,",
              "<code>_edge_labels</code>, <code>_overlay</code>, <code>_overlay_edge_props</code>,",
              "<code>_overlay_edge_labels</code>, <code>_streams</code>, <code>_windows</code>,",
              "<code>_phases</code>, <code>_link_props</code>, <code>_spans</code>, <code>_legend_colours</code>,",
              "<code>_legend_shapes</code>, <code>_legend_linetypes</code>,",
              "<code>_settings</code>). Only <code>_node_props</code> is required."))),
            fileInput("batch", NULL, multiple = TRUE, accept = ".csv",
                      buttonLabel = "Choose CSV files…"),
            textInput("batch_prefix", "Prefix (only if the files hold several diagrams)", ""),
            hr(),
            div(class = "btn-row",
              actionButton("load_example", "Load Kingdon example", class = "btn-xs"),
              actionButton("new_blank", "New blank diagram", class = "btn-xs"))),
          div(class = "panel-box", h5("Export"),
            textInput("prefix", "File prefix", "timeline"),
            div(class = "btn-row",
              downloadButton("dl_html", "Interactive HTML"),
              downloadButton("dl_svg", "SVG"),
              downloadButton("dl_zip", "All CSVs (.zip)")))),
        tabPanel("Events",
          tableUI("nodes", "Events (node_props)", paste(
            "<b>id</b>, <b>time</b>, <b>stream</b> required. <i>time</i>: number, date",
            "(YYYY-MM-DD / YYYY-MM) or phase name. <i>slot</i> 0–1 = manual vertical",
            "position inside the stream (blank = auto). Style: shape, colour, alpha, stroke,",
            "stroke_width, linetype, fontcolour, fontsize, width, height, label_position.",
            "<i>image</i>: picture URL shown when hovering the event in the HTML;",
            "<i>image_source</i>: web page the picture comes from (linked in the HTML; blank = the image URL).",
            "Type <code>\\n</code> in a label for a line break.")),
          tableUI("spans", "Spans (how long an event is carried)", paste(
            "One row per period an event is carried: <b>id</b> (event), <i>start</i> (blank =",
            "event time), <b>end</b>, label (e.g. the carrying actor), band colour / alpha /",
            "width, centre-line linetype / line_colour / line_width, source, description."))),
        tabPanel("Links",
          matrixUI("adj", "Timeline links (adjacency matrix)", paste(
            "Each link = one step of a timeline from an event to a later event.",
            "The value is the <b>timeline code</b> (see Timelines tab); several timelines",
            "sharing a link: <code>1;3</code>.")),
          matrixUI("elab", "Link labels (edge_labels)", "Text drawn at the middle of a link.",
                   value_label = "Label")),
        tabPanel("Timelines",
          tableUI("edge_props", "Timelines (edge_props)", paste(
            "One row per timeline. <b>weight</b> = the code used in the Links matrix.",
            "Edit <i>colour</i>, <i>alpha</i>, <i>width</i>, <i>linetype</i> (solid, dashed,",
            "dotted, dotdash, longdash, twodash or a dash pattern like <code>8,3</code>).")),
          tableUI("link_props", "Link styles (link_props)", paste(
            "Optional overrides for single links, e.g. the actor behind a move between",
            "streams: <b>from</b>, <b>to</b> (event ids), <i>weight</i> (timeline code; blank =",
            "all timelines on that link), colour, alpha, width, linetype, label, description",
            "(label/description show in the HTML tooltip)."))),
        tabPanel("Streams",
          tableUI("streams", "Streams", paste(
            "Any number of streams; <i>order</i> = top-to-bottom. Style: colour, alpha,",
            "stroke, stroke_width, linetype, height, fontcolour."))),
        tabPanel("Windows",
          tableUI("windows", "Windows", paste(
            "Periods such as policy windows. <i>start</i>/<i>end</i> on the time axis",
            "(end blank = single line). <i>streams</i>: ids separated by <code>;</code>",
            "(blank = all)."))),
        tabPanel("Overlay",
          matrixUI("ov", "Overlay links", "Annotation links (e.g. coupling) drawn as arcs; value = overlay code."),
          tableUI("ov_props", "Overlay link styles (overlay_edge_props)",
                  "One row per overlay code: weight, label, colour, alpha, width, linetype."),
          matrixUI("ovlab", "Overlay link labels", "Text at the middle of overlay arcs.",
                   value_label = "Label")),
        tabPanel("Phases",
          tableUI("phases", "Phases (ordinal time axis)", paste(
            "Optional. When <i>time</i> values are names (e.g. 'Agenda', 'Decision'), list",
            "them here with an <i>order</i>; <i>label</i> is shown on the axis."))),
        tabPanel("Legend",
          tableUI("legend_colours", "Legend: event colours", "colour, label"),
          tableUI("legend_shapes", "Legend: event shapes",
                  "shape (circle, ellipse, rect, rounded, diamond, triangle), label"),
          tableUI("legend_linetypes", "Legend: line types",
                  "linetype, label, colour, width (e.g. one row per actor used in Link styles)")),
        tabPanel("Settings",
          div(class = "panel-box", h5("Settings"),
            div(class = "btn-row",
              downloadButton("dl_settings", "Settings CSV", class = "btn-xs"),
              actionButton("reset_settings", "Reset to defaults", class = "btn-xs")),
            fileInput("csv_settings", NULL, accept = ".csv", buttonLabel = "Import settings CSV…"),
            div(class = "settings-grid", uiOutput("settings_ui"))))
      )),
    column(7,
      div(class = "panel-box", h5("Preview (interactive HTML)"),
        uiOutput("msgs"),
        div(id = "preview", uiOutput("preview"))),
      div(class = "panel-box", h5("R code"),
        div(class = "help", "Reproduce the current diagram from R."),
        verbatimTextOutput("rcode")))
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(ver = list(), prefix = "timeline", settings_ver = 0)

  load_spec <- function(spec, prefix = "timeline") {
    rv$nodes          <- as_chr_df(spec$nodes, "node_props")
    rv$streams        <- as_chr_df(spec$streams, "streams")
    rv$edge_props     <- as_chr_df(spec$edge_props, "edge_props")
    rv$ov_props       <- as_chr_df(spec$overlay_edge_props, "edge_props")
    rv$windows        <- as_chr_df(spec$windows, "windows")
    rv$phases         <- as_chr_df(spec$phases, "phases")
    rv$legend_colours <- as_chr_df(spec$legend_colours, "legend_colours")
    rv$legend_shapes  <- as_chr_df(spec$legend_shapes, "legend_shapes")
    rv$link_props     <- as_chr_df(spec$link_props, "link_props")
    rv$legend_linetypes <- as_chr_df(spec$legend_linetypes, "legend_linetypes")
    rv$spans          <- as_chr_df(spec$spans, "spans")
    rv$adj   <- spec$adj
    rv$elab  <- spec$edge_labels
    rv$ov    <- spec$overlay
    rv$ovlab <- spec$overlay_edge_labels
    rv$settings <- spec$settings
    rv$prefix <- prefix
    updateTextInput(session, "prefix", value = prefix)
    keys <- c("nodes", "streams", "edge_props", "ov_props", "windows", "phases",
              "legend_colours", "legend_shapes", "link_props", "legend_linetypes", "spans",
              "adj", "elab", "ov", "ovlab")
    v <- isolate(rv$ver)
    for (k in keys) v[[k]] <- (v[[k]] %||% 0) + 1
    rv$ver <- v
    rv$settings_ver <- isolate(rv$settings_ver) + 1
  }
  load_spec(kingdon_example(), "kingdon")

  observeEvent(input$prefix, rv$prefix <- input$prefix)

  # modules
  tableServer("nodes", rv, "nodes", "node_props", "node_props")
  tableServer("streams", rv, "streams", "streams", "streams")
  tableServer("edge_props", rv, "edge_props", "edge_props", "edge_props")
  tableServer("ov_props", rv, "ov_props", "edge_props", "overlay_edge_props")
  tableServer("windows", rv, "windows", "windows", "windows")
  tableServer("phases", rv, "phases", "phases", "phases")
  tableServer("legend_colours", rv, "legend_colours", "legend_colours", "legend_colours")
  tableServer("legend_shapes", rv, "legend_shapes", "legend_shapes", "legend_shapes")
  tableServer("link_props", rv, "link_props", "link_props", "link_props")
  tableServer("legend_linetypes", rv, "legend_linetypes", "legend_linetypes", "legend_linetypes")
  tableServer("spans", rv, "spans", "spans", "spans")
  matrixServer("adj", rv, "adj", "adj")
  matrixServer("elab", rv, "elab", "edge_labels")
  matrixServer("ov", rv, "ov", "overlay")
  matrixServer("ovlab", rv, "ovlab", "overlay_edge_labels")

  # import / new
  observeEvent(input$load_example, load_spec(kingdon_example(), "kingdon"))
  observeEvent(input$new_blank, {
    nodes <- data.frame(id = c("a", "b"), time = c("2000", "2005"),
                        stream = c("stream1", "stream2"), stringsAsFactors = FALSE)
    adj <- matrix(c("", "", "1", ""), 2, 2, dimnames = list(nodes$id, nodes$id))
    load_spec(timeline_spec(adj, nodes, settings = list(title = "New timeline")), "timeline")
  })
  observeEvent(input$batch, {
    d <- tempfile("mtl_import_")  # unique per upload; a shared folder mixes diagrams
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    file.copy(input$batch$datapath, file.path(d, input$batch$name), overwrite = TRUE)
    pre <- trimws(input$batch_prefix %||% ""); if (!nzchar(pre)) pre <- NULL
    spec <- tryCatch(withCallingHandlers(read_timeline_csvs(d, prefix = pre),
                       warning = function(w) { showNotification(conditionMessage(w), type = "warning")
                                               invokeRestart("muffleWarning") }),
                     error = function(e) { showNotification(e$message, type = "error", duration = 10); NULL })
    req(spec)
    np <- input$batch$name[grepl("node_props\\.csv$", input$batch$name)][1]
    load_spec(spec, sub("_?node_props\\.csv$", "", np) %||% "timeline")
    showNotification(sprintf("Imported %d files", nrow(input$batch)))
  })

  # settings
  output$settings_ui <- renderUI({
    rv$settings_ver; s <- isolate(rv$settings)
    tagList(lapply(names(SETTING_GROUPS), function(g) tagList(
      tags$h6(style = "margin:12px 0 6px;font-weight:700;color:#2d3748", g),
      fluidRow(lapply(SETTING_GROUPS[[g]], function(k) column(6, setting_input(k, s[[k]])))))))
  })
  observe({
    keys <- names(timeline_defaults())
    vals <- lapply(keys, function(k) input[[paste0("set_", k)]])
    names(vals) <- keys
    vals <- vals[!vapply(vals, is.null, logical(1))]
    if (!length(vals)) return()
    rv$settings <- suppressWarnings(maketimeliner:::.merge_settings(
      utils::modifyList(isolate(rv$settings), vals)))
  })
  observeEvent(input$reset_settings, { rv$settings <- timeline_defaults(); rv$settings_ver <- rv$settings_ver + 1 })
  observeEvent(input$csv_settings, {
    df <- tryCatch(maketimeliner:::.read_csv(input$csv_settings$datapath), error = function(e) NULL)
    req(df)
    rv$settings <- maketimeliner:::.merge_settings(df)
    rv$settings_ver <- rv$settings_ver + 1
  })
  output$dl_settings <- downloadHandler(
    filename = function() paste0(rv$prefix, "_settings.csv"),
    content = function(f) utils::write.csv(maketimeliner:::.settings_to_df(rv$settings), f, row.names = FALSE))

  # build
  current_spec <- reactive({
    req(rv$nodes, rv$settings)
    nodes <- rv$nodes[nzchar(rv$nodes$id), , drop = FALSE]
    nodes$label <- gsub("\\n", "\n", nodes$label, fixed = TRUE)
    ids <- nodes$id
    keep_rows <- function(df, col) df[nzchar(df[[col]]), , drop = FALSE]
    timeline_spec(
      adj_matrix = mat_resize(rv$adj, ids), node_props = nodes,
      streams = keep_rows(rv$streams, "id"),
      edge_props = keep_rows(rv$edge_props, "weight"),
      edge_labels = if (is.null(rv$elab)) NULL else mat_resize(rv$elab, ids),
      adj_overlay = if (is.null(rv$ov)) NULL else mat_resize(rv$ov, ids),
      overlay_edge_props = keep_rows(rv$ov_props, "weight"),
      overlay_edge_labels = if (is.null(rv$ovlab)) NULL else mat_resize(rv$ovlab, ids),
      windows = keep_rows(rv$windows, "start"),
      phases = keep_rows(rv$phases, "phase"),
      legend_node_colour = keep_rows(rv$legend_colours, "colour"),
      legend_node_shape = keep_rows(rv$legend_shapes, "shape"),
      link_props = keep_rows(rv$link_props, "from"),
      legend_linetypes = keep_rows(rv$legend_linetypes, "linetype"),
      spans = keep_rows(rv$spans, "id"),
      settings = rv$settings)
  })
  built <- debounce(reactive({
    warns <- character(0)
    res <- tryCatch(withCallingHandlers({
      sp <- current_spec()
      render_timeline(sp, html_file = NULL, svg_file = NULL)
    }, warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) list(error = conditionMessage(e)))
    res$warnings <- unique(warns)
    res
  }), 500)

  output$msgs <- renderUI({
    b <- built()
    tagList(if (!is.null(b$error)) div(class = "err", paste("Error:", b$error)),
            if (length(b$warnings)) div(class = "warn", paste("Warning:", b$warnings, collapse = "\n")))
  })
  output$preview <- renderUI({
    b <- built(); req(is.null(b$error))
    tags$iframe(srcdoc = b$html, sandbox = "allow-scripts allow-downloads")
  })

  output$dl_html <- downloadHandler(
    filename = function() paste0(rv$prefix, ".html"),
    content = function(f) { b <- built(); validate(need(is.null(b$error), b$error)); writeLines(b$html, f, useBytes = TRUE) })
  output$dl_svg <- downloadHandler(
    filename = function() paste0(rv$prefix, ".svg"),
    content = function(f) { b <- built(); validate(need(is.null(b$error), b$error)); writeLines(b$svg, f, useBytes = TRUE) })
  output$dl_zip <- downloadHandler(
    filename = function() paste0(rv$prefix, "_csvs.zip"),
    content = function(f) {
      d <- tempfile("mtl_export_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
      paths <- write_timeline_csvs(current_spec(), d, rv$prefix)
      old <- setwd(d); on.exit(setwd(old), add = TRUE, after = FALSE)
      utils::zip(f, basename(paths), flags = "-q")
    })

  output$rcode <- renderText({
    sp <- tryCatch(current_spec(), error = function(e) NULL); req(sp)
    dp <- function(x) paste(deparse(x, width.cutoff = 100), collapse = "\n")
    nonempty <- function(df) !is.null(df) && nrow(df) > 0
    paste0(
      "library(maketimeliner)\n\n",
      "# Option A — from the exported CSV set (Import tab → All CSVs):\n",
      "spec <- read_timeline_csvs(\"path/to/", rv$prefix, "_csvs\")\n",
      "render_timeline(spec, \"", rv$prefix, ".html\", \"", rv$prefix, ".svg\")\n\n",
      "# Option B — fully self-contained:\n",
      "nodes <- ", dp(sp$nodes[, colSums(sp$nodes != "" & !is.na(sp$nodes)) > 0, drop = FALSE]), "\n",
      "adj <- ", dp(sp$adj), "\n",
      "streams <- ", dp(sp$streams), "\n",
      if (nonempty(sp$edge_props)) paste0("edge_props <- ", dp(sp$edge_props), "\n") else "edge_props <- NULL\n",
      if (nonempty(sp$windows)) paste0("windows <- ", dp(sp$windows), "\n") else "windows <- NULL\n",
      "settings <- ", dp(sp$settings[!mapply(identical, sp$settings, timeline_defaults()[names(sp$settings)])]), "\n",
      "timeline_to_outputs(adj, nodes, streams, edge_props, windows = windows,\n",
      "                    settings = settings, html_file = \"", rv$prefix, ".html\",\n",
      "                    svg_file = \"", rv$prefix, ".svg\")\n",
      "# (overlay, phases and legend tables: see Option A)\n")
  })
}

shinyApp(ui, server)
