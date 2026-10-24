#' Default rendering settings
#'
#' Returns the full list of settings understood by [render_timeline()]. Every
#' entry can be overridden from R (`settings = list(...)` or `...`), from the
#' `*_settings.csv` file (two columns: `Setting`, `Value`), or from the
#' Settings panel of [run_app()].
#'
#' @return A named list. Numeric and logical entries are coerced to the same
#'   type when supplied as strings (e.g. from CSV).
#' @export
#' @examples
#' str(timeline_defaults())
timeline_defaults <- function() {
  list(
    title              = "Timeline",
    # time axis
    time_scale         = "auto",      # auto | numeric | date | ordinal
    date_format        = "auto",      # "auto" or a strftime format, e.g. "%Y-%m"
    axis_label         = "",
    show_axis          = TRUE,
    show_grid          = TRUE,
    plot_width         = 900,
    # streams
    stream_height      = 140,
    stream_gap         = 6,
    stream_label_width = 130,
    stream_fontsize    = 13,
    stream_colours     = "#EEF3FB,#FBF3E8,#EEF8EE,#F6EEF8,#F2F2F2",
    stream_alpha       = 1,
    # nodes (events)
    node_shape         = "circle",    # circle | ellipse | rect | rounded | diamond | triangle | none
    node_colour        = "#FFFFFF",
    node_alpha         = 1,
    node_stroke        = "#333333",
    node_stroke_width  = 1.5,
    node_linetype      = "solid",
    node_width         = 14,
    node_height        = 14,
    fontsize           = 11,
    fontcolour         = "#222222",
    show_node_labels   = TRUE,
    label_position     = "above",     # above | below | center | left | right
    min_node_sep       = 60,
    # timelines (structural edges)
    edge_palette       = paste(.okabe_ito, collapse = ","),
    edge_width         = 3,
    edge_alpha         = 0.9,
    edge_linetype      = "solid",
    curve              = "sigmoid",   # sigmoid | straight | step
    curve_tension      = 0.5,
    bundle_gap         = 4,
    timeline_labels    = "end",       # none | start | end | both
    edge_label_fontsize = 10,
    # overlay edges (couplings / annotations)
    overlay_edge_colour   = "#777777",
    overlay_edge_width    = 1.2,
    overlay_edge_alpha    = 0.9,
    overlay_edge_linetype = "dashed",
    overlay_curvature     = 0.25,
    # windows
    window_colour      = "#F4C430",
    window_alpha       = 0.18,
    window_stroke      = "#C89B00",
    window_stroke_width = 1,
    window_linetype    = "dashed",
    # page
    padding            = 30,
    background         = "#FFFFFF",
    font_family        = "system-ui, -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif",
    tooltip_image_width = 240,        # px; width of node pictures on mouse-over (scales with tooltip text)
    tooltip_fontsize   = 14,          # px; start size of mouse-over text (adjustable in the HTML)
    details_fontsize   = 15,          # px; start size of the details pane (adjustable in the HTML)
    embed_images       = TRUE,        # download pictures at render time into the HTML
    image_max_mb       = 5,           # skip embedding pictures larger than this
    show_legend        = TRUE,
    legend_title       = "Legend"
  )
}

# Allowed values for enumerated settings (used for validation and the app).
.setting_choices <- list(
  time_scale      = c("auto", "numeric", "date", "ordinal"),
  node_shape      = c("circle", "ellipse", "rect", "rounded", "diamond", "triangle", "none"),
  label_position  = c("above", "below", "center", "left", "right"),
  curve           = c("sigmoid", "straight", "step"),
  timeline_labels = c("none", "start", "end", "both"),
  edge_linetype   = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
  node_linetype   = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
  overlay_edge_linetype = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
  window_linetype = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
)

# Merge user settings over defaults, coercing types to match the defaults.
.merge_settings <- function(settings = list()) {
  d <- timeline_defaults()
  if (is.data.frame(settings)) settings <- .settings_from_df(settings)
  settings <- as.list(settings)
  unknown <- setdiff(names(settings), names(d))
  if (length(unknown))
    warning("Ignoring unknown setting(s): ", paste(unknown, collapse = ", "),
            call. = FALSE)
  for (k in intersect(names(settings), names(d))) {
    v <- settings[[k]]
    if (is.null(v) || length(v) == 0L || (length(v) == 1L && is.na(v))) next
    if (is.logical(d[[k]])) {
      v <- toupper(trimws(as.character(v)))
      v <- v %in% c("TRUE", "T", "1", "YES", "Y")
    } else if (is.numeric(d[[k]])) {
      v <- suppressWarnings(as.numeric(v))
      if (is.na(v)) next
    } else {
      v <- as.character(v)
      if (k %in% names(.setting_choices) && !v %in% .setting_choices[[k]]) {
        warning("Setting '", k, "' = '", v, "' is not one of: ",
                paste(.setting_choices[[k]], collapse = ", "),
                "; using default.", call. = FALSE)
        next
      }
    }
    d[[k]] <- v
  }
  d
}

.settings_from_df <- function(df) {
  df <- .norm_names(df)
  if (!all(c("setting", "value") %in% names(df)))
    stop("Settings table needs columns 'Setting' and 'Value'.", call. = FALSE)
  s_list <- as.list(as.character(df$value))
  names(s_list) <- trimws(as.character(df$setting))
  s_list
}

.settings_to_df <- function(s) {
  data.frame(Setting = names(s),
             Value   = vapply(s, function(v) as.character(v)[1], character(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}
