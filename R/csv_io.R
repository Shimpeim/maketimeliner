# CSV import / export, matxingraphout-style: one file per table, recognised
# by suffix, e.g. "case1_node_props.csv", "case1_adj.csv".

# Longest suffixes first so "_overlay_edge_props" wins over "_edge_props".
.csv_suffixes <- c(
  overlay_edge_labels = "overlay_edge_labels",
  overlay_edge_props  = "overlay_edge_props",
  legend_colours      = "legend_colou?rs",
  legend_shapes       = "legend_shapes",
  edge_labels         = "edge_labels",
  edge_props          = "edge_props",
  node_props          = "node_props",
  settings            = "settings",
  overlay             = "overlay",
  windows             = "windows",
  streams             = "streams",
  phases              = "phases",
  adj                 = "adj"
)

.csv_kind <- function(fname) {
  b <- tolower(basename(fname))
  for (k in names(.csv_suffixes)) {
    m <- regexpr(paste0("(^|_)", .csv_suffixes[[k]], "\\.csv$"), b)
    if (m > 0) {
      prefix <- substr(basename(fname), 1L, m - 1L)
      return(c(kind = k, prefix = prefix))
    }
  }
  c(kind = NA_character_, prefix = NA_character_)
}

.read_csv <- function(path) {
  utils::read.csv(path, header = TRUE, check.names = FALSE,
                  stringsAsFactors = FALSE, na.strings = c("", "NA"),
                  fileEncoding = "UTF-8-BOM", colClasses = "character")
}

.read_matrix_csv <- function(path) {
  df <- .read_csv(path)
  m  <- as.matrix(df[, -1, drop = FALSE])
  rownames(m) <- trimws(as.character(df[[1]]))
  colnames(m) <- trimws(colnames(df)[-1])
  m[is.na(m)] <- ""
  m
}

#' Read a set of timeline CSV files
#'
#' Files are recognised by suffix (with an optional shared prefix), following
#' matxingraphout conventions:
#'
#' | Suffix | Content |
#' |---|---|
#' | `_node_props.csv` | events: `id, label, time, stream, slot, shape, colour, alpha, stroke, stroke_width, linetype, fontcolour, fontsize, width, height, label_position, description` |
#' | `_adj.csv` | adjacency matrix; first column = from-ids, headers = to-ids, cells = timeline code(s), `;`-separated |
#' | `_edge_props.csv` | timelines: `weight, label, colour, alpha, width, linetype, description` |
#' | `_edge_labels.csv` | matrix of link texts |
#' | `_overlay.csv`, `_overlay_edge_props.csv`, `_overlay_edge_labels.csv` | annotation links |
#' | `_streams.csv` | `id, label, order, colour, alpha, stroke, stroke_width, linetype, height, fontcolour, description` |
#' | `_windows.csv` | `start, end, label, colour, alpha, stroke, stroke_width, linetype, streams, description` |
#' | `_phases.csv` | `phase, order, label` (ordinal time axis) |
#' | `_legend_colours.csv`, `_legend_shapes.csv` | legend tables |
#' | `_settings.csv` | `Setting, Value` |
#'
#' Only `_node_props.csv` is required.
#'
#' @param path A directory, or a character vector of CSV file paths.
#' @param prefix Optional file-name prefix to select when a directory holds
#'   several diagrams (e.g. `"case1"` for `case1_node_props.csv`).
#' @return A `timeline_spec`.
#' @export
#' @examples
#' dir <- system.file("extdata", "kingdon", package = "maketimeliner")
#' spec <- read_timeline_csvs(dir)
#' spec
read_timeline_csvs <- function(path, prefix = NULL) {
  files <- if (length(path) == 1L && dir.exists(path))
    list.files(path, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  else path
  if (!length(files)) stop("No CSV files found.", call. = FALSE)
  info <- t(vapply(files, .csv_kind, character(2)))
  keep <- !is.na(info[, "kind"])
  pre  <- sub("_$", "", info[, "prefix"])
  if (!is.null(prefix)) keep <- keep & pre == sub("_$", "", prefix)
  files <- files[keep]; info <- info[keep, , drop = FALSE]; pre <- pre[keep]
  if (!any(info[, "kind"] == "node_props"))
    stop("No *_node_props.csv found", if (!is.null(prefix)) paste0(" for prefix '", prefix, "'"),
         ".", call. = FALSE)
  if (length(unique(pre[info[, "kind"] == "node_props"])) > 1L)
    stop("Several diagrams found (prefixes: ",
         paste(unique(pre[info[, "kind"] == "node_props"]), collapse = ", "),
         "); choose one with `prefix =`.", call. = FALSE)
  get <- function(kind) {
    f <- files[info[, "kind"] == kind]
    if (!length(f)) return(NULL)
    if (length(f) > 1L) stop("Several files for '", kind, "': ",
                             paste(basename(f), collapse = ", "), call. = FALSE)
    f
  }
  rd <- function(kind) { f <- get(kind); if (is.null(f)) NULL else .read_csv(f) }
  rm <- function(kind) { f <- get(kind); if (is.null(f)) NULL else .read_matrix_csv(f) }
  timeline_spec(
    adj_matrix          = rm("adj"),
    node_props          = rd("node_props"),
    streams             = rd("streams"),
    edge_props          = rd("edge_props"),
    edge_labels         = rm("edge_labels"),
    adj_overlay         = rm("overlay"),
    overlay_edge_props  = rd("overlay_edge_props"),
    overlay_edge_labels = rm("overlay_edge_labels"),
    windows             = rd("windows"),
    phases              = rd("phases"),
    legend_node_colour  = rd("legend_colours"),
    legend_node_shape   = rd("legend_shapes"),
    settings            = { s <- rd("settings"); if (is.null(s)) list() else s }
  )
}

#' Write a timeline specification to CSV files
#'
#' Inverse of [read_timeline_csvs()]: writes one CSV per non-empty table as
#' `<prefix>_<table>.csv` into `dir`.
#'
#' @param spec A `timeline_spec`.
#' @param dir Output directory (created if needed).
#' @param prefix File-name prefix.
#' @return Invisibly, the paths written.
#' @export
write_timeline_csvs <- function(spec, dir, prefix = "timeline") {
  stopifnot(inherits(spec, "timeline_spec"))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  fp <- function(k) file.path(dir, paste0(prefix, "_", k, ".csv"))
  wdf <- function(df, k) {
    if (is.null(df) || !nrow(df)) return(NULL)
    df <- df[, colSums(!is.na(df) & df != "", na.rm = TRUE) > 0 | names(df) %in% c("id", "time", "stream", "weight", "start", "phase", "colour", "shape", "label"),
             drop = FALSE]
    utils::write.csv(df, fp(k), row.names = FALSE, na = "", fileEncoding = "UTF-8")
    fp(k)
  }
  wm <- function(m, k) {
    if (is.null(m) || !any(nzchar(m))) return(NULL)
    df <- data.frame(id = rownames(m), m, check.names = FALSE, stringsAsFactors = FALSE)
    names(df)[1] <- ""
    df[df == ""] <- "0"
    if (k %in% c("edge_labels", "overlay_edge_labels")) df[-1][df[-1] == "0"] <- ""
    utils::write.csv(df, fp(k), row.names = FALSE, na = "", fileEncoding = "UTF-8")
    fp(k)
  }
  out <- c(
    wdf(spec$nodes, "node_props"), wm(spec$adj, "adj"),
    wdf(spec$streams, "streams"), wdf(spec$edge_props, "edge_props"),
    wm(spec$edge_labels, "edge_labels"), wm(spec$overlay, "overlay"),
    wdf(spec$overlay_edge_props, "overlay_edge_props"),
    wm(spec$overlay_edge_labels, "overlay_edge_labels"),
    wdf(spec$windows, "windows"), wdf(spec$phases, "phases"),
    wdf(spec$legend_colours, "legend_colours"), wdf(spec$legend_shapes, "legend_shapes")
  )
  utils::write.csv(.settings_to_df(spec$settings), fp("settings"), row.names = FALSE,
                   fileEncoding = "UTF-8")
  invisible(c(out, fp("settings")))
}
