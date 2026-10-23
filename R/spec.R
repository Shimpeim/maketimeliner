# Canonical columns for each table (shared by CSV I/O and the Shiny app).
.table_cols <- list(
  node_props = c("id", "label", "time", "stream", "slot", "shape", "colour",
                 "alpha", "stroke", "stroke_width", "linetype", "fontcolour",
                 "fontsize", "width", "height", "label_position", "image",
                 "description"),
  streams    = c("id", "label", "order", "colour", "alpha", "stroke",
                 "stroke_width", "linetype", "height", "fontcolour", "description"),
  edge_props = c("weight", "label", "colour", "alpha", "width", "linetype",
                 "description"),
  windows    = c("start", "end", "label", "colour", "alpha", "stroke",
                 "stroke_width", "linetype", "streams", "description"),
  phases     = c("phase", "order", "label"),
  legend_colours = c("colour", "label"),
  legend_shapes  = c("shape", "label")
)

# Pad a data.frame to the canonical columns (keeps extra columns at the end).
.conform <- function(df, table) {
  cols <- .table_cols[[table]]
  if (is.null(df)) {
    df <- as.data.frame(.empty_cols(cols), stringsAsFactors = FALSE)
    return(df)
  }
  df <- .norm_names(df)
  for (cc in setdiff(cols, names(df))) df[[cc]] <- rep(NA, nrow(df))
  df[, c(cols, setdiff(names(df), cols)), drop = FALSE]
}

.empty_cols <- function(cols) {
  cols_l <- lapply(cols, function(i) character(0))
  names(cols_l) <- cols
  cols_l
}

# Coerce an adjacency input (matrix or data.frame with ids in column 1)
# to a square character matrix with dimnames; "0", NA and "" mean no edge.
.as_code_matrix <- function(m, ids, what = "adj_matrix") {
  n <- length(ids)
  out <- matrix("", n, n, dimnames = list(ids, ids))
  if (is.null(m)) return(out)
  if (is.data.frame(m)) {
    rn <- as.character(m[[1]])
    m  <- as.matrix(m[, -1, drop = FALSE])
    rownames(m) <- rn
  }
  m <- as.matrix(m)
  if (is.null(rownames(m)) || is.null(colnames(m))) {
    if (nrow(m) == n && ncol(m) == n) {
      dimnames(m) <- list(ids, ids)
    } else stop(what, " has no dimnames and its size does not match node_props.",
                call. = FALSE)
  }
  storage.mode(m) <- "character"
  m[is.na(m)] <- ""
  m <- trimws(m)
  m[m %in% c("0", "0.0", "FALSE")] <- ""
  miss <- setdiff(union(rownames(m), colnames(m)), ids)
  if (length(miss) && any(nzchar(m[rownames(m) %in% miss, ]) ,
                          nzchar(m[, colnames(m) %in% miss])))
    warning(what, ": ignoring edges of id(s) not in node_props: ",
            paste(miss, collapse = ", "), call. = FALSE)
  r <- intersect(rownames(m), ids); cc <- intersect(colnames(m), ids)
  out[r, cc] <- m[r, cc, drop = FALSE]
  out
}

#' Build a timeline specification
#'
#' Collects and normalises every input table into one object that
#' [render_timeline()] draws and [write_timeline_csvs()] saves. The data model
#' follows **matxingraphout**: events are nodes, and the adjacency matrix links
#' them; each non-zero cell holds the *code* (weight) of the timeline that runs
#' along that link. Several timelines may share a link: write the codes
#' separated by `;` (e.g. `"1;3"`).
#'
#' @param adj_matrix Square matrix (numeric or character) or a data.frame whose
#'   first column holds row ids, with dimnames = node ids. Cell `[i, j]` = code(s)
#'   of the timeline(s) running from event `i` to event `j`.
#' @param node_props data.frame of events. Required: `id`, `time`, `stream`.
#'   Optional: `label`, `slot` (0-1 vertical position inside the stream; blank =
#'   automatic), `shape`, `colour`, `alpha`, `stroke`, `stroke_width`,
#'   `linetype`, `fontcolour`, `fontsize`, `width`, `height`, `label_position`,
#'   `image` (picture URL shown on mouse-over in the HTML), `description`
#'   (shown in the HTML info panel). `color`/`fontcolor` accepted.
#' @param streams data.frame of streams (lanes): `id`, `label`, `order`,
#'   `colour`, `alpha`, `stroke`, `stroke_width`, `linetype`, `height`,
#'   `fontcolour`, `description`. Any number of streams. `NULL` = derived from
#'   `node_props$stream`.
#' @param edge_props data.frame, one row per timeline code: `weight` (the code),
#'   `label`, `colour`, `alpha`, `width`, `linetype`, `description`.
#' @param edge_labels Optional character matrix (same dims as `adj_matrix`) of
#'   text drawn at the midpoint of each link.
#' @param adj_overlay,overlay_edge_props,overlay_edge_labels Annotation links
#'   (e.g. coupling by a policy entrepreneur) drawn as arcs on top; same
#'   structure as the structural versions. Not treated as timelines.
#' @param windows data.frame of time windows (e.g. policy windows): `start`,
#'   `end`, `label`, `colour`, `alpha`, `stroke`, `stroke_width`, `linetype`,
#'   `streams` (`;`-separated stream ids; blank = all), `description`.
#' @param phases Optional data.frame (`phase`, `order`, `label`) fixing the order
#'   of an ordinal time axis.
#' @param legend_node_colour data.frame(`colour`, `label`) for the legend.
#' @param legend_node_shape data.frame(`shape`, `label`) for the legend.
#' @param settings Named list or `Setting`/`Value` data.frame; see
#'   [timeline_defaults()].
#'
#' @return An object of class `timeline_spec` (a named list).
#' @export
timeline_spec <- function(adj_matrix = NULL, node_props, streams = NULL,
                          edge_props = NULL, edge_labels = NULL,
                          adj_overlay = NULL, overlay_edge_props = NULL,
                          overlay_edge_labels = NULL, windows = NULL,
                          phases = NULL, legend_node_colour = NULL,
                          legend_node_shape = NULL, settings = list()) {
  if (missing(node_props) || is.null(node_props) || !NROW(node_props))
    stop("node_props must be a data.frame with at least one row.", call. = FALSE)
  nodes <- .conform(node_props, "node_props")
  for (req in c("id", "time", "stream"))
    if (all(is.na(nodes[[req]]) | !nzchar(trimws(as.character(nodes[[req]])))))
      stop("node_props needs a non-empty '", req, "' column.", call. = FALSE)
  nodes$id     <- trimws(as.character(nodes$id))
  nodes$stream <- trimws(as.character(nodes$stream))
  nodes$time   <- trimws(as.character(nodes$time))
  if (anyDuplicated(nodes$id))
    stop("Duplicate node id(s): ",
         paste(unique(nodes$id[duplicated(nodes$id)]), collapse = ", "), call. = FALSE)
  if (any(is.na(nodes$time) | !nzchar(nodes$time)))
    stop("Every node needs a 'time' value (missing for: ",
         paste(nodes$id[is.na(nodes$time) | !nzchar(nodes$time)], collapse = ", "),
         ").", call. = FALSE)
  nodes$label <- ifelse(is.na(nodes$label) | !nzchar(as.character(nodes$label)),
                        nodes$id, as.character(nodes$label))
  nodes$label <- gsub("\\n", "\n", nodes$label, fixed = TRUE)

  # streams: derive / complete
  st <- if (is.null(streams)) NULL else .conform(streams, "streams")
  used <- unique(nodes$stream)
  if (is.null(st) || !nrow(st)) {
    st <- .conform(data.frame(id = used, stringsAsFactors = FALSE), "streams")
  } else {
    st$id <- trimws(as.character(st$id))
    extra <- setdiff(used, st$id)
    if (length(extra)) {
      warning("Stream(s) used by nodes but missing from streams table were added: ",
              paste(extra, collapse = ", "), call. = FALSE)
      add <- .conform(data.frame(id = extra, stringsAsFactors = FALSE), "streams")
      st  <- rbind(st[, names(add)], add)
    }
  }
  st$label <- ifelse(is.na(st$label) | !nzchar(as.character(st$label)),
                     st$id, as.character(st$label))
  st$label <- gsub("\\n", "\n", st$label, fixed = TRUE)
  st$order <- .num(st$order, NA)
  st$order[is.na(st$order)] <- seq_len(nrow(st))[is.na(st$order)]

  ids <- nodes$id
  structure(list(
    adj                 = .as_code_matrix(adj_matrix, ids, "adj_matrix"),
    nodes               = nodes,
    streams             = st,
    edge_props          = .conform(edge_props, "edge_props"),
    edge_labels         = if (is.null(edge_labels)) NULL else
                            .as_code_matrix(edge_labels, ids, "edge_labels"),
    overlay             = if (is.null(adj_overlay)) NULL else
                            .as_code_matrix(adj_overlay, ids, "adj_overlay"),
    overlay_edge_props  = .conform(overlay_edge_props, "edge_props"),
    overlay_edge_labels = if (is.null(overlay_edge_labels)) NULL else
                            .as_code_matrix(overlay_edge_labels, ids, "overlay_edge_labels"),
    windows             = .conform(windows, "windows"),
    phases              = .conform(phases, "phases"),
    legend_colours      = .conform(legend_node_colour, "legend_colours"),
    legend_shapes       = .conform(legend_node_shape, "legend_shapes"),
    settings            = .merge_settings(settings)
  ), class = "timeline_spec")
}

#' @export
print.timeline_spec <- function(x, ...) {
  e <- .edges_from_matrix(x$adj)
  cat("<timeline_spec> \"", x$settings$title, "\"\n", sep = "")
  cat("  events   :", nrow(x$nodes), "\n")
  cat("  streams  :", paste(x$streams$id[order(x$streams$order)], collapse = " | "), "\n")
  cat("  timelines:", length(unique(e$code)), " (links:", nrow(e), ")\n")
  cat("  windows  :", nrow(x$windows), "\n")
  invisible(x)
}

# Long edge list (from, to, code) from a code matrix.
.edges_from_matrix <- function(m) {
  empty <- data.frame(from = character(0), to = character(0),
                      code = character(0), stringsAsFactors = FALSE)
  if (is.null(m) || !length(m)) return(empty)
  idx <- which(m != "", arr.ind = TRUE)
  if (!nrow(idx)) return(empty)
  idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  rows <- lapply(seq_len(nrow(idx)), function(k) {
    i <- idx[k, 1]; j <- idx[k, 2]
    codes <- trimws(strsplit(m[i, j], "[;|]")[[1]])
    codes <- codes[nzchar(codes) & codes != "0"]
    if (!length(codes)) return(NULL)
    data.frame(from = rownames(m)[i], to = colnames(m)[j], code = codes,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) empty else out
}
