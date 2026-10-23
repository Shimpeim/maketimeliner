#' Render a timeline specification to SVG and interactive HTML
#'
#' @param spec A `timeline_spec` (from [timeline_spec()] or
#'   [read_timeline_csvs()]).
#' @param html_file Path for the self-contained interactive HTML file, or
#'   `NULL` to skip writing.
#' @param svg_file Path for the static SVG file, or `NULL` to skip writing.
#' @param ... Settings overriding `spec$settings` (see [timeline_defaults()]),
#'   e.g. `title = "My case"`, `curve = "step"`.
#'
#' @return Invisibly, a list with `svg` (string), `html` (string), `layout`
#'   (node positions, link table, stream bands, time scale) and `spec`.
#' @export
#' @examples
#' spec <- kingdon_example()
#' out  <- render_timeline(spec, html_file = NULL, svg_file = NULL)
#' substr(out$svg, 1, 80)
render_timeline <- function(spec, html_file = "timeline.html",
                            svg_file = "timeline.svg", ...) {
  if (!inherits(spec, "timeline_spec"))
    stop("spec must be a timeline_spec; build one with timeline_spec() or read_timeline_csvs().",
         call. = FALSE)
  over <- list(...)
  if (length(over)) spec$settings <- .merge_settings(utils::modifyList(spec$settings, over))
  L    <- .layout(spec)
  svg  <- .svg_build(L, spec)
  html <- .html_build(L, spec, svg)
  if (!is.null(svg_file))  writeLines(svg,  svg_file,  useBytes = TRUE)
  if (!is.null(html_file)) writeLines(html, html_file, useBytes = TRUE)
  invisible(list(svg = svg, html = html, spec = spec,
                 layout = list(nodes = L$nodes[, c("id", "label", "time", "stream", "x", "y")],
                               edges = L$edges[, c("from", "to", "code", "label", "colour", "path")],
                               streams = L$streams[, c("id", "label", "y0", "y1")],
                               time_scale = L$ts[c("kind", "lo", "hi", "ticks", "labels")],
                               width = L$width)))
}

#' Draw a multi-stream timeline (matxingraphout-style call)
#'
#' One-call interface mirroring `matxingraphout::graph_to_outputs()`: pass the
#' adjacency matrix and node table (plus optional tables) and get SVG + HTML.
#'
#' @inheritParams timeline_spec
#' @inheritParams render_timeline
#' @param ... Settings, see [timeline_defaults()].
#' @return As [render_timeline()].
#' @export
#' @examples
#' nodes <- data.frame(id = c("a", "b", "c"), time = c(2001, 2004, 2008),
#'                     stream = c("problem", "politics", "policy"))
#' adj <- matrix(0, 3, 3, dimnames = list(nodes$id, nodes$id))
#' adj["a", "b"] <- 1; adj["b", "c"] <- 1
#' out <- timeline_to_outputs(adj, nodes, html_file = NULL, svg_file = NULL,
#'                            title = "Minimal")
timeline_to_outputs <- function(adj_matrix, node_props, streams = NULL,
                                edge_props = NULL, edge_labels = NULL,
                                adj_overlay = NULL, overlay_edge_props = NULL,
                                overlay_edge_labels = NULL, windows = NULL,
                                phases = NULL, legend_node_colour = NULL,
                                legend_node_shape = NULL, settings = list(),
                                html_file = "timeline.html",
                                svg_file = "timeline.svg", ...) {
  spec <- timeline_spec(adj_matrix, node_props, streams, edge_props, edge_labels,
                        adj_overlay, overlay_edge_props, overlay_edge_labels,
                        windows, phases, legend_node_colour, legend_node_shape,
                        settings = utils::modifyList(as.list(settings), list(...)))
  render_timeline(spec, html_file = html_file, svg_file = svg_file)
}

# ── HTML ─────────────────────────────────────────────────────────────────────

.html_build <- function(L, spec, svg) {
  tmpl_path <- system.file("template", "timeline_template.html", package = "maketimeliner")
  if (!nzchar(tmpl_path)) stop("timeline_template.html not found; reinstall maketimeliner.",
                               call. = FALSE)
  tmpl <- paste(readLines(tmpl_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  data_json <- jsonlite::toJSON(.html_data(L), auto_unbox = TRUE, null = "null",
                                na = "null", digits = NA)
  data_json <- gsub("</", "<\\/", data_json, fixed = TRUE)
  ver <- tryCatch(as.character(utils::packageVersion("maketimeliner")),
                  error = function(e) "")
  tmpl <- .fill(tmpl, "__TITLE__", .esc(L$S$title))
  tmpl <- .fill(tmpl, "__GENERATED__", format(Sys.time(), "%Y-%m-%d %H:%M"))
  tmpl <- .fill(tmpl, "__VERSION__", ver)
  tmpl <- .fill(tmpl, "__DATA__", data_json)
  .fill(tmpl, "__SVG__", svg)
}

.html_data <- function(L) {
  nd <- L$nodes; e <- L$edges; cs <- L$codes; ts <- L$ts
  codes_of <- lapply(nd$id, function(i) unique(e$code[e$from == i | e$to == i]))
  tl <- lapply(seq_len(nrow(cs)), function(k) {
    c <- cs$code[k]
    ids <- unique(c(e$from[e$code == c], e$to[e$code == c]))
    ids <- ids[order(nd$t[match(ids, nd$id)], match(ids, nd$id))]
    list(code = c, label = cs$label[k], colour = cs$colour[k],
         description = cs$description[k], nodes = I(ids))
  })
  tl <- Filter(function(t) length(t$nodes) > 0, tl)
  w  <- L$windows
  list(
    title = L$S$title,
    timelines = tl,
    nodes = lapply(seq_len(nrow(nd)), function(i)
      list(id = nd$id[i], label = gsub("\n", " ", nd$label[i]), t = nd$t[i],
           time_label = as.character(ts$fmt(nd$time[i])), stream = nd$stream[i],
           description = nd$description[i], codes = I(codes_of[[i]]))),
    edges = lapply(seq_len(nrow(e)), function(i)
      list(code = e$code[i], from = e$from[i], to = e$to[i], text = e$text[i])),
    streams = lapply(seq_len(nrow(L$streams)), function(i)
      list(id = L$streams$id[i], label = gsub("\n", " ", L$streams$label[i]),
           colour = L$streams$colour[i], description = L$streams$description[i])),
    windows = lapply(seq_len(nrow(w)), function(i) {
      s <- as.character(w$start[i]); en <- .chr(w$end[i], s)
      t0 <- ts$f(s); t1 <- ts$f(en)
      list(label = w$label[i], description = w$description[i],
           period = if (identical(s, en)) as.character(ts$fmt(s))
                    else paste(ts$fmt(s), "\u2013", ts$fmt(en)),
           t0 = min(t0, t1), t1 = max(t0, t1))
    }),
    overlays = lapply(seq_len(nrow(L$overlay)), function(i)
      list(code = L$overlay$code[i], label = L$overlay$label[i],
           from = L$overlay$from[i], to = L$overlay$to[i], text = L$overlay$text[i]))
  )
}
