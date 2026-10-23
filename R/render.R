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
                                legend_node_shape = NULL, link_props = NULL,
                                legend_linetypes = NULL, settings = list(),
                                html_file = "timeline.html",
                                svg_file = "timeline.svg", ...) {
  spec <- timeline_spec(adj_matrix, node_props, streams, edge_props, edge_labels,
                        adj_overlay, overlay_edge_props, overlay_edge_labels,
                        windows, phases, legend_node_colour, legend_node_shape,
                        link_props = link_props, legend_linetypes = legend_linetypes,
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
  img <- .safe_url(nd$image)
  if (isTRUE(L$S$embed_images)) img <- .embed_images(img, L$S$image_max_mb)
  w  <- L$windows
  list(
    title = L$S$title,
    image_width = L$S$tooltip_image_width,
    timelines = tl,
    nodes = lapply(seq_len(nrow(nd)), function(i)
      list(id = nd$id[i], label = gsub("\n", " ", nd$label[i]), t = nd$t[i],
           time_label = as.character(ts$fmt(nd$time[i])), stream = nd$stream[i],
           description = nd$description[i], image = img[i],
           codes = I(codes_of[[i]]))),
    edges = lapply(seq_len(nrow(e)), function(i)
      list(code = e$code[i], from = e$from[i], to = e$to[i], text = e$text[i],
           link_label = e$link_label[i], link_description = e$link_description[i])),
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

# Picture URLs for <img src>: http(s), protocol-relative, data:image/ and
# relative / absolute file paths are kept; anything else (e.g. javascript:)
# is dropped with a warning.
.safe_url <- function(x) {
  x <- trimws(.chr(x, ""))
  ok <- !nzchar(x) | grepl("^(https?:)?//", x, ignore.case = TRUE) |
    grepl("^data:image/", x, ignore.case = TRUE) |
    !grepl("^[A-Za-z][A-Za-z0-9+.-]*:", x) |
    grepl("^[A-Za-z]:[\\\\/]", x) | grepl("^file:", x, ignore.case = TRUE)
  if (any(!ok))
    warning("Ignoring unsupported image URL(s): ", paste(x[!ok], collapse = ", "),
            call. = FALSE)
  x[!ok] <- ""
  x
}

# Download (or read) pictures and inline them as data: URIs so the HTML does
# not depend on the viewer reaching the image host (hotlink rules, proxies,
# IDE viewers, offline use). Results (including failures) are cached per
# session; on failure the original URL is kept and a warning names the reason.
# maketimeliner:::.clear_image_cache() forces a re-download.
.img_cache <- new.env(parent = emptyenv())

.embed_images <- function(urls, max_mb = 5) {
  vapply(urls, function(u) {
    if (!nzchar(u) || grepl("^data:", u, ignore.case = TRUE)) return(u)
    key <- paste(u, max_mb)
    hit <- .img_cache[[key]]
    if (is.null(hit)) {
      hit <- tryCatch(list(ok = TRUE, value = .fetch_image(u, max_mb)),
                      error = function(e) list(ok = FALSE, value = conditionMessage(e)))
      assign(key, hit, envir = .img_cache)
    }
    if (hit$ok) return(hit$value)
    warning("Could not embed image ", u, ": ", hit$value,
            " (the HTML will try to load the URL directly).", call. = FALSE)
    u
  }, character(1), USE.NAMES = FALSE)
}

.fetch_image <- function(u, max_mb) {
  remote <- grepl("^(https?:)?//", u, ignore.case = TRUE)
  if (remote) {
    if (startsWith(u, "//")) u <- paste0("https:", u)
    tf <- tempfile(); on.exit(unlink(tf), add = TRUE)
    ua <- paste0("maketimeliner/",
                 tryCatch(as.character(utils::packageVersion("maketimeliner")),
                          error = function(e) "dev"),
                 " (R ", getRversion(), "; https://github.com/Shimpeim/maketimeliner)")
    old <- options(timeout = max(20, getOption("timeout"))); on.exit(options(old), add = TRUE)
    st <- suppressWarnings(utils::download.file(u, tf, method = "libcurl", quiet = TRUE,
                                                mode = "wb",
                                                headers = c("User-Agent" = ua)))
    if (!identical(as.integer(st), 0L)) stop("download failed")
    path <- tf
  } else {
    path <- sub("^file://", "", u, ignore.case = TRUE)
    path <- utils::URLdecode(path)
    if (!file.exists(path)) stop("file not found (relative to ", getwd(), ")")
  }
  sz <- file.size(path)
  if (sz > max_mb * 1024^2)
    stop(sprintf("%.1f MB exceeds image_max_mb = %s", sz / 1024^2, max_mb))
  raw <- readBin(path, "raw", sz)
  mime <- .sniff_mime(raw)
  if (is.na(mime)) stop("not a recognised image (got ",
                        if (sz) "non-image content" else "an empty file",
                        "; the server may have refused the request)")
  paste0("data:", mime, ";base64,", jsonlite::base64_enc(raw))
}

.sniff_mime <- function(raw) {
  h <- function(n) as.integer(raw[seq_len(min(n, length(raw)))])
  b <- h(16)
  if (length(b) >= 3 && all(b[1:3] == c(0xFF, 0xD8, 0xFF))) return("image/jpeg")
  if (length(b) >= 8 && all(b[1:8] == c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))) return("image/png")
  if (length(b) >= 4 && all(b[1:4] == c(0x47, 0x49, 0x46, 0x38))) return("image/gif")
  if (length(b) >= 12 && all(b[1:4] == c(0x52, 0x49, 0x46, 0x46)) &&
      all(b[9:12] == c(0x57, 0x45, 0x42, 0x50))) return("image/webp")
  if (length(b) >= 2 && all(b[1:2] == c(0x42, 0x4D))) return("image/bmp")
  head <- raw[seq_len(min(1024, length(raw)))]
  txt  <- rawToChar(head[head != as.raw(0)])
  if (grepl("<svg", txt, fixed = TRUE, useBytes = TRUE) ||
      grepl("<SVG", txt, fixed = TRUE, useBytes = TRUE)) return("image/svg+xml")
  NA_character_
}

.clear_image_cache <- function() rm(list = ls(.img_cache), envir = .img_cache)
