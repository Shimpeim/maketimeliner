# Internal helpers shared by the layout, SVG and HTML builders.

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
}

# XML / HTML escape.
.esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

# Vectorised numeric coercion with a default for NA / blank.
.num <- function(x, def) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[is.na(v)] <- def
  v
}

# Vectorised character coercion with a default for NA / blank.
.chr <- function(x, def) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- def
  x
}

# Column of df, or a vector of NA when absent.
.col <- function(df, name) {
  if (!is.null(df) && name %in% names(df)) df[[name]] else rep(NA, NROW(df))
}

# Normalise column names: lower case, trimmed, British/US aliases unified.
.norm_names <- function(df) {
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  nm <- tolower(trimws(names(df)))
  alias <- c(color = "colour", fontcolor = "fontcolour", opacity = "alpha",
             lty = "linetype", line_type = "linetype", lwd = "width",
             linewidth = "width", strokewidth = "stroke_width",
             stroke_colour = "stroke", stroke_color = "stroke",
             fill = "colour")
  for (a in names(alias)) {
    if (a %in% nm && !alias[[a]] %in% nm) nm[nm == a] <- alias[[a]]
  }
  names(df) <- nm
  df
}

# Convert any R / CSS colour to #RRGGBB; leave unparseable strings untouched
# (so CSS-only names still reach the SVG).
.hex <- function(x, def = "#000000") {
  x <- .chr(x, def)
  vapply(x, function(v) {
    if (identical(tolower(v), "none") || identical(tolower(v), "transparent"))
      return("none")
    rgb <- tryCatch(grDevices::col2rgb(v), error = function(e) NULL)
    if (is.null(rgb)) return(v)
    sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
  }, character(1), USE.NAMES = FALSE)
}

# Linetype -> stroke-dasharray (scaled gently with line width).
.dasharray <- function(lt, w = 1) {
  lt <- tolower(.chr(lt, "solid"))
  k  <- pmax(1, .num(w, 1) / 2)
  base <- list(solid = NULL, blank = NULL,
               dashed = c(6, 4), dotted = c(1.5, 3), dotdash = c(1.5, 3, 6, 3),
               longdash = c(12, 4), twodash = c(6, 3, 2, 3))
  vapply(seq_along(lt), function(i) {
    if (grepl("^[0-9., ]+$", lt[i])) return(gsub(" ", "", lt[i]))  # literal
    d <- base[[lt[i]]]
    if (is.null(d)) "" else paste(format(d * k[i], trim = TRUE), collapse = ",")
  }, character(1))
}

# Build an attribute string for stroke / dash.
.stroke_attr <- function(col, width, alpha, lt) {
  da <- .dasharray(lt, width)
  paste0(" stroke=\"", .esc(col), "\" stroke-width=\"", width, "\"",
         ifelse(alpha < 1, paste0(" stroke-opacity=\"", alpha, "\""), ""),
         ifelse(nzchar(da), paste0(" stroke-dasharray=\"", da, "\""), ""),
         ifelse(tolower(lt) == "blank", " stroke-opacity=\"0\"", ""))
}

# Approximate rendered text width in px.
.text_w <- function(txt, fs) {
  lines <- strsplit(as.character(txt), "\n", fixed = TRUE)
  vapply(lines, function(l) if (length(l)) max(nchar(l, type = "width")) else 0L,
         integer(1)) * fs * 0.58
}

# Multi-line <text> with tspans.
.text_svg <- function(x, y, txt, fs, col, anchor = "middle", extra = "",
                      weight = "normal", valign = "baseline") {
  lines <- strsplit(as.character(txt), "\n", fixed = TRUE)[[1]]
  if (!length(lines)) return("")
  n  <- length(lines)
  lh <- fs * 1.2
  y0 <- switch(valign,
               middle = y - (n - 1) * lh / 2 + fs * 0.35,
               bottom = y - (n - 1) * lh,
               y)
  spans <- vapply(seq_len(n), function(i) {
    sprintf("<tspan x=\"%s\" y=\"%s\">%s</tspan>", .r(x), .r(y0 + (i - 1) * lh),
            .esc(lines[i]))
  }, character(1))
  sprintf("<text font-size=\"%s\" fill=\"%s\" text-anchor=\"%s\" font-weight=\"%s\"%s>%s</text>",
          fs, .esc(col), anchor, weight, extra, paste(spans, collapse = ""))
}

.r <- function(x) format(round(x, 2), trim = TRUE, scientific = FALSE)

# Okabe-Ito palette (colour-blind safe) for timelines without edge_props.
.okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2",
                "#CC79A7", "#F0E442", "#000000")

# Replace every occurrence of a fixed placeholder without regex/backslash
# interpretation of the replacement value.
.fill <- function(x, ph, val) {
  out <- character(0)
  repeat {
    i <- regexpr(ph, x, fixed = TRUE)
    if (i < 0) break
    out <- c(out, substr(x, 1L, i - 1L), val)
    x   <- substring(x, i + nchar(ph))
  }
  paste0(paste(out, collapse = ""), x)
}
