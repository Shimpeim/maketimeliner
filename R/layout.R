# Layout: time scale, stream bands, node positions, link paths.

# ── Time scale ────────────────────────────────────────────────────────────────

.parse_date1 <- function(v) {
  v <- trimws(v)
  if (grepl("^[0-9]{4}[-/][0-9]{1,2}$", v)) v <- paste0(v, "-01")
  v <- gsub("/", "-", v, fixed = TRUE)
  for (f in c("%Y-%m-%d", "%d.%m.%Y")) {
    d <- as.Date(v, format = f)
    if (!is.na(d)) return(as.numeric(d))
  }
  NA_real_
}
.parse_dates <- function(x) vapply(as.character(x), .parse_date1, numeric(1),
                                   USE.NAMES = FALSE)

# Build the time scale from all time values used (nodes + windows).
.time_scale <- function(values, kind = "auto", phases = NULL,
                        date_format = "auto", plot_width = 900) {
  values <- trimws(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  is_num <- function(v) !anyNA(suppressWarnings(as.numeric(v)))
  if (kind == "auto") {
    kind <- if (is_num(values)) "numeric"
            else if (!anyNA(.parse_dates(values))) "date"
            else "ordinal"
  }
  if (kind == "numeric") {
    if (!is_num(values))
      stop("time_scale = 'numeric' but some time values are not numeric: ",
           paste(utils::head(values[is.na(suppressWarnings(as.numeric(values)))], 5),
                 collapse = ", "), call. = FALSE)
    f   <- function(x) suppressWarnings(as.numeric(as.character(x)))
    fmt <- function(x) as.character(x)
  } else if (kind == "date") {
    bad <- values[is.na(.parse_dates(values))]
    if (length(bad))
      stop("time_scale = 'date' but these values are not dates (YYYY-MM-DD, ",
           "YYYY-MM or DD.MM.YYYY): ", paste(utils::head(bad, 5), collapse = ", "),
           call. = FALSE)
    f   <- .parse_dates
    fmt <- function(x) as.character(x)
  } else {
    lv <- character(0); lab <- character(0)
    if (!is.null(phases) && nrow(phases)) {
      ph  <- phases[!is.na(phases$phase) & nzchar(as.character(phases$phase)), ]
      o   <- order(.num(ph$order, Inf), seq_len(nrow(ph)))
      lv  <- trimws(as.character(ph$phase[o]))
      lab <- .chr(ph$label[o], NA)
      lab[is.na(lab)] <- lv[is.na(lab)]
    }
    extra <- setdiff(unique(values), lv)
    if (length(extra) && length(lv))
      warning("Ordinal time values not listed in phases were added at the end of the axis: ",
              paste(utils::head(extra, 8), collapse = ", "), call. = FALSE)
    lv  <- c(lv, extra); lab <- c(lab, extra)
    f   <- function(x) as.numeric(match(trimws(as.character(x)), lv))
    fmt <- function(x) { i <- match(trimws(as.character(x)), lv); ifelse(is.na(i), x, lab[i]) }
  }
  v  <- f(values)
  lo <- min(v); hi <- max(v)
  if (kind == "ordinal") {
    lo <- 1 - 0.5; hi <- length(lv) + 0.5
    ticks <- seq_along(lv); labels <- lab
  } else {
    span <- hi - lo
    pad  <- if (span > 0) span * 0.03 else 1
    lo <- lo - pad; hi <- hi + pad
    n  <- max(3, round(plot_width / 110))
    if (kind == "date") {
      pd <- pretty(as.Date(c(lo, hi), origin = "1970-01-01"), n = n)
      ticks  <- as.numeric(pd)
      labels <- if (identical(date_format, "auto")) attr(pd, "labels") %||% format(pd)
                else format(pd, date_format)
    } else {
      ticks  <- pretty(c(lo, hi), n = n)
      labels <- format(ticks, trim = TRUE, scientific = FALSE)
    }
    keep <- ticks >= lo & ticks <= hi
    ticks <- ticks[keep]; labels <- labels[keep]
  }
  list(kind = kind, f = f, fmt = fmt, lo = lo, hi = hi,
       ticks = ticks, labels = labels)
}

# ── Code normalisation ───────────────────────────────────────────────────────

# "1", "1.0", 1 all map to "1"; non-numeric codes are trimmed.
.code_key <- function(x) {
  x <- trimws(as.character(x))
  n <- suppressWarnings(as.numeric(x))
  ifelse(is.na(n), x, format(n, trim = TRUE, scientific = FALSE))
}

.code_order <- function(codes) {
  u <- unique(codes)
  n <- suppressWarnings(as.numeric(u))
  u[order(is.na(n), n, u)]
}

# Resolve per-code style from an edge_props table with fallbacks.
.code_styles <- function(codes, props, S, overlay = FALSE) {
  codes <- .code_order(.code_key(codes))
  pal   <- trimws(strsplit(S$edge_palette, ",", fixed = TRUE)[[1]])
  if (!length(pal)) pal <- .okabe_ito
  pk <- if (nrow(props)) .code_key(props$weight) else character(0)
  out <- lapply(seq_along(codes), function(i) {
    r <- match(codes[i], pk)
    g <- function(col) if (is.na(r)) NA else props[[col]][r]
    if (overlay) {
      dcol <- S$overlay_edge_colour; dw <- S$overlay_edge_width
      da <- S$overlay_edge_alpha;   dl <- S$overlay_edge_linetype
    } else {
      dcol <- pal[(i - 1) %% length(pal) + 1]; dw <- S$edge_width
      da <- S$edge_alpha; dl <- S$edge_linetype
    }
    data.frame(code = codes[i],
               label = .chr(g("label"), codes[i]),
               colour = .hex(g("colour"), dcol),
               alpha = .num(g("alpha"), da),
               width = .num(g("width"), dw),
               linetype = .chr(g("linetype"), dl),
               description = .chr(g("description"), ""),
               stringsAsFactors = FALSE)
  })
  if (!length(out))
    return(data.frame(code = character(0), label = character(0),
                      colour = character(0), alpha = numeric(0),
                      width = numeric(0), linetype = character(0),
                      description = character(0), stringsAsFactors = FALSE))
  do.call(rbind, out)
}

# ── Main layout ──────────────────────────────────────────────────────────────

.layout <- function(spec) {
  S  <- spec$settings
  nd <- spec$nodes
  st <- spec$streams[order(spec$streams$order, seq_len(nrow(spec$streams))), ]

  # time scale over nodes + windows
  win <- spec$windows
  win <- win[!(is.na(win$start) | !nzchar(trimws(as.character(win$start)))), , drop = FALSE]
  sp <- spec$spans
  sp <- sp[!is.na(sp$id) & nzchar(trimws(as.character(sp$id))), , drop = FALSE]
  if (nrow(sp)) {
    bad <- setdiff(trimws(as.character(sp$id)), nd$id)
    if (length(bad)) warning("spans: unknown event id(s) ignored: ", paste(bad, collapse = ", "), call. = FALSE)
    sp <- sp[trimws(as.character(sp$id)) %in% nd$id, , drop = FALSE]
    sp$id <- trimws(as.character(sp$id))
    sp$start <- ifelse(is.na(sp$start) | !nzchar(trimws(as.character(sp$start))),
                       nd$time[match(sp$id, nd$id)], trimws(as.character(sp$start)))
    sp$end <- trimws(as.character(sp$end))
    sp <- sp[!is.na(sp$end) & nzchar(sp$end), , drop = FALSE]
  }
  tvals <- c(nd$time, as.character(win$start), as.character(win$end), sp$start, sp$end)
  ts <- .time_scale(tvals, S$time_scale, spec$phases, S$date_format, S$plot_width)

  pad     <- S$padding
  title_h <- if (nzchar(S$title)) 36 else 0
  x0      <- pad + S$stream_label_width
  xmap    <- function(t) x0 + (t - ts$lo) / (ts$hi - ts$lo) * S$plot_width

  # stream bands
  scol <- trimws(strsplit(S$stream_colours, ",", fixed = TRUE)[[1]])
  if (!length(scol)) scol <- "#F2F2F2"
  st$height  <- .num(st$height, S$stream_height)
  st$colour  <- .hex(ifelse(is.na(st$colour) | !nzchar(as.character(st$colour)),
                            scol[(seq_len(nrow(st)) - 1) %% length(scol) + 1],
                            as.character(st$colour)))
  st$alpha   <- .num(st$alpha, S$stream_alpha)
  st$stroke  <- .hex(st$stroke, "none")
  st$stroke_width <- .num(st$stroke_width, 1)
  st$linetype <- .chr(st$linetype, "solid")
  st$fontcolour <- .hex(st$fontcolour, "#333333")
  st$description <- .chr(st$description, "")
  top <- pad + title_h
  st$y0 <- top + c(0, cumsum(st$height + S$stream_gap))[seq_len(nrow(st))]
  st$y1 <- st$y0 + st$height
  bands_bottom <- max(st$y1)

  # node styles
  nd$t   <- ts$f(nd$time)
  nd$x   <- xmap(nd$t)
  nd$shape  <- .chr(nd$shape, S$node_shape)
  nd$colour <- .hex(nd$colour, S$node_colour)
  nd$alpha  <- .num(nd$alpha, S$node_alpha)
  nd$stroke <- .hex(nd$stroke, S$node_stroke)
  nd$stroke_width <- .num(nd$stroke_width, S$node_stroke_width)
  nd$linetype <- .chr(nd$linetype, S$node_linetype)
  nd$fontcolour <- .hex(nd$fontcolour, S$fontcolour)
  nd$fontsize <- .num(nd$fontsize, S$fontsize)
  nd$width  <- .num(nd$width, S$node_width)
  nd$height <- .num(nd$height, S$node_height)
  nd$label_position <- .chr(nd$label_position, S$label_position)
  nd$description <- .chr(nd$description, "")
  nd$slot <- suppressWarnings(as.numeric(as.character(nd$slot)))
  span_end_x <- stats::setNames(nd$x, nd$id)
  if (nrow(sp)) {
    sx <- xmap(ts$f(sp$end))
    for (k in seq_len(nrow(sp))) if (!is.na(sx[k]))
      span_end_x[sp$id[k]] <- max(span_end_x[sp$id[k]], sx[k])
  }

  edges <- .edges_from_matrix(spec$adj)
  edges$code <- .code_key(edges$code)

  # lanes inside each stream (nodes without a manual slot)
  nd$y <- NA_real_
  for (s in seq_len(nrow(st))) {
    sid <- st$id[s]
    in_s <- which(nd$stream == sid)
    if (!length(in_s)) next
    man <- in_s[!is.na(nd$slot[in_s])]
    nd$y[man] <- st$y0[s] + pmin(pmax(nd$slot[man], 0), 1) * st$height[s]
    auto <- in_s[is.na(nd$slot[in_s])]
    if (!length(auto)) next
    auto <- auto[order(nd$x[auto], auto)]
    # A lane is occupied from a node until its last same-stream successor, so
    # no event is placed on top of a link running along that lane.
    last_x <- numeric(0); busy <- numeric(0)
    lane_of <- stats::setNames(rep(NA_integer_, nrow(nd)), nd$id)
    for (i in auto) {
      preds <- unique(edges$from[edges$to == nd$id[i]])
      preds <- preds[preds %in% nd$id[auto]]
      pref  <- suppressWarnings(mean(lane_of[preds], na.rm = TRUE))
      if (!is.finite(pref)) pref <- 1
      xi    <- nd$x[i]
      free  <- which(xi - last_x >= S$min_node_sep &
                     (xi >= busy - 1e-6 | seq_along(busy) %in% lane_of[preds]))
      cand  <- c(free, length(last_x) + 1L)
      pick  <- cand[which.min(abs(cand - pref) + (cand > length(last_x)) * 0.5)]
      succ  <- unique(edges$to[edges$from == nd$id[i]])
      succ  <- succ[succ %in% nd$id[auto]]
      bx    <- max(c(xi, nd$x[match(succ, nd$id)], span_end_x[[nd$id[i]]] + S$min_node_sep / 2))
      if (pick > length(last_x)) { last_x <- c(last_x, xi); busy <- c(busy, bx) }
      else { last_x[pick] <- xi; busy[pick] <- max(bx, if (xi >= busy[pick]) xi else busy[pick]) }
      lane_of[nd$id[i]] <- pick
    }
    L <- length(last_x)
    nd$y[auto] <- st$y0[s] + st$height[s] * lane_of[nd$id[auto]] / (L + 1)
  }

  # timeline styles + link geometry
  cs <- .code_styles(edges$code, spec$edge_props, S)
  edges <- .link_geometry(edges, nd, cs, S, overlay = FALSE)
  edges <- .apply_link_props(edges, spec$link_props)
  if (nrow(edges) && !is.null(spec$edge_labels))
    edges$text <- .chr(spec$edge_labels[cbind(edges$from, edges$to)], "")
  else edges$text <- rep("", nrow(edges))

  ov <- .edges_from_matrix(spec$overlay)
  ov$code <- .code_key(ov$code)
  os <- .code_styles(ov$code, spec$overlay_edge_props, S, overlay = TRUE)
  ov <- .link_geometry(ov, nd, os, S, overlay = TRUE)
  if (nrow(ov) && !is.null(spec$overlay_edge_labels))
    ov$text <- .chr(spec$overlay_edge_labels[cbind(ov$from, ov$to)], "")
  else ov$text <- rep("", nrow(ov))

  # spans (durations)
  if (nrow(sp)) {
    t0 <- ts$f(sp$start); t1 <- ts$f(sp$end)
    badt <- is.na(t0) | is.na(t1)
    if (any(badt)) {
      warning("spans with start/end not on the time axis ignored: ",
              paste(sp$id[badt], collapse = ", "), call. = FALSE)
      sp <- sp[!badt, , drop = FALSE]; t0 <- t0[!badt]; t1 <- t1[!badt]
    }
    ni <- match(sp$id, nd$id)
    sp$x0 <- xmap(pmin(t0, t1)); sp$x1 <- xmap(pmax(t0, t1))
    sp$width <- .num(sp$width, 8)
    sp$k  <- stats::ave(seq_len(nrow(sp)), sp$id, FUN = seq_along)
    sp$y  <- nd$y[ni] + (sp$k - 1) * (sp$width + 2)
    sp$colour <- .hex(sp$colour, "#999999"); sp$alpha <- .num(sp$alpha, 0.3)
    sp$linetype <- .chr(sp$linetype, "solid")
    sp$line_colour <- .hex(ifelse(is.na(sp$line_colour) | !nzchar(as.character(sp$line_colour)),
                                  sp$colour, as.character(sp$line_colour)))
    sp$line_width <- .num(sp$line_width, 1.6)
    sp$label <- .chr(sp$label, ""); sp$source <- .chr(sp$source, "")
    sp$description <- .chr(sp$description, "")
  }

  # windows
  if (nrow(win)) {
    ws <- ts$f(win$start)
    we <- ts$f(.chr(win$end, NA))
    we[is.na(we)] <- ws[is.na(we)]
    if (anyNA(ws)) stop("Window start values not on the time scale: ",
                        paste(win$start[is.na(ws)], collapse = ", "), call. = FALSE)
    if (ts$kind == "ordinal") { ws <- ws - 0.4; we <- we + 0.4 }
    win$x0 <- xmap(pmin(ws, we)); win$x1 <- xmap(pmax(ws, we))
    win$y0 <- NA_real_; win$y1 <- NA_real_
    for (k in seq_len(nrow(win))) {
      ss <- trimws(strsplit(.chr(win$streams[k], ""), "[;|,]")[[1]])
      ss <- ss[nzchar(ss) & ss %in% st$id]
      if (!length(ss)) ss <- st$id
      win$y0[k] <- min(st$y0[st$id %in% ss]); win$y1[k] <- max(st$y1[st$id %in% ss])
    }
    win$label  <- .chr(win$label, "")
    win$colour <- .hex(win$colour, S$window_colour)
    win$alpha  <- .num(win$alpha, S$window_alpha)
    win$stroke <- .hex(win$stroke, S$window_stroke)
    win$stroke_width <- .num(win$stroke_width, S$window_stroke_width)
    win$linetype <- .chr(win$linetype, S$window_linetype)
    win$description <- .chr(win$description, "")
  }

  # right margin for timeline end labels
  right <- pad
  if (S$timeline_labels %in% c("end", "both") && nrow(cs))
    right <- right + max(.text_w(cs$label, S$fontsize)) + 16

  list(S = S, ts = ts, streams = st, nodes = nd, edges = edges, codes = cs,
       overlay = ov, overlay_codes = os, windows = win, spans = sp,
       x0 = x0, x1 = x0 + S$plot_width, xmap = xmap, top = top,
       bands_bottom = bands_bottom,
       width = x0 + S$plot_width + right,
       axis_bottom = bands_bottom + if (S$show_axis) 44 else 10)
}

# Compute path strings for links; bundles parallel timelines on one link.
.link_geometry <- function(e, nd, cs, S, overlay = FALSE) {
  if (!nrow(e)) {
    e$path <- character(0); e$mx <- numeric(0); e$my <- numeric(0)
    e$x1 <- e$y1 <- e$x2 <- e$y2 <- numeric(0)
    return(cbind(e, cs[0, setdiff(names(cs), "code")]))
  }
  e <- cbind(e, cs[match(e$code, cs$code), setdiff(names(cs), "code")])
  rank_code <- match(e$code, cs$code)
  pi <- match(e$from, nd$id); pj <- match(e$to, nd$id)
  e$x1 <- nd$x[pi]; e$y1 <- nd$y[pi]; e$x2 <- nd$x[pj]; e$y2 <- nd$y[pj]
  key <- paste(e$from, e$to, sep = "\r")
  e$off <- 0
  for (k in unique(key)) {
    w <- which(key == k)
    if (length(w) < 2L) next
    w <- w[order(rank_code[w])]
    sp <- S$bundle_gap + max(e$width[w])
    e$off[w] <- (seq_along(w) - 1 - (length(w) - 1) / 2) * sp
  }
  e$y1 <- e$y1 + e$off; e$y2 <- e$y2 + e$off
  dx <- e$x2 - e$x1; dy <- e$y2 - e$y1
  if (overlay) {
    len <- sqrt(dx^2 + dy^2); len[len == 0] <- 1
    cx <- (e$x1 + e$x2) / 2 - dy / len * len * S$overlay_curvature
    cy <- (e$y1 + e$y2) / 2 + dx / len * len * S$overlay_curvature
    e$path <- sprintf("M%s %s Q%s %s %s %s", .r(e$x1), .r(e$y1), .r(cx), .r(cy),
                      .r(e$x2), .r(e$y2))
    e$mx <- 0.25 * e$x1 + 0.5 * cx + 0.25 * e$x2
    e$my <- 0.25 * e$y1 + 0.5 * cy + 0.25 * e$y2
    return(e)
  }
  e$path <- vapply(seq_len(nrow(e)), function(i) {
    x1 <- e$x1[i]; y1 <- e$y1[i]; x2 <- e$x2[i]; y2 <- e$y2[i]
    if (S$curve == "straight" || abs(dx[i]) < 1 || abs(dy[i]) < 0.5) {
      sprintf("M%s %s L%s %s", .r(x1), .r(y1), .r(x2), .r(y2))
    } else if (S$curve == "step") {
      xm <- (x1 + x2) / 2
      sprintf("M%s %s H%s V%s H%s", .r(x1), .r(y1), .r(xm), .r(y2), .r(x2))
    } else {
      c <- dx[i] * min(max(S$curve_tension, 0), 1)
      sprintf("M%s %s C%s %s %s %s %s %s", .r(x1), .r(y1), .r(x1 + c), .r(y1),
              .r(x2 - c), .r(y2), .r(x2), .r(y2))
    }
  }, character(1))
  e$mx <- (e$x1 + e$x2) / 2; e$my <- (e$y1 + e$y2) / 2
  e
}

# Per-link overrides (link_props): non-blank cells replace the timeline style
# on matching links; blank weight matches every timeline on the link.
.apply_link_props <- function(e, lp) {
  e$link_label <- rep("", nrow(e)); e$link_description <- rep("", nrow(e))
  if (is.null(lp) || !nrow(lp) || !nrow(e)) return(e)
  lp <- lp[!is.na(lp$from) & nzchar(trimws(as.character(lp$from))), , drop = FALSE]
  unmatched <- character(0)
  for (k in seq_len(nrow(lp))) {
    w   <- .chr(lp$weight[k], "")
    hit <- e$from == trimws(as.character(lp$from[k])) & e$to == trimws(as.character(lp$to[k])) &
           (if (nzchar(w)) e$code == .code_key(w) else TRUE)
    if (!any(hit)) { unmatched <- c(unmatched, paste0(lp$from[k], "->", lp$to[k])); next }
    v <- function(col) { x <- lp[[col]][k]; if (is.na(x) || !nzchar(trimws(as.character(x)))) NULL else x }
    if (!is.null(v("colour")))   e$colour[hit]   <- .hex(v("colour"))
    if (!is.null(v("alpha")))    e$alpha[hit]    <- .num(v("alpha"), 1)
    if (!is.null(v("width")))    e$width[hit]    <- .num(v("width"), 1)
    if (!is.null(v("linetype"))) e$linetype[hit] <- as.character(v("linetype"))
    if (!is.null(v("label")))    e$link_label[hit] <- as.character(v("label"))
    if (!is.null(v("description"))) e$link_description[hit] <- as.character(v("description"))
  }
  if (length(unmatched))
    warning("link_props rows match no link: ", paste(unmatched, collapse = ", "), call. = FALSE)
  e
}
