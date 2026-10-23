# SVG rendering of a layout produced by .layout().
# Every interactive element carries class + data-* attributes that the HTML
# template's JavaScript uses (tl-edge, tl-node, tl-label, tl-window, ...).

.shape_svg <- function(shape, x, y, w, h, fill, alpha, stroke, sw, lt, attrs = "") {
  st <- paste0(" fill=\"", .esc(fill), "\"",
               if (alpha < 1) paste0(" fill-opacity=\"", alpha, "\"") else "",
               .stroke_attr(stroke, sw, 1, lt), attrs)
  switch(shape,
    circle  = sprintf("<circle cx=\"%s\" cy=\"%s\" r=\"%s\"%s/>", .r(x), .r(y),
                      .r(min(w, h) / 2), st),
    ellipse = sprintf("<ellipse cx=\"%s\" cy=\"%s\" rx=\"%s\" ry=\"%s\"%s/>",
                      .r(x), .r(y), .r(w / 2), .r(h / 2), st),
    rect    = sprintf("<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\"%s/>",
                      .r(x - w / 2), .r(y - h / 2), .r(w), .r(h), st),
    rounded = sprintf("<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"%s\"%s/>",
                      .r(x - w / 2), .r(y - h / 2), .r(w), .r(h),
                      .r(min(w, h) / 4), st),
    diamond = sprintf("<polygon points=\"%s,%s %s,%s %s,%s %s,%s\"%s/>",
                      .r(x), .r(y - h / 2), .r(x + w / 2), .r(y), .r(x),
                      .r(y + h / 2), .r(x - w / 2), .r(y), st),
    triangle = sprintf("<polygon points=\"%s,%s %s,%s %s,%s\"%s/>",
                       .r(x), .r(y - h / 2), .r(x + w / 2), .r(y + h / 2),
                       .r(x - w / 2), .r(y + h / 2), st),
    none    = sprintf("<circle cx=\"%s\" cy=\"%s\" r=\"%s\" fill=\"transparent\" stroke=\"none\"%s/>",
                      .r(x), .r(y), .r(max(4, min(w, h) / 2)), attrs),
    sprintf("<circle cx=\"%s\" cy=\"%s\" r=\"%s\"%s/>", .r(x), .r(y),
            .r(min(w, h) / 2), st))
}

.node_label_svg <- function(n, ff) {
  pos <- n$label_position; fs <- n$fontsize; w <- n$width; h <- n$height
  ex  <- paste0(" class=\"tl-nlabel\" data-id=\"", .esc(n$id), "\"")
  switch(pos,
    below  = .text_svg(n$x, n$y + h / 2 + fs + 2, n$label, fs, n$fontcolour, "middle", ex),
    center = .text_svg(n$x, n$y, n$label, fs, n$fontcolour, "middle", ex, valign = "middle"),
    left   = .text_svg(n$x - w / 2 - 5, n$y, n$label, fs, n$fontcolour, "end", ex, valign = "middle"),
    right  = .text_svg(n$x + w / 2 + 5, n$y, n$label, fs, n$fontcolour, "start", ex, valign = "middle"),
    .text_svg(n$x, n$y - h / 2 - 5, n$label, fs, n$fontcolour, "middle", ex, valign = "bottom"))
}

.svg_build <- function(L, spec) {
  S <- L$S; ff <- S$font_family
  out <- character(0)
  add <- function(...) out[length(out) + 1L] <<- paste0(...)

  # ── legend geometry (computed first; canvas height depends on it) ────────
  leg <- .legend_items(L, spec)
  leg_lines <- character(0); leg_h <- 0
  if (S$show_legend && length(leg)) {
    lx0 <- S$padding; ly <- L$axis_bottom + 14
    maxw <- L$width - 2 * S$padding
    leg_lines <- c(leg_lines, .text_svg(lx0, ly + 12, S$legend_title, 12, "#333333",
                                        "start", "", "bold"))
    ly <- ly + 26
    for (sec in leg) {
      leg_lines <- c(leg_lines, .text_svg(lx0, ly + 10, sec$title, 10.5, "#777777",
                                          "start", " class=\"tl-legend-sec\""))
      ly <- ly + 18; cx <- lx0
      for (it in sec$items) {
        iw <- 30 + .text_w(it$label, 11) + 18
        if (cx + iw > lx0 + maxw && cx > lx0) { cx <- lx0; ly <- ly + 22 }
        leg_lines <- c(leg_lines, sprintf("<g class=\"tl-legend-item\"%s>%s%s</g>",
          it$attrs, it$glyph(cx + 12, ly + 6),
          .text_svg(cx + 30, ly + 10, it$label, 11, "#333333", "start")))
        cx <- cx + iw
      }
      ly <- ly + 30
    }
    leg_h <- ly - L$axis_bottom
  }
  W <- L$width; H <- L$axis_bottom + leg_h + S$padding / 2

  add(sprintf("<svg xmlns=\"http://www.w3.org/2000/svg\" id=\"tl-svg\" width=\"%s\" height=\"%s\" viewBox=\"0 0 %s %s\" font-family=\"%s\">",
              .r(W), .r(H), .r(W), .r(H), .esc(ff)))
  add(sprintf("<rect class=\"tl-bg\" x=\"0\" y=\"0\" width=\"%s\" height=\"%s\" fill=\"%s\"/>",
              .r(W), .r(H), .esc(S$background)))
  if (nzchar(S$title))
    add(.text_svg(S$padding, S$padding + 16, S$title, 18, "#1a1a2e", "start",
                  " class=\"tl-title\"", "bold"))

  # ── streams ──────────────────────────────────────────────────────────────
  add("<g class=\"tl-streams\">")
  st <- L$streams
  for (s in seq_len(nrow(st))) {
    add(sprintf("<g class=\"tl-stream\" data-stream=\"%s\">", .esc(st$id[s])))
    add(sprintf("<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"%s\"%s%s/>",
                .r(S$padding), .r(st$y0[s]), .r(L$x1 - S$padding + 10),
                .r(st$height[s]), .esc(st$colour[s]),
                if (st$alpha[s] < 1) paste0(" fill-opacity=\"", st$alpha[s], "\"") else "",
                .stroke_attr(st$stroke[s], st$stroke_width[s], 1, st$linetype[s])))
    add(.text_svg(S$padding + 10, (st$y0[s] + st$y1[s]) / 2, st$label[s],
                  S$stream_fontsize, st$fontcolour[s], "start",
                  " class=\"tl-slabel\"", "bold", valign = "middle"))
    add("</g>")
  }
  add("</g>")

  # ── axis / grid ──────────────────────────────────────────────────────────
  ts <- L$ts
  if (S$show_grid || S$show_axis) {
    add("<g class=\"tl-axis\">")
    for (k in seq_along(ts$ticks)) {
      x <- L$xmap(ts$ticks[k])
      if (S$show_grid)
        add(sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"#000\" stroke-opacity=\"0.07\"/>",
                    .r(x), .r(L$top), .r(x), .r(L$bands_bottom)))
      if (S$show_axis) {
        add(sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"#666\"/>",
                    .r(x), .r(L$bands_bottom + 4), .r(x), .r(L$bands_bottom + 9)))
        add(.text_svg(x, L$bands_bottom + 22, ts$labels[k], 10.5, "#555555", "middle"))
      }
    }
    if (S$show_axis) {
      add(sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"#666\"/>",
                  .r(L$x0), .r(L$bands_bottom + 4), .r(L$x1), .r(L$bands_bottom + 4)))
      if (nzchar(S$axis_label))
        add(.text_svg((L$x0 + L$x1) / 2, L$bands_bottom + 38, S$axis_label, 11,
                      "#333333", "middle"))
    }
    add("</g>")
  }

  # ── windows ──────────────────────────────────────────────────────────────
  w <- L$windows
  if (nrow(w)) {
    add("<g class=\"tl-windows\">")
    for (k in seq_len(nrow(w))) {
      ww <- max(w$x1[k] - w$x0[k], 0)
      add(sprintf("<g class=\"tl-window\" data-win=\"%d\">", k))
      if (ww < 0.5) {
        add(sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\"%s/>",
                    .r(w$x0[k]), .r(w$y0[k]), .r(w$x0[k]), .r(w$y1[k]),
                    .stroke_attr(w$stroke[k], max(w$stroke_width[k], 1.5), 1, w$linetype[k])))
      } else {
        add(sprintf("<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"%s\" fill-opacity=\"%s\"%s/>",
                    .r(w$x0[k]), .r(w$y0[k]), .r(ww), .r(w$y1[k] - w$y0[k]),
                    .esc(w$colour[k]), w$alpha[k],
                    .stroke_attr(w$stroke[k], w$stroke_width[k], 1, w$linetype[k])))
      }
      if (nzchar(w$label[k]))
        add(.text_svg(w$x0[k] + ww / 2, w$y0[k] + 13, w$label[k], 10.5,
                      if (w$stroke[k] == "none") "#555555" else w$stroke[k],
                      "middle", "", "bold"))
      add(sprintf("<title>%s</title>", .esc(paste(c(w$label[k], w$description[k])[nzchar(c(w$label[k], w$description[k]))], collapse = ": "))))
      add("</g>")
    }
    add("</g>")
  }

  # ── overlay links (under timelines) ──────────────────────────────────────
  ov <- L$overlay
  if (nrow(ov)) {
    add("<g class=\"tl-overlays\" fill=\"none\">")
    for (i in seq_len(nrow(ov))) {
      add(sprintf("<path class=\"tl-overlay\" data-ov=\"%s\" data-from=\"%s\" data-to=\"%s\" d=\"%s\" fill=\"none\"%s><title>%s</title></path>",
                  .esc(ov$code[i]), .esc(ov$from[i]), .esc(ov$to[i]), ov$path[i],
                  .stroke_attr(ov$colour[i], ov$width[i], ov$alpha[i], ov$linetype[i]),
                  .esc(ov$label[i])))
      if (nzchar(ov$text[i]))
        add(.text_svg(ov$mx[i], ov$my[i] - 4, ov$text[i], S$edge_label_fontsize,
                      ov$colour[i], "middle", " class=\"tl-ovlabel\""))
    }
    add("</g>")
  }

  # ── timelines ───────────────────────────────────────────────────────────
  e <- L$edges
  add("<g class=\"tl-edges\" fill=\"none\" stroke-linecap=\"round\">")
  for (i in seq_len(nrow(e))) {
    add(sprintf("<path class=\"tl-edge\" id=\"tl-e%d\" data-tl=\"%s\" data-from=\"%s\" data-to=\"%s\" d=\"%s\" fill=\"none\"%s><title>%s</title></path>",
                i, .esc(e$code[i]), .esc(e$from[i]), .esc(e$to[i]), e$path[i],
                .stroke_attr(e$colour[i], e$width[i], e$alpha[i], e$linetype[i]),
                .esc(e$label[i])))
  }
  add("</g>")
  if (nrow(e) && any(nzchar(e$text))) {
    add("<g class=\"tl-elabels\">")
    for (i in which(nzchar(e$text)))
      add(.text_svg(e$mx[i], e$my[i] - e$width[i] - 2, e$text[i],
                    S$edge_label_fontsize, e$colour[i], "middle",
                    sprintf(" class=\"tl-elabel\" data-tl=\"%s\" paint-order=\"stroke\" stroke=\"%s\" stroke-width=\"3\"",
                            .esc(e$code[i]), .esc(S$background))))
    add("</g>")
  }

  # ── nodes ────────────────────────────────────────────────────────────────
  nd <- L$nodes
  add("<g class=\"tl-nodes\">")
  for (i in seq_len(nrow(nd))) {
    n <- nd[i, ]
    add(sprintf("<g class=\"tl-node\" data-id=\"%s\" data-stream=\"%s\">",
                .esc(n$id), .esc(n$stream)))
    add(.shape_svg(n$shape, n$x, n$y, n$width, n$height, n$colour, n$alpha,
                   n$stroke, n$stroke_width, n$linetype))
    if (S$show_node_labels) add(.node_label_svg(n, ff))
    add(sprintf("<title>%s</title>", .esc(paste0(gsub("\n", " ", n$label), " (",
                                                 L$ts$fmt(n$time), ")"))))
    add("</g>")
  }
  add("</g>")

  # ── timeline name labels (start / end), stacked per anchoring event ────
  if (S$timeline_labels != "none" && nrow(e)) {
    add("<g class=\"tl-labels\">")
    anchors <- list()
    for (cd in unique(e$code)) {
      ec <- e[e$code == cd, ]
      if (S$timeline_labels %in% c("end", "both"))
        for (k in which(!ec$to %in% ec$from))
          anchors[[length(anchors) + 1]] <- data.frame(code = cd, node = ec$to[k],
            side = "end", y = ec$y2[k], stringsAsFactors = FALSE)
      if (S$timeline_labels %in% c("start", "both"))
        for (k in which(!ec$from %in% ec$to))
          anchors[[length(anchors) + 1]] <- data.frame(code = cd, node = ec$from[k],
            side = "start", y = ec$y1[k], stringsAsFactors = FALSE)
    }
    an <- do.call(rbind, anchors)
    an <- an[!duplicated(an[, c("code", "node", "side")]), , drop = FALSE]
    lh <- S$fontsize * 1.25
    for (g in unique(paste(an$node, an$side))) {
      a  <- an[paste(an$node, an$side) == g, , drop = FALSE]
      a  <- a[order(a$y, match(a$code, L$codes$code)), , drop = FALSE]
      ni <- match(a$node[1], nd$id)
      ys <- nd$y[ni] + (seq_len(nrow(a)) - 1 - (nrow(a) - 1) / 2) * lh
      if (nrow(a) == 1L) ys <- a$y
      for (k in seq_len(nrow(a))) {
        sty <- L$codes[L$codes$code == a$code[k], ]
        endside <- a$side[k] == "end"
        x <- nd$x[ni] + (if (endside) 1 else -1) * (nd$width[ni] / 2 + 6)
        add(.text_svg(x, ys[k], sty$label, S$fontsize, sty$colour,
                      if (endside) "start" else "end",
                      sprintf(" class=\"tl-label\" data-tl=\"%s\"", .esc(a$code[k])),
                      "bold", valign = "middle"))
      }
    }
    add("</g>")
  }

  if (length(leg_lines)) {
    add("<g class=\"tl-legend\">")
    out <- c(out, leg_lines)
    add("</g>")
  }
  add("</svg>")
  paste(out, collapse = "\n")
}

# Legend sections: timelines, overlay types, windows, node colours, node shapes.
.legend_items <- function(L, spec) {
  secs <- list()
  line_glyph <- function(col, w, a, lt) function(x, y)
    sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke-linecap=\"round\"%s/>",
            .r(x - 10), .r(y), .r(x + 12), .r(y), .stroke_attr(col, w, a, lt))
  used <- unique(L$edges$code)
  cs <- L$codes[L$codes$code %in% used, ]
  if (nrow(cs))
    secs[[length(secs) + 1]] <- list(title = "Timelines", items = lapply(seq_len(nrow(cs)), function(i)
      list(label = cs$label[i], glyph = line_glyph(cs$colour[i], cs$width[i], cs$alpha[i], cs$linetype[i]),
           attrs = sprintf(" data-tl=\"%s\"", .esc(cs$code[i])))))
  os <- L$overlay_codes[L$overlay_codes$code %in% unique(L$overlay$code), ]
  if (nrow(os))
    secs[[length(secs) + 1]] <- list(title = "Links", items = lapply(seq_len(nrow(os)), function(i)
      list(label = os$label[i], glyph = line_glyph(os$colour[i], os$width[i], os$alpha[i], os$linetype[i]),
           attrs = "")))
  w <- L$windows
  if (nrow(w) && any(nzchar(w$label))) {
    k <- which(nzchar(w$label) & !duplicated(w$label))
    secs[[length(secs) + 1]] <- list(title = "Windows", items = lapply(k, function(i)
      list(label = w$label[i], attrs = "", glyph = function(x, y)
        sprintf("<rect x=\"%s\" y=\"%s\" width=\"20\" height=\"12\" fill=\"%s\" fill-opacity=\"%s\"%s/>",
                .r(x - 10), .r(y - 6), .esc(w$colour[i]), max(w$alpha[i], 0.25),
                .stroke_attr(w$stroke[i], w$stroke_width[i], 1, w$linetype[i])))))
  }
  lc <- spec$legend_colours
  lc <- lc[!is.na(lc$colour) & nzchar(as.character(lc$colour)), , drop = FALSE]
  if (nrow(lc))
    secs[[length(secs) + 1]] <- list(title = "Event colours", items = lapply(seq_len(nrow(lc)), function(i)
      list(label = .chr(lc$label[i], ""), attrs = "", glyph = function(x, y)
        .shape_svg("circle", x, y, 13, 13, .hex(lc$colour[i]), 1, "#555555", 1, "solid"))))
  ls <- spec$legend_shapes
  ls <- ls[!is.na(ls$shape) & nzchar(as.character(ls$shape)), , drop = FALSE]
  if (nrow(ls))
    secs[[length(secs) + 1]] <- list(title = "Event shapes", items = lapply(seq_len(nrow(ls)), function(i)
      list(label = .chr(ls$label[i], ""), attrs = "", glyph = function(x, y)
        .shape_svg(as.character(ls$shape[i]), x, y, 14, 14, "#FFFFFF", 1, "#333333", 1.4, "solid"))))
  secs
}
