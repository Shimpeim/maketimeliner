mini <- function() {
  nodes <- data.frame(id = c("a", "b", "c", "d"), time = c(2000, 2002, 2004, 2004),
                      stream = c("s1", "s2", "s1", "s2"), stringsAsFactors = FALSE)
  adj <- matrix(0, 4, 4, dimnames = list(nodes$id, nodes$id))
  adj["a", "b"] <- 1; adj["b", "c"] <- 1; adj["b", "d"] <- 2
  list(nodes = nodes, adj = adj)
}

test_that("happy path: SVG and HTML strings with defaults", {
  m <- mini()
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  expect_match(out$svg, "^<svg")
  expect_match(out$html, "<!DOCTYPE html>", fixed = TRUE)
  expect_equal(sum(gregexpr("class=\"tl-edge\"", out$svg)[[1]] > 0), 3)
  expect_equal(nrow(out$layout$nodes), 4)
  expect_false(grepl("__DATA__|__SVG__|__TITLE__", out$html))
})

test_that("files are written", {
  m <- mini(); d <- tempfile(); dir.create(d)
  timeline_to_outputs(m$adj, m$nodes, html_file = file.path(d, "t.html"),
                      svg_file = file.path(d, "t.svg"))
  expect_true(file.exists(file.path(d, "t.html")))
  expect_true(file.exists(file.path(d, "t.svg")))
})

test_that("nodes are placed in their stream bands; x follows time", {
  m <- mini()
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  n <- out$layout$nodes; s <- out$layout$streams
  for (i in seq_len(nrow(n))) {
    b <- s[s$id == n$stream[i], ]
    expect_true(n$y[i] >= b$y0 && n$y[i] <= b$y1)
  }
  expect_true(all(diff(n$x[order(as.numeric(n$time))]) >= 0))
})

test_that("multiple timeline codes on one link are bundled with offsets", {
  m <- mini(); m$adj["a", "b"] <- "1;2"
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  e <- out$layout$edges[out$layout$edges$from == "a", ]
  expect_equal(sort(e$code), c("1", "2"))
  expect_false(identical(e$path[1], e$path[2]))
})

test_that("edge_props styles timelines: colour, alpha, width, linetype", {
  m <- mini()
  ep <- data.frame(weight = 1, label = "Issue", colour = "red", alpha = 0.5,
                   width = 7, linetype = "dotted")
  out <- timeline_to_outputs(m$adj, m$nodes, edge_props = ep,
                             html_file = NULL, svg_file = NULL)
  expect_match(out$svg, "stroke=\"#FF0000\" stroke-width=\"7\" stroke-opacity=\"0.5\" stroke-dasharray=", fixed = TRUE)
  expect_match(out$svg, ">Issue<", fixed = TRUE)
})

test_that("node style columns (colour/alpha/linetype/stroke_width) reach the SVG", {
  m <- mini()
  m$nodes$colour <- c("#112233", NA, NA, NA); m$nodes$alpha <- c(0.4, NA, NA, NA)
  m$nodes$linetype <- c("dashed", NA, NA, NA); m$nodes$stroke_width <- c(2.5, NA, NA, NA)
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  expect_match(out$svg, "fill=\"#112233\" fill-opacity=\"0.4\" stroke=\"#333333\" stroke-width=\"2.5\" stroke-dasharray=", fixed = TRUE)
})

test_that("any number of streams, ordered by 'order'", {
  nodes <- data.frame(id = letters[1:5], time = 1:5, stream = c("x", "y", "z", "w", "x"))
  adj <- matrix(0, 5, 5, dimnames = list(nodes$id, nodes$id))
  adj["a", "b"] <- adj["b", "c"] <- adj["c", "d"] <- adj["d", "e"] <- 1
  st <- data.frame(id = c("w", "x", "y", "z"), order = c(4, 1, 2, 3))
  out <- timeline_to_outputs(adj, nodes, streams = st, html_file = NULL, svg_file = NULL)
  s <- out$layout$streams
  expect_equal(s$id, c("x", "y", "z", "w"))
  expect_true(all(diff(s$y0) > 0))
})

test_that("date and ordinal time scales", {
  m <- mini(); m$nodes$time <- c("2001-01-15", "2002-06", "2004-03-01", "2004-03-01")
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  expect_equal(out$layout$time_scale$kind, "date")
  m <- mini(); m$nodes$time <- c("Agenda", "Alternatives", "Decision", "Decision")
  ph <- data.frame(phase = c("Decision", "Agenda", "Alternatives"), order = c(3, 1, 2))
  out <- timeline_to_outputs(m$adj, m$nodes, phases = ph, html_file = NULL, svg_file = NULL)
  expect_equal(out$layout$time_scale$kind, "ordinal")
  n <- out$layout$nodes
  expect_lt(n$x[n$id == "a"], n$x[n$id == "b"])
  expect_equal(out$layout$time_scale$labels, c("Agenda", "Alternatives", "Decision"))
})

test_that("settings: string values from CSV are coerced; unknown keys warn", {
  s <- maketimeliner:::.merge_settings(data.frame(Setting = c("plot_width", "show_legend"),
                                                  Value = c("500", "FALSE")))
  expect_identical(s$plot_width, 500)
  expect_false(s$show_legend)
  expect_warning(maketimeliner:::.merge_settings(list(nonsense = 1)), "unknown")
  expect_warning(maketimeliner:::.merge_settings(list(curve = "zigzag")), "not one of")
})

test_that("windows, overlay and legend are drawn", {
  m <- mini()
  w <- data.frame(start = 2001, end = 2003, label = "Window", streams = "s1")
  ov <- matrix(0, 4, 4, dimnames = list(m$nodes$id, m$nodes$id)); ov["a", "d"] <- 1
  out <- timeline_to_outputs(m$adj, m$nodes, windows = w, adj_overlay = ov,
                             legend_node_shape = data.frame(shape = "diamond", label = "Event"),
                             html_file = NULL, svg_file = NULL)
  expect_match(out$svg, "class=\"tl-window\"", fixed = TRUE)
  expect_match(out$svg, "class=\"tl-overlay\"", fixed = TRUE)
  expect_match(out$svg, "Event shapes", fixed = TRUE)
})

test_that("errors are informative", {
  m <- mini()
  expect_error(timeline_spec(m$adj, m$nodes[, c("id", "stream")]), "time")
  bad <- m$nodes; bad$id[2] <- "a"
  expect_error(timeline_spec(m$adj, bad), "Duplicate")
  expect_error(render_timeline(list()), "timeline_spec")
  m$nodes$time <- c("1", "x", "3", "4")
  expect_error(timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL,
                                   time_scale = "numeric"), "not numeric")
})

test_that("unknown stream in nodes is added with a warning", {
  m <- mini()
  expect_warning(timeline_spec(m$adj, m$nodes, streams = data.frame(id = "s1")), "s2")
})

test_that("CSV round trip preserves the diagram", {
  spec <- kingdon_example()
  d <- tempfile(); write_timeline_csvs(spec, d, prefix = "rt")
  spec2 <- read_timeline_csvs(d)
  a <- render_timeline(spec, NULL, NULL); b <- render_timeline(spec2, NULL, NULL)
  expect_identical(a$svg, b$svg)
  expect_true(file.exists(file.path(d, "rt_settings.csv")))
})

test_that("read_timeline_csvs selects by prefix and rejects ambiguity", {
  d <- tempfile(); spec <- kingdon_example()
  write_timeline_csvs(spec, d, "one"); write_timeline_csvs(spec, d, "two")
  expect_error(read_timeline_csvs(d), "prefix")
  expect_s3_class(read_timeline_csvs(d, prefix = "two"), "timeline_spec")
})

test_that("HTML data JSON is valid and script-safe", {
  m <- mini(); m$nodes$description <- c("</script><b>x", NA, NA, NA)
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  js <- regmatches(out$html, regexpr("const D = .*?;\n", out$html))
  expect_false(grepl("</script>", js, fixed = TRUE))
  d <- jsonlite::fromJSON(sub(";\n$", "", sub("^const D = ", "", js)), simplifyVector = FALSE)
  expect_length(d$nodes, 4)
  expect_length(d$timelines, 2)
})

html_data <- function(html) {
  js <- regmatches(html, regexpr("const D = .*?;\n", html))
  jsonlite::fromJSON(sub(";\n$", "", sub("^const D = ", "", js)), simplifyVector = FALSE)
}

test_that("node image URLs reach the HTML data; unsafe schemes are dropped", {
  m <- mini()
  m$nodes$image <- c("https://example.org/a.png", "img/b.jpg",
                     "javascript:alert(1)", NA)
  expect_warning(out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL,
                                            svg_file = NULL, tooltip_image_width = 300,
                                            embed_images = FALSE),
                 "unsupported image")
  d <- html_data(out$html)
  imgs <- vapply(d$nodes, function(n) n$image, character(1))
  expect_equal(imgs, c("https://example.org/a.png", "img/b.jpg", "", ""))
  expect_equal(d$image_width, 300)
  expect_match(out$html, "imgTag(n.image)", fixed = TRUE)
})

test_that("image column survives the CSV round trip", {
  spec <- kingdon_example()
  spec$nodes$image[1] <- "https://example.org/x.png"
  d <- tempfile(); write_timeline_csvs(spec, d, "img")
  expect_equal(read_timeline_csvs(d)$nodes$image[1], "https://example.org/x.png")
})

test_that("local pictures are embedded as data URIs (embed_images = TRUE)", {
  png <- tempfile(fileext = ".png")
  writeBin(jsonlite::base64_dec("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="), png)
  m <- mini(); m$nodes$image <- c(png, "", "", "")
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  d <- html_data(out$html)
  expect_match(d$nodes[[1]]$image, "^data:image/png;base64,")
  out2 <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL,
                              embed_images = FALSE)
  expect_equal(html_data(out2$html)$nodes[[1]]$image, png)
})

test_that("unembeddable pictures keep the URL and warn with a reason", {
  m <- mini(); m$nodes$image <- c("no/such/file.png", "", "", "")
  expect_warning(out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL),
                 "file not found")
  expect_equal(html_data(out$html)$nodes[[1]]$image, "no/such/file.png")
  txt <- tempfile(fileext = ".png"); writeLines("not an image", txt)
  m$nodes$image[1] <- txt
  expect_warning(timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL),
                 "not a recognised image")
  big <- tempfile(fileext = ".png"); writeBin(as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, rep(0, 3000))), big)
  m$nodes$image[1] <- big
  expect_warning(timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL,
                                     image_max_mb = 0.001), "exceeds image_max_mb")
})

test_that("link_props override style on single links and feed tooltip + legend", {
  m <- mini()
  lp <- data.frame(from = "a", to = "b", weight = "", linetype = "2,4", width = 5,
                   label = "State", description = "Actor: the state (interpretation)")
  lt <- data.frame(linetype = "2,4", label = "State")
  out <- timeline_to_outputs(m$adj, m$nodes, link_props = lp, legend_linetypes = lt,
                             html_file = NULL, svg_file = NULL, embed_images = FALSE)
  e <- out$layout$edges
  expect_match(out$svg, "stroke-width=\"5\"[^>]*stroke-dasharray=\"2,4\"[^>]*><title>1 — State</title>")
  expect_match(out$svg, ">Line types<", fixed = TRUE)
  d <- html_data(out$html)
  ab <- Filter(function(x) x$from == "a", d$edges)[[1]]
  expect_equal(ab$link_label, "State")
  expect_equal(ab$link_description, "Actor: the state (interpretation)")
  other <- Filter(function(x) x$from == "b", d$edges)
  expect_true(all(vapply(other, function(x) x$link_label == "", logical(1))))
})

test_that("link_props weight restricts the override to one timeline; unmatched rows warn", {
  m <- mini(); m$adj["a", "b"] <- "1;2"
  lp <- data.frame(from = "a", to = "b", weight = 2, colour = "#00FF00")
  out <- timeline_to_outputs(m$adj, m$nodes, link_props = lp, html_file = NULL, svg_file = NULL)
  e <- out$layout$edges[out$layout$edges$from == "a", ]
  expect_equal(e$colour[e$code == "2"], "#00FF00")
  expect_false(e$colour[e$code == "1"] == "#00FF00")
  expect_warning(timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL,
                                     link_props = data.frame(from = "c", to = "a", linetype = "dashed")),
                 "match no link")
})

test_that("link_props and legend_linetypes survive the CSV round trip", {
  m <- mini()
  sp <- timeline_spec(m$adj, m$nodes,
                      link_props = data.frame(from = "a", to = "b", linetype = "dotted", label = "X"),
                      legend_linetypes = data.frame(linetype = "dotted", label = "X"))
  d <- tempfile(); write_timeline_csvs(sp, d, "lp")
  expect_true(file.exists(file.path(d, "lp_link_props.csv")))
  sp2 <- read_timeline_csvs(d)
  expect_equal(sp2$link_props$label, "X")
  expect_identical(render_timeline(sp, NULL, NULL)$svg, render_timeline(sp2, NULL, NULL)$svg)
})

test_that("legend line-type samples use round caps so dot patterns are visible (#12)", {
  m <- mini()
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL,
                             legend_linetypes = data.frame(linetype = "0.1,5", label = "dots"))
  leg <- regmatches(out$svg, regexpr("<line [^>]*stroke-dasharray=\"0.1,5\"[^>]*/>", out$svg))
  expect_length(leg, 1)
  expect_match(leg, "stroke-linecap=\"round\"", fixed = TRUE)
})

test_that("spans draw a band from start to end at the event, with source in HTML", {
  m <- mini()
  sp <- data.frame(id = "a", end = 2003, label = "State", colour = "#FF0000",
                   linetype = "10,6", source = "Text: p. 3", description = "carried by the state")
  out <- timeline_to_outputs(m$adj, m$nodes, spans = sp, html_file = NULL, svg_file = NULL)
  expect_match(out$svg, "class=\"tl-span\" data-id=\"a\"", fixed = TRUE)
  n <- out$layout$nodes; xa <- n$x[n$id == "a"]
  band <- regmatches(out$svg, regexpr("<line x1=\"[0-9.]+\" y1=\"[0-9.]+\" x2=\"[0-9.]+\"[^>]*stroke=\"#FF0000\" stroke-width=\"8\"", out$svg))
  x <- as.numeric(sub(".*\"", "", regmatches(band, gregexpr("x[12]=\"[0-9.]+", band))[[1]]))
  expect_equal(x[1], xa, tolerance = 0.01)
  expect_gt(x[2], xa)
  d <- html_data(out$html)
  expect_equal(d$spans[[1]]$source, "Text: p. 3")
  expect_equal(d$spans[[1]]$period, "2000 – 2003")
})

test_that("a span keeps its lane reserved, so a later event in that stream moves lane", {
  nodes <- data.frame(id = c("a", "b"), time = c(1, 3), stream = "s")
  adj <- matrix("", 2, 2, dimnames = list(nodes$id, nodes$id))
  plain <- timeline_to_outputs(adj, nodes, html_file = NULL, svg_file = NULL, time_scale = "numeric")
  expect_equal(plain$layout$nodes$y[1], plain$layout$nodes$y[2])
  withspan <- timeline_to_outputs(adj, nodes, spans = data.frame(id = "a", end = 5),
                                  html_file = NULL, svg_file = NULL, time_scale = "numeric")
  expect_false(isTRUE(all.equal(withspan$layout$nodes$y[1], withspan$layout$nodes$y[2])))
})

test_that("several spans per event stack; bad spans warn; CSV round trip", {
  m <- mini()
  sp <- data.frame(id = c("a", "a", "zz"), start = c("", "2001", ""), end = c(2002, 2004, 2003))
  expect_warning(out <- timeline_to_outputs(m$adj, m$nodes, spans = sp, html_file = NULL, svg_file = NULL),
                 "unknown event id")
  expect_equal(length(gregexpr("class=\"tl-span\"", out$svg)[[1]]), 2)
  ok <- timeline_spec(m$adj, m$nodes, spans = sp[1:2, ])
  d <- tempfile(); write_timeline_csvs(ok, d, "sp")
  expect_true(file.exists(file.path(d, "sp_spans.csv")))
  expect_identical(render_timeline(ok, NULL, NULL)$svg, render_timeline(read_timeline_csvs(d), NULL, NULL)$svg)
})

test_that("ordinal values missing from phases warn", {
  m <- mini(); m$nodes$time <- c("A", "B", "C", "C")
  expect_warning(timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL,
                                     phases = data.frame(phase = c("A", "B"), order = 1:2)),
                 "not listed in phases")
})

test_that(".fill keeps text beyond one million characters (#14)", {
  big <- strrep("a", 1500000)
  x <- maketimeliner:::.fill(paste0("<p>__X__</p>", big, "</html>"), "__X__", "ok")
  expect_equal(nchar(x), nchar(big) + nchar("<p>ok</p></html>"))
  expect_match(substr(x, nchar(x) - 6, nchar(x)), "</html>", fixed = TRUE)
})

test_that("HTML with more than 1 MB of embedded pictures is complete (#14)", {
  m <- mini()
  png <- tempfile(fileext = ".png")
  hdr <- jsonlite::base64_dec("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
  writeBin(c(hdr, as.raw(rep(0, 900000))), png)   # valid PNG header + padding: ~1.2 MB as base64
  m$nodes$image <- c(png, png, "", "")
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL)
  expect_gt(nchar(out$html), 1e6)
  expect_match(substr(out$html, nchar(out$html) - 20, nchar(out$html)), "</html>", fixed = TRUE)
  expect_true(jsonlite::validate(sub(";\n$", "", sub("^const D = ", "",
    regmatches(out$html, regexpr("const D = .*?;\n", out$html))))))
})

test_that("embedded pictures stay linked to their web source (#19)", {
  m <- mini()
  png <- tempfile(fileext = ".png")
  writeBin(jsonlite::base64_dec("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="), png)
  m$nodes$image <- c(png, "https://example.org/b.png", "", "")
  m$nodes$image_source <- c("https://commons.wikimedia.org/wiki/File:A.png", "", "", "")
  # the placeholder web picture cannot be embedded: warning expected, link must survive
  expect_warning(out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL),
                 "Could not embed")
  d <- html_data(out$html)
  expect_match(d$nodes[[1]]$image, "^data:image/png")
  expect_equal(d$nodes[[1]]$image_source, "https://commons.wikimedia.org/wiki/File:A.png")
  expect_equal(d$nodes[[1]]$image_url, png)
  # no image_source given: the web image URL itself is the source link
  expect_equal(d$nodes[[2]]$image_source, "https://example.org/b.png")
  expect_equal(d$nodes[[3]]$image_source, "")
  expect_match(out$html, "Image source: <a href=", fixed = TRUE)
})

test_that("text sizes are settings passed to the HTML, with no upper limit in the viewer (#17, #18)", {
  m <- mini()
  out <- timeline_to_outputs(m$adj, m$nodes, html_file = NULL, svg_file = NULL,
                             tooltip_fontsize = 22, details_fontsize = 26)
  d <- html_data(out$html)
  expect_equal(d$tooltip_fontsize, 22)
  expect_equal(d$details_fontsize, 26)
  expect_match(out$html, 'id="ttp"', fixed = TRUE)
  expect_false(grepl("Math.min(Math.max(z, 0.7), 2.6)", out$html, fixed = TRUE))
  expect_false(grepl("W0 * 4", out$html, fixed = TRUE))
})
