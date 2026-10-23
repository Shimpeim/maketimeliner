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
