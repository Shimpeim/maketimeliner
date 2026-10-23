# Extracted from test-maketimeliner.R:266

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "maketimeliner", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
mini <- function() {
  nodes <- data.frame(id = c("a", "b", "c", "d"), time = c(2000, 2002, 2004, 2004),
                      stream = c("s1", "s2", "s1", "s2"), stringsAsFactors = FALSE)
  adj <- matrix(0, 4, 4, dimnames = list(nodes$id, nodes$id))
  adj["a", "b"] <- 1; adj["b", "c"] <- 1; adj["b", "d"] <- 2
  list(nodes = nodes, adj = adj)
}
html_data <- function(html) {
  js <- regmatches(html, regexpr("const D = .*?;\n", html))
  jsonlite::fromJSON(sub(";\n$", "", sub("^const D = ", "", js)), simplifyVector = FALSE)
}

# test -------------------------------------------------------------------------
m <- mini()
sp <- data.frame(id = "a", end = 2003, label = "State", colour = "#FF0000",
                   linetype = "10,6", source = "Text: p. 3", description = "carried by the state")
out <- timeline_to_outputs(m$adj, m$nodes, spans = sp, html_file = NULL, svg_file = NULL)
expect_match(out$svg, "class=\"tl-span\" data-id=\"a\"", fixed = TRUE)
n <- out$layout$nodes
xa <- n$x[n$id == "a"]
band <- regmatches(out$svg, regexpr("<line x1=\"[0-9.]+\" y1=\"[0-9.]+\" x2=\"[0-9.]+\"[^>]*stroke=\"#FF0000\" stroke-width=\"8\"", out$svg))
x <- as.numeric(regmatches(band, gregexpr("[0-9.]+", band))[[1]][c(1, 3)])
expect_equal(x[1], xa, tolerance = 0.01)
expect_gt(x[2], xa)
