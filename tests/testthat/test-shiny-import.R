test_that("Shiny batch import loads each uploaded diagram, even back to back (#11)", {
  skip_if_not_installed("shiny"); skip_if_not_installed("DT")
  app_dir <- system.file("shinyapp", package = "maketimeliner")
  skip_if(!nzchar(app_dir))
  root <- tempfile(); dir.create(root)
  # two diagrams that differ from the app's built-in example
  a <- kingdon_example(); a$settings$title <- "Diagram A"
  a$nodes <- a$nodes[1:6, ]; a$adj <- a$adj[1:6, 1:6]
  a$overlay <- NULL; a$overlay_edge_labels <- NULL
  b <- timeline_spec(matrix(c("", "", "1", ""), 2, dimnames = list(c("x", "y"), c("x", "y"))),
                     data.frame(id = c("x", "y"), time = c(1, 2), stream = c("s", "t")),
                     settings = list(title = "Diagram B"))
  fa <- write_timeline_csvs(a, file.path(root, "a"), "alpha")
  fb <- write_timeline_csvs(b, file.path(root, "b"), "beta")
  up <- function(f) data.frame(name = basename(f), size = file.size(f), type = "text/csv",
                               datapath = normalizePath(f), stringsAsFactors = FALSE)
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    session$setInputs(batch = up(fa))
    expect_equal(rv$prefix, "alpha"); expect_equal(nrow(rv$nodes), 6L)
    session$setInputs(batch = up(fb))          # immediately after: same second
    expect_equal(rv$prefix, "beta"); expect_equal(nrow(rv$nodes), 2L)
    expect_equal(rv$settings$title, "Diagram B")
  })
})
