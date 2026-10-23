#' Launch the maketimeliner Shiny editor
#'
#' Opens a browser GUI in which every input table (events, links, timelines,
#' streams, windows, overlay links, phases, legend, settings) can be edited
#' and imported / exported as CSV, with a live preview of the interactive HTML
#' and downloads of HTML, SVG and the CSV set.
#'
#' @param ... Arguments passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#' @return Invisibly, the return value of [shiny::runApp()].
#' @export
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function(...) {
  for (p in c("shiny", "DT"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("The '", p, "' package is required. Install it with: install.packages('", p, "')",
           call. = FALSE)
  app_dir <- system.file("shinyapp", package = "maketimeliner")
  if (!nzchar(app_dir))
    stop("Could not locate the shinyapp directory. Try reinstalling the package.", call. = FALSE)
  shiny::runApp(app_dir, ...)
}
