#' Example: an illustrative Multiple Streams (Kingdon) diagram
#'
#' Reads the synthetic example shipped in `inst/extdata/kingdon/`: three
#' streams (problems, policies, politics), four timelines (an issue, a
#' proposal, a policy entrepreneur, the national mood) that cross streams and
#' converge in a policy window. The case is invented for illustration.
#'
#' @return A `timeline_spec`.
#' @export
#' @examples
#' spec <- kingdon_example()
#' \dontrun{
#' render_timeline(spec, "kingdon.html", "kingdon.svg")
#' }
kingdon_example <- function() {
  read_timeline_csvs(system.file("extdata", "kingdon", package = "maketimeliner"))
}
