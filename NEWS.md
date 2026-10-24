# maketimeliner (development version)

Every error found during development is recorded as a GitHub issue (symptom,
cause, fix steps, verification, fixing commit) and closed with a link to the
fix: <https://github.com/Shimpeim/maketimeliner/issues?q=is%3Aissue>.

## 2026-10-24 — no fixed font sizes in the interface

* All interface text (sidebar, pane header, buttons, picture viewer) scales with
  a new "Interface" A-/A+ control; start size from the new setting `ui_fontsize`;
  the diagram keeps its scale when the sidebar grows (#20).

## 2026-10-24 — sizes without limits; adjustable hover text; picture sources

* Removed the arbitrary limits on pane text size and diagram zoom (#17).
* Hover (tooltip) text size adjustable in the HTML ("Text size" in the sidebar);
  new settings `tooltip_fontsize`, `details_fontsize` for the start sizes (#18).
* New `image_source` column: every picture keeps a link to its web source, also
  when embedded; shown in the details pane, the picture viewer and the tooltip (#19).

## 2026-10-24 — zoomable details pane

* Details pane: larger text (15 px), A-/A+ and Ctrl/Cmd + scroll or pinch to
  zoom, resizable width (drag the edge or the double-arrow button), pictures
  fill the pane and open in a zoomable viewer on click; full title as heading (#16).

## 2026-10-24 — readable HTML viewer

* The interactive HTML now opens at a readable scale (streams fill the panel
  height; text never below ~80 %) instead of fitting the whole width; scroll
  pans, Ctrl/Cmd + scroll or pinch zooms; new +, -, "Readable size" and
  "Whole diagram" buttons (#15).

## 2026-10-24 — fix: complete HTML with large embedded pictures

* Fixed: with `embed_images = TRUE`, pages longer than one million characters
  were cut off, because `substring()` defaults to `last = 1000000` (#14).

## 2026-10-23 — durations (spans)

* New `spans` table (`_spans.csv`, Shiny "Spans" on the Events tab): an event
  can be carried over a period, drawn as a band from start to end in the
  event's stream (band colour/alpha/width + centre line pattern, e.g. the
  carrying actor). Several spans per event stack. The lane stays reserved until
  the span ends. `source` and `description` appear in the HTML tooltip and the
  event's details ("Carried").
* Ordinal axes now warn when a time value is missing from `phases` instead of
  silently appending it at the end.
* Fixed during development: a span test parsed attribute names as numbers (#13).

## 2026-10-23 — per-link styles

* New `link_props` table (`_link_props.csv`, Shiny "Link styles"): overrides
  colour, alpha, width and linetype of single links, with a label and
  description shown in the HTML tooltip — e.g. to show which actor carried a
  move between streams. New `legend_linetypes` table for a line-type legend.
* Fixed: dotted line-type samples were invisible in the legend (#12).

## 2026-10-23 — Shiny import fix

* Fixed: batch CSV upload failed when two uploads happened within the same
  second, because they shared one temporary folder; import/export folders are
  now unique and deleted afterwards (#11). New test `test-shiny-import.R`.

## 2026-10-23 — image embedding (1dd3789)

* Event pictures (`image` column) are downloaded or read at render time and
  embedded in the HTML (`embed_images = TRUE`, `image_max_mb = 5`), so tooltips
  work even when the viewer cannot reach the image host.
* Fixed: mouse-over picture showed `[image not available]` for a valid
  Wikimedia URL (#8).
* Fixed: test failure from `graphics::plot.new()` while creating a test PNG (#9).
* Fixed: test warnings from a URL test that tried to download placeholder URLs (#10).

## 2026-10-23 — mouse-over pictures (c8b7440)

* New `image` column for events: picture URL shown on mouse-over in the HTML
  tooltip and in the details panel; `tooltip_image_width` setting.

## 2026-10-23 — maketimeliner 0.1.0, first release (5348ad4)

* Multi-stream timelines from a matxingraphout-style data model; SVG +
  self-contained interactive HTML; CSV import/export; Shiny editor.
* Errors fixed during initial development, before the first push:
  * `print()` failed because `nzchar()` drops matrix dimensions (#1).
  * An event was placed on top of another timeline's link (#2).
  * Timeline name labels overlapped at shared end events (#3).
  * Shiny batch import failed when the prefix input was `NULL` (#4).
  * Shiny Links list view edited whole columns instead of cells (#5).
  * `R CMD check` warning: `inst/html` is reserved (#6).
  * `R CMD check` warning: non-ASCII character in `R/render.R` (#7).
