# maketimeliner (development version)

Every error found during development is recorded as a GitHub issue (symptom,
cause, fix steps, verification, fixing commit) and closed with a link to the
fix: <https://github.com/Shimpeim/maketimeliner/issues?q=is%3Aissue>.

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
