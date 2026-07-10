# blindspot (development version)

* Added `bs_field_guide()`: reads every district-year through the paper's
  seven-signal interpretation protocol (S1-S7) and assigns a FLAG / WATCH /
  No-action verdict, degrading gracefully when the optional adjacency,
  monthly-SPI, or genomic inputs are absent.
* Added `bs_field_guide_table()` to render the field guide as a `gt` or
  `flextable` table (scan or worked-example layout, concern shading), with
  file export inferred from the extension (html / docx / pdf / rtf / png /
  pptx).
* Added `bs_field_guide_help()`, a console interpretation aid: the seven
  signals, the flag rule, common misreadings, and a live worked example
  narrated from real signal values.
* Added the `synth_field_guide` dataset -- a precomputed field guide on
  `synth_surveillance` so the help, examples, and tests run without INLA.

## blindspot 0.0.0.9000

* Initial development version
* Package framework established with core metadata and documentation structure
* Design principles documented
* S3 class specifications defined (blindspot_expected, blindspot_spi, blindspot_result)
* Function specifications documented for all 13 exported functions
* Package architecture and file structure planned