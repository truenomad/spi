# knit the INLA articles to plain .Rmd, which CI renders without INLA; run
# from the package root: Rscript --no-init-file vignettes/precompile.R [name]
pkgload::load_all(quiet = TRUE)

sources <- list.files("vignettes", pattern = "[.]Rmd[.]orig$")
wanted <- commandArgs(trailingOnly = TRUE)

if (length(wanted) > 0) {
  sources <- sources[sub("[.]Rmd[.]orig$", "", sources) %in% wanted]
}

withr::with_dir("vignettes", {
  for (src in sources) {
    knitr::knit(
      src,
      output = sub("[.]orig$", "", src),
      envir = new.env(),
      quiet = TRUE
    )
  }
})
