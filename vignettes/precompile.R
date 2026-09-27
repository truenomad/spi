# knit the INLA articles to plain .Rmd, which CI renders without INLA; run
# from the package root: Rscript --no-init-file vignettes/precompile.R [name]
pkgload::load_all(quiet = TRUE)

# without a pandoc target, knitr captions every figure "plot of chunk <label>"
knitr::opts_knit$set(rmarkdown.pandoc.to = "html")

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

  # record each knitted source's checksum, so CI can tell when an article
  # was edited without being knitted again
  sums_file <- "precompiled.md5"
  sums <- if (file.exists(sums_file)) {
    utils::read.table(
      sums_file,
      col.names = c("md5", "file"),
      colClasses = "character"
    )
  } else {
    data.frame(md5 = character(), file = character())
  }

  sums <- rbind(
    sums[!sums$file %in% sources, ],
    data.frame(md5 = unname(tools::md5sum(sources)), file = sources)
  )
  sums <- sums[order(sums$file), ]

  writeLines(paste0(sums$md5, "  ", sums$file), sums_file)
})
