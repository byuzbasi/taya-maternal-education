#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || identical(args[1], "help")) {
  cat("From the repository root, run one of:\n",
      "  Rscript --vanilla run.R primary help\n",
      "  Rscript --vanilla run.R h3 help\n",
      "  Rscript --vanilla run.R s12 output/s12\n",
      sep = "")
} else {
  scripts <- c(primary = "R/primary/run.R", h3 = "R/h3/run.R", s12 = "R/s12.R")
  if (!args[1] %in% names(scripts)) stop("Choose primary, h3, or s12.", call. = FALSE)
  path <- scripts[[args[1]]]
  if (!file.exists(path)) stop("Run this command from the repository root.", call. = FALSE)
  source(path, local = .GlobalEnv)
}
