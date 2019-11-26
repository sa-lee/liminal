# download PDFsense data
library(readr)
url <- "http://www.physics.smu.edu/botingw/PDFsense_web_histlogy/tsv.zip"
dir <- tempdir() 
zip <- file.path(dir, "pdfsense.zip")

tsv_path <- file.path(dir, "tsv", "CT14_signed")
download.file(url, destfile = zip)

unzip(zip, exdir = dir)

mat <- read_tsv(
  file.path(tsv_path, "residual_all_norm_-1_RawData.tsv"),
  col_names = FALSE
)

meta <- read_tsv(
  file.path(tsv_path, "metadata_RawData.tsv")
)

pdfsense <- as.data.frame(cbind(meta, mat))

unlink(dir, recursive = TRUE)

usethis::use_data(pdfsense, overwrite = TRUE)
