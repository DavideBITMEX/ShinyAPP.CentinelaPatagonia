library(usethis)
use_github()

install.packages(c("shinylive", "httpuv"))
library(shinylive)
library(httpuv)

shinylive::export(appdir = "R", destdir = "docs")

