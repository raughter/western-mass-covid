library(rmarkdown)

rmarkdown::render(
  input       = "wastewater_report.Rmd",
  output_file = "wastewater.html",
  output_dir  = "."
)

file.copy("wastewater.html", "docs/wastewater.html", overwrite = TRUE)
archive_file <- paste0("archive/wastewater-", Sys.Date(), ".html")
file.copy("wastewater.html", archive_file, overwrite = TRUE)
