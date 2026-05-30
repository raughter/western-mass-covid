library(rmarkdown)

# Render the report
rmarkdown::render(
  input       = "wastewater_report.Rmd",
  output_file = "wastewater.html",
  output_dir  = "."
)

# Copy to docs/ for GitHub Pages
file.copy("wastewater.html", "docs/wastewater.html", overwrite = TRUE)

# Archive copy with date-stamped name
archive_file <- paste0("archive/wastewater-", Sys.Date(), ".html")
file.copy("wastewater.html", archive_file, overwrite = TRUE)
