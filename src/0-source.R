# Source.R
# 
# Setting paths and formatting

# Paths
path = file.path("~/Dropbox", "Ethics Data Analysis", "Stage 2 Analyses")
result_path = file.path(path, "results")

# Plot formatting
theme_set(
  theme_bw(10) + 
    theme(
      panel.grid.minor = element_blank(), 
      axis.ticks.y = element_blank(),
      plot.title=element_text(size = 12, hjust = 0, face = "italic"),
      plot.title.position = "plot",
      plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
      plot.caption=element_text(size=8, margin=margin(t=8), color="#7a7d7e"), 
      legend.position = "right", 
      legend.background = element_blank(),   
      legend.key = element_blank(), 
      strip.background = element_rect(fill = "#2C3E50"), 
      strip.text = element_text(color = "white")
    )
)

# Table formatting
options(knitr.kable.NA = '')

# helper functions

#' Function to save CLMM results to dropbox 
save_results = function(obj) {
  file_path = sprintf("%s/%s.RDS", file.path(result_path, "models"), deparse(substitute(obj)))
  saveRDS(obj, file = file.path(file_path))
}

#' Function to read CLMM results from dropbox
read_result = function (f) {
  readRDS(sprintf("%s/%s.RDS", file.path(result_path, "models"), f))
}