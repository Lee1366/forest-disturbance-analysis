scripts <- c(
  "00_generate_example_data.R",
  "01_prepare_data.R",
  "02_classify_disturbance.R",
  "03_statistical_analysis.R",
  "04_post_disturbance_recovery.R",
  "05_figures_and_maps.R"
)

invisible(lapply(scripts, source))

message("Workflow complete. See the generated data/ and outputs/ folders.")

