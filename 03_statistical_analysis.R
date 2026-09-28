required <- c("dplyr", "readr", "ARTool", "emmeans")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

input_path <- "data/classified_harvest.csv"
if (!file.exists(input_path)) stop("Run R/02_classify_disturbance.R first.")

analysis_data <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::filter(
    !is.na(delta_regular_harvest),
    !is.na(proximity),
    !is.na(ownership)
  ) |>
  dplyr::mutate(
    proximity = factor(proximity, levels = c("Other", "Neighbour", "Hotspot")),
    ownership = factor(ownership, levels = c("BK1", "BK2", "BK3"))
  )

art_model <- ARTool::art(
  delta_regular_harvest ~ proximity * ownership,
  data = analysis_data
)

anova_results <- as.data.frame(stats::anova(art_model))
anova_results$term <- rownames(anova_results)
rownames(anova_results) <- NULL
anova_results <- anova_results |>
  dplyr::relocate(term)

proximity_contrasts <- as.data.frame(ARTool::art.con(art_model, "proximity"))
ownership_contrasts <- as.data.frame(ARTool::art.con(art_model, "ownership"))

readr::write_csv(anova_results, "outputs/art_anova_results.csv")
readr::write_csv(proximity_contrasts, "outputs/proximity_contrasts.csv")
readr::write_csv(ownership_contrasts, "outputs/ownership_contrasts.csv")
message("Completed aligned rank transform analysis and post-hoc contrasts.")

