required <- c("dplyr", "readr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

input_path <- "data/prepared_harvest.csv"
if (!file.exists(input_path)) stop("Run R/01_prepare_data.R first.")

data <- readr::read_csv(input_path, show_col_types = FALSE)

district_year <- data |>
  dplyr::group_by(district_id, state, grid_row, grid_col, year) |>
  dplyr::summarise(
    total_harvest = sum(total_harvest, na.rm = TRUE),
    salvage_harvest = sum(salvage_harvest, na.rm = TRUE),
    .groups = "drop"
  )

positive_damage <- district_year$salvage_harvest[district_year$salvage_harvest > 0]
cutoffs <- stats::quantile(positive_damage, probs = c(0.50, 0.75, 0.90), na.rm = TRUE)

district_year <- district_year |>
  dplyr::mutate(
    disturbance_class = dplyr::case_when(
      salvage_harvest >= cutoffs[[3]] ~ "Hotspot",
      salvage_harvest >= cutoffs[[2]] ~ "High",
      salvage_harvest >= cutoffs[[1]] ~ "Moderate",
      TRUE ~ "Low"
    )
  )

hotspots <- district_year |>
  dplyr::filter(disturbance_class == "Hotspot") |>
  dplyr::select(year, hotspot_id = district_id, hotspot_row = grid_row, hotspot_col = grid_col)

proximity <- district_year |>
  dplyr::select(district_id, year, grid_row, grid_col, disturbance_class) |>
  dplyr::left_join(hotspots, by = "year", relationship = "many-to-many") |>
  dplyr::mutate(
    is_neighbour = abs(grid_row - hotspot_row) + abs(grid_col - hotspot_col) == 1L
  ) |>
  dplyr::group_by(district_id, year, grid_row, grid_col, disturbance_class) |>
  dplyr::summarise(
    proximity = dplyr::case_when(
      disturbance_class == "Hotspot" ~ "Hotspot",
      any(is_neighbour, na.rm = TRUE) ~ "Neighbour",
      TRUE ~ "Other"
    ),
    .groups = "drop"
  )

classified <- data |>
  dplyr::left_join(
    proximity |>
      dplyr::select(district_id, year, disturbance_class, proximity),
    by = c("district_id", "year")
  ) |>
  dplyr::mutate(
    disturbance_class = factor(
      disturbance_class,
      levels = c("Low", "Moderate", "High", "Hotspot"),
      ordered = TRUE
    ),
    proximity = factor(proximity, levels = c("Other", "Neighbour", "Hotspot"))
  )

readr::write_csv(classified, "data/classified_harvest.csv")
readr::write_csv(
  tibble::tibble(
    percentile = c("50th", "75th", "90th"),
    salvage_harvest_cutoff = as.numeric(cutoffs)
  ),
  "outputs/disturbance_thresholds.csv"
)
message("Classified disturbance severity and spatial proximity.")

