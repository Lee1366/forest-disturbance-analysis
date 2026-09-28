required <- c("dplyr", "readr", "ggplot2", "sf")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

classified_path <- "data/classified_harvest.csv"
recovery_path <- "outputs/recovery_summary.csv"
if (!file.exists(classified_path) || !file.exists(recovery_path)) {
  stop("Run the preceding scripts first.")
}

data <- readr::read_csv(classified_path, show_col_types = FALSE)
recovery <- readr::read_csv(recovery_path, show_col_types = FALSE)

theme_portfolio <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom"
  )

distribution_plot <- data |>
  dplyr::filter(salvage_harvest > 0) |>
  ggplot2::ggplot(ggplot2::aes(x = salvage_harvest)) +
  ggplot2::geom_histogram(bins = 35, fill = "#2C7FB8", color = "white") +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    title = "Distribution of synthetic salvage harvesting",
    x = "Salvage harvest (log scale)",
    y = "District-year observations"
  ) +
  theme_portfolio

response_plot <- data |>
  dplyr::filter(!is.na(delta_regular_harvest)) |>
  ggplot2::ggplot(
    ggplot2::aes(x = proximity, y = delta_regular_harvest, fill = ownership)
  ) +
  ggplot2::geom_boxplot(outlier.alpha = 0.25) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::labs(
    title = "Regular-harvest response by disturbance proximity",
    x = NULL,
    y = "Deviation from previous 10-year mean (%)",
    fill = "Ownership"
  ) +
  theme_portfolio

recovery_plot <- recovery |>
  ggplot2::ggplot(
    ggplot2::aes(x = relative_year, y = mean_deviation, color = ownership, fill = ownership)
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = mean_deviation - se_deviation,
      ymax = mean_deviation + se_deviation
    ),
    alpha = 0.15,
    color = NA
  ) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_x_continuous(breaks = -5:5) +
  ggplot2::labs(
    title = "Harvest dynamics around isolated disturbance events",
    x = "Years relative to disturbance",
    y = "Deviation from pre-disturbance mean (%)",
    color = "Ownership",
    fill = "Ownership"
  ) +
  theme_portfolio

district_index <- data |>
  dplyr::distinct(district_id, grid_row, grid_col) |>
  dplyr::arrange(grid_row, grid_col)

grid <- sf::st_make_grid(
  sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))),
  n = c(3, 3)
)
district_map <- sf::st_sf(
  district_id = district_index$district_id,
  geometry = grid
)

map_data <- data |>
  dplyr::filter(year == 2018, ownership == "BK1") |>
  dplyr::select(district_id, disturbance_class)

map_plot <- district_map |>
  dplyr::left_join(map_data, by = "district_id") |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = disturbance_class), color = "white", linewidth = 0.7) +
  ggplot2::scale_fill_manual(
    values = c(Low = "#B8E186", Moderate = "#FDDC7A", High = "#F98E52", Hotspot = "#C51B2E"),
    drop = FALSE,
    na.value = "grey90"
  ) +
  ggplot2::labs(
    title = "Synthetic district disturbance classes in 2018",
    fill = "Class"
  ) +
  ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom"
  )

ggplot2::ggsave("outputs/01_salvage_distribution.png", distribution_plot, width = 7, height = 4.5, dpi = 300)
ggplot2::ggsave("outputs/02_harvest_response.png", response_plot, width = 7, height = 4.8, dpi = 300)
ggplot2::ggsave("outputs/03_event_recovery.png", recovery_plot, width = 7, height = 4.8, dpi = 300)
ggplot2::ggsave("outputs/04_synthetic_map.png", map_plot, width = 6, height = 5.2, dpi = 300)
message("Created figures in outputs/.")

