required <- c("dplyr", "tidyr", "readr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

input_path <- "data/classified_harvest.csv"
if (!file.exists(input_path)) stop("Run R/02_classify_disturbance.R first.")

data <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::mutate(is_hotspot = disturbance_class == "Hotspot")

identify_single_year_events <- function(x) {
  x |>
    dplyr::arrange(year) |>
    dplyr::mutate(
      new_episode = is_hotspot & !dplyr::lag(is_hotspot, default = FALSE),
      episode_id = cumsum(new_episode)
    ) |>
    dplyr::filter(is_hotspot) |>
    dplyr::group_by(episode_id) |>
    dplyr::filter(dplyr::n() == 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(event_year = year)
}

events <- data |>
  dplyr::group_by(district_id, ownership) |>
  dplyr::group_modify(~ identify_single_year_events(.x)) |>
  dplyr::ungroup()

event_data <- data |>
  dplyr::inner_join(events, by = c("district_id", "ownership"), relationship = "many-to-many") |>
  dplyr::mutate(relative_year = year - event_year) |>
  dplyr::group_by(district_id, ownership, event_year) |>
  dplyr::mutate(
    baseline_n = sum(relative_year %in% -10:-1 & !is.na(regular_harvest)),
    baseline_regular = mean(regular_harvest[relative_year %in% -10:-1], na.rm = TRUE),
    deviation_pct = dplyr::if_else(
      baseline_n >= 8L & is.finite(baseline_regular) & baseline_regular > 0,
      100 * (regular_harvest - baseline_regular) / baseline_regular,
      NA_real_
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(relative_year >= -5L, relative_year <= 5L)

recovery_summary <- event_data |>
  dplyr::filter(!is.na(deviation_pct)) |>
  dplyr::group_by(ownership, relative_year) |>
  dplyr::summarise(
    mean_deviation = mean(deviation_pct),
    sd_deviation = stats::sd(deviation_pct),
    n = dplyr::n(),
    se_deviation = sd_deviation / sqrt(n),
    .groups = "drop"
  )

wilcoxon_results <- event_data |>
  dplyr::filter(relative_year %in% 1:3, !is.na(deviation_pct)) |>
  dplyr::group_by(ownership, relative_year) |>
  dplyr::summarise(
    n = dplyr::n(),
    median_deviation = stats::median(deviation_pct),
    p_value = if (dplyr::n() >= 3L) {
      stats::wilcox.test(deviation_pct, mu = 0, exact = FALSE)$p.value
    } else {
      NA_real_
    },
    .groups = "drop"
  )

readr::write_csv(event_data, "data/event_time_harvest.csv")
readr::write_csv(recovery_summary, "outputs/recovery_summary.csv")
readr::write_csv(wilcoxon_results, "outputs/recovery_wilcoxon_tests.csv")
message("Completed isolated-event recovery analysis.")

