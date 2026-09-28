required <- c("dplyr", "readr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

input_path <- "data/example_forest_harvest.csv"
if (!file.exists(input_path)) stop("Run R/00_generate_example_data.R first.")

raw <- readr::read_csv(input_path, show_col_types = FALSE)

required_columns <- c(
  "district_id", "state", "grid_row", "grid_col", "year", "ownership",
  "total_harvest", "salvage_harvest"
)
missing_columns <- setdiff(required_columns, names(raw))
if (length(missing_columns)) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "))
}

previous_n_mean <- function(x, n = 10L) {
  vapply(seq_along(x), function(i) {
    previous <- x[seq_len(i - 1L)]
    previous <- utils::tail(previous, n)
    if (length(previous) < n || all(is.na(previous))) NA_real_ else mean(previous, na.rm = TRUE)
  }, numeric(1))
}

prepared <- raw |>
  dplyr::mutate(
    year = as.integer(year),
    ownership = factor(ownership, levels = c("BK1", "BK2", "BK3")),
    total_harvest = as.numeric(total_harvest),
    salvage_harvest = as.numeric(salvage_harvest)
  ) |>
  dplyr::group_by(district_id, state, grid_row, grid_col, year, ownership) |>
  dplyr::summarise(
    total_harvest = sum(total_harvest, na.rm = TRUE),
    salvage_harvest = sum(salvage_harvest, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(regular_harvest = pmax(total_harvest - salvage_harvest, 0)) |>
  dplyr::group_by(district_id, ownership) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    previous_10_year_mean = previous_n_mean(regular_harvest, 10L),
    delta_regular_harvest = dplyr::if_else(
      !is.na(previous_10_year_mean) & previous_10_year_mean > 0,
      100 * (regular_harvest - previous_10_year_mean) / previous_10_year_mean,
      NA_real_
    )
  ) |>
  dplyr::ungroup()

quality_summary <- tibble::tibble(
  rows = nrow(prepared),
  districts = dplyr::n_distinct(prepared$district_id),
  first_year = min(prepared$year),
  last_year = max(prepared$year),
  missing_total_harvest = sum(is.na(prepared$total_harvest)),
  missing_delta = sum(is.na(prepared$delta_regular_harvest))
)

readr::write_csv(prepared, "data/prepared_harvest.csv")
readr::write_csv(quality_summary, "outputs/data_quality_summary.csv")
message("Prepared data and wrote quality-control summary.")

