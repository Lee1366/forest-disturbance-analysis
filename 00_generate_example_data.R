required <- c("dplyr", "tidyr", "readr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

set.seed(1366)

districts <- tibble::tibble(
  district_id = sprintf("D%02d", 1:9),
  state = rep(c("North", "Central", "South"), each = 3),
  grid_row = rep(1:3, each = 3),
  grid_col = rep(1:3, times = 3)
)

events <- tibble::tribble(
  ~district_id, ~event_year,
  "D02", 2007L,
  "D05", 2013L,
  "D08", 2018L
)

example_data <- tidyr::crossing(
  district_id = districts$district_id,
  year = 2000:2024,
  ownership = c("BK1", "BK2", "BK3")
) |>
  dplyr::left_join(districts, by = "district_id") |>
  dplyr::left_join(events, by = "district_id") |>
  dplyr::mutate(
    district_number = as.integer(sub("D", "", district_id)),
    ownership_factor = dplyr::recode(ownership, BK1 = 0.90, BK2 = 1.15, BK3 = 1.35),
    trend = 1 + 0.006 * (year - 2000),
    baseline = (7800 + district_number * 430) * ownership_factor * trend,
    event_distance = abs(year - event_year),
    disturbance_multiplier = dplyr::case_when(
      event_distance == 0 ~ 5.5,
      event_distance == 1 ~ 2.0,
      TRUE ~ 1.0
    ),
    salvage_harvest = pmax(
      0,
      stats::rlnorm(dplyr::n(), log(900), 0.65) * disturbance_multiplier
    ),
    regular_response = dplyr::case_when(
      year == event_year ~ -0.18,
      year == event_year + 1L ~ -0.10,
      year == event_year + 2L ~ -0.04,
      TRUE ~ 0
    ),
    regular_harvest_true = pmax(
      500,
      baseline * (1 + regular_response + stats::rnorm(dplyr::n(), 0, 0.07))
    ),
    total_harvest = round(regular_harvest_true + salvage_harvest, 1),
    salvage_harvest = round(salvage_harvest, 1)
  ) |>
  dplyr::select(
    district_id, state, grid_row, grid_col, year, ownership,
    total_harvest, salvage_harvest
  )

readr::write_csv(example_data, "data/example_forest_harvest.csv")
message("Created data/example_forest_harvest.csv")

