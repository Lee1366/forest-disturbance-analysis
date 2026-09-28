# Forest Disturbance and Harvest Dynamics in Austria

This portfolio project demonstrates an R workflow for studying how severe forest disturbances are associated with changes in regular timber harvesting across Austrian forest districts and ownership groups.

The workflow is based on methods developed during a BOKU University research project. To protect unpublished and project-specific data, the included dataset is **synthetic**. It reproduces the structure needed to demonstrate data preparation, disturbance classification, statistical analysis, event-based recovery analysis, spatial joins and scientific visualisation.

## Research questions

1. How can district-years with exceptionally high salvage logging be identified?
2. Do regular-harvest responses differ between disturbance hotspots, neighbouring districts and other districts?
3. Do these responses vary among ownership groups?
4. How does harvesting develop before and after an isolated disturbance event?

## Workflow

The scripts are numbered in execution order:

1. `00_generate_example_data.R` creates a reproducible synthetic dataset.
2. `01_prepare_data.R` validates, aggregates and prepares the data.
3. `02_classify_disturbance.R` identifies hotspots and neighbouring districts.
4. `03_statistical_analysis.R` performs aligned rank transform analysis and post-hoc comparisons.
5. `04_post_disturbance_recovery.R` conducts an event-time recovery analysis.
6. `05_figures_and_maps.R` creates scientific figures and a synthetic spatial map.

## Main variables

- `district_id`: synthetic forest district identifier
- `year`: observation year
- `ownership`: `BK1`, `BK2` or `BK3`
- `total_harvest`: total reported timber harvest
- `salvage_harvest`: disturbance-related harvest
- `regular_harvest`: total harvest minus salvage harvest
- `delta_regular_harvest`: percentage deviation from the preceding ten-year mean
- `disturbance_class`: low, moderate, high or hotspot
- `proximity`: hotspot, neighbouring district or other district

## Methods demonstrated

- reproducible data generation and validation
- grouped aggregation and missing-value checks
- rolling historical baselines
- percentile-based hotspot classification
- spatial neighbourhood classification
- aligned rank transform analysis and post-hoc contrasts
- event-time analysis around isolated disturbance years
- publication-style figures and an `sf` map

## Requirements

Install the required packages once:

```r
install.packages(c(
  "dplyr", "tidyr", "readr", "ggplot2", "sf",
  "ARTool", "emmeans"
))
```

## Run the project

Open R or RStudio in the repository folder and run:

```r
source("run_all.R")
```

The workflow creates `data/` and `outputs/` automatically.

## Data note

The generated data are synthetic and must not be interpreted as official Austrian forestry statistics or as research results. This repository demonstrates analytical skills and reproducible workflow design.

## Author

Nazli Golestani  
Environmental researcher and data analyst  
[GitHub profile](https://github.com/Lee1366)

