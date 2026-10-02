# Run once from the repository root.
packages <- c("ARTool", "Cairo", "FSA", "cowplot", "dplyr", "emmeans", "forcats", "gganimate", "ggplot2", "ggpubr", "gifski", "magick", "magrittr", "moments", "openxlsx", "patchwork", "purrr", "readxl", "scales", "sf", "stringr", "tibble", "tidyr", "transformr", "writexl", "zoo")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
