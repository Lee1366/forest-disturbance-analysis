# Run once from RStudio; analysis scripts do not install packages.
packages <- c("ARTool", "Cairo", "cowplot", "dplyr", "gganimate", "ggplot2", "ggpubr", "gifski", "magick", "magrittr", "moments", "openxlsx", "patchwork", "purrr", "readxl", "sf", "stringr", "tibble", "tidyr", "writexl", "zoo", "transformr")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
