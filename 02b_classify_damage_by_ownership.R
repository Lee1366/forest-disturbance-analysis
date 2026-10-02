local({
# Cleaned copy of BK.R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "writexl", "sf", "purrr", "openxlsx", "ggplot2", "tibble", "magick")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(purrr)
library(openxlsx)
library(ggplot2)
library(tibble)
library(magick)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "damage_classification_by_ownership")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

count_or_zero <- function(counts, category) {
  value <- unname(counts[category])
  if (!length(value) || is.na(value)) 0 else value
}

##BK category for hot spots
##25.06.2025

# Install these packages if you haven't


shapefile_path <- file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp")
austria_districts <- st_read(shapefile_path)

# 3. Load data and filter BK1
df <- read_excel(file.path(project_root, "data", "Final_data_2000_2024.xlsx"), sheet = "final2")
df_BK1 <- df %>% filter(BK == 1)

# 4. Aggregate data for BK1 by district and year
aggregated_BK1 <- df_BK1 %>%
  group_by(ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(
    ZUORDNUNG = first(ZUORDNUNG),
    BUNDESLAND_NAME = first(BUNDESLAND_NAME),
    Total_Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE),
    Total_Schadholz = sum(Schadholz, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Damage classification
total_rows <- nrow(aggregated_BK1)
hotspot_n     <- ceiling(total_rows * 0.10)
highdamage_n  <- ceiling(total_rows * 0.30)
moderate_n    <- ceiling(total_rows * 0.30)
lowdamage_n   <- total_rows - (hotspot_n + highdamage_n + moderate_n)

classified_BK1 <- aggregated_BK1 %>%
  arrange(desc(Total_Schadholz)) %>%
  mutate(
    Damage_Class = case_when(
      row_number() <= hotspot_n ~ "Hot Spot",
      row_number() <= hotspot_n + highdamage_n ~ "High Damage",
      row_number() <= hotspot_n + highdamage_n + moderate_n ~ "Moderate Damage",
      TRUE ~ "Low Damage"
    )
  )

###########################################
##saving

# Define path to save the Excel file
file_path <- file.path(project_root, "outputs", "2000_24_damage_BK.xlsx")

# Create a new workbook
wb <- createWorkbook()

# Add a sheet with the classified BK1 data
addWorksheet(wb, "BK1_Damage_Classes")
writeData(wb, sheet = "BK1_Damage_Classes", classified_BK1)

# Save the workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)
#########################################

# Working directory is configured in the header.
# 6. Merge with shapefile
merged_data <- austria_districts %>%
  left_join(classified_BK1, by = c("g_name" = "ERHEBUNGSBEZIRK"))

# 7. Loop through each year and save maps
years <- 2000:2024


for (yr in years) {
  message("Saving map for year: ", yr)
  
  yearly_data <- merged_data %>% filter(JAHR == yr)
  
  # Ensure proper order of factor
  yearly_data$Damage_Class <- factor(
    yearly_data$Damage_Class,
    levels = c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  )
  
  counts <- yearly_data %>%
    group_by(Damage_Class) %>%
    summarise(n = n_distinct(g_name)) %>%
    deframe()
  
  labels_with_counts <- c(
    paste0("Hot Spot (", count_or_zero(counts, "Hot Spot"), ")"),
    paste0("High Damage (", count_or_zero(counts, "High Damage"), ")"),
    paste0("Moderate Damage (", count_or_zero(counts, "Moderate Damage"), ")"),
    paste0("Low Damage (", count_or_zero(counts, "Low Damage"), ")")
  )
  names(labels_with_counts) <- c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  
  yearly_plot <- ggplot(yearly_data) +
    geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
    geom_sf_text(aes(label = g_name), size = 1.5, color = "black") +
    scale_fill_manual(
      values = c(
        "Low Damage" = "palegreen",
        "Moderate Damage" = "#fec43f",
        "High Damage" = "coral",
        "Hot Spot" = "firebrick2"
      ),
      labels = labels_with_counts,
      drop = FALSE,
      guide = guide_legend(reverse = FALSE)
    ) +
    labs(
      title = paste("Damage Classification (BK1)", yr),
      fill = "Damage Level"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 12),
      legend.text = element_text(),
      legend.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Save each map as PNG
  ggsave(
    filename = paste0("damage_map_BK1_", yr, ".png"),
    plot = yearly_plot,
    width = 7, height = 6, dpi = 300, bg = "white"
  )
}

# 8. Combine images into one big mosaic
image_list <- lapply(years, function(yr) {
  image_read(paste0("damage_map_BK1_", yr, ".png"))
})

combined_image <- image_montage(
  image_join(image_list),
  tile = "5x5",
  geometry = "600x500+2+2"
)

# Save the combined map
image_write(combined_image, path = "combined_damage_maps_BK1.png", format = "png")
##############################################
################################################
###############################################
##BK2

# Working directory is configured in the header.
# Load shapefile
shapefile_path <- file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp")
austria_districts <- st_read(shapefile_path)

# Load harvest data
df <- read_excel(file.path(project_root, "data", "Final_data_2000_2024.xlsx"), sheet = "final2")


# Filter BK2
df_BK2 <- df %>% filter(BK == 2)

# Aggregate data and compute damage class thresholds
aggregated_df_BK2 <- df_BK2 %>%
  group_by(ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(
    ZUORDNUNG = first(ZUORDNUNG),
    BUNDESLAND_NAME = first(BUNDESLAND_NAME),
    Total_Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE),
    Total_Schadholz = sum(Schadholz, na.rm = TRUE),
    .groups = "drop"
  )

# Compute thresholds
total_rows <- nrow(aggregated_df_BK2)
hotspot_n     <- ceiling(total_rows * 0.10)
highdamage_n  <- ceiling(total_rows * 0.30)
moderate_n    <- ceiling(total_rows * 0.30)
lowdamage_n   <- total_rows - (hotspot_n + highdamage_n + moderate_n)

# Assign damage class
aggregated_df_BK2 <- aggregated_df_BK2 %>%
  arrange(desc(Total_Schadholz)) %>%
  mutate(
    Damage_Class = case_when(
      row_number() <= hotspot_n ~ "Hot Spot",
      row_number() <= hotspot_n + highdamage_n ~ "High Damage",
      row_number() <= hotspot_n + highdamage_n + moderate_n ~ "Moderate Damage",
      TRUE ~ "Low Damage"
    )
  )
#############################
##saving

file_path <- file.path(project_root, "outputs", "2000_24_damage_BK.xlsx")

# Load the existing workbook
wb <- loadWorkbook(file_path)

# Add BK2 data to a new sheet
addWorksheet(wb, "BK2_Damage_Classes")
writeData(wb, sheet = "BK2_Damage_Classes", aggregated_df_BK2)

# Save the updated workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)


##########################################

merged_data_BK2 <- austria_districts %>%
  left_join(aggregated_df_BK2, by = c("g_name" = "ERHEBUNGSBEZIRK"))

years <- 2000:2024

for (yr in years) {
  message("Saving map for year: ", yr)
  
  yearly_data <- merged_data_BK2 %>% filter(JAHR == yr)
  
  yearly_data$Damage_Class <- factor(
    yearly_data$Damage_Class,
    levels = c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  )
  
  counts <- yearly_data %>%
    group_by(Damage_Class) %>%
    summarise(n = n_distinct(g_name)) %>%
    deframe()
  
  labels_with_counts <- c(
    paste0("Hot Spot (", count_or_zero(counts, "Hot Spot"), ")"),
    paste0("High Damage (", count_or_zero(counts, "High Damage"), ")"),
    paste0("Moderate Damage (", count_or_zero(counts, "Moderate Damage"), ")"),
    paste0("Low Damage (", count_or_zero(counts, "Low Damage"), ")")
  )
  names(labels_with_counts) <- c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  
  yearly_plot <- ggplot(yearly_data) +
    geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
    geom_sf_text(aes(label = g_name), size = 1.5, color = "black") +
    scale_fill_manual(
      values = c(
        "Low Damage" = "palegreen",
        "Moderate Damage" = "#fec43f",
        "High Damage" = "coral",
        "Hot Spot" = "firebrick2"
      ),
      labels = labels_with_counts,
      drop = FALSE
    ) +
    labs(
      title = paste("Damage Classification BK2 (Großwald)", yr),
      fill = "Damage Level"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 12),
      legend.text = element_text(),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white")
    )
  
  ggsave(
    filename = paste0("damage_map_BK2_", yr, ".png"),
    plot = yearly_plot,
    width = 7, height = 6, dpi = 300, bg = "white"
  )
}


########

image_list <- lapply(years, function(yr) {
  image_read(paste0("damage_map_BK2_", yr, ".png"))
})

combined_image <- image_montage(
  image_join(image_list),
  tile = "5x5",
  geometry = "600x500+2+2"
)

image_write(combined_image, path = "combined_damage_maps_BK2.png", format = "png")

###############################################
##############################################
##############################################
##BK3

# Working directory is configured in the header.
# Shapefile
shapefile_path <- file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp")
austria_districts <- st_read(shapefile_path)

# Excel data
df <- read_excel(file.path(project_root, "data", "Final_data_2000_2024.xlsx"), sheet = "final2")


####
# Filter BK3 (ÖBF)
df_BK3 <- df %>% filter(BK == 3)

# Aggregate by district and year
aggregated_df_BK3 <- df_BK3 %>%
  group_by(ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(
    ZUORDNUNG = first(ZUORDNUNG),
    BUNDESLAND_NAME = first(BUNDESLAND_NAME),
    Total_Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE),
    Total_Schadholz = sum(Schadholz, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate thresholds
total_rows <- nrow(aggregated_df_BK3)
hotspot_n     <- ceiling(total_rows * 0.10)
highdamage_n  <- ceiling(total_rows * 0.30)
moderate_n    <- ceiling(total_rows * 0.30)
lowdamage_n   <- total_rows - (hotspot_n + highdamage_n + moderate_n)

# Assign Damage_Class
aggregated_df_BK3 <- aggregated_df_BK3 %>%
  arrange(desc(Total_Schadholz)) %>%
  mutate(
    Damage_Class = case_when(
      row_number() <= hotspot_n ~ "Hot Spot",
      row_number() <= hotspot_n + highdamage_n ~ "High Damage",
      row_number() <= hotspot_n + highdamage_n + moderate_n ~ "Moderate Damage",
      TRUE ~ "Low Damage"
    )
  )


###################################
##saving


file_path <- file.path(project_root, "outputs", "2000_24_damage_BK.xlsx")

# Load existing workbook
wb <- loadWorkbook(file_path)

# Add new worksheet for BK3
addWorksheet(wb, "BK3_Damage_Classes")

# Write BK3 data to the new sheet
writeData(wb, sheet = "BK3_Damage_Classes", aggregated_df_BK3)

# Save the updated file
saveWorkbook(wb, file = file_path, overwrite = TRUE)


###########################################
#######
merged_data_BK3 <- austria_districts %>%
  left_join(aggregated_df_BK3, by = c("g_name" = "ERHEBUNGSBEZIRK"))
####


years <- 2000:2024

for (yr in years) {
  message("Saving map for year: ", yr)
  
  yearly_data <- merged_data_BK3 %>% filter(JAHR == yr)
  
  yearly_data$Damage_Class <- factor(
    yearly_data$Damage_Class,
    levels = c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  )
  
  counts <- yearly_data %>%
    group_by(Damage_Class) %>%
    summarise(n = n_distinct(g_name)) %>%
    deframe()
  
  labels_with_counts <- c(
    paste0("Hot Spot (", count_or_zero(counts, "Hot Spot"), ")"),
    paste0("High Damage (", count_or_zero(counts, "High Damage"), ")"),
    paste0("Moderate Damage (", count_or_zero(counts, "Moderate Damage"), ")"),
    paste0("Low Damage (", count_or_zero(counts, "Low Damage"), ")")
  )
  names(labels_with_counts) <- c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  
  yearly_plot <- ggplot(yearly_data) +
    geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
    geom_sf_text(aes(label = g_name), size = 1.5, color = "black") +
    scale_fill_manual(
      values = c(
        "Low Damage" = "palegreen",
        "Moderate Damage" = "#fec43f",
        "High Damage" = "coral",
        "Hot Spot" = "firebrick2"
      ),
      labels = labels_with_counts,
      drop = FALSE
    ) +
    labs(
      title = paste("Damage Classification BK3 (ÖBF)", yr),
      fill = "Damage Level"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 12),
      legend.text = element_text(),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white")
    )
  
  ggsave(
    filename = paste0("damage_map_BK3_", yr, ".png"),
    plot = yearly_plot,
    width = 7, height = 6, dpi = 300, bg = "white"
  )
}

#########


image_list <- lapply(years, function(yr) {
  image_read(paste0("damage_map_BK3_", yr, ".png"))
})

combined_image <- image_montage(
  image_join(image_list),
  tile = "5x5",
  geometry = "600x500+2+2"
)

image_write(combined_image, path = "combined_damage_maps_BK3.png", format = "png")
#############################################

})
