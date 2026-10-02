local({
# Cleaned copy of category_for_damage.R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "writexl", "sf", "ggplot2", "tibble", "magick", "purrr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(ggplot2)
library(tibble)
library(magick)
library(purrr)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "damage_classification")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

count_or_zero <- function(counts, category) {
  value <- unname(counts[category])
  if (!length(value) || is.na(value)) 0 else value
}

##10% hot spot, 30% high damages, 30% medium, 30% low
##04-06.2025 
#17.06.2025

# Working directory is configured in the header.
# Working directory is configured in the header.
# Install these packages if haven't


df <- read_excel(file.path(project_root, "data", "Final_data_2000_2024.xlsx"), sheet = "final2")

# View the first few rows
head(df)

# Number of total observations
total_rows <- nrow(df)

## sum up the BK for districts

aggregated_df <- df %>%
  group_by(ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(
    Total_Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE),
    Total_Schadholz = sum(Schadholz, na.rm = TRUE),
    .groups = "drop"
  )


total_rows <- nrow(aggregated_df)

# Calculate breakpoints
hotspot_n     <- ceiling(total_rows * 0.10)                      # Top 10%
highdamage_n  <- ceiling(total_rows * 0.30)                      # Next 30%
moderate_n    <- ceiling(total_rows * 0.30)                      # Next 30%
lowdamage_n   <- total_rows - (hotspot_n + highdamage_n + moderate_n)  # Remaining ~30%


# Sort and classify all rows

aggregated_df <- df %>%
  group_by(ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(
    ZUORDNUNG = first(ZUORDNUNG),
    BUNDESLAND_NAME = first(BUNDESLAND_NAME),
    Total_Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE),
    Total_Schadholz = sum(Schadholz, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Schadholz)) %>%
  mutate(
    Damage_Class = case_when(
      row_number() <= hotspot_n ~ "Hot Spot",
      row_number() <= hotspot_n + highdamage_n ~ "High Damage",
      row_number() <= hotspot_n + highdamage_n + moderate_n ~ "Moderate Damage",
      TRUE ~ "Low Damage"
    )
  )


# View the result
table(aggregated_df$Damage_Class)


# Install writexl if not already installe


#write_xlsx(aggregated_df, file.path(project_root, "outputs", "final_damage_withoutBK.xlsx"))

# Working directory is configured in the header.
write_xlsx(aggregated_df, file.path(project_root, "outputs", "2000_24_damage_withoutBK.xlsx"))

# Create a vector of all years you want to check
all_years <- 2000:2024

# Extract the years where at least one district was a Hot Spot
#hotspot_years <- df %>%
  #filter(Damage_Class == "Hot Spot") %>%
  #pull(JAHR) %>%
  #unique()

# Find which years are NOT in the hotspot data
#years_without_hotspot <- setdiff(all_years, hotspot_years)

# Print the result
#years_without_hotspot


hotspot_counts <- aggregated_df %>%
  filter(Damage_Class == "Hot Spot") %>%
  group_by(JAHR) %>%
  summarise(Hot_Spot_Count = n()) %>%
  arrange(JAHR)

# Show the result
print(hotspot_counts)

print(hotspot_counts, n = 25)

write_xlsx(hotspot_counts, file.path(project_root, "outputs", "new_hotspot_counts_per_year_withoutBK2.xlsx"))

#####################
##count each district only once per year (now it is the same as above)

hotspot_counts_unique <- aggregated_df %>%
  filter(Damage_Class == "Hot Spot") %>%
  distinct(JAHR, ERHEBUNGSBEZIRK) %>%  # Only keep one row per district per year
  group_by(JAHR) %>%
  summarise(Hot_Spot_Count = n()) %>%
  arrange(JAHR)

# Show the result
print(hotspot_counts_unique, n = 25)

######################


write_xlsx(hotspot_counts_unique, file.path(project_root, "outputs", "hotspot_counts_per_year.xlsx"))
#################################################
##map


shapefile_path <- file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp")
austria_districts <- st_read(shapefile_path)

#damage file excel

damage_data <- read_excel(file.path(project_root, "outputs", "2000_24_damage_withoutBK.xlsx"))

#Merging

  merged_data <- austria_districts %>%
    left_join(damage_data, by = c("g_name" = "ERHEBUNGSBEZIRK"))
  
  class(merged_data)

#to see table
table(merged_data$JAHR)
###########################################################
##2023 and 24 as example


merged_data$Damage_Class <- factor(
  merged_data$Damage_Class,
  levels = c("Low Damage", "Moderate Damage", "High Damage", "Hot Spot")
)


example_years <- merged_data %>%
  filter(JAHR %in% c(2023, 2024))


ggplot(example_years) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
  geom_sf_text(aes(label = g_name), size = 2, check_overlap = TRUE) +
  scale_fill_manual(
    values = c(
      "Low Damage" = "palegreen",
      "Moderate Damage" = "#fec43f",
      "High Damage" = "coral",
      "Hot Spot" = "firebrick2"
    ),
    na.value = "grey90"
  ) +
  facet_wrap(~ JAHR) +
  labs(
    title = "Forest Damage Categories by District (2023 & 2024)",
    fill = "Damage Level"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )

#######################################
##only hot spots for 2023


# Filter to just hotspots in 2023
hotspots_2023 <- merged_data %>%
  filter(JAHR == 2023, Damage_Class == "Hot Spot")

# Plot only hotspots with district names
ggplot(hotspots_2023) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
  geom_sf_text(aes(label = g_name), size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Hot Spot" = "firebrick2")) +
  labs(
    title = "Hot Spot Districts in 2023",
    fill = "Damage Level"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )


#########################################
#25 years, takes toooooo long


merged_data$Damage_Class <- factor(
  merged_data$Damage_Class,
  levels = c("Low Damage", "Moderate Damage", "High Damage", "Hot Spot")
)


ggplot(merged_data) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c(
      "Low Damage" = "palegreen",
      "Moderate Damage" = "#fec43f",
      "High Damage" = "coral",
      "Hot Spot" = "firebrick2"
    ),
    na.value = "grey90"
  ) +
  facet_wrap(~ JAHR, ncol = 5) +
  labs(
    title = "Forest Damage Categories by District (2000–2024)",
    fill = "Damage Level"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    legend.position = "bottom"
  )
################################################################

##################### i used this one####################


# Working directory is configured in the header.
years <- 2000:2024

for (yr in years) {
  message("Saving map for year: ", yr)
  
  yearly_data <- merged_data %>% filter(JAHR == yr)
  
  # Ensure consistent factor order
  yearly_data$Damage_Class <- factor(
    yearly_data$Damage_Class,
    levels = c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  )
  
  # Count how many districts are in each class (based on ERHEBUNGSBEZIRK, not BK)
  counts <- yearly_data %>%
    group_by(Damage_Class) %>%
    summarise(n = n_distinct(g_name)) %>%
    deframe()
  
  # Build named vector with counts in the labels
  labels_with_counts <- c(
    paste0("Hot Spot (", count_or_zero(counts, "Hot Spot"), ")"),
    paste0("High Damage (", count_or_zero(counts, "High Damage"), ")"),
    paste0("Moderate Damage (", count_or_zero(counts, "Moderate Damage"), ")"),
    paste0("Low Damage (", count_or_zero(counts, "Low Damage"), ")")
  )
  names(labels_with_counts) <- c("Hot Spot", "High Damage", "Moderate Damage", "Low Damage")
  
  yearly_plot <- ggplot(yearly_data) +
    geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
    geom_sf_text(aes(label = g_name), size = 1.5, color = "black") +  # district names
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
      title = paste("Damage Classification", yr),
      fill = "Damage Level"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(color = "black", size = 12),
      legend.text = element_text(color = "black"),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(
    filename = paste0("damage_map_", yr, ".png"),
    plot = yearly_plot,
    width = 7, height = 6, dpi = 300, bg = "white"
  )
}
###############################################################

####### make one single photo ############


years <- 2000:2024
image_list <- lapply(years, function(yr) {
  image_read(paste0("damage_map_", yr, ".png"))
})

# Create a large mosaic (e.g., 5 rows x 5 columns)
combined_image <- image_montage(
  image_join(image_list),
  tile = "5x5",  # adjust to control layout
  geometry = "600x500+2+2"  # width x height per image + spacing
)

# Save the result
image_write(combined_image, path = "combined_damage_maps.png", format = "png")
############################################################

})
