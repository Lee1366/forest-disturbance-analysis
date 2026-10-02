local({
# Cleaned copy of Austria_districts.R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("sf", "dplyr", "ggplot2", "readxl", "writexl", "gganimate", "gifski", "transformr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(gganimate)
library(gifski)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "district_maps")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##shape Austria
##02-06.2025 and 03.06


damage_data <- read_excel(file.path(project_root, "data", "final_classified_damage2.xlsx"), sheet = "Sheet1")

######shape file

shapefile_path <- file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww2.shp")
austria_districts <- st_read(shapefile_path)


#mergeing
merged_data <- austria_districts %>%
  left_join(damage_data, by = c("g_name" = "ERHEBUNGSBEZIRK"))


##check the names
district_names_shp <- unique(austria_districts$g_name)
district_names_data <- unique(damage_data$ERHEBUNGSBEZIRK)

##Find names in damage data that don’t exist in shapefile
setdiff(district_names_data, district_names_shp)

#Find names in shapefile not found in damage data
setdiff(district_names_shp, district_names_data)

## Correcting or formatting the text so it matches properly.
damage_data$ERHEBUNGSBEZIRK <- trimws(damage_data$ERHEBUNGSBEZIRK)
austria_districts$NAME_2 <- trimws(austria_districts$NAME_2)


##one year

merged_data_2023 <- merged_data %>% filter(JAHR == 2023)


ggplot(merged_data_2023) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c("Low Damage" = "palegreen", "Moderate Damage" = "#fec43f", "High Damage" = "coral",
               "Hot Spot" = "firebrick2"), na.value = "grey90"
  ) +
  labs(
    title = "Forest Damage Categories by District (2023)",
    fill = "Damage Level"
  ) +
  theme_minimal()

#######################################################
###with districts name nad bundesland border:


# First, create a separate layer for Bundesland borders
bundesland_borders <- merged_data_2023 %>%
  group_by(BUNDESLAND_NAME) %>%
  summarise(geometry = st_union(geometry))  # Merge geometries by Bundesland


ggplot(merged_data_2023) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
  geom_sf(data = bundesland_borders, fill = NA, color = "black", size = 0.6) +  # Bundesland borders
  geom_sf_text(aes(label = g_name), size = 2, color = "black") +  # Add district names
  scale_fill_manual(
    values = c(
      "Low Damage" = "palegreen",
      "Moderate Damage" = "#fec43f",
      "High Damage" = "coral",
      "Hot Spot" = "firebrick2"
    ),
    na.value = "grey90"
  ) +
  labs(
    title = "Forest Damage Categories by District (2023)",
    fill = "Damage Level"
  ) +
  theme_minimal()

#################

###############################################################

##distrcits name extracts in an excel file


# Read the shapefile (replace path with actual file path)
#shapefile_path <- "path/to/your/shapefile/austria_districts.shp"
#austria_districts <- st_read(shapefile_path)

# View first few rows of the attribute table
head(austria_districts)

# Drop geometry to get just the attribute table
districts_table <- st_drop_geometry(austria_districts)

# Drop geometry
merged_data_no_geom <- st_drop_geometry(merged_data)

# Write to Excel
write_xlsx(merged_data_no_geom, file.path(project_root, "outputs", "merged_data2.xlsx"))

getwd()

# Export the attribute table to Excel
write_xlsx(districts_table, file.path(project_root, "outputs", "austria_districts_table.xlsx"))

# Define the new path where you want to save the Excel file
output_path <- file.path(project_root, "outputs", "austria_districts_table.xlsx")

# Write the attribute table to that location
write_xlsx(districts_table, output_path)

###########################################################################
########## it takes too much time ########################

##for all years:
ggplot(merged_data) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "Low Damage" = "palegreen",
      "Moderate Damage" = "#fec43f",
      "High Damage" = "coral",
      "Hot Spot" = "firebrick2"
    ),
    na.value = "grey90"
  ) +
  labs(
    title = "Forest Damage Categories by District (2000–2024)",
    fill = "Damage Level"
  ) +
  theme_minimal() +
  facet_wrap(~JAHR)

#########################################################################


#######################################################################################
###################
##Lets try the animation format


ggplot(merged_data) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "Low Damage" = "palegreen",
      "Moderate Damage" = "#fec43f",
      "High Damage" = "coral",
      "Hot Spot" = "firebrick2"
    ),
    na.value = "grey90"
  ) +
  labs(
    title = "Forest Damage Categories by District",
    subtitle = "Year: {frame_time}",
    fill = "Damage Level"
  ) +
  theme_minimal() +
  transition_time(JAHR) +  # ✅ Fixed here
  ease_aes('linear')

########################################################
##it was so long, i stopped it##
##lets try only 5 years as an example:
years_to_use <- c(2000, 2005, 2010, 2015, 2020)

merged_data_subset <- merged_data %>%
  filter(JAHR %in% years_to_use)

ggplot(merged_data_subset) +
  geom_sf(aes(fill = Damage_Class), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "Low Damage" = "palegreen",
      "Moderate Damage" = "#fec43f",
      "High Damage" = "coral",
      "Hot Spot" = "firebrick2"
    ),
    na.value = "grey90"
  ) +
  labs(
    title = "Forest Damage Categories by District",
    subtitle = "Year: {frame_time}",
    fill = "Damage Level"
  ) +
  theme_minimal() +
  transition_time(JAHR) +
  ease_aes("linear")


animate(
  last_plot(),
  nframes = 5,         # One per year
  fps = 1,             # 1 frame per second
  width = 600,
  height = 400,
  renderer = gifski_renderer()
)


animation <- animate(
  last_plot(),         # Or replace with your ggplot object like `p`
  nframes = 5,
  fps = 1,
  width = 600,
  height = 400,
  renderer = gifski_renderer()
)

# Show it in RStudio Viewer
animation

anim_save("forest_damage_5years.gif", animation = animation)
getwd()
#################################

})
