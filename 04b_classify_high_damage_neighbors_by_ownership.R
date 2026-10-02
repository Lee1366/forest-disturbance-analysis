local({
# Cleaned copy of HighDamage_BKs(1).R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "writexl", "sf", "purrr", "openxlsx", "tidyr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(purrr)
library(openxlsx)
library(tidyr)
library(stringr)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "high_damage_neighbors_by_ownership")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##02.07.2025
##High damage BK, neighboring situation

# Install these packages if you haven't


# Load Excel data
df <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"),
                 sheet = "BK1")


# Step 1: Get district-years for High Damage and Hot Spot
high_damage_dy <- df %>% filter(Damage_Class == "High Damage") %>% select(g_name, JAHR)
hotspot_dy     <- df %>% filter(Damage_Class == "Hot Spot")    %>% select(g_name, JAHR)

# Step 2: Function to check neighbor relationship (without row duplication)
is_neighbor_of <- function(target_name, target_year, neighbor_df, neighbors_string) {
  neighbors <- str_split(neighbors_string, ",\\s*")[[1]]
  any(neighbors %in% (neighbor_df %>% filter(JAHR == target_year) %>% pull(g_name)))
}

# Step 3: Classify all rows — preserving Hot Spot rows directly
df_classified <- df %>%
  rowwise() %>%
  mutate(
    New_Damage_Category = case_when(
      Damage_Class == "Hot Spot" ~ "Hot Spot",
      Damage_Class == "High Damage" ~ "High Damage",
      is_neighbor_of(g_name, JAHR, high_damage_dy, neighbors) &
        is_neighbor_of(g_name, JAHR, hotspot_dy, neighbors) ~ "High Damage w/ Hotspot Border",
      is_neighbor_of(g_name, JAHR, high_damage_dy, neighbors) ~ "High Damage Neighbor",
      is_neighbor_of(g_name, JAHR, hotspot_dy, neighbors) ~ "Only Hotspot Neighbor",
      TRUE ~ "None"
    )
  ) %>%
  ungroup()


########################################
############saving


output_path <- file.path(project_root, "outputs", "BK_HighDamage_Neighbor_Categorization.xlsx")

# Create a new workbook and add the sheet
wb <- createWorkbook()
addWorksheet(wb, "BK1_HD_Neighbor")

# Write the classified data
writeData(wb, sheet = "BK1_HD_Neighbor", x = df_classified)

# Save the workbook
saveWorkbook(wb, file = output_path, overwrite = TRUE)
##################################################################
######################################

###BK2
# Load Excel data
df <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"),
                 sheet = "BK2")

# Step 1: Get district-years for High Damage and Hot Spot
high_damage_dy <- df %>% filter(Damage_Class == "High Damage") %>% select(g_name, JAHR)
hotspot_dy     <- df %>% filter(Damage_Class == "Hot Spot")    %>% select(g_name, JAHR)

# Step 2: Function to check neighbor relationship (without row duplication)
is_neighbor_of <- function(target_name, target_year, neighbor_df, neighbors_string) {
  neighbors <- str_split(neighbors_string, ",\\s*")[[1]]
  any(neighbors %in% (neighbor_df %>% filter(JAHR == target_year) %>% pull(g_name)))
}

# Step 3: Classify all rows — preserving Hot Spot rows directly
df_classified <- df %>%
  rowwise() %>%
  mutate(
    New_Damage_Category = case_when(
      Damage_Class == "Hot Spot" ~ "Hot Spot",
      Damage_Class == "High Damage" ~ "High Damage",
      is_neighbor_of(g_name, JAHR, high_damage_dy, neighbors) &
        is_neighbor_of(g_name, JAHR, hotspot_dy, neighbors) ~ "High Damage w/ Hotspot Border",
      is_neighbor_of(g_name, JAHR, high_damage_dy, neighbors) ~ "High Damage Neighbor",
      is_neighbor_of(g_name, JAHR, hotspot_dy, neighbors) ~ "Only Hotspot Neighbor",
      TRUE ~ "None"
    )
  ) %>%
  ungroup()
####################
##saving

file_path <- file.path(project_root, "outputs", "BK_HighDamage_Neighbor_Categorization.xlsx")

# Load the workbook
wb <- loadWorkbook(file_path)

# Add a new worksheet (e.g., named "BK1_Final_Tags")
addWorksheet(wb, "BK2_HD_Neighbor")

# Write the data
writeData(wb, sheet = "BK2_HD_Neighbor", df_classified)

# Save the workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)
#######################################
################################
##BK3

# Load Excel data
df <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"),
                 sheet = "BK3")

# Step 1: Get district-years for High Damage and Hot Spot
high_damage_dy <- df %>% filter(Damage_Class == "High Damage") %>% select(g_name, JAHR)
hotspot_dy     <- df %>% filter(Damage_Class == "Hot Spot")    %>% select(g_name, JAHR)

# Step 2: Function to check neighbor relationship (without row duplication)
is_neighbor_of <- function(target_name, target_year, neighbor_df, neighbors_string) {
  neighbors <- str_split(neighbors_string, ",\\s*")[[1]]
  any(neighbors %in% (neighbor_df %>% filter(JAHR == target_year) %>% pull(g_name)))
}

# Step 3: Classify all rows — preserving Hot Spot rows directly
df_classified <- df %>%
  rowwise() %>%
  mutate(
    New_Damage_Category = case_when(
      Damage_Class == "Hot Spot" ~ "Hot Spot",
      Damage_Class == "High Damage" ~ "High Damage",
      is_neighbor_of(g_name, JAHR, high_damage_dy, neighbors) &
        is_neighbor_of(g_name, JAHR, hotspot_dy, neighbors) ~ "High Damage w/ Hotspot Border",
      is_neighbor_of(g_name, JAHR, high_damage_dy, neighbors) ~ "High Damage Neighbor",
      is_neighbor_of(g_name, JAHR, hotspot_dy, neighbors) ~ "Only Hotspot Neighbor",
      TRUE ~ "None"
    )
  ) %>%
  ungroup()
####################
##saving

file_path <- file.path(project_root, "outputs", "BK_HighDamage_Neighbor_Categorization.xlsx")

# Load the workbook
wb <- loadWorkbook(file_path)

# Add a new worksheet (e.g., named "BK1_Final_Tags")
addWorksheet(wb, "BK3_HD_Neighbor")

# Write the data
writeData(wb, sheet = "BK3_HD_Neighbor", df_classified)

# Save the workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)

})
