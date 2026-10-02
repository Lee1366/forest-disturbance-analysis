local({
# Cleaned copy of BK_neighborigSituation.R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("sf", "dplyr", "readxl", "writexl", "purrr", "openxlsx")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(sf)
library(dplyr)
library(readxl)
library(writexl)
library(purrr)
library(openxlsx)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "ownership_neighbors_and_deltas")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##BK1,2 and 3 for neighboring situation, and delta this and next year
##26.06.2025


##BK1

# 1. Load the shapefile (district boundaries)
austria_districts <- st_read(file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp"))

# 2. Load BK1 data which already includes Damage_Class (Hot Spot, High Damage, etc.)
bk1_data <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "BK1_Damage_Classes")

# 3. Merge the shape file and your data
bk1_sf <- austria_districts %>%
  left_join(bk1_data, by = c("g_name" = "ERHEBUNGSBEZIRK"))

# 4. Create chunks of 5 years
years <- sort(unique(bk1_sf$JAHR))
year_chunks <- split(years, ceiling(seq_along(years) / 5))
all_results <- list()

# 5. Process each year chunk
for (chunk_idx in seq_along(year_chunks)) {
  
  current_chunk <- year_chunks[[chunk_idx]]
  message("Processing years: ", paste(current_chunk, collapse = ", "))
  
  chunk_results <- map(current_chunk, function(y) {
    year_data <- bk1_sf %>% filter(JAHR == y)
    
    # Get hot spot districts based on existing classification
    hotspot_districts <- year_data %>%
      filter(Damage_Class == "Hot Spot") %>%
      pull(g_name)
    
    # Find neighbors for each district
    neighbors <- st_touches(year_data, year_data)
    
    # Assign new damage category
    year_data$Neighbor_Category <- sapply(seq_along(neighbors), function(i) {
      current <- year_data$g_name[i]
      neighbor_names <- year_data$g_name[neighbors[[i]]]
      
      if (current %in% hotspot_districts) {
        return("Hot Spot")
      } else if (any(neighbor_names %in% hotspot_districts)) {
        return("Hotspot Neighbor")
      } else {
        return("None")
      }
    })
    
    return(year_data)
  })
  
  all_results[[chunk_idx]] <- bind_rows(chunk_results)
  message("✅ Finished chunk ", chunk_idx)
}
getwd()


# 6. Combine all years
bk1_with_neighbors <- bind_rows(all_results)
#####


# 6. Get neighbor names from shapefile
neighbors_list <- st_touches(austria_districts, austria_districts)

neighbor_names <- lapply(seq_along(neighbors_list), function(i) {
  austria_districts$g_name[neighbors_list[[i]]]
})

neighbor_df <- data.frame(
  g_name = austria_districts$g_name,
  neighbors = sapply(neighbor_names, paste, collapse = ", ")
)

# 7. Join neighbor name info to BK1 results
bk1_with_neighbors <- bk1_with_neighbors %>%
  left_join(neighbor_df, by = "g_name")

########


df_bk1 <- bk1_with_neighbors

# 1. Compute regular harvest
df_bk1 <- df_bk1 %>%
  mutate(Regular_Harvest = Total_Gesamteinschlag - Total_Schadholz)

# 2. Compute 10-year mean of Regular Harvest per district (up to that year)
df_bk1 <- df_bk1 %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    Regular_Mean = sapply(seq_along(JAHR), function(i) {
      if (i >= 10) {
        mean(Regular_Harvest[(i - 9):i], na.rm = TRUE)
      } else {
        NA
      }
    })
  ) %>%
  ungroup()

# 3. Compute next year’s regular harvest
df_bk1 <- df_bk1 %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(Regular_Harvest_Lead = lead(Regular_Harvest)) %>%
  ungroup()

# 4. Compute delta for this year and next year
df_bk1 <- df_bk1 %>%
  mutate(
    Delta_ThisYear = ((Regular_Harvest - Regular_Mean) / Regular_Mean) * 100,
    Delta_NextYear = ((Regular_Harvest_Lead - Regular_Mean) / Regular_Mean) * 100
  )


###############################


df_bk1_export <- st_drop_geometry(df_bk1)

# Save
write_xlsx(list(BK1 = df_bk1_export), file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"))
##############################################
###########################################
############################################

##BK2


austria_districts <- st_read(file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp"))

# 2. Load BK2 data which already includes Damage_Class (Hot Spot, High Damage, etc.)
bk2_data <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "BK2_Damage_Classes")

# 3. Merge the shape file and your data
bk2_sf <- austria_districts %>%
  left_join(bk2_data, by = c("g_name" = "ERHEBUNGSBEZIRK"))

# 4. Create chunks of 5 years
years <- sort(unique(bk2_sf$JAHR))
year_chunks <- split(years, ceiling(seq_along(years) / 5))
all_results <- list()

# 5. Loop through year chunks to classify neighbor status
for (chunk_idx in seq_along(year_chunks)) {
  
  current_chunk <- year_chunks[[chunk_idx]]
  message("Processing years: ", paste(current_chunk, collapse = ", "))
  
  chunk_results <- map(current_chunk, function(y) {
    year_data <- bk2_sf %>% filter(JAHR == y)
    
    hotspot_districts <- year_data %>%
      filter(Damage_Class == "Hot Spot") %>%
      pull(g_name)
    
    neighbors <- st_touches(year_data, year_data)
    
    year_data$Neighbor_Category <- sapply(seq_along(neighbors), function(i) {
      current <- year_data$g_name[i]
      neighbor_names <- year_data$g_name[neighbors[[i]]]
      
      if (current %in% hotspot_districts) {
        return("Hot Spot")
      } else if (any(neighbor_names %in% hotspot_districts)) {
        return("Hotspot Neighbor")
      } else {
        return("None")
      }
    })
    
    return(year_data)
  })
  
  all_results[[chunk_idx]] <- bind_rows(chunk_results)
  message("✅ Finished chunk ", chunk_idx)
}

# 6. Combine all years into one sf object
bk2_with_neighbors <- bind_rows(all_results)

# 7. Get neighbor names from shapefile
neighbors_list <- st_touches(austria_districts, austria_districts)
neighbor_names <- lapply(seq_along(neighbors_list), function(i) {
  austria_districts$g_name[neighbors_list[[i]]]
})

neighbor_df <- data.frame(
  g_name = austria_districts$g_name,
  neighbors = sapply(neighbor_names, paste, collapse = ", ")
)

# 8. Join neighbor name info to BK2 results
bk2_with_neighbors <- bk2_with_neighbors %>%
  left_join(neighbor_df, by = "g_name")

# 9. Calculate Regular Harvest and Delta values
df_bk2 <- bk2_with_neighbors %>%
  mutate(Regular_Harvest = Total_Gesamteinschlag - Total_Schadholz) %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    Regular_Mean = sapply(seq_along(JAHR), function(i) {
      if (i >= 10) {
        mean(Regular_Harvest[(i - 9):i], na.rm = TRUE)
      } else {
        NA
      }
    }),
    Regular_Harvest_Lead = lead(Regular_Harvest)
  ) %>%
  ungroup() %>%
  mutate(
    Delta_ThisYear = ((Regular_Harvest - Regular_Mean) / Regular_Mean) * 100,
    Delta_NextYear = ((Regular_Harvest_Lead - Regular_Mean) / Regular_Mean) * 100
  )

# 10. Save as Excel


df_bk2_export <- st_drop_geometry(df_bk2)
# Load the existing Excel file
wb <- loadWorkbook(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"))

# Add a new sheet for BK2
addWorksheet(wb, "BK2")

# Write the data (without geometry)
writeData(wb, sheet = "BK2", x = df_bk2_export)

# Save the workbook
saveWorkbook(wb, file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), overwrite = TRUE)
############################################
###############################################
###################################################
##BK3


austria_districts <- st_read(file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp"))

# 2. Load BK3 data (make sure sheet name is "BK3_Damage_Classes")
bk3_data <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "BK3_Damage_Classes")

# 3. Merge shape file and BK3 data
bk3_sf <- austria_districts %>%
  left_join(bk3_data, by = c("g_name" = "ERHEBUNGSBEZIRK"))

# 4. Chunk the years into 5-year blocks
years <- sort(unique(bk3_sf$JAHR))
year_chunks <- split(years, ceiling(seq_along(years) / 5))
all_results <- list()

# 5. Loop through each year chunk to classify neighbors
for (chunk_idx in seq_along(year_chunks)) {
  current_chunk <- year_chunks[[chunk_idx]]
  message("Processing years: ", paste(current_chunk, collapse = ", "))
  
  chunk_results <- map(current_chunk, function(y) {
    year_data <- bk3_sf %>% filter(JAHR == y)
    
    # Identify hotspot districts
    hotspot_districts <- year_data %>%
      filter(Damage_Class == "Hot Spot") %>%
      pull(g_name)
    
    # Find neighbors
    neighbors <- st_touches(year_data, year_data)
    
    # Assign neighbor classification
    year_data$Neighbor_Category <- sapply(seq_along(neighbors), function(i) {
      current <- year_data$g_name[i]
      neighbor_names <- year_data$g_name[neighbors[[i]]]
      
      if (current %in% hotspot_districts) {
        return("Hot Spot")
      } else if (any(neighbor_names %in% hotspot_districts)) {
        return("Hotspot Neighbor")
      } else {
        return("None")
      }
    })
    
    return(year_data)
  })
  
  all_results[[chunk_idx]] <- bind_rows(chunk_results)
  message("✅ Finished chunk ", chunk_idx)
}

# 6. Combine results
bk3_with_neighbors <- bind_rows(all_results)

# 7. Get neighbor names
neighbors_list <- st_touches(austria_districts, austria_districts)
neighbor_names <- lapply(seq_along(neighbors_list), function(i) {
  austria_districts$g_name[neighbors_list[[i]]]
})
neighbor_df <- data.frame(
  g_name = austria_districts$g_name,
  neighbors = sapply(neighbor_names, paste, collapse = ", ")
)

# 8. Add neighbor names to BK3
bk3_with_neighbors <- bk3_with_neighbors %>%
  left_join(neighbor_df, by = "g_name")

# 9. Calculate Regular Harvest
bk3_with_neighbors <- bk3_with_neighbors %>%
  mutate(Regular_Harvest = Total_Gesamteinschlag - Total_Schadholz)

# 10. Compute 10-year Regular Mean
bk3_with_neighbors <- bk3_with_neighbors %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    Regular_Mean = sapply(seq_along(JAHR), function(i) {
      if (i >= 10) {
        mean(Regular_Harvest[(i - 9):i], na.rm = TRUE)
      } else {
        NA
      }
    })
  ) %>%
  ungroup()

# 11. Compute next year’s harvest
bk3_with_neighbors <- bk3_with_neighbors %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(Regular_Harvest_Lead = lead(Regular_Harvest)) %>%
  ungroup()

# 12. Compute Deltas
bk3_with_neighbors <- bk3_with_neighbors %>%
  mutate(
    Delta_ThisYear = ((Regular_Harvest - Regular_Mean) / Regular_Mean) * 100,
    Delta_NextYear = ((Regular_Harvest_Lead - Regular_Mean) / Regular_Mean) * 100
  )

# 13. Remove geometry for saving
df_bk3_export <- st_drop_geometry(bk3_with_neighbors)
#############

# Load workbook
wb <- loadWorkbook(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"))

# Add new sheet
addWorksheet(wb, "BK3")

# Write data
writeData(wb, sheet = "BK3", x = df_bk3_export)

# Save workbook
saveWorkbook(wb, file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), overwrite = TRUE)
getwd()

})
