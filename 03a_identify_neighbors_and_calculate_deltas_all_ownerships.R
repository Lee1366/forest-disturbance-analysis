local({
# Cleaned copy of category_hotspotneighbor.R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "writexl", "sf", "purrr", "openxlsx", "ggplot2", "tidyr", "moments", "ggpubr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(purrr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(moments)
library(ggpubr)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "hotspot_neighbors")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##18.06.2025
##category:hotspot, hotspot neighbor, none

# Working directory is configured in the header.
# Install these packages if you haven't


austria_districts <- st_read(file.path(project_root, "data/shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp"))

#damage file excel
damage_data <- read_excel(file.path(project_root, "data", "final_damage_withoutBK.xlsx"))

# Step 2: Merge shape + data
austria_damage_sf <- austria_districts %>%
  left_join(damage_data, by = c("g_name" = "ERHEBUNGSBEZIRK"))


#######################
##lets try easier way, with 5 years loop


years <- sort(unique(austria_damage_sf$JAHR))

# Step 2: Split years into chunks of 5
year_chunks <- split(years, ceiling(seq_along(years) / 5))

# Step 3: Create empty list to collect results
all_results <- list()

# Step 4: Loop over each 5-year chunk
for (chunk_idx in seq_along(year_chunks)) {
  
  current_chunk <- year_chunks[[chunk_idx]]
  message("Processing years: ", paste(current_chunk, collapse = ", "))
  
  # Process each year in the current chunk
  chunk_results <- map(current_chunk, function(y) {
    
    year_data <- austria_damage_sf %>% filter(JAHR == y)
    
    # Get Hot Spot districts for that year
    hotspot_districts <- year_data %>%
      filter(Damage_Class == "Hot Spot") %>%
      pull(g_name)
    
    # Get neighbors
    neighbors <- st_touches(year_data, year_data)
    
    # Assign category
    year_data$Damage_Category <- sapply(seq_along(neighbors), function(i) {
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
  
  # Add this chunk's result to master list
  all_results[[chunk_idx]] <- bind_rows(chunk_results)
  
  message("✅ Finished chunk ", chunk_idx, "/", length(year_chunks), "\n")
}


####combine the loops

final_classified_sf <- bind_rows(all_results)

# Save to file
#st_write(final_classified_sf, file.path(project_root, "data/shapefiles", "neighbors.shp"))

# Or plot a year
#plot(final_classified_sf %>% filter(JAHR == 2010)["Damage_Category"])


# Drop geometry column to get a normal data frame
df_no_geom <- sf::st_drop_geometry(final_classified_sf)

# Write to Excel file
write_xlsx(df_no_geom, file.path(project_root, "outputs", "neighbors_category.xlsx"))
#################################################

##1. Get neighbors as names for each district

# Compute neighbors from shapefile
neighbors_list <- st_touches(austria_districts, austria_districts)

# Extract district names
neighbor_names <- lapply(seq_along(neighbors_list), function(i) {
  austria_districts$g_name[neighbors_list[[i]]]
})

# Turn into a data frame
neighbor_df <- data.frame(
  g_name = austria_districts$g_name,
  neighbors = sapply(neighbor_names, paste, collapse = ", ")
)

##2. Join this to your full dataset (which includes years)

# Join neighbor info to full classified data (all years)
final_classified_sf <- final_classified_sf %>%
  left_join(neighbor_df, by = "g_name")


# Drop geometry to write to Excel
df_no_geom <- sf::st_drop_geometry(final_classified_sf)

# Load the existing Excel file
wb <- loadWorkbook(file.path(project_root, "outputs", "neighbors_category.xlsx"))

# Add a second sheet (e.g., named "Damage_Neighbors")
addWorksheet(wb, "Neighbors_names")

# Write the data
writeData(wb, sheet = "Neighbors_names", x = df_no_geom)

# Save the workbook
saveWorkbook(wb, file.path(project_root, "outputs", "neighbors_category.xlsx"), overwrite = TRUE)
##########################################################################

##Create Regular_Harvest column from the last sheet saved


df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), sheet = "Neighbors_names")


df <- df %>%
  mutate(Regular_Harvest = Total_Gesamteinschlag - Total_Schadholz)

###################

##mean 10 years for each district-year

df <- df %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    Regular_Mean = sapply(seq_along(JAHR), function(i) {
      current_year <- JAHR[i]
      past_values <- Regular_Harvest[1:i]  # from first year up to current year
      
      if (length(past_values) >= 10) {
        mean(past_values, na.rm = TRUE)
      } else {
        NA
      }
    })
  ) %>%
  ungroup()
##################################################

#delta regular harvest for current year


df <- df %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(Regular_Harvest_Lead = lead(Regular_Harvest)) %>%
  ungroup()
################

df <- df %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    Regular_Harvest_Lead = lead(Regular_Harvest),  # next year's harvest
    Delta_ThisYear = ((Regular_Harvest - Regular_Mean) / Regular_Mean) * 100,
    Delta_NextYear = ((Regular_Harvest_Lead - Regular_Mean) / Regular_Mean) * 100
  ) %>%
  ungroup()

#########################################################################

##filtering the hotspot_neighbors and continue with that

df_hotspot_neighbor <- df %>%
  filter(Damage_Category == "Hotspot Neighbor")


# Define path to file
file_path <- file.path(project_root, "outputs", "neighbors_category.xlsx")

# Load workbook
wb <- loadWorkbook(file_path)

# Save the full delta table required by the following analyses.
addWorksheet(wb, "Hotspot_Neighbor_All%")
writeData(wb, "Hotspot_Neighbor_All%", df)

# Add new worksheet
addWorksheet(wb, "Hotspot_Neighbor")

# Write the filtered data
writeData(wb, sheet = "Hotspot_Neighbor", x = df_hotspot_neighbor)

# Save the updated file
saveWorkbook(wb, file = file_path, overwrite = TRUE)
###################################################################
## T-test, but i am doing it with no filtering version, and filter it again

# Read the relevant sheet (adjust name as needed)
df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), sheet = "Hotspot_Neighbor_All%")


df_neighbors <- df %>%
  filter(Damage_Category == "Hotspot Neighbor", !is.na(Delta_ThisYear))

t.test(df_neighbors$Delta_ThisYear, mu = 0)

t_this <- t.test(df_neighbors$Delta_ThisYear, mu = 0)

######
##next year

df_neighbors <- df %>%
  filter(Damage_Category == "Hotspot Neighbor", !is.na(Delta_NextYear))

t.test(df_neighbors$Delta_NextYear, mu = 0)

t_next <- t.test(df_neighbors$Delta_NextYear, mu = 0)
############################
##saving

# Summary for This Year
t_summary_this <- data.frame(
  Group = "Hotspot Neighbor",
  Year = "This Year",
  Mean = t_this$estimate,
  CI_Lower = t_this$conf.int[1],
  CI_Upper = t_this$conf.int[2],
  t_value = t_this$statistic,
  df = t_this$parameter,
  p_value = t_this$p.value
)

# Summary for Next Year
t_summary_next <- data.frame(
  Group = "Hotspot Neighbor",
  Year = "Next Year",
  Mean = t_next$estimate,
  CI_Lower = t_next$conf.int[1],
  CI_Upper = t_next$conf.int[2],
  t_value = t_next$statistic,
  df = t_next$parameter,
  p_value = t_next$p.value
)

########################
##saving

# Combine both into one table
t_summary_combined <- bind_rows(t_summary_this, t_summary_next)

# Create and save to new Excel file
wb <- createWorkbook()
addWorksheet(wb, "Hotspot_Neighbor")
writeData(wb, sheet = "Hotspot_Neighbor", x = t_summary_combined)
saveWorkbook(wb, file = file.path(project_root, "outputs", "t_test.xlsx"), overwrite = TRUE)

##################################################################
##t-test for hotspot category (just as curiosity)

df_hotspot <- df %>%
  filter(Damage_Category == "Hot Spot") %>%
  filter(!is.na(Delta_ThisYear), !is.na(Delta_NextYear))

##this year
t_this_hotspot <- t.test(df_hotspot$Delta_ThisYear, mu = 0)

t.test(df_hotspot$Delta_ThisYear, mu = 0)

#next year
t_next_hotspot <- t.test(df_hotspot$Delta_NextYear, mu = 0)

t.test(df_hotspot$Delta_NextYear, mu = 0)

################################
##saving


t_summary_hotspot_this <- data.frame(
  Group = "Hot Spot",
  Year = "This Year",
  Mean = t_this_hotspot$estimate,
  CI_Lower = t_this_hotspot$conf.int[1],
  CI_Upper = t_this_hotspot$conf.int[2],
  t_value = t_this_hotspot$statistic,
  df = t_this_hotspot$parameter,
  p_value = t_this_hotspot$p.value
)

t_summary_hotspot_next <- data.frame(
  Group = "Hot Spot",
  Year = "Next Year",
  Mean = t_next_hotspot$estimate,
  CI_Lower = t_next_hotspot$conf.int[1],
  CI_Upper = t_next_hotspot$conf.int[2],
  t_value = t_next_hotspot$statistic,
  df = t_next_hotspot$parameter,
  p_value = t_next_hotspot$p.value
)

# Combine both rows
t_summary_hotspot <- bind_rows(t_summary_hotspot_this, t_summary_hotspot_next)

# Load the existing Excel file
file_path <- file.path(project_root, "outputs", "t_test.xlsx")
wb <- loadWorkbook(file_path)

# Add a new worksheet for Hot Spot results
addWorksheet(wb, "Hotspot")

# Write the two-row summary to the new sheet
writeData(wb, sheet = "Hotspot", x = t_summary_hotspot)

# Save the workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)

#######################################
##for none districts

df_None <- df %>%
  filter(Damage_Category == "None") %>%
  filter(!is.na(Delta_ThisYear), !is.na(Delta_NextYear))

##this year

t_this_none <- t.test(df_None$Delta_ThisYear, mu = 0)

t.test(df_None$Delta_ThisYear, mu = 0)

#next year
t_next_none <- t.test(df_None$Delta_NextYear, mu = 0)

t.test(df_None$Delta_NextYear, mu = 0)

########
####saving


t_summary_none_this <- data.frame(
  Group = "None",
  Year = "This Year",
  Mean = t_this_none$estimate,
  CI_Lower = t_this_none$conf.int[1],
  CI_Upper = t_this_none$conf.int[2],
  t_value = t_this_none$statistic,
  df = t_this_none$parameter,
  p_value = t_this_none$p.value
)

t_summary_none_next <- data.frame(
  Group = "None",
  Year = "Next Year",
  Mean = t_next_none$estimate,
  CI_Lower = t_next_none$conf.int[1],
  CI_Upper = t_next_none$conf.int[2],
  t_value = t_next_none$statistic,
  df = t_next_none$parameter,
  p_value = t_next_none$p.value
)

# Combine the two rows
t_summary_none <- bind_rows(t_summary_none_this, t_summary_none_next)

# Load the existing Excel file
file_path <- file.path(project_root, "outputs", "t_test.xlsx")
wb <- loadWorkbook(file_path)

# Add a new worksheet for the "None" group
addWorksheet(wb, "None")

# Write the summary table to the new sheet
writeData(wb, sheet = "None", x = t_summary_none)

# Save the updated workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)


##################################################

##visualisation


df_plot <- df %>%
  filter(Damage_Category == "Hotspot Neighbor") %>%
  summarise(
    mean_delta = mean(Delta_ThisYear, na.rm = TRUE),
    sd = sd(Delta_ThisYear, na.rm = TRUE),
    n = sum(!is.na(Delta_ThisYear)),
    se = sd / sqrt(n),
    ci = 1.96 * se
  )

# Plot
ggplot(df_plot, aes(x = "Hotspot Neighbor", y = mean_delta)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  geom_errorbar(aes(ymin = mean_delta - ci, ymax = mean_delta + ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Mean Delta Regular Harvest (%), This Year",
    y = "Δ Regular Harvest (%)",
    x = ""
  ) +
  theme_minimal()

#############################################################################
#################################################


# Historical manually entered summaries; these are not recalculated results.
t_plot_data <- bind_rows(
  # Hot Spot
  data.frame(Group = "Hot Spot", Year = "This Year", Mean = -39.22, CI_Lower = -42.94, CI_Upper = -35.50),
  data.frame(Group = "Hot Spot", Year = "Next Year", Mean = -33.40, CI_Lower = -37.44, CI_Upper = -29.37),
  
  # Hotspot Neighbor
  data.frame(Group = "Hotspot Neighbor", Year = "This Year", Mean = -17.14, CI_Lower = -20.08, CI_Upper = -14.20),
  data.frame(Group = "Hotspot Neighbor", Year = "Next Year", Mean = -15.60, CI_Lower = -19.10, CI_Upper = -12.09),
  
  # None
  data.frame(Group = "None", Year = "This Year", Mean = -10.79, CI_Lower = -12.96, CI_Upper = -8.62),
  data.frame(Group = "None", Year = "Next Year", Mean = -13.25, CI_Lower = -15.61, CI_Upper = -10.89)
)


# Reorder groups if needed
t_plot_data$Group <- factor(t_plot_data$Group, levels = c("Hot Spot", "Hotspot Neighbor", "None"))

# Create the bar plot with error bars
ggplot(t_plot_data, aes(x = Group, y = Mean, fill = Year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.7), 
                width = 0.2) +
  labs(title = "Change in Regular Harvest by Group",
       subtitle = "Compared to 10-year mean",
       y = "Mean Change in Regular Harvest (%)",
       x = "Damage Category",
       fill = "Year") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30")

#####################################
##box plot for symmetric check


df_long <- df_neighbors %>%
  select(g_name, JAHR, Delta_ThisYear, Delta_NextYear) %>%
  pivot_longer(cols = c(Delta_ThisYear, Delta_NextYear),
               names_to = "Year_Type",
               values_to = "Delta_Value")

# Create boxplot
ggplot(df_long, aes(x = Year_Type, y = Delta_Value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.3, color = "black", size = 1) +
  labs(
    title = "Distribution of Delta Regular Harvest (%)",
    x = "",
    y = "Delta Regular Harvest (%)"
  ) +
  theme_minimal()
#####################################

###histogram


df_long <- df_neighbors %>%
  select(g_name, JAHR, Delta_ThisYear, Delta_NextYear) %>%
  pivot_longer(cols = c(Delta_ThisYear, Delta_NextYear),
               names_to = "Year_Type",
               values_to = "Delta_Value")

# Plot histogram
ggplot(df_long, aes(x = Delta_Value)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.8) +
  facet_wrap(~ Year_Type, scales = "free_y") +
  labs(
    title = "Histograms of Delta Regular Harvest (%)",
    x = "Delta Regular Harvest (%)",
    y = "Frequency"
  ) +
  theme_minimal()
#####################################

###none group boy plot


df_none <- df %>%
  filter(Damage_Category == "None") %>%
  select(g_name, JAHR, Delta_ThisYear, Delta_NextYear)

# Reshape to long format
df_none_long <- df_none %>%
  pivot_longer(
    cols = c(Delta_ThisYear, Delta_NextYear),
    names_to = "Year_Type",
    values_to = "Delta_Value"
  )

# Create boxplot
ggplot(df_none_long, aes(x = Year_Type, y = Delta_Value)) +
  geom_boxplot(fill = "lightgray", color = "black", width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.3, color = "black", size = 1) +
  labs(
    title = "Boxplot of Delta Regular Harvest (%) - None Group",
    x = "",
    y = "Delta Regular Harvest (%)"
  ) +
  theme_minimal()
######################################

##histogram for none group


df_none_long <- df_none %>%
  pivot_longer(
    cols = c(Delta_ThisYear, Delta_NextYear),
    names_to = "Year_Type",
    values_to = "Delta_Value"
  )

# Plot histogram
ggplot(df_none_long, aes(x = Delta_Value)) +
  geom_histogram(binwidth = 5, fill = "lightgray", color = "black", alpha = 0.8) +
  facet_wrap(~ Year_Type, scales = "free_y") +
  labs(
    title = "Histogram of Delta Regular Harvest (%) - None Group",
    x = "Delta Regular Harvest (%)",
    y = "Frequency"
  ) +
  theme_minimal()
################################
##combine


df_neighbors_long <- df %>%
  filter(Damage_Category == "Hotspot Neighbor") %>%
  select(g_name, JAHR, Delta_ThisYear, Delta_NextYear) %>%
  pivot_longer(cols = c(Delta_ThisYear, Delta_NextYear),
               names_to = "Year_Type",
               values_to = "Delta_Value") %>%
  mutate(Group = "Hotspot Neighbor")

df_none_long <- df %>%
  filter(Damage_Category == "None") %>%
  select(g_name, JAHR, Delta_ThisYear, Delta_NextYear) %>%
  pivot_longer(cols = c(Delta_ThisYear, Delta_NextYear),
               names_to = "Year_Type",
               values_to = "Delta_Value") %>%
  mutate(Group = "None")

# Combine both
combined_df <- bind_rows(df_neighbors_long, df_none_long)

# Plot
ggplot(combined_df, aes(x = interaction(Year_Type, Group), y = Delta_Value, fill = Group)) +
  geom_boxplot(width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "black", size = 1) +
  labs(
    title = "Delta Regular Harvest (%) by Year and Group",
    x = "",
    y = "Delta Regular Harvest (%)"
  ) +
  scale_fill_manual(values = c("Hotspot Neighbor" = "skyblue", "None" = "lightgray")) +
  theme_minimal()
#################################
##You can statistically compare the Delta_ThisYear and Delta_NextYear between 
#Hotspot Neighbor and None groups using a two-sample t-test. 
#This will help you check whether the mean delta values are significantly different across the two groups.


file_path <- file.path(project_root, "outputs", "t_test.xlsx")

# Run both t-tests
t_this_less <- t.test(
  df$Delta_ThisYear[df$Damage_Category == "Hotspot Neighbor"],
  df$Delta_ThisYear[df$Damage_Category == "None"],
  alternative = "less",
  var.equal = FALSE
)
 
print(t_this_less)

t_next_less <- t.test(
  df$Delta_NextYear[df$Damage_Category == "Hotspot Neighbor"],
  df$Delta_NextYear[df$Damage_Category == "None"],
  alternative = "less",
  var.equal = FALSE
)

print(t_next_less)

# Create summaries
t_summary <- data.frame(
  Group_1 = c("Hotspot Neighbor", "Hotspot Neighbor"),
  Group_2 = c("None", "None"),
  Comparison = c("Delta_ThisYear", "Delta_NextYear"),
  Alternative_Hypothesis = c("Hotspot Neighbor < None", "Hotspot Neighbor < None"),
  Mean_Group1 = c(t_this_less$estimate[1], t_next_less$estimate[1]),
  Mean_Group2 = c(t_this_less$estimate[2], t_next_less$estimate[2]),
  t_value = c(t_this_less$statistic, t_next_less$statistic),
  df = c(t_this_less$parameter, t_next_less$parameter),
  p_value = c(t_this_less$p.value, t_next_less$p.value),
  CI_Lower = c(t_this_less$conf.int[1], t_next_less$conf.int[1]),
  CI_Upper = c(
    ifelse(length(t_this_less$conf.int) > 1, t_this_less$conf.int[2], NA),
    ifelse(length(t_next_less$conf.int) > 1, t_next_less$conf.int[2], NA)
  )
)

# Load workbook
wb <- loadWorkbook(file_path)

# Add worksheet
addWorksheet(wb, "Hotspot_vs_None_Both")

# Write the table
writeData(wb, "Hotspot_vs_None_Both", t_summary)

# Save
saveWorkbook(wb, file_path, overwrite = TRUE)


###################################################################

##boxplot


df_subset <- df %>%
  filter(Damage_Category %in% c("Hotspot Neighbor", "None")) %>%
  select(Damage_Category, Delta_ThisYear, Delta_NextYear) %>%
  pivot_longer(cols = c(Delta_ThisYear, Delta_NextYear),
               names_to = "Delta_Type",
               values_to = "Delta_Value")

# Rename delta types for plot clarity
df_subset$Delta_Type <- recode(df_subset$Delta_Type,
                               "Delta_ThisYear" = "This Year",
                               "Delta_NextYear" = "Next Year")

# Create boxplot
ggplot(df_subset, aes(x = Damage_Category, y = Delta_Value, fill = Damage_Category)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  facet_wrap(~Delta_Type) +
  labs(
    title = "Delta in Regular Harvest: Hotspot Neighbor vs None",
    y = "Delta in Regular Harvest (%)",
    x = "District Category"
  ) +
  scale_fill_manual(values = c("Hotspot Neighbor" = "#FFA07A", "None" = "#87CEFA")) +
  theme_minimal() +
  theme(legend.position = "none")

###############################################################
##qqtest for symmetric check

df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), sheet = "Hotspot_Neighbor_All%")  # Update with your path

# Step 2: Reshape to long format and filter only the two groups
df_long <- df %>%
  pivot_longer(
    cols = c(Delta_ThisYear, Delta_NextYear),
    names_to = "Year",
    values_to = "Delta"
  ) %>%
  mutate(
    Year = case_when(
      Year == "Delta_ThisYear" ~ "This Year",
      Year == "Delta_NextYear" ~ "Next Year"
    )
  ) %>%
  filter(Damage_Category %in% c("Hotspot Neighbor", "None"))

# Step 3: Q-Q plots for both years and both groups
ggqqplot(
  df_long,
  x = "Delta",
  facet.by = c("Damage_Category", "Year"),
  color = "Damage_Category"
)
###########################


df_long %>%
  group_by(Damage_Category, Year) %>%
  summarise(skew = moments::skewness(Delta, na.rm = TRUE))

###########################
##box plot


df_long <- df %>%
  pivot_longer(
    cols = c(Delta_ThisYear, Delta_NextYear),
    names_to = "Year_Type",
    values_to = "Delta"
  )


df_plot <- df_long %>%
  filter(Damage_Category %in% c("Hotspot Neighbor", "None")) %>%
  mutate(
    Year_Type = recode(Year_Type,
                       "Delta_ThisYear" = "This Year",
                       "Delta_NextYear" = "Next Year")
  )


ggplot(df_plot, aes(x = Year_Type, y = Delta, fill = Damage_Category)) +
  geom_boxplot(position = position_dodge(0.7), width = 0.6, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
              alpha = 0.3, size = 1, color = "black") +
  labs(
    title = "Delta Regular Harvest: Hotspot Neighbors vs None (This & Next Year)",
    x = "Year",
    y = "Delta Regular Harvest",
    fill = "Group"
  ) +
  theme_minimal()

########################################

})
