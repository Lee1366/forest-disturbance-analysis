local({
# Cleaned copy of high_damage(1).R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "writexl", "sf", "purrr", "openxlsx", "tidyr", "stringr", "ggplot2", "ggpubr", "moments")
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
library(ggplot2)
library(ggpubr)
library(moments)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "high_damage_neighbors")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##high damage district impact
##24.06.2025

# Install these packages if you haven't


df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"),
                 sheet = "Hotspot_Neighbor_All%")


# Step 1: Filter to remove Hot Spot
df_filtered <- df %>%
  filter(Damage_Class != "Hot Spot")

# Step 2: Get list of High Damage district-years
high_damage_years <- df_filtered %>%
  filter(Damage_Class == "High Damage") %>%
  select(g_name, JAHR)

# Step 3: Create New Damage Category
df_classified <- df_filtered %>%
  rowwise() %>%
  mutate(
    New_Damage_Category = case_when(
      # If this district-year is high damage
      Damage_Class == "High Damage" ~ "High Damage",
      
      # Otherwise, check if any of its neighbors are high damage in the same year
      any(str_split(neighbors, ",\\s*")[[1]] %in% high_damage_years$g_name[high_damage_years$JAHR == JAHR]) ~ "High Damage Neighbor",
      
      # If not, it's none
      TRUE ~ "None"
    )
  ) %>%
  ungroup()

###########################
##saving


file_path <- file.path(project_root, "outputs", "neighbors_category.xlsx")

# Load the existing workbook
wb <- loadWorkbook(file_path)

# Add a new sheet (change the name if you want)
addWorksheet(wb, "HighDamage_Category")

# Write the dataframe to the new sheet
writeData(wb, sheet = "HighDamage_Category", x = df_classified)

# Save the workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)

###################################################################
### high damage neighbors without hot spot borders

# Load Excel data
df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"),
                 sheet = "Hotspot_Neighbor_All%")


high_damage_dy <- df %>%
  filter(Damage_Class == "High Damage") %>%
  select(g_name, JAHR)

hotspot_dy <- df %>%
  filter(Damage_Class == "Hot Spot") %>%
  select(g_name, JAHR)

# Step 2: Expand neighbors into rows
expanded_neighbors <- df %>%
  select(g_name, JAHR, neighbors) %>%
  separate_rows(neighbors, sep = ",\\s*")

# Step 3: Get neighbors of each category
high_damage_neighbors <- expanded_neighbors %>%
  inner_join(high_damage_dy, by = c("neighbors" = "g_name", "JAHR")) %>%
  select(g_name, JAHR) %>%
  distinct()

hotspot_neighbors <- expanded_neighbors %>%
  inner_join(hotspot_dy, by = c("neighbors" = "g_name", "JAHR")) %>%
  select(g_name, JAHR) %>%
  distinct()

# Step 4: Identify pure and overlapping neighbor types
pure_high_damage_neighbors <- anti_join(high_damage_neighbors, hotspot_neighbors,
                                        by = c("g_name", "JAHR"))

high_damage_with_hotspot_border <- inner_join(high_damage_neighbors, hotspot_neighbors,
                                              by = c("g_name", "JAHR"))

# Step 5: Final classification
df_classified <- df %>%
  filter(Damage_Class != "Hot Spot") %>%
  left_join(high_damage_dy %>% mutate(tag = "High Damage"),
            by = c("g_name", "JAHR")) %>%
  left_join(pure_high_damage_neighbors %>% mutate(tag2 = "High Damage Neighbor"),
            by = c("g_name", "JAHR")) %>%
  left_join(high_damage_with_hotspot_border %>% mutate(tag3 = "High Damage w/ Hotspot Border"),
            by = c("g_name", "JAHR")) %>%
  mutate(New_Damage_Category = case_when(
    tag == "High Damage" ~ "High Damage",
    tag2 == "High Damage Neighbor" ~ "High Damage Neighbor",
    tag3 == "High Damage w/ Hotspot Border" ~ "High Damage w/ Hotspot Border",
    TRUE ~ "None"  # You can leave this or remove it later
  )) %>%
  select(-tag, -tag2, -tag3)
########################################
############saving


file_path <- file.path(project_root, "outputs", "neighbors_category.xlsx")
wb <- loadWorkbook(file_path)

addWorksheet(wb, "HighDamageClean")
writeData(wb, "HighDamageClean", df_classified)

saveWorkbook(wb, file = file_path, overwrite = TRUE)

########################################

##30.06.2025
##box plot for symmetric check


df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), sheet = "HighDamageClean")  # Replace with actual file path

# Step 2: Reshape to long format for plotting both years together
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
  )


df_filtered <- df_long %>%
  filter(New_Damage_Category %in% c("High Damage Neighbor", "None"))

# Step 3: Create side-by-side boxplots
ggplot(df_filtered, aes(x = New_Damage_Category, y = Delta, fill = New_Damage_Category)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "gray40") +
  facet_wrap(~Year) +
  labs(
    title = "Delta Harvesting Comparison: High-Damage Neighbors vs. None",
    x = "Group",
    y = "Delta Regular Harvest"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
############################

# Optional: check normality for each group


df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), sheet = "HighDamageClean")  # Update with your path

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
  filter(New_Damage_Category %in% c("High Damage Neighbor", "None"))

# Step 3: Q-Q plots for both years and both groups
ggqqplot(
  df_long,
  x = "Delta",
  facet.by = c("New_Damage_Category", "Year"),
  color = "New_Damage_Category"
)
###########################################################


df_long %>%
  group_by(New_Damage_Category, Year) %>%
  summarise(skew = moments::skewness(Delta, na.rm = TRUE))
###############################################################
##it is symmetric we can continue with T-Test
##t test for high damage neighbors

# Read the relevant sheet (adjust name as needed)
df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), sheet = "HighDamageClean")


# Step 2: One-sample t-tests
# --- This Year ---
df_this <- df %>%
  filter(New_Damage_Category == "High Damage Neighbor", !is.na(Delta_ThisYear))

t_this <- t.test(df_this$Delta_ThisYear, mu = 0)

# --- Next Year ---
df_next <- df %>%
  filter(New_Damage_Category == "High Damage Neighbor", !is.na(Delta_NextYear))

t_next <- t.test(df_next$Delta_NextYear, mu = 0)

# Step 3: Extract results into data frames
result_this <- data.frame(
  Year = "This Year",
  Mean = mean(df_this$Delta_ThisYear),
  t_statistic = t_this$statistic,
  df = t_this$parameter,
  p_value = t_this$p.value,
  conf_low = t_this$conf.int[1],
  conf_high = t_this$conf.int[2]
)

result_next <- data.frame(
  Year = "Next Year",
  Mean = mean(df_next$Delta_NextYear),
  t_statistic = t_next$statistic,
  df = t_next$parameter,
  p_value = t_next$p.value,
  conf_low = t_next$conf.int[1],
  conf_high = t_next$conf.int[2]
)

# Step 4: Combine results
final_results <- bind_rows(result_this, result_next)

# Step 5: Save to Excel
output_path <- file.path(project_root, "outputs", "high_damage_ttest_results.xlsx")
write.xlsx(final_results, file = output_path, sheetName = "TTest_HighDamage", overwrite = TRUE)
#########################################

##t test for none high damage neighbors


df <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), 
                 sheet = "HighDamageClean")

# Step 2: One-sample t-tests for "None"
# --- This Year ---
df_this_none <- df %>%
  filter(New_Damage_Category == "None", !is.na(Delta_ThisYear))

t_this_none <- t.test(df_this_none$Delta_ThisYear, mu = 0)

# --- Next Year ---
df_next_none <- df %>%
  filter(New_Damage_Category == "None", !is.na(Delta_NextYear))

t_next_none <- t.test(df_next_none$Delta_NextYear, mu = 0)

# Step 3: Format results
result_this_none <- data.frame(
  Year = "This Year",
  Mean = mean(df_this_none$Delta_ThisYear),
  t_statistic = t_this_none$statistic,
  df = t_this_none$parameter,
  p_value = t_this_none$p.value,
  conf_low = t_this_none$conf.int[1],
  conf_high = t_this_none$conf.int[2]
)

result_next_none <- data.frame(
  Year = "Next Year",
  Mean = mean(df_next_none$Delta_NextYear),
  t_statistic = t_next_none$statistic,
  df = t_next_none$parameter,
  p_value = t_next_none$p.value,
  conf_low = t_next_none$conf.int[1],
  conf_high = t_next_none$conf.int[2]
)

final_results_none <- bind_rows(result_this_none, result_next_none)

# Step 4: Save both sheets to Excel
# Load the previous file and add a new sheet
file_path <- file.path(project_root, "outputs", "high_damage_ttest_results.xlsx")

wb <- loadWorkbook(file_path)

addWorksheet(wb, "TTest_None")
writeData(wb, sheet = "TTest_None", final_results_none)

saveWorkbook(wb, file = file_path, overwrite = TRUE)
############################################

##mean delta one sample t test high damage neighbors and none ---> two sample t test


df <- readxl::read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), 
                         sheet = "HighDamageClean")

# Filter data for This Year
df_this <- df %>%
  filter(New_Damage_Category %in% c("High Damage Neighbor", "None"),
         !is.na(Delta_ThisYear)) %>%
  select(New_Damage_Category, Delta_ThisYear)

# Filter data for Next Year
df_next <- df %>%
  filter(New_Damage_Category %in% c("High Damage Neighbor", "None"),
         !is.na(Delta_NextYear)) %>%
  select(New_Damage_Category, Delta_NextYear)


# This Year comparison
t_test_this <- t.test(Delta_ThisYear ~ New_Damage_Category, data = df_this, var.equal = FALSE)

# Next Year comparison
t_test_next <- t.test(Delta_NextYear ~ New_Damage_Category, data = df_next, var.equal = FALSE)


result_this <- data.frame(
  Year = "This Year",
  Mean_HighDamage = mean(df_this$Delta_ThisYear[df_this$New_Damage_Category == "High Damage Neighbor"]),
  Mean_None = mean(df_this$Delta_ThisYear[df_this$New_Damage_Category == "None"]),
  t_statistic = t_test_this$statistic,
  df = t_test_this$parameter,
  p_value = t_test_this$p.value,
  conf_low = t_test_this$conf.int[1],
  conf_high = t_test_this$conf.int[2]
)

result_next <- data.frame(
  Year = "Next Year",
  Mean_HighDamage = mean(df_next$Delta_NextYear[df_next$New_Damage_Category == "High Damage Neighbor"]),
  Mean_None = mean(df_next$Delta_NextYear[df_next$New_Damage_Category == "None"]),
  t_statistic = t_test_next$statistic,
  df = t_test_next$parameter,
  p_value = t_test_next$p.value,
  conf_low = t_test_next$conf.int[1],
  conf_high = t_test_next$conf.int[2]
)

final_results <- bind_rows(result_this, result_next)

# Step 5: Load existing workbook and add new sheet
file_path <- file.path(project_root, "outputs", "high_damage_ttest_results.xlsx")
wb <- loadWorkbook(file_path)

addWorksheet(wb, "HighDamage_vs_None")
writeData(wb, sheet = "HighDamage_vs_None", final_results)

# Step 6: Save workbook (overwrite)
saveWorkbook(wb, file = file_path, overwrite = TRUE)

})
