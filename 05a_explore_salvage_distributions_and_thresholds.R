local({
# Cleaned copy of Histogram(1).R; formulas and research alternatives retained.
# See README.md and REVIEW_NOTES.md before interpreting outputs.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) stop("Run from the repository root.")
required_packages <- c("sf", "dplyr", "ggplot2", "readxl", "scales")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run 00_setup_packages.R to install: ", paste(missing, collapse = ", "))
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)

script_output <- file.path(project_root, "outputs", "05a_explore_salvage_distributions_and_thresholds")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##Histogram for salvage wood
##16.06.2025


data <- read_excel(file.path(project_root, "data", "final_classified_damage.xlsx"), sheet = "Sheet1")

# Optional: check column names
colnames(data)

# Plot histogram
ggplot(data, aes(x = Schadholz)) +
  geom_histogram(binwidth = 350) +  # Adjust binwidth to match your scale
  labs(
    title = "Distribution of Salvage Wood in Austrian Districts (25 Years)",
    x = "Salvage Wood (EFM)",
    y = "Number of District-Year Observations"
  )
######################

ggplot(data, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1) +  # Use log scale so binwidths adjust automatically
  scale_x_log10() +
  labs(
    title = "Log Distribution of Salvage Wood",
    x = "Salvage Wood (log scale, EFM)",
    y = "Number of District-Year Observations"
  )
#########################


data_filtered <- data %>% filter(Schadholz > 0)

# Now plot with log scale
ggplot(data_filtered, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_log10() +
  labs(
    title = "Log Distribution of Salvage Wood",
    x = "Salvage Wood (log scale, EFM)",
    y = "Number of District-Year Observations"
  )
#####################


ggplot(data_filtered, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
  scale_x_log10(labels = scales::comma_format()) +  # normal numbers with commas, no scientific notation
  labs(
    title = "Histogram of Salvage Wood (Log Scale)",
    x = "Salvage Wood (EFM, log scale)",
    y = "Number of District-Year Observations"
  )
################################
##top 10% threshold


data_filtered <- data %>% filter(Schadholz > 0)

# Calculate 90th percentile (top 10% threshold)
hotspot_threshold <- quantile(data_filtered$Schadholz, 0.9)

# Plot histogram with log scale and threshold line
ggplot(data_filtered, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = hotspot_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Salvage Wood (Log Scale)",
    subtitle = paste("Red dashed line = 90th percentile (", round(hotspot_threshold), "Efm)", sep = ""),
    x = "Salvage Wood (Efm, log scale)",
    y = "Number of District-Year Observations"
  )
############################
##top 1% threshold

data_filtered <- data %>% filter(Schadholz > 0)

# Calculate 99th percentile (top 1% threshold)
hotspot_threshold <- quantile(data_filtered$Schadholz, 0.99)

# Plot histogram with log scale and threshold line
ggplot(data_filtered, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = hotspot_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Salvage Wood (Log Scale)",
    subtitle = paste("Red dashed line = 99th percentile (", round(hotspot_threshold), "Efm)", sep = ""),
    x = "Salvage Wood (Efm, log scale)",
    y = "Number of District-Year Observations"
  )
###############


data_filtered <- data %>% filter(Schadholz > 0)

# Calculate the 99th percentile threshold
hotspot_threshold <- quantile(data_filtered$Schadholz, 0.99)

# Extract rows that are in the top 1%
hotspot_data <- data_filtered %>% 
  filter(Schadholz >= hotspot_threshold) %>%
  arrange(desc(Schadholz))  # optional: sort from highest to lowest

# Show the district, year, and salvage wood value
hotspot_data %>% 
  select(ERHEBUNGSBEZIRK, JAHR, Schadholz)

###########################
##threshold 0.5%


data_filtered <- data %>% filter(Schadholz > 0)

# Calculate 99.5th percentile (top 0.5% threshold)
hotspot_threshold <- quantile(data_filtered$Schadholz, 0.995)

# Plot histogram with log scale and threshold line
ggplot(data_filtered, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = hotspot_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Salvage Wood (Log Scale)",
    subtitle = paste("Red dashed line = 99.5th percentile (", round(hotspot_threshold), "Efm)", sep = ""),
    x = "Salvage Wood (Efm, log scale)",
    y = "Number of District-Year Observations"
  )
######################
# Filter out non-positive values
data_filtered <- data %>% filter(Schadholz > 0)

# Calculate the 99th percentile threshold
hotspot_threshold <- quantile(data_filtered$Schadholz, 0.995)

# Extract rows that are in the top 1%
hotspot_data <- data_filtered %>% 
  filter(Schadholz >= hotspot_threshold) %>%
  arrange(desc(Schadholz))  # optional: sort from highest to lowest

# Show the district, year, and salvage wood value
hotspot_data %>% 
  select(ERHEBUNGSBEZIRK, JAHR, Schadholz)
########################################


data_filtered <- data %>% filter(Schadholz > 0)

# Calculate thresholds
threshold_10 <- quantile(data_filtered$Schadholz, 0.90)
threshold_1 <- quantile(data_filtered$Schadholz, 0.99)
threshold_0.5 <- quantile(data_filtered$Schadholz, 0.995)

# Create a new column classifying thresholds
data_classified <- data_filtered %>%
  mutate(
    Threshold = case_when(
      Schadholz >= threshold_0.5 ~ "Top 0.5%",
      Schadholz >= threshold_1 ~ "Top 1%",
      Schadholz >= threshold_10 ~ "Top 10%",
      TRUE ~ "Below 10%"
    )
  ) %>%
  select(ERHEBUNGSBEZIRK, JAHR, Gesamteinschlag, Schadholz, Threshold) %>%
  arrange(desc(Schadholz))

# View the resulting table
data_classified

#############################################
#histogram totall without bK

data <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), sheet = "Neighbors_names")

# Filter out zero or negative values for log scale
data_filtered <- data %>% filter(Total_Schadholz > 0)

# Calculate 90th percentile (top 10% threshold)
hotspot_threshold <- quantile(data_filtered$Total_Schadholz, 0.9)

# Plot histogram with log scale and threshold line
ggplot(data_filtered, aes(x = Total_Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = hotspot_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Salvage Wood (Log Scale)",
    subtitle = paste("Red dashed line = 90th percentile (", round(hotspot_threshold), "Efm)", sep = ""),
    x = "Salvage Wood (Efm, log scale)",
    y = "Number of District-Year Observations"
  )
############################
##no log salvage without BK


data <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), 
                   sheet = "Neighbors_names")

# Calculate percentile cutoffs
p90 <- quantile(data$Total_Schadholz, 0.90, na.rm = TRUE)  # cutoff for top 10%
p60 <- quantile(data$Total_Schadholz, 0.60, na.rm = TRUE)  # cutoff for next 30%
p30 <- quantile(data$Total_Schadholz, 0.30, na.rm = TRUE)  # cutoff for next 30%

# Assign damage category based on thresholds
data <- data %>%
  mutate(Damage_Category = case_when(
    Total_Schadholz >= p90 ~ "Hot Spot",
    Total_Schadholz >= p60 & Total_Schadholz < p90 ~ "High Damage",
    Total_Schadholz >= p30 & Total_Schadholz < p60 ~ "Moderate Damage",
    TRUE ~ "Low Damage"
  ))

# Plot histogram with cutoffs
ggplot(data, aes(x = Total_Schadholz, fill = Damage_Category)) +
  geom_histogram(binwidth = 5000, color = "black", alpha = 0.7, position = "identity") +
  geom_vline(xintercept = c(p90, p60, p30), 
             linetype = "dashed", 
             color = c("red", "blue", "darkgreen"), 
             size = 1) +
  scale_fill_manual(values = c(
    "Hot Spot" = "red", 
    "High Damage" = "orange", 
    "Moderate Damage" = "yellow", 
    "Low Damage" = "green")) +
  labs(
    title = "Histogram of Salvage Wood with Damage Categories",
    x = "Salvage Wood (Efm)",
    y = "Number of District-Year Observations",
    fill = "Damage Category"
  )
############################################
## Log salvage without BK


data <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), 
                   sheet = "Neighbors_names")

# Calculate percentile cutoffs on original scale
p90 <- quantile(data$Total_Schadholz, 0.90, na.rm = TRUE)
p60 <- quantile(data$Total_Schadholz, 0.60, na.rm = TRUE)
p30 <- quantile(data$Total_Schadholz, 0.30, na.rm = TRUE)

# Assign damage category based on original scale
data <- data %>%
  mutate(Damage_Category = case_when(
    Total_Schadholz >= p90 ~ "Hot Spot",
    Total_Schadholz >= p60 & Total_Schadholz < p90 ~ "High Damage",
    Total_Schadholz >= p30 & Total_Schadholz < p60 ~ "Moderate Damage",
    TRUE ~ "Low Damage"
  ))

# Plot histogram with log10 scale on x axis
ggplot(data, aes(x = Total_Schadholz, fill = Damage_Category)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7, position = "identity") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = c(p90, p60, p30), 
             linetype = "dashed", 
             color = c("red", "blue", "darkgreen"), 
             size = 1) +
  scale_fill_manual(values = c(
    "Hot Spot" = "red", 
    "High Damage" = "orange", 
    "Moderate Damage" = "yellow", 
    "Low Damage" = "green")) +
  labs(
    title = "Histogram of Salvage Wood (Log Scale) with Damage Categories",
    x = "Salvage Wood (Efm, log scale)",
    y = "Number of District-Year Observations",
    fill = "Damage Category"
  )
#######################################################
##histogram for Harvesting without Bk

# Load data
data <- read_excel(file.path(project_root, "outputs", "neighbors_category.xlsx"), 
                   sheet = "Neighbors_names")

##log

ggplot(data, aes(x = Total_Gesamteinschlag)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  scale_x_log10() +
  labs(
    title = "Histogram of Total Harvest (Log Scale)",
    x = "Log10(Total_Gesamteinschlag)",
    y = "Number of Observations"
  )
############################
#no log

ggplot(data, aes(x = Total_Gesamteinschlag)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "black") +
  coord_cartesian(xlim = c(0, 500000)) +  # Adjust this limit based on your data
  labs(
    title = "Histogram of Total Harvest (Efm)",
    x = "Total_Gesamteinschlag",
    y = "Number of Observations"
  )
#########################################################

##histogram for schadholz for BK1

data <- read_excel(file.path(project_root, "data", "final_classified_damage.xlsx"), sheet = "Sheet1")


bk1_data <- data %>% filter(BK == 1 & Schadholz > 0)

# Calculate 90th percentile threshold
bk1_threshold <- quantile(bk1_data$Schadholz, 0.9, na.rm = TRUE)

# Plot histogram
ggplot(bk1_data, aes(x = Schadholz)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  geom_vline(xintercept = bk1_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Schadholz for BK1 (Real Scale)",
    subtitle = paste("Red line = 90th percentile (", round(bk1_threshold), "Efm)", sep = ""),
    x = "Schadholz (Efm)",
    y = "Number of District-Year Observations"
  )
#####################
#log 10

ggplot(bk1_data, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = bk1_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Schadholz for BK1 (Log Scale)",
    subtitle = paste("Red dashed line = 90th percentile (", round(bk1_threshold), "Efm)", sep = ""),
    x = "Schadholz (log scale)",
    y = "Number of Observations"
  )
############################

##log 10 BK2, schadholz

# BK1 only
bk2_data <- data %>% filter(BK == 2 & Schadholz > 0)

# Calculate 90th percentile threshold
bk2_threshold <- quantile(bk2_data$Schadholz, 0.9, na.rm = TRUE)

ggplot(bk2_data, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = bk2_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Schadholz for BK2 (Log Scale)",
    subtitle = paste("Red dashed line = 90th percentile (", round(bk2_threshold), "Efm)", sep = ""),
    x = "Schadholz (log scale)",
    y = "Number of Observations"
  )
####################################

##log 10 BK3, schadholz

# BK1 only
bk3_data <- data %>% filter(BK == 3 & Schadholz > 0)

# Calculate 90th percentile threshold
bk3_threshold <- quantile(bk3_data$Schadholz, 0.9, na.rm = TRUE)

ggplot(bk3_data, aes(x = Schadholz)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  scale_x_log10(labels = label_number()) +
  geom_vline(xintercept = bk3_threshold, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Histogram of Schadholz for BK3 (Log Scale)",
    subtitle = paste("Red dashed line = 90th percentile (", round(bk3_threshold), "Efm)", sep = ""),
    x = "Schadholz (log scale)",
    y = "Number of Observations"
  )
})
