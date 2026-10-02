local({
# Cleaned copy of histogram_2(1).R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "ggplot2", "tidyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "distribution_diagnostics")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##histogram for all groups to see normallity
##16.07.2025


BK1 <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"), sheet = "BK1") %>%
  mutate(BK = "BK1")

BK2 <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"), sheet = "BK2") %>%
  mutate(BK = "BK2")

BK3 <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"), sheet = "BK3") %>%
  mutate(BK = "BK3")

all_BK <- bind_rows(BK1, BK2, BK3)


# Reshape data to long format for easier plotting


all_BK_long <- all_BK %>%
  pivot_longer(
    cols = c(Delta_ThisYear, Delta_NextYear),
    names_to = "Year_Type",
    values_to = "Delta_Regular_Harvest"
  )

# Histogram

ggplot(all_BK_long, aes(x = Delta_Regular_Harvest)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  xlim(-150, 150) +  # restrict x-axis to focus on most data
  facet_grid(BK ~ Year_Type, scales = "free") +
  labs(title = "Histogram of Delta Regular Harvesting (zoomed)")

##Boxplot

ggplot(all_BK_long, aes(x = BK, y = Delta_Regular_Harvest, fill = Year_Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, position = position_dodge(width = 0.75)) +
  geom_jitter(aes(color = Year_Type),
              size = 1.2, alpha = 0.4,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  labs(title = "Boxplot of Delta Regular Harvesting by BK and Year",
       x = "Ownership Type (BK)",
       y = "Delta Regular Harvesting (limited to ±200)",
       fill = "Year", color = "Year") +
  ylim(-200, 200) +
  theme_minimal()


##qq

ggplot(all_BK_long, aes(sample = Delta_Regular_Harvest)) +
stat_qq() +
  stat_qq_line() +
  facet_grid(BK ~ Year_Type, scales = "free") +
  labs(title = "Q-Q Plots of Delta Regular Harvesting by BK and Year",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#######################

##Neighboring Category

ggplot(all_BK_long, aes(x = Delta_Regular_Harvest)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
 xlim(-200, 200) +
  facet_grid(Neighbor_Category ~ Year_Type, scales = "free") +
  labs(title = "Histogram of Delta Regular Harvesting by Neighbor Category and Year",
       x = "Delta Regular Harvesting",
       y = "Count") +
  theme_minimal()

###boxplot

ggplot(all_BK_long, aes(x = Neighbor_Category, y = Delta_Regular_Harvest, fill = Year_Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, position = position_dodge(width = 0.75)) +
  geom_jitter(aes(color = Year_Type),
              size = 1.2, alpha = 0.4,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  labs(title = "Boxplot of Delta Regular Harvesting by Neighbor Category and Year",
       x = "Neighbor Category",
       y = "Delta Regular Harvesting",
       fill = "Year",
       color = "Year") +
  ylim(-200, 200) +
  theme_minimal()

##qqplot

ggplot(all_BK_long, aes(sample = Delta_Regular_Harvest)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Neighbor_Category ~ Year_Type, scales = "free") +
  labs(title = "Q-Q Plots of Delta Regular Harvesting by Neighbor Category and Year",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


#####################################


all_BK_thisyear <- all_BK_long %>% filter(Year_Type == "Delta_ThisYear")
p1 <- ggplot(all_BK_thisyear, aes(x = Neighbor_Category, y = Delta_Regular_Harvest, fill = BK)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = BK),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1.2, alpha = 0.4) +
  labs(title = "Delta Regular Harvesting by Neighbor Category and BK (This Year)",
       x = "Neighbor Category",
       y = "Delta Regular Harvesting",
       fill = "BK", color = "BK") +
  ylim(-200, 200) +
  theme_minimal()

# Next Year plot
all_BK_nextyear <- all_BK_long %>% filter(Year_Type == "Delta_NextYear")
p2 <- ggplot(all_BK_nextyear, aes(x = Neighbor_Category, y = Delta_Regular_Harvest, fill = BK)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = BK),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1.2, alpha = 0.4) +
  labs(title = "Delta Regular Harvesting by Neighbor Category and BK (Next Year)",
       x = "Neighbor Category",
       y = "Delta Regular Harvesting",
       fill = "BK", color = "BK") +
  ylim(-200, 200) +
  theme_minimal()

# Print plots separately
print(p1)
print(p2)

})
