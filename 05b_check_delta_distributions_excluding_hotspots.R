local({
# Cleaned copy of histogram_3(1).R; formulas and research alternatives retained.
# See README.md and REVIEW_NOTES.md before interpreting outputs.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) stop("Run from the repository root.")
required_packages <- c("readxl", "dplyr", "ggplot2", "tidyr")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run 00_setup_packages.R to install: ", paste(missing, collapse = ", "))
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

script_output <- file.path(project_root, "outputs", "05b_check_delta_distributions_excluding_hotspots")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##Histogram excluding hot spot districts
##18.07.2025


BK1 <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"), sheet = "BK1") %>%
  mutate(BK = "BK1")

BK2 <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"), sheet = "BK2") %>%
  mutate(BK = "BK2")

BK3 <- read_excel(file.path(project_root, "data", "BK_final_with_deltas.xlsx"), sheet = "BK3") %>%
  mutate(BK = "BK3")

all_BK <- bind_rows(BK1, BK2, BK3)

##excluding hotspot districts
all_BK_no_hotspot <- all_BK %>%
  filter(Neighbor_Category != "Hot Spot")


all_BK_no_hotspot_long <- all_BK_no_hotspot %>%
  pivot_longer(
    cols = c(Delta_ThisYear, Delta_NextYear),
    names_to = "Year_Type",
    values_to = "Delta_Regular_Harvest"
  )


ggplot(all_BK_no_hotspot_long, aes(x = Delta_Regular_Harvest)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  xlim(-150, 150) +
  facet_grid(BK ~ Year_Type, scales = "free") +
  labs(title = "Histogram of Delta Regular Harvesting (No Hotspots)")


ggplot(all_BK_no_hotspot_long, aes(x = BK, y = Delta_Regular_Harvest, fill = Year_Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, position = position_dodge(width = 0.75)) +
  geom_jitter(aes(color = Year_Type),
              size = 1.2, alpha = 0.4,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  labs(title = "Boxplot of Delta Regular Harvesting by BK and Year (No Hotspots)",
       x = "Ownership Type (BK)",
       y = "Delta Regular Harvesting (±200)",
       fill = "Year", color = "Year") +
  ylim(-200, 200) +
  theme_minimal()


ggplot(all_BK_no_hotspot_long, aes(sample = Delta_Regular_Harvest)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(BK ~ Year_Type, scales = "free") +
  labs(title = "Q-Q Plots of Delta Regular Harvesting by BK and Year (No Hotspots)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


# This Year plot
all_BK_thisyear <- all_BK_no_hotspot_long %>% filter(Year_Type == "Delta_ThisYear")
p1 <- ggplot(all_BK_thisyear, aes(x = Neighbor_Category, y = Delta_Regular_Harvest, fill = BK)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = BK),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1.2, alpha = 0.4) +
  labs(title = "Delta Regular Harvesting by Neighbor Category and BK (This Year) No Hotspots",
       x = "Neighbor Category",
       y = "Delta Regular Harvesting",
       fill = "BK", color = "BK") +
  ylim(-200, 200) +
  theme_minimal()

# Next Year plot
all_BK_nextyear <- all_BK_no_hotspot_long %>% filter(Year_Type == "Delta_NextYear")
p2 <- ggplot(all_BK_nextyear, aes(x = Neighbor_Category, y = Delta_Regular_Harvest, fill = BK)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = BK),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1.2, alpha = 0.4) +
  labs(title = "Delta Regular Harvesting by Neighbor Category and BK (Next Year) No Hotspots",
       x = "Neighbor Category",
       y = "Delta Regular Harvesting",
       fill = "BK", color = "BK") +
  ylim(-200, 200) +
  theme_minimal()

# Print plots separately
print(p1)
print(p2)
})
