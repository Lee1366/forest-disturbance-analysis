local({
# Cleaned copy of hotspot_recovery(1).R; formulas and research alternatives retained.
# See README.md and REVIEW_NOTES.md before interpreting outputs.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) stop("Run from the repository root.")
required_packages <- c("dplyr", "readxl", "ggplot2", "purrr", "patchwork", "cowplot", "tidyr")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run 00_setup_packages.R to install: ", paste(missing, collapse = ", "))
library(dplyr)
library(readxl)
library(ggplot2)
library(purrr)
library(patchwork)
library(cowplot)
library(tidyr)

script_output <- file.path(project_root, "outputs", "09a_explore_hotspot_recovery_baselines")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##17.09.2025
##Hypothesis 4
##hotspot recovery


bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK1")

# Ensure your relevant columns exist: ERHEBUNGSBEZIRK, JAHR, Damage_Class, Regular_Harvest


hotspots_bk1 <- bk1 %>%
  filter(Damage_Class == "Hot Spot") %>%
  arrange(g_name, JAHR)


hotspots_bk1 <- hotspots_bk1 %>%
  group_by(g_name) %>%
  mutate(
    gap = JAHR - lag(JAHR, default = first(JAHR)-2),
    event_id = cumsum(gap > 1) + 1
  ) %>%
  ungroup()


hotspot_start_bk1 <- hotspots_bk1 %>%
  group_by(g_name, event_id) %>%
  summarise(start_year = min(JAHR), .groups = "drop")

hotspot_start_bk1

#Step 4: Join hotspot start years with the full BK1 dataset


bk1_events <- hotspot_start_bk1 %>%
  mutate(event_id = row_number()) %>%   # give each hotspot start a unique ID
  group_by(g_name, start_year, event_id) %>%
  group_split() %>%
  map_dfr(~ {
    bk1 %>%
      filter(g_name == .x$g_name,
             JAHR >= .x$start_year,
             JAHR <= .x$start_year + 5) %>%
      mutate(
        start_year = .x$start_year,
        event_id = .x$event_id,
        years_after = JAHR - .x$start_year
      )
  })


#1. Compute baseline (year 0 = hotspot year) and compare

bk1_changes <- bk1_events %>%
  group_by(event_id, years_after) %>%
  summarise(mean_harvest = mean(Regular_Harvest, na.rm = TRUE), .groups = "drop") %>%
  group_by(event_id) %>%
  mutate(
    baseline = mean_harvest[years_after == 0],
    pct_change = 100 * (mean_harvest - baseline) / baseline
  ) %>%
  ungroup()


#2. Summarize across all hotspots

bk1_pct_summary <- bk1_changes %>%
  group_by(years_after) %>%
  summarise(
    mean_pct = mean(pct_change, na.rm = TRUE),
    sd_pct = sd(pct_change, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se_pct = sd_pct / sqrt(n))


#3. Visualize

p_bk1 <- ggplot(bk1_pct_summary, aes(x = years_after, y = mean_pct)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 2, color = "steelblue") +
  geom_errorbar(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct),
                width = 0.2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Relative Change in Regular Harvesting After Hotspot (BK1)",
    x = "Years After Hotspot",
    y = "% Change from Hotspot Year"
  ) +
  theme_minimal(base_size = 14)

print(p_bk1)   


nrow(bk1_pct_summary)
head(bk1_pct_summary)

#########################################################


# Load BK2 data
bk2 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK2")

# Step 1: Identify hotspot years
hotspots_bk2 <- bk2 %>%
  filter(Damage_Class == "Hot Spot") %>%
  arrange(g_name, JAHR)

# Step 2: Create event_id for each hotspot episode
hotspots_bk2 <- hotspots_bk2 %>%
  group_by(g_name) %>%
  mutate(
    gap = JAHR - lag(JAHR, default = first(JAHR) - 2),
    event_id = cumsum(gap > 1) + 1
  ) %>%
  ungroup()

# Step 3: Extract first year of each hotspot
hotspot_start_bk2 <- hotspots_bk2 %>%
  group_by(g_name, event_id) %>%
  summarise(start_year = min(JAHR), .groups = "drop")

# Step 4: Join hotspot start years with full BK2 dataset
bk2_events <- hotspot_start_bk2 %>%
  mutate(event_id = row_number()) %>%   # unique ID
  group_by(g_name, start_year, event_id) %>%
  group_split() %>%
  map_dfr(~ {
    bk2 %>%
      filter(g_name == .x$g_name,
             JAHR >= .x$start_year,
             JAHR <= .x$start_year + 5) %>%
      mutate(
        start_year = .x$start_year,
        event_id = .x$event_id,
        years_after = JAHR - .x$start_year
      )
  })

# Step 5: Compute baseline (year 0) and percentage change
bk2_changes <- bk2_events %>%
  group_by(event_id, years_after) %>%
  summarise(mean_harvest = mean(Regular_Harvest, na.rm = TRUE), .groups = "drop") %>%
  group_by(event_id) %>%
  mutate(
    baseline = mean_harvest[years_after == 0],
    pct_change = 100 * (mean_harvest - baseline) / baseline
  ) %>%
  ungroup()

# Step 6: Summarize across all hotspots
bk2_pct_summary <- bk2_changes %>%
  group_by(years_after) %>%
  summarise(
    mean_pct = mean(pct_change, na.rm = TRUE),
    sd_pct = sd(pct_change, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se_pct = sd_pct / sqrt(n))

# Step 7: Plot
p_bk2 <- ggplot(bk2_pct_summary, aes(x = years_after, y = mean_pct)) +
  geom_line(color = "darkorange", linewidth = 1.2) +
  geom_point(size = 2, color = "darkorange") +
  geom_errorbar(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct),
                width = 0.2, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Relative Change in Regular Harvesting After Hotspot (BK2)",
    x = "Years After Hotspot",
    y = "% Change from Hotspot Year"
  ) +
  theme_minimal(base_size = 14)

print(p_bk2)

###########################################################################


# Load BK3 data
bk3 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK3")

# Step 1: Identify hotspot years
hotspots_bk3 <- bk3 %>%
  filter(Damage_Class == "Hot Spot") %>%
  arrange(g_name, JAHR)

# Step 2: Create event_id for each hotspot episode
hotspots_bk3 <- hotspots_bk3 %>%
  group_by(g_name) %>%
  mutate(
    gap = JAHR - lag(JAHR, default = first(JAHR) - 2),
    event_id = cumsum(gap > 1) + 1
  ) %>%
  ungroup()

# Step 3: Extract first year of each hotspot
hotspot_start_bk3 <- hotspots_bk3 %>%
  group_by(g_name, event_id) %>%
  summarise(start_year = min(JAHR), .groups = "drop")

# Step 4: Join hotspot start years with full BK3 dataset
bk3_events <- hotspot_start_bk3 %>%
  mutate(event_id = row_number()) %>%   # unique ID
  group_by(g_name, start_year, event_id) %>%
  group_split() %>%
  map_dfr(~ {
    bk3 %>%
      filter(g_name == .x$g_name,
             JAHR >= .x$start_year,
             JAHR <= .x$start_year + 5) %>%
      mutate(
        start_year = .x$start_year,
        event_id = .x$event_id,
        years_after = JAHR - .x$start_year
      )
  })

# Step 5: Compute baseline (year 0) and percentage change
bk3_changes <- bk3_events %>%
  group_by(event_id, years_after) %>%
  summarise(mean_harvest = mean(Regular_Harvest, na.rm = TRUE), .groups = "drop") %>%
  group_by(event_id) %>%
  mutate(
    baseline = mean_harvest[years_after == 0],
    pct_change = 100 * (mean_harvest - baseline) / baseline
  ) %>%
  ungroup()

# Step 6: Summarize across all hotspots
bk3_pct_summary <- bk3_changes %>%
  group_by(years_after) %>%
  summarise(
    mean_pct = mean(pct_change, na.rm = TRUE),
    sd_pct = sd(pct_change, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se_pct = sd_pct / sqrt(n))

# Step 7: Plot
p_bk3 <- ggplot(bk3_pct_summary, aes(x = years_after, y = mean_pct)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(size = 2, color = "darkgreen") +
  geom_errorbar(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct),
                width = 0.2, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Relative Change in Regular Harvesting After Hotspot (BK3)",
    x = "Years After Hotspot",
    y = "% Change from Hotspot Year"
  ) +
  theme_minimal(base_size = 14)

print(p_bk3)


###################################################


p_bk1_noleg <- p_bk1 + theme(legend.position = "none")
p_bk2_noleg <- p_bk2 + theme(legend.position = "none")
p_bk3_noleg <- p_bk3 + theme(legend.position = "none")

# Extract legend from one plot

shared_legend <- get_legend(
  p_bk1 + theme(legend.position = "bottom")
)

# Combine plots in a single row or column layout
combined_plot <- (p_bk1_noleg | p_bk2_noleg | p_bk3_noleg) /
  plot_spacer()  # empty space for legend

# Add legend below using cowplot::ggdraw
final_plot <- ggdraw(combined_plot) +
  draw_grob(shared_legend, x = 0.5, y = 0.02, hjust = 0.5, vjust = 0)

# Show plot
print(final_plot)


##########################################


bk1_pct_summary$BK <- "BK1"
bk2_pct_summary$BK <- "BK2"
bk3_pct_summary$BK <- "BK3"

combined_df <- bind_rows(bk1_pct_summary, bk2_pct_summary, bk3_pct_summary)

# Plot
ggplot(combined_df, aes(x = years_after, y = mean_pct, color = BK, group = BK)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  scale_color_manual(values = c("BK1" = "steelblue", "BK2" = "forestgreen", "BK3" = "firebrick2")) +
  labs(
    title = "Relative Change in Regular Harvesting After Hotspot",
    x = "Years After Hotspot",
    y = "% Change from Hotspot Year",
    color = "Ownership Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )


# Save
ggsave("combined_BK1_BK2_BK3.png",
       final_plot,
       width = 20,
       height = 8,
       dpi = 600,
       units = "in",
       type = "cairo-png")

###################################################################
#############################################

##for salvage wood just for myself


# Step 1: Load BK1
bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK1")

# Step 2: Identify hotspot years
hotspots_bk1 <- bk1 %>%
  filter(Damage_Class == "Hot Spot") %>%
  arrange(g_name, JAHR)

hotspots_bk1 <- hotspots_bk1 %>%
  group_by(g_name) %>%
  mutate(
    gap = JAHR - lag(JAHR, default = first(JAHR) - 2),
    event_id = cumsum(gap > 1) + 1
  ) %>%
  ungroup()

hotspot_start_bk1 <- hotspots_bk1 %>%
  group_by(g_name, event_id) %>%
  summarise(start_year = min(JAHR), .groups = "drop")

# Step 3: Create dataset of years after hotspot
bk1_events <- hotspot_start_bk1 %>%
  mutate(event_id = row_number()) %>%
  group_by(g_name, start_year, event_id) %>%
  group_split() %>%
  map_dfr(~ {
    bk1 %>%
      filter(g_name == .x$g_name,
             JAHR >= .x$start_year,
             JAHR <= .x$start_year + 5) %>%
      mutate(
        start_year = .x$start_year,
        event_id = .x$event_id,
        years_after = JAHR - .x$start_year
      )
  })


# Step 4: Compute baseline and % change for Total_schadholz
bk1_salvage_changes <- bk1_events %>%
  group_by(event_id, years_after) %>%
  summarise(mean_salvage = mean(Total_Schadholz, na.rm = TRUE), .groups = "drop") %>%
  group_by(event_id) %>%
  mutate(
    baseline = mean_salvage[years_after == 0],
    pct_change = 100 * (mean_salvage - baseline) / baseline
  ) %>%
  ungroup()

# Step 5: Summarize across all hotspots
bk1_salvage_summary <- bk1_salvage_changes %>%
  group_by(years_after) %>%
  summarise(
    mean_pct = mean(pct_change, na.rm = TRUE),
    sd_pct = sd(pct_change, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se_pct = sd_pct / sqrt(n))

# Step 6: Visualize
p_bk1_salvage <- ggplot(bk1_salvage_summary, aes(x = years_after, y = mean_pct)) +
  geom_line(color = "firebrick2", linewidth = 1.2) +
  geom_point(size = 2, color = "firebrick2") +
  geom_errorbar(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct),
                width = 0.2, color = "firebrick2") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Relative Change in Total Salvage Wood After Hotspot (BK1)",
    x = "Years After Hotspot",
    y = "% Change from Hotspot Year"
  ) +
  theme_minimal(base_size = 14)

print(p_bk1_salvage)
########################################################################

#Prepare the data


anova_data <- bind_rows(
  bk1_changes %>% filter(years_after %in% c(0,5)) %>% mutate(BK = "BK1"),
  bk2_changes %>% filter(years_after %in% c(0,5)) %>% mutate(BK = "BK2"),
  bk3_changes %>% filter(years_after %in% c(0,5)) %>% mutate(BK = "BK3")
) %>%
  select(event_id, BK, years_after, mean_harvest) %>%
  mutate(Year = factor(years_after, levels = c(0,5)))


# Check normality

anova_model <- aov(mean_harvest ~ BK * Year, data = anova_data)
summary(anova_model)

# Extract residuals
residuals_anova <- residuals(anova_model)

# Shapiro-Wilk test
shapiro.test(residuals_anova)

# Optional: Q-Q plot
qqnorm(residuals_anova)
qqline(residuals_anova, col = "red")
############################################################

##23.09.2025
##new hypo

# --- Libraries ---


bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"),
                  sheet = "BK1")

# Make sure the sheet has these columns:
# g_name (district name), JAHR (year), Damage_Class, Total_harvest


# --- 2. Identify hotspot events (year 0 = first year of each hotspot) ---
hotspots_bk1 <- bk1 %>%
  filter(Damage_Class == "Hot Spot") %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    gap = JAHR - lag(JAHR, default = first(JAHR)-2),
    event_id = cumsum(gap > 1) + 1
  ) %>%
  group_by(g_name, event_id) %>%
  summarise(start_year = min(JAHR), .groups = "drop")

# --- 3. Extract harvesting around each event ---
bk1_events <- hotspots_bk1 %>%
  mutate(event_id = row_number()) %>%
  group_by(g_name, start_year, event_id) %>%
  group_split() %>%
  map_dfr(~ {
    # baseline: 10 years before hotspot start
    baseline <- bk1 %>%
      filter(g_name == .x$g_name,
             JAHR >= .x$start_year - 10,
             JAHR <  .x$start_year) %>%
      summarise(baseline = mean(Total_Gesamteinschlag, na.rm = TRUE)) %>%
      pull(baseline)
    
    # get years 0–5 after hotspot
    bk1 %>%
      filter(g_name == .x$g_name,
             JAHR >= .x$start_year,
             JAHR <= .x$start_year + 5) %>%
      mutate(
        years_after = JAHR - .x$start_year,
        baseline = baseline,
        pct_change = 100 * (Total_Gesamteinschlag - baseline) / baseline,
        event_id = .x$event_id
      )
  })

# --- 4. Summarize across events ---
bk1_total_summary <- bk1_events %>%
  group_by(years_after) %>%
  summarise(
    mean_pct = mean(pct_change, na.rm = TRUE),
    sd_pct   = sd(pct_change, na.rm = TRUE),
    n        = n(),
    se_pct   = sd_pct / sqrt(n)
  )

# --- 5. Plot ---
ggplot(bk1_total_summary, aes(x = years_after, y = mean_pct)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(size = 2, color = "darkred") +
  geom_errorbar(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct),
                width = 0.2, color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Deviation in Total Harvesting Relative to 10-Year Pre-Event Baseline (BK1)",
    x = "Years After Hotspot Event",
    y = "% Change from Pre-Event 10-Year Average"
  ) +
  theme_minimal(base_size = 14)
######################################################


# --- Libraries ---


process_bk <- function(file_path, sheet_name) {
  bk <- read_excel(file_path, sheet = sheet_name)
  
  # Identify hotspot events
  hotspots <- bk %>%
    filter(Damage_Class == "Hot Spot") %>%
    arrange(g_name, JAHR) %>%
    group_by(g_name) %>%
    mutate(
      gap = JAHR - lag(JAHR, default = first(JAHR)-2),
      event_id = cumsum(gap > 1) + 1
    ) %>%
    group_by(g_name, event_id) %>%
    summarise(start_year = min(JAHR), .groups = "drop")
  
  # Extract harvesting around each event and compute % change relative to 10-year baseline
  bk_events <- hotspots %>%
    mutate(event_id = row_number()) %>%
    group_by(g_name, start_year, event_id) %>%
    group_split() %>%
    map_dfr(~ {
      baseline <- bk %>%
        filter(g_name == .x$g_name,
               JAHR >= .x$start_year - 10,
               JAHR <  .x$start_year) %>%
        summarise(baseline = mean(Total_Gesamteinschlag, na.rm = TRUE)) %>%
        pull(baseline)
      
      bk %>%
        filter(g_name == .x$g_name,
               JAHR >= .x$start_year,
               JAHR <= .x$start_year + 5) %>%
        mutate(
          years_after = JAHR - .x$start_year,
          baseline = baseline,
          pct_change = 100 * (Total_Gesamteinschlag - baseline) / baseline,
          event_id = .x$event_id
        )
    })
  
  # Summarize across events
  summary_df <- bk_events %>%
    group_by(years_after) %>%
    summarise(
      mean_pct = mean(pct_change, na.rm = TRUE),
      sd_pct   = sd(pct_change, na.rm = TRUE),
      n        = n(),
      se_pct   = sd_pct / sqrt(n)
    ) %>%
    mutate(BK = sheet_name)
  
  return(summary_df)
}

# --- 2. File path ---
file_path <- file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx")

# --- 3. Process BK1, BK2, BK3 ---
bk1_summary <- process_bk(file_path, "BK1")
bk2_summary <- process_bk(file_path, "BK2")
bk3_summary <- process_bk(file_path, "BK3")

# --- 4. Combine all ---
combined_summary <- bind_rows(bk1_summary, bk2_summary, bk3_summary)

# --- 5. Plot combined ---
ggplot(combined_summary, aes(x = years_after, y = mean_pct, color = BK)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Deviation in Total Harvesting Relative to 10-Year Pre-Event Baseline",
    x = "Years After Hotspot Event",
    y = "% Change from Pre-Event 10-Year Average",
    color = "Forest Type"
  ) +
  theme_minimal(base_size = 14)
####################################

##with shaded ribbon instead of Error bar

# --- Combined plot with shaded SE ribbons ---
ggplot(combined_summary, aes(x = years_after, y = mean_pct, color = BK, fill = BK)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Deviation in Total Harvesting Relative to 10-Year Pre-Event Baseline",
    x = "Years After Hotspot Event",
    y = "% Change from Pre-Event 10-Year Average",
    color = "Forest Type",
    fill = "Forest Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
#############################################################


process_bk_regular <- function(sheet_name, bk_label) {
  bk <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = sheet_name)
  
  hotspots <- bk %>%
    filter(Damage_Class == "Hot Spot") %>%
    arrange(g_name, JAHR) %>%
    group_by(g_name) %>%
    mutate(
      gap = JAHR - lag(JAHR, default = first(JAHR)-2),
      event_id = cumsum(gap > 1) + 1
    ) %>%
    ungroup()
  
  hotspot_start <- hotspots %>%
    group_by(g_name, event_id) %>%
    summarise(start_year = min(JAHR), .groups = "drop")
  
  events <- hotspot_start %>%
    mutate(event_id = row_number()) %>%
    group_by(g_name, start_year, event_id) %>%
    group_split() %>%
    map_dfr(~ {
      bk %>%
        filter(g_name == .x$g_name,
               JAHR >= .x$start_year,
               JAHR <= .x$start_year + 5) %>%
        mutate(
          start_year = .x$start_year,
          event_id = .x$event_id,
          years_after = JAHR - .x$start_year
        )
    })
  
  events <- events %>%
    group_by(event_id) %>%
    mutate(
      baseline_reg = sapply(start_year, function(x) {
        mean(bk$Regular_Harvest[bk$g_name == g_name[1] & bk$JAHR >= (x-10) & bk$JAHR <= (x-1)], na.rm = TRUE)
      }),
      pct_change_regular = 100 * (Regular_Harvest - baseline_reg) / baseline_reg,
      BK = bk_label
    ) %>%
    ungroup()
  
  summary_df <- events %>%
    group_by(years_after, BK) %>%
    summarise(
      mean_pct = mean(pct_change_regular, na.rm = TRUE),
      sd_pct = sd(pct_change_regular, na.rm = TRUE),
      n = n(),
      se_pct = sd_pct / sqrt(n),
      .groups = "drop"
    )
  
  return(summary_df)
}

# --- Process BK1, BK2, BK3 ---
bk1_summary <- process_bk_regular("BK1", "BK1")
bk2_summary <- process_bk_regular("BK2", "BK2")
bk3_summary <- process_bk_regular("BK3", "BK3")

# --- Combine all ---
all_bks_summary <- bind_rows(bk1_summary, bk2_summary, bk3_summary)

# --- Plot ---
ggplot(all_bks_summary, aes(x = years_after, y = mean_pct, color = BK, group = BK)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_pct - se_pct, ymax = mean_pct + se_pct, fill = BK), alpha = 0.15, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "Regular Harvesting Deviation Relative to Pre-Event 10-Year Average",
    x = "Years After Hotspot Event",
    y = "% Change from Pre-Event 10-Year Average",
    color = "Forest Type (BK)",
    fill = "Forest Type (BK)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
})
