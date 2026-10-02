local({
# Cleaned copy of hypo4(1).R; formulas and research alternatives retained.
# See README.md and REVIEW_NOTES.md before interpreting outputs.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) stop("Run from the repository root.")
required_packages <- c("dplyr", "readxl", "tidyr", "ggplot2", "forcats", "FSA")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run 00_setup_packages.R to install: ", paste(missing, collapse = ", "))
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(forcats)
library(FSA)

script_output <- file.path(project_root, "outputs", "09b_analyze_short_hotspot_episodes_and_recovery")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##08.10.2025
##hypo 4


bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"),
                  sheet = "BK1") %>%
  mutate(JAHR = as.numeric(JAHR))

# Adjust column names if needed:
# - Damage_Class: "Hot Spot", "High", "Moderate", "Low"
# - Total_Schadholz: salvage volume column
bk1 <- bk1 %>% mutate(hotspot = if_else(Damage_Class == "Hot Spot", 1L, 0L))

# --- 2) Build non-overlapping hotspot episodes per district ---
bk1_ep <- bk1 %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    start_of_run = hotspot == 1L & lag(hotspot, default = 0L) == 0L,
    gap = if_else(is.na(lag(JAHR)), FALSE, JAHR - lag(JAHR) > 1),
    start_of_run = start_of_run | (hotspot == 1L & gap),
    episode_id = cumsum(start_of_run)
  ) %>%
  ungroup()

# --- 3) Episode lengths (only considering hotspot years) ---
episode_lengths <- bk1_ep %>%
  filter(hotspot == 1L) %>%
  group_by(g_name, episode_id) %>%
  summarise(ep_len = n(), .groups = "drop")

# --- 4) Keep only single-year and two-year episodes ---
valid_episodes <- episode_lengths %>% filter(ep_len %in% c(1, 2))

bk1_ep_filtered <- bk1_ep %>%
  semi_join(valid_episodes, by = c("g_name","episode_id"))

# --- 5) Choose event year:
# - if ep_len == 1: that year
# - if ep_len == 2: pick the year with higher Total_Schadholz
events_bk1 <- bk1_ep_filtered %>%
  filter(hotspot == 1L) %>%
  left_join(valid_episodes, by = c("g_name","episode_id")) %>%
  group_by(g_name, episode_id, ep_len) %>%
  {
    # for 2-year episodes, choose max salvage; for 1-year, just that row
    two_year <- filter(., ep_len == 2) %>%
      slice_max(order_by = Total_Schadholz, n = 1, with_ties = FALSE)
    one_year <- filter(., ep_len == 1) %>%
      slice_min(order_by = JAHR, n = 1, with_ties = FALSE) # single row anyway
    bind_rows(two_year, one_year)
  } %>%
  ungroup() %>%
  transmute(g_name, event_year = JAHR, episode_id, ep_len)

# --- 6) Attach event year and extract classes at t0..t3 ---
bk1_ev <- bk1 %>%
  left_join(events_bk1, by = "g_name") %>%
  filter(!is.na(event_year)) %>%
  mutate(tau = JAHR - event_year) %>%
  filter(tau %in% 0:3) %>%
  select(g_name, event_year, JAHR, tau, Damage_Class, Total_Schadholz, ep_len)

bk1_classes <- bk1_ev %>%
  mutate(label = paste0("class_t", tau)) %>%
  select(g_name, event_year, ep_len, label, Damage_Class) %>%
  pivot_wider(names_from = label, values_from = Damage_Class)

# --- 7) Quick summaries of what hotspots become (t+1..t+3) ---
table_t1 <- bk1_classes %>% count(class_t1) %>% mutate(pct = 100*n/sum(n))
table_t2 <- bk1_classes %>% count(class_t2) %>% mutate(pct = 100*n/sum(n))
table_t3 <- bk1_classes %>% count(class_t3) %>% mutate(pct = 100*n/sum(n))


# How many hotspot episodes total (before filtering)
bk1_ep %>% filter(hotspot==1) %>% group_by(g_name, episode_id) %>%
  summarise(ep_len=n(), .groups="drop") %>% count(ep_len)

# This shows how many episodes of length 1,2,3,4... exist before filtering.

#########
######################################


bk1_dev <- bk1 %>%
  left_join(events_bk1, by = "g_name") %>%
  filter(!is.na(event_year)) %>%
  mutate(tau = JAHR - event_year) %>%
  group_by(g_name, event_year) %>%
  mutate(
    baseline_total = mean(Total_Gesamteinschlag[tau %in% -10:-1], na.rm = TRUE),
    deviation_pct  = 100 * (Total_Gesamteinschlag - baseline_total) / baseline_total
  ) %>%
  ungroup()

# --- Average across all BK1 districts for each tau ---
bk1_mean <- bk1_dev %>%
  filter(tau >= -5, tau <= 5) %>%
  group_by(tau) %>%
  summarise(mean_dev = mean(deviation_pct, na.rm = TRUE),
            sd = sd(deviation_pct, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n)) %>%
  ungroup()

ggplot(bk1_mean, aes(x = tau, y = mean_dev)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(color = "blue", size = 1.3) +
  geom_ribbon(aes(ymin = mean_dev - se, ymax = mean_dev + se),
              fill = "blue", alpha = 0.2) +
  scale_x_continuous(breaks = seq(-5,5,1)) +
  labs(x = "Years since disturbance event (τ)",
       y = "Deviation from pre-event 10-year mean (%)",
       title = "BK1 (Kleinwald): average deviation of total harvest from 10-yr baseline") +
  theme_minimal()

#######################################


#######################################################################################################
##BK2


path <- file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx")

# --- BK2: read & prep ---
bk2 <- read_excel(path, sheet = "BK2") %>%
  mutate(JAHR = as.numeric(JAHR),
         hotspot = if_else(Damage_Class == "Hot Spot", 1L, 0L))

# --- build non-overlapping hotspot episodes per district ---
bk2_ep <- bk2 %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    start_of_run = hotspot == 1L & lag(hotspot, default = 0L) == 0L,
    gap = if_else(is.na(lag(JAHR)), FALSE, JAHR - lag(JAHR) > 1),
    start_of_run = start_of_run | (hotspot == 1L & gap),
    episode_id = cumsum(start_of_run)
  ) %>% ungroup()

# episode lengths (hotspot-only)
episode_lengths2 <- bk2_ep %>%
  filter(hotspot == 1L) %>%
  group_by(g_name, episode_id) %>%
  summarise(ep_len = n(), .groups = "drop")

# keep only single- and two-year episodes
valid_episodes2 <- episode_lengths2 %>% filter(ep_len %in% c(1,2))

bk2_ep_filtered <- bk2_ep %>%
  semi_join(valid_episodes2, by = c("g_name","episode_id"))

# choose event year:
#  - if ep_len==1: that year
#  - if ep_len==2: pick the year with higher Total_Schadholz
events_bk2 <- bk2_ep_filtered %>%
  filter(hotspot == 1L) %>%
  left_join(valid_episodes2, by = c("g_name","episode_id")) %>%
  group_by(g_name, episode_id, ep_len) %>%
  {
    two_year <- filter(., ep_len == 2) %>%
      slice_max(order_by = Total_Schadholz, n = 1, with_ties = FALSE)
    one_year <- filter(., ep_len == 1) %>%
      slice_min(order_by = JAHR, n = 1, with_ties = FALSE)
    bind_rows(two_year, one_year)
  } %>%
  ungroup() %>%
  transmute(g_name, event_year = JAHR, episode_id, ep_len)

# --- compute baseline & deviations (10-year pre-event of Total_Gesamteinschlag) ---
bk2_dev <- bk2 %>%
  left_join(events_bk2, by = "g_name") %>%
  filter(!is.na(event_year)) %>%
  mutate(tau = JAHR - event_year) %>%
  group_by(g_name, event_year) %>%
  mutate(
    baseline_total = mean(Total_Gesamteinschlag[tau %in% -10:-1], na.rm = TRUE),
    deviation_pct  = 100 * (Total_Gesamteinschlag - baseline_total) / baseline_total
  ) %>% ungroup()

# --- average across BK2 districts for each tau (plot window -5..+5) ---
bk2_mean <- bk2_dev %>%
  filter(tau >= -5, tau <= 5) %>%
  group_by(tau) %>%
  summarise(mean_dev = mean(deviation_pct, na.rm = TRUE),
            sd = sd(deviation_pct, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n), .groups = "drop")

ggplot(bk2_mean, aes(x = tau, y = mean_dev)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(color = "green", size = 1.3) +
  geom_ribbon(aes(ymin = mean_dev - se, ymax = mean_dev + se),
              fill = "green", alpha = 0.2) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  labs(x = "Years since disturbance event (τ)",
       y = "Deviation from pre-event 10-year mean (%)",
       title = "BK2 (Großwald): average deviation of total harvest from 10-yr baseline") +
  theme_minimal()


# How many hotspot episodes total (before filtering)
bk2_ep %>% filter(hotspot==1) %>% group_by(g_name, episode_id) %>%
  summarise(ep_len=n(), .groups="drop") %>% count(ep_len)


# 1) Which episodes are very long?
long_eps <- bk2_ep %>%
  filter(hotspot == 1) %>%
  group_by(g_name, episode_id) %>%
  summarise(ep_len = n(),
            years = paste(sort(unique(JAHR)), collapse = ", "),
            .groups = "drop") %>%
  arrange(desc(ep_len))

head(long_eps, 10)  # inspect the longest ones

# 2) Inspect the exact district & years for the 22-year run
long_eps %>% filter(ep_len >= 10)

##thats correct
#######################################################

##hotspot frequency BK2


bk2_ev <- events_bk2 %>%
  distinct(g_name, event_year) %>%
  arrange(g_name, event_year)

# 2) gaps between successive events within each district
bk2_gaps <- bk2_ev %>%
  group_by(g_name) %>%
  mutate(gap_years = event_year - dplyr::lag(event_year)) %>%
  filter(!is.na(gap_years) & gap_years > 0) %>%  # keep real gaps only
  ungroup()

# 3) pooled (all gaps together) vs. equal-weighted across districts
bk2_pooled_mean <- mean(bk2_gaps$gap_years)

bk2_equal_weighted <- bk2_gaps %>%
  group_by(g_name) %>%
  summarise(mean_gap_dist = mean(gap_years), .groups = "drop") %>%
  summarise(
    mean_of_means   = mean(mean_gap_dist),
    median_of_means = median(mean_gap_dist),
    n_districts     = n()              # number of districts with ≥2 events
  )

list(
  pooled = bk2_pooled_mean,
  equal_weighted = bk2_equal_weighted
)


#########################################
#############
##BK3


path <- file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx")

# --- BK3: read & prep ---
bk3 <- read_excel(path, sheet = "BK3") %>%
  mutate(JAHR = as.numeric(JAHR),
         hotspot = if_else(Damage_Class == "Hot Spot", 1L, 0L))

# --- build non-overlapping hotspot episodes per district ---
bk3_ep <- bk3 %>%
  arrange(g_name, JAHR) %>%
  group_by(g_name) %>%
  mutate(
    start_of_run = hotspot == 1L & lag(hotspot, default = 0L) == 0L,
    gap = if_else(is.na(lag(JAHR)), FALSE, JAHR - lag(JAHR) > 1),
    start_of_run = start_of_run | (hotspot == 1L & gap),
    episode_id = cumsum(start_of_run)
  ) %>% ungroup()

# episode lengths (hotspot-only)
episode_lengths3 <- bk3_ep %>%
  filter(hotspot == 1L) %>%
  group_by(g_name, episode_id) %>%
  summarise(ep_len = n(), .groups = "drop")

# keep only single- and two-year episodes
valid_episodes3 <- episode_lengths3 %>% filter(ep_len %in% c(1, 2))

bk3_ep_filtered <- bk3_ep %>%
  semi_join(valid_episodes3, by = c("g_name","episode_id"))

# choose event year:
#  - if ep_len==1: that year
#  - if ep_len==2: pick the year with higher Total_Schadholz
events_bk3 <- bk3_ep_filtered %>%
  filter(hotspot == 1L) %>%
  left_join(valid_episodes3, by = c("g_name","episode_id")) %>%
  group_by(g_name, episode_id, ep_len) %>%
  {
    two_year <- filter(., ep_len == 2) %>%
      slice_max(order_by = Total_Schadholz, n = 1, with_ties = FALSE)
    one_year <- filter(., ep_len == 1) %>%
      slice_min(order_by = JAHR, n = 1, with_ties = FALSE)
    bind_rows(two_year, one_year)
  } %>%
  ungroup() %>%
  transmute(g_name, event_year = JAHR, episode_id, ep_len)

# --- compute baseline & deviations (10-year pre-event of Total_Gesamteinschlag) ---
bk3_dev <- bk3 %>%
  left_join(events_bk3, by = "g_name") %>%
  filter(!is.na(event_year)) %>%
  mutate(tau = JAHR - event_year) %>%
  group_by(g_name, event_year) %>%
  mutate(
    baseline_total = mean(Total_Gesamteinschlag[tau %in% -10:-1], na.rm = TRUE),
    deviation_pct  = 100 * (Total_Gesamteinschlag - baseline_total) / baseline_total
  ) %>% ungroup()

# --- average across BK3 districts for each tau (plot window -5..+5) ---
bk3_mean <- bk3_dev %>%
  filter(tau >= -5, tau <= 5) %>%
  group_by(tau) %>%
  summarise(mean_dev = mean(deviation_pct, na.rm = TRUE),
            sd = sd(deviation_pct, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n), .groups = "drop")

ggplot(bk3_mean, aes(x = tau, y = mean_dev)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(color = "purple", size = 1.3) +
  geom_ribbon(aes(ymin = mean_dev - se, ymax = mean_dev + se),
              fill = "purple", alpha = 0.2) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  labs(x = "Years since disturbance event (τ)",
       y = "Deviation from pre-event 10-year mean (%)",
       title = "BK3 (ÖBf): average deviation of total harvest from 10-yr baseline") +
  theme_minimal()


# How many hotspot episodes total (before filtering)
bk3_ep %>% filter(hotspot==1) %>% group_by(g_name, episode_id) %>%
  summarise(ep_len=n(), .groups="drop") %>% count(ep_len)

###############################

##hotspot frequency BK3


bk3_ev <- events_bk3 %>%
  distinct(g_name, event_year) %>%
  arrange(g_name, event_year)

# 2) gaps between successive events within each district
bk3_gaps <- bk3_ev %>%
  group_by(g_name) %>%
  mutate(gap_years = event_year - dplyr::lag(event_year)) %>%
  filter(!is.na(gap_years) & gap_years > 0) %>%
  ungroup()

# 3) pooled (all gaps together) vs equal-weighted across districts
bk3_pooled_mean <- mean(bk3_gaps$gap_years)

bk3_equal_weighted <- bk3_gaps %>%
  group_by(g_name) %>%
  summarise(mean_gap_dist = mean(gap_years), .groups="drop") %>%
  summarise(
    mean_of_means   = mean(mean_gap_dist),
    median_of_means = median(mean_gap_dist),
    n_districts     = n()   # districts with ≥2 events
  )

list(
  pooled = bk3_pooled_mean,
  equal_weighted = bk3_equal_weighted
)

#########################
######################

##tests
##09.10.2025

bk1_dev$BK <- "BK1"
bk2_dev$BK <- "BK2"
bk3_dev$BK <- "BK3"

all_bk_dev <- bind_rows(bk1_dev, bk2_dev, bk3_dev)

##Step 2. Filter to the post-disturbance year (dt+3)

year3 <- all_bk_dev %>%
  filter(tau == 3)

##Step 3. Normality check (for deciding which test to use)

year3 %>%
  group_by(BK) %>%
  summarise(p_value = shapiro.test(deviation_pct)$p.value)
##result: not normal

##Step 4 – Recovery to baseline (dt+3)

# Wilcoxon tests per BK
year3 %>%
  group_by(BK) %>%
  summarise(
    mean_dev = mean(deviation_pct, na.rm = TRUE),
    median_dev = median(deviation_pct, na.rm = TRUE),
    p_wilcox = wilcox.test(deviation_pct, mu = 0)$p.value
  )

##visualisation

# Historical manually entered summary; not recalculated results.
recovery_summary <- data.frame(
  BK = c("BK1", "BK2", "BK3"),
  mean_dev = c(22.3, 6.16, -15.8),
  median_dev = c(11.4, 1.98, -20.8),
  p_wilcox = c(0.000106, 0.405, 0.064)
)


# The original script did not define year3_clean. Restore the intended cleaning
# step here before running this final section; its rule affects the results.
if (!exists("year3_clean", inherits = FALSE)) {
  stop("Missing original cleaning step: define year3_clean from year3 before continuing.")
}
recovery_summary <- year3_clean %>%
  mutate(BK = fct_relevel(BK, "BK1","BK2","BK3")) %>%
  group_by(BK) %>%
  summarise(
    mean_dev   = mean(deviation_pct, na.rm = TRUE),
    median_dev = median(deviation_pct, na.rm = TRUE),
    sd_dev     = sd(deviation_pct, na.rm = TRUE),
    n          = dplyr::n(),
    se_dev     = sd_dev / sqrt(n),
    p_wilcox   = wilcox.test(deviation_pct, mu = 0)$p.value,
    .groups = "drop"
  )

names(recovery_summary)   # quick check: should include "mean_dev"
print(recovery_summary)


ggplot(recovery_summary, aes(x = BK, y = mean_dev)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_dev - se_dev, ymax = mean_dev + se_dev), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p_wilcox < 0.05, "*", ""),
                vjust = ifelse(mean_dev >= 0, -0.6, 1.2)), size = 6) +
  labs(title = "dt+3: Total harvesting deviation vs. baseline",
       x = "Ownership (BK)", y = "Mean deviation from baseline (%)",
       subtitle = "* = Wilcoxon p < 0.05 (vs 0)") +
  theme_minimal() +
  theme(legend.position = "none")


#Kruskal–Wallis

kruskal.test(deviation_pct ~ BK, data = year3_clean)


# Pairwise post-hoc after Kruskal–Wallis


post <- dunnTest(deviation_pct ~ BK, data = year3_clean, method = "bh")
post
})
