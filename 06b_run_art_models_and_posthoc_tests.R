local({
# Cleaned copy of Non_parametric_complete(1).R; formulas and research alternatives retained.
# See README.md and REVIEW_NOTES.md before interpreting outputs.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) stop("Run from the repository root.")
required_packages <- c("readxl", "dplyr", "magrittr", "ARTool", "writexl", "emmeans", "ggplot2", "Cairo")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run 00_setup_packages.R to install: ", paste(missing, collapse = ", "))
library(readxl)
library(dplyr)
library(magrittr)
library(ARTool)
library(writexl)
library(emmeans)
library(ggplot2)
library(Cairo)

script_output <- file.path(project_root, "outputs", "06b_run_art_models_and_posthoc_tests")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##Non-Parametric Anova for complete data
##11.09.2025


bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK1") %>%
  mutate(BK = as.factor(1))

bk2 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK2") %>%
  mutate(BK = as.factor(2))

bk3 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK3") %>%
  mutate(BK = as.factor(3))

# Combine into one data frame
df_anova <- bind_rows(bk1, bk2, bk3)

##Make sure Neighbor_Category is also a factor:
df_anova$Neighbor_Category <- as.factor(df_anova$Neighbor_Category)

##remove NA s

df_art_this <- df_anova %>%
  filter(!is.na(Delta_ThisYear), !is.na(BK), !is.na(Neighbor_Category))


# ART model for Delta_ThisYear
art_this <- art(Delta_ThisYear ~ Neighbor_Category * BK, data = df_art_this)
anova(art_this)

##next year
df_art_next <- df_anova %>%
  filter(!is.na(Delta_NextYear), !is.na(BK), !is.na(Neighbor_Category))

art_next <- art(Delta_NextYear ~ Neighbor_Category * BK, data = df_art_next)
anova(art_next)


# Run and convert ANOVA results for this year
anova_art_this <- anova(art_this)
anova_art_this_df <- transform(as.data.frame(anova_art_this), Term = rownames(anova_art_this))
anova_art_this_df$Year <- "This Year"

# Run and convert ANOVA results for next year
anova_art_next <- anova(art_next)
anova_art_next_df <- transform(as.data.frame(anova_art_next), Term = rownames(anova_art_next))
anova_art_next_df$Year <- "Next Year"


# Save both data frames in one Excel file with two sheets

write_xlsx(
  list(
    "ART_This_Year" = anova_art_this_df,
    "ART_Next_Year" = anova_art_next_df
  ),
  path = file.path(project_root, "outputs", "art_anova_complete_results2.xlsx")
)

####################################################

##Post Anova


# Load and tag each BK sheet
bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK1") %>%
  mutate(BK = as.factor(1))

bk2 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK2") %>%
  mutate(BK = as.factor(2))

bk3 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK3") %>%
  mutate(BK = as.factor(3))

# Combine into one dataframe
df_anova <- bind_rows(bk1, bk2, bk3)

##Make sure Neighbor_Category is also a factor:
df_anova$Neighbor_Category <- as.factor(df_anova$Neighbor_Category)

#####remove NA s

df_art_this <- df_anova %>%
  filter(!is.na(Delta_ThisYear), !is.na(BK), !is.na(Neighbor_Category))

##next year
df_art_next <- df_anova %>%
  filter(!is.na(Delta_NextYear), !is.na(BK), !is.na(Neighbor_Category))


df_art_this$BK <- as.factor(df_art_this$BK)
df_art_next$BK <- as.factor(df_art_next$BK)

# Fit ART models
art_this <- art(Delta_ThisYear ~ Neighbor_Category * BK, data = df_art_this)
art_next <- art(Delta_NextYear ~ Neighbor_Category * BK, data = df_art_next)

# ANOVA results
anova_art_this_df <- transform(as.data.frame(anova(art_this)), Term = rownames(anova(art_this))) %>% mutate(Year = "This Year")
anova_art_next_df <- transform(as.data.frame(anova(art_next)), Term = rownames(anova(art_next))) %>% mutate(Year = "Next Year")

# Post-hoc for This Year
ph_neighbor_this_df <- as.data.frame(art.con(art_this, "Neighbor_Category")) %>%
  mutate(Year = "This Year", Factor = "Neighbor_Category")

ph_bk_this_df <- as.data.frame(art.con(art_this, "BK")) %>%
  mutate(Year = "This Year", Factor = "BK")

# Post-hoc for Next Year
ph_neighbor_next_df <- as.data.frame(art.con(art_next, "Neighbor_Category")) %>%
  mutate(Year = "Next Year", Factor = "Neighbor_Category")

ph_bk_next_df <- as.data.frame(art.con(art_next, "BK")) %>%
  mutate(Year = "Next Year", Factor = "BK")

# Combine post-hoc contrasts
posthoc_all <- bind_rows(
  ph_neighbor_this_df, ph_bk_this_df,
  ph_neighbor_next_df, ph_bk_next_df
)

# Save all to Excel with sheets
write_xlsx(
  list(
    "ART_This_Year_ANOVA" = anova_art_this_df,
    "ART_Next_Year_ANOVA" = anova_art_next_df,
    "PostHoc_Main_Effects" = posthoc_all
  ),
  path = file.path(project_root, "outputs", "art_anova_complete_results.xlsx")
)
#############################
##interaction plot this year


df_anova$BK <- factor(df_anova$BK, 
                      levels = c(1, 2, 3), 
                      labels = c("BK1", "BK2", "BK3"))


# Create interaction plot

ggplot(df_anova, aes(x = Neighbor_Category, y = Delta_ThisYear, color = BK, group = BK)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
    title = "Interaction Plot: Neighbor Category × Ownership (This Year)",
    y = "Change in Regular Logging",
    x = "Neighbor Category",
    color = "Ownership Type"
  ) +
  theme_minimal()


###this works

ggplot(df_anova, aes(x = Neighbor_Category, y = Delta_ThisYear, color = BK, group = BK)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
    title = "Interaction Plot: Neighbor Category × Ownership (This Year)",
    y = "Change in Regular Logging",
    x = "Neighbor Category",
    color = "Ownership Type"
  ) +
  theme_minimal()


####################

##for paper


CairoPNG(
  filename = "interaction_plot.png",
  width = 10, height = 6, units = "in", dpi = 600
)

# Create the interaction plot
ggplot(df_anova, aes(x = Neighbor_Category, y = Delta_ThisYear, color = BK, group = BK)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_color_manual(
    values = c("BK1" = "forestgreen", "BK2" = "goldenrod", "BK3" = "steelblue"),
    labels = c("BK1" = "Small Forest", "BK2" = "Large Forest", "BK3" = "Federal Forest")
  ) +
  scale_x_discrete(
    labels = c("Hot Spot" = "Hotspot", "Hotspot Neighbor" = "Neighbor", "None" = "Non-neighbor")
  ) +
  labs(
    x = "Proximity to Hotspot District",
    y = "Change in Regular Harvest",
    color = "Ownership Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major.x = element_blank(),  # remove major vertical lines
    #panel.grid.minor.x = element_blank()
  )

dev.off()


#########################################


# Save high-resolution PNG
CairoPNG(
  filename = "interaction_plot3.png",
  width = 10, height = 6, units = "in", dpi = 600
)

# Create the interaction plot
ggplot(df_anova, aes(x = Neighbor_Category, y = Delta_ThisYear, color = BK, group = BK)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
   stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_color_manual(
    values = c("BK1" = "forestgreen", "BK2" = "goldenrod", "BK3" = "steelblue"),
    labels = c("BK1" = "Small Forest", "BK2" = "Large Forest", "BK3" = "Federal Forest")
  ) +
  scale_x_discrete(
    labels = c("Hot Spot" = "Hotspot", "Hotspot Neighbor" = "Neighbor", "None" = "Non-neighbor")
  ) +
  scale_y_continuous(
    breaks = seq(-30, 30, by = 10)  # ticks at -20, -10, 0, 10, 20
  ) +
  labs(
    x = "Proximity to Hotspot District",
    y = "Change in Regular Harvest",
    color = "Ownership Type"
  ) +
  theme_minimal(base_size = 16) +   # all fonts bigger
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.background = element_rect(color = "darkgrey", fill = "white", linewidth = 0.6), # box around legend
    panel.grid.major.x = element_blank()
  )

dev.off()


######################################################
##interaction plot next yaer only for example

ggplot(df_anova, aes(x = Neighbor_Category, y = Delta_NextYear, color = BK, group = BK)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
    title = "Interaction Plot: Neighbor Category × Ownership (Next Year)",
    y = "Change in Regular Logging",
    x = "Neighbor Category",
    color = "Ownership Type"
  ) +
  theme_minimal()
})
