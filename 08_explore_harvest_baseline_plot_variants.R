local({
# Cleaned copy of hiebssatz(1).R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "magrittr", "ARTool", "writexl", "ggplot2", "patchwork", "zoo", "tidyr", "cowplot")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(dplyr)
library(magrittr)
library(ARTool)
library(writexl)
library(ggplot2)
library(patchwork)
library(zoo)
library(tidyr)
library(cowplot)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "harvest_baseline_plot_variants")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##hiebssatz for interaction plot
##22.07.2025


# Load and tag each BK sheet
bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK1") %>%
  mutate(BK = as.factor(1))

bk2 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK2") %>%
  mutate(BK = as.factor(2))

bk3 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK3") %>%
  mutate(BK = as.factor(3))

# Combine into one dataframe
df <- bind_rows(bk1, bk2, bk3)


##Updated Step-by-Step (No BK separation)

##1. Estimate Hiebssatz (exclude "Hot Spot" from Damage_Class)

hieb_df <- df %>%
  filter(Damage_Class != "Hot Spot") %>%
  group_by(JAHR) %>%
  summarise(Hiebssatz = mean(Total_Gesamteinschlag, na.rm = TRUE))


############################################################
##extra. instad of Heibssatz

AAC_df <- df %>%
  group_by(g_name) %>%
  arrange(JAHR, .by_group = TRUE) %>%
  mutate(AAC = rollapply(
    Regular_Harvest,
    width = 10,
    FUN = mean,
    align = "right",
    fill = NA,
    na.rm = TRUE
  )) %>%
  ungroup()

#####################################
harvest_df <- df %>%
  group_by(JAHR) %>%
  summarise(
    TotalHarvest = mean(Total_Gesamteinschlag, na.rm = TRUE),
    SalvageWood = mean(Total_Schadholz, na.rm = TRUE)
  )


plot_df <- left_join(harvest_df, hieb_df, by = "JAHR")

##extra
# Exploratory alternative omitted: district-level join replaced the annual plot table
# and removed its Hiebssatz column. The annual join immediately above is retained.


ggplot(plot_df, aes(x = as.factor(JAHR))) +
  geom_col(aes(y = TotalHarvest), fill = "#69b3a2", width = 0.7) +
  
  geom_line(aes(y = SalvageWood, group = 1), color = "red", size = 1, linetype = "solid") +
  
  geom_line(aes(y = Hiebssatz, group = 1), color = "black", size = 1, linetype = "solid") +
  
  labs(
    title = "Total Harvest with Salvage Wood and Estimated Hiebssatz (2000–2024)",
    x = "Year",
    y = "Volume",
    #caption = "Bars = Total Harvest; red line = Salvage Wood; black line = Estimated Hiebssatz (excl. Hot Spots)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#################################
##the same but with another excel file. BK together

df <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "All")


hieb_df <- df %>%
  filter(Damage_Class != "Hot Spot") %>%
  group_by(JAHR) %>%
  summarise(Hiebssatz = mean(Gesamteinschlag, na.rm = TRUE))


harvest_df <- df %>%
  group_by(JAHR) %>%
  summarise(
    TotalHarvest = mean(Gesamteinschlag, na.rm = TRUE),
    SalvageWood = mean(Schadholz, na.rm = TRUE)
  )


plot_df <- left_join(harvest_df, hieb_df, by = "JAHR")


ggplot(plot_df, aes(x = as.factor(JAHR))) +
  geom_col(aes(y = TotalHarvest), fill = "lightgreen", width = 0.7) +
  
  geom_line(aes(y = SalvageWood, group = 1), color = "red", size = 1, linetype = "solid") +
  
  geom_line(aes(y = Hiebssatz, group = 1), color = "black", size = 1, linetype = "solid") +
  
  labs(
    title = "Total Harvest with Salvage Wood and Estimated Hiebssatz (2000–2024)",
    x = "Year",
    y = "Volume",
    caption = "Bars = Total Harvest; red line = Salvage Wood; black line = Estimated Hiebssatz (excl. Hot Spots)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##################################################################

##separated BKs


# Load data
df <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "All")


# Function to build each plot with consistent aesthetics
make_bk_plot <- function(df, bk_value) {
  hieb_df <- df %>%
    filter(Damage_Class != "Hot Spot", BK == bk_value) %>%
    group_by(JAHR) %>%
    summarise(Hiebssatz = mean(Gesamteinschlag, na.rm = TRUE))
  
  harvest_df <- df %>%
    filter(BK == bk_value) %>%
    group_by(JAHR) %>%
    summarise(
      TotalHarvest = mean(Gesamteinschlag, na.rm = TRUE),
      SalvageWood = mean(Schadholz, na.rm = TRUE)
    )
  
  plot_df <- left_join(harvest_df, hieb_df, by = "JAHR") %>%
    pivot_longer(cols = c("TotalHarvest", "SalvageWood", "Hiebssatz"), 
                 names_to = "Type", values_to = "Volume")
  
  ggplot(plot_df, aes(x = as.factor(JAHR))) +
    geom_col(data = filter(plot_df, Type == "TotalHarvest"),
             aes(y = Volume, fill = "Total Harvest"), width = 0.7) +
    geom_line(data = filter(plot_df, Type == "SalvageWood"),
              aes(y = Volume, color = "Salvage Wood", group = 1), size = 1) +
    geom_line(data = filter(plot_df, Type == "Hiebssatz"),
              aes(y = Volume, color = "Hiebssatz", group = 1), size = 1) +
    scale_fill_manual(name = NULL, values = c("Total Harvest" = "lightgreen")) +
    scale_color_manual(name = NULL, 
                       values = c("Salvage Wood" = "red", "Hiebssatz" = "black")) +
    labs(
      title = paste("BK", bk_value),
      x = "Year",
      y = "Volume"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# Generate individual plots
plot_bk1 <- make_bk_plot(df, 1)
plot_bk2 <- make_bk_plot(df, 2)
plot_bk3 <- make_bk_plot(df, 3)

# Combine with shared legend
combined_plot <- (plot_bk1 / plot_bk2 / plot_bk3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Show
print(combined_plot)
####################################################

##Hiebssatz with BKs, sum up

df <- read_excel(file.path(project_root, "data", "damage_withoutBKold.xlsx"), sheet = 1)


hieb_df <- df %>%
  filter(Damage_Class != "Hot Spot") %>%
  group_by(JAHR) %>%
  summarise(Hiebssatz = mean(Total_Gesamteinschlag, na.rm = TRUE))


harvest_df <- df %>%
  group_by(JAHR) %>%
  summarise(
    TotalHarvest = mean(Total_Gesamteinschlag, na.rm = TRUE),
    SalvageWood = mean(Total_Schadholz, na.rm = TRUE)
  )


plot_df <- left_join(harvest_df, hieb_df, by = "JAHR")


ggplot(plot_df, aes(x = as.factor(JAHR))) +
  geom_col(aes(y = TotalHarvest), fill = "lightblue", width = 0.7) +
  
  geom_line(aes(y = SalvageWood, group = 1), color = "red", size = 1, linetype = "solid") +
  
  geom_line(aes(y = Hiebssatz, group = 1), color = "black", size = 1, linetype = "solid") +
  
  labs(
    title = "Total Harvest with Salvage Wood and Estimated Hiebssatz (2000–2024)",
    x = "Year",
    y = "Volume",
    caption = "Bars = Total Harvest; red line = Salvage Wood; black line = Estimated Hiebssatz (excl. Hot Spots)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#######################################################################


##AAC


df <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "All")

df <- df %>%
  mutate(Regular_Harvest = Gesamteinschlag - Schadholz)


make_bk_plot <- function(df, bk_value) {
  
  # Filter data for given BK
  filtered_df <- df %>% filter(BK == bk_value)
  
  # Calculate AAC per district: 10-year rolling mean of Regular_Harvest
  aac_df <- filtered_df %>%
    arrange(ERHEBUNGSBEZIRK, JAHR) %>%
    group_by(ERHEBUNGSBEZIRK) %>%
    mutate(AAC = rollapply(Regular_Harvest, width = 10, FUN = mean, align = "right", fill = NA, na.rm = TRUE)) %>%
    ungroup()
  
  # Average AAC by year across districts
  aac_yearly <- aac_df %>%
    group_by(JAHR) %>%
    summarise(AAC = mean(AAC, na.rm = TRUE))
  
  # Calculate mean total harvest and salvage wood by year and BK
  harvest_df <- filtered_df %>%
    group_by(JAHR) %>%
    summarise(
      TotalHarvest = mean(Gesamteinschlag, na.rm = TRUE),
      SalvageWood = mean(Schadholz, na.rm = TRUE)
    )
  
  # Join AAC with harvest data
  plot_df <- left_join(harvest_df, aac_yearly, by = "JAHR") %>%
    pivot_longer(cols = c("TotalHarvest", "SalvageWood", "AAC"), 
                 names_to = "Type", values_to = "Volume")
  
  # Create the plot
  p <- ggplot(plot_df, aes(x = as.factor(JAHR))) +
    geom_col(data = filter(plot_df, Type == "TotalHarvest"),
             aes(y = Volume, fill = "Total Harvest"), width = 0.7) +
    geom_line(data = filter(plot_df, Type == "SalvageWood"),
              aes(y = Volume, color = "Salvage Wood", group = 1), size = 1) +
    geom_line(data = filter(plot_df, Type == "AAC"),
              aes(y = Volume, color = "AAC", group = 1), size = 1) +
    scale_fill_manual(name = NULL, values = c("Total Harvest" = "darkseagreen")) +
    scale_color_manual(name = NULL, 
                       values = c("Salvage Wood" = "red", "AAC" = "black")) +
    labs(
      title = paste("BK", bk_value),
      x = "Year",
      y = "Volume"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(p)
}

# Generate plots and combine
plot_bk1 <- make_bk_plot(df, 1)
plot_bk2 <- make_bk_plot(df, 2)
plot_bk3 <- make_bk_plot(df, 3)

combined_plot <- (plot_bk1 / plot_bk2 / plot_bk3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

print(combined_plot)
########################################################


df <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "All")

df <- df %>%
  mutate(Regular_Harvest = Gesamteinschlag - Schadholz)

make_bk_plot <- function(df, bk_value) {
  
  # Map BK values to names
  bk_names <- c("1" = "Small Private Forests (<200 ha)", "2" = "Large Private Forests (≥200 ha)",
                "3" = "Austrian Federal Forests (ÖBf)")
  
  filtered_df <- df %>% filter(BK == bk_value)
  
  aac_df <- filtered_df %>%
    arrange(ERHEBUNGSBEZIRK, JAHR) %>%
    group_by(ERHEBUNGSBEZIRK) %>%
    mutate(AAC = zoo::rollapply(Regular_Harvest, width = 10, FUN = mean, align = "right", fill = NA, na.rm = TRUE)) %>%
    ungroup()
  
  aac_yearly <- aac_df %>%
    group_by(JAHR) %>%
    summarise(AAC = mean(AAC, na.rm = TRUE))
  
  harvest_df <- filtered_df %>%
    group_by(JAHR) %>%
    summarise(
      TotalHarvest = mean(Gesamteinschlag, na.rm = TRUE),
      SalvageWood = mean(Schadholz, na.rm = TRUE)
    )
  
  plot_df <- left_join(harvest_df, aac_yearly, by = "JAHR") %>%
    pivot_longer(cols = c("TotalHarvest", "SalvageWood", "AAC"), 
                 names_to = "Type", values_to = "Volume") %>%
    filter(JAHR >= 2009, JAHR <= 2024)
  
  p <- ggplot(plot_df, aes(x = as.factor(JAHR))) +
    geom_col(data = filter(plot_df, Type == "TotalHarvest"),
             aes(y = Volume, fill = "Total Harvest"), width = 0.7) +
    geom_line(data = filter(plot_df, Type == "SalvageWood"),
              aes(y = Volume, color = "Salvage Wood", group = 1), size = 1) +
    geom_line(data = filter(plot_df, Type == "AAC"),
              aes(y = Volume, color = "10-Year Moving Average of Regular Harvest", group = 1), size = 1) +
    scale_fill_manual(name = NULL, values = c("Total Harvest" = "darkseagreen")) +
    scale_color_manual(name = NULL, 
                       values = c("Salvage Wood" = "red", "10-Year Moving Average of Regular Harvest" = "black")) +
    labs(
      title = bk_names[as.character(bk_value)],
      x = "Year",
      y = "Volume (Efm)"
    ) +
    coord_cartesian(ylim = c(0, 150000)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 20),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 18,angle = 45, hjust = 1),
      axis.text.y = element_text(size = 19),
      legend.position = "right",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  return(p)
}


plot_bk1 <- make_bk_plot(df, 1) + labs(x = NULL)  # Keep y title, remove x title
plot_bk2 <- make_bk_plot(df, 2) + labs(y = NULL)  # Keep x title, remove y title
plot_bk3 <- make_bk_plot(df, 3) + labs(x = NULL, y = NULL)  # No titles


plot_bk2 <- plot_bk2 + theme(axis.text.y = element_blank())
plot_bk3 <- plot_bk3 + theme(axis.text.y = element_blank())


combined_plot <- (plot_bk1 | plot_bk2 | plot_bk3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

print(combined_plot)


ggsave(
  "combined_plot3.png",
  combined_plot,
  width = 22,
  height = 10,
  dpi = 600,
  units = "in",
  type = "cairo-png"  # forces anti-aliased smooth lines
)


##################################################

##used in the paper but the final version is saved on map writing 2
##salvage wood, heibssatz and regular harvest 


df <- read_excel(file.path(project_root, "outputs", "2000_24_damage_BK.xlsx"), sheet = "All")

# Calculate Regular Harvest
df <- df %>%
  mutate(Regular_Harvest = Gesamteinschlag - Schadholz)

# Function to make plot
make_bk_plot <- function(df, bk_value) {
  
  # Map BK values to names
  bk_names <- c("1" = "Small Private Forests (<200 ha)", 
                "2" = "Large Private Forests (≥200 ha)",
                "3" = "Austrian Federal Forests (ÖBf)")
  
  filtered_df <- df %>% filter(BK == bk_value)
  
  # Calculate AAC
  aac_df <- filtered_df %>%
    arrange(ERHEBUNGSBEZIRK, JAHR) %>%
    group_by(ERHEBUNGSBEZIRK) %>%
    mutate(AAC = zoo::rollapply(Regular_Harvest, width = 10, FUN = mean, align = "right", fill = NA, na.rm = TRUE)) %>%
    ungroup()
  
  aac_yearly <- aac_df %>%
    group_by(JAHR) %>%
    summarise(AAC = mean(AAC, na.rm = TRUE))
  
  harvest_df <- filtered_df %>%
    group_by(JAHR) %>%
    summarise(
      RegularHarvest = mean(Regular_Harvest, na.rm = TRUE),
      SalvageWood = mean(Schadholz, na.rm = TRUE)
    )
  
  plot_df <- left_join(harvest_df, aac_yearly, by = "JAHR") %>%
    pivot_longer(cols = c("RegularHarvest", "SalvageWood", "AAC"), 
                 names_to = "Type", values_to = "Volume") %>%
    filter(JAHR >= 2009, JAHR <= 2024)
  
  p <- ggplot(plot_df, aes(x = as.factor(JAHR))) +
    geom_col(data = filter(plot_df, Type == "RegularHarvest"),
             aes(y = Volume, fill = "Regular Harvest"), width = 0.7) +
    geom_line(data = filter(plot_df, Type == "SalvageWood"),
              aes(y = Volume, color = "Salvage Wood", group = 1), size = 1) +
    geom_line(data = filter(plot_df, Type == "AAC"),
              aes(y = Volume, color = "10-Year Moving Average of Regular Harvest", group = 1), size = 1) +
    scale_fill_manual(name = NULL, values = c("Regular Harvest" = "skyblue")) +
    scale_color_manual(name = NULL, 
                       values = c("Salvage Wood" = "red", "10-Year Moving Average of Regular Harvest" = "black")) +
    labs(
      title = bk_names[as.character(bk_value)],
      x = "Year",
      y = "Volume (Efm)"
    ) +
    coord_cartesian(ylim = c(0, 150000)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 20),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 18,angle = 45, hjust = 1),
      axis.text.y = element_text(size = 19),
      legend.position = c(0.85, 0.85),  # inside top-right
      legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
      #legend.spacing.y = unit(0.5, "cm"), # space between items
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  return(p)
}


# Make individual plots
plot_bk1 <- make_bk_plot(df, 1) + labs(x = NULL)
plot_bk2 <- make_bk_plot(df, 2) + labs(y = NULL)
plot_bk3 <- make_bk_plot(df, 3) + labs(x = NULL, y = NULL)

# Optional: remove y-axis text for BK2 and BK3 for cleaner combined plot
plot_bk2 <- plot_bk2 + theme(axis.text.y = element_blank())
plot_bk3 <- plot_bk3 + theme(axis.text.y = element_blank())


# Remove legends from subplots
plot_bk1_noleg <- plot_bk1 + theme(legend.position = "none")
plot_bk2_noleg <- plot_bk2 + theme(legend.position = "none")
plot_bk3_noleg <- plot_bk3 + theme(legend.position = "none")

# Combine plots
combined_noleg <- plot_bk1_noleg | plot_bk2_noleg | plot_bk3_noleg

# Extract legend from one of the original plots
legend <- cowplot::get_legend(plot_bk1 + theme(legend.position = "right"))

# Overlay legend inside combined plot
final_plot <- ggdraw(combined_noleg) +
  draw_grob(legend, x = 1.32, y = 1.2, hjust = 1, vjust = 1)  # adjust coordinates

print(final_plot)


ggsave(
  "new_combined_plot.png",
  final_plot,
  width = 22,
  height = 10,
  dpi = 600,
  units = "in",
  type = "cairo-png"  # forces anti-aliased smooth lines
)

})
