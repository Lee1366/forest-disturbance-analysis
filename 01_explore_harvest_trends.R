local({
# Cleaned copy of Harvest_ab2000(1).R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "ggplot2", "dplyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(ggplot2)
library(dplyr)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "harvest_trends")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

#30.05.2025
#harvesting for Bundesländer ab 2000

##04.09.2025


getwd()
# Working directory is configured in the header.
data <- read_excel(file.path(project_root, "data", "Final_data_2000_2024.xlsx"), sheet = "final2")


# Plot: Line for each district, faceted by Bundesland
# Ensure JAHR is numeric or integer
data$JAHR <- as.integer(data$JAHR)


aggregated_data <- data %>%
  group_by(BUNDESLAND_NAME, ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE), .groups = "drop")

aggregated_data <- data %>%
  group_by(BUNDESLAND_NAME, ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(
    Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

aggregated_data$JAHR <- as.integer(aggregated_data$JAHR)


ggplot(aggregated_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 0.5, size = 0.6) +
  facet_wrap("BUNDESLAND_NAME") +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  labs(
    title = "Harvest Volume per District (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # remove vertical grid lines
       # remove minor ones if any
  )
##################################################
##salvage wood all Austria

aggregated_data <- data %>%
  group_by(BUNDESLAND_NAME, ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(Schadholz = sum(Schadholz, na.rm = TRUE), .groups = "drop")

aggregated_data <- data %>%
  group_by(BUNDESLAND_NAME, ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(
    Schadholz = sum(Schadholz, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

aggregated_data$JAHR <- as.integer(aggregated_data$JAHR)

ggplot(aggregated_data, aes(
  x = JAHR,
  y = Schadholz,
  color = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 0.5, size = 0.6) +
  facet_wrap("BUNDESLAND_NAME") +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  labs(
    title = "Salvage wood Volume per District (2000–2024)",
    x = "Year",
    y = "Salvage Wood Volume (1,000 EFM)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # remove vertical grid lines
    # remove minor ones if any
  )
###########################
# Restore total-harvest data for the state-specific total-harvest plots.
aggregated_data <- data %>%
  group_by(BUNDESLAND_NAME, ERHEBUNGSBEZIRK, JAHR) %>%
  summarise(Gesamteinschlag = sum(Gesamteinschlag, na.rm = TRUE) / 1000, .groups = "drop")

#Burgenland

Burgenland_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Burgenland")


ggplot(Burgenland_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Burgenland (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() )
###########################

#Kärnten

Kärnten_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Kärnten")


ggplot(Kärnten_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Kärnten (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )
############################
##Niederösterreich

Niederösterreich_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Niederösterreich")


ggplot(Niederösterreich_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Niederösterreich (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )
#######################
##Oberösterreich

Oberösterreich_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Oberösterreich")


ggplot(Oberösterreich_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Oberösterreich (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )
############################

##Salzburg

Salzburg_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Salzburg")


ggplot(Salzburg_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Salzburg (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )
#############################


##Steiermark

Steiermark_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Steiermark")


ggplot(Steiermark_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Steiermark (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )
#######################


##Tirol

Tirol_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Tirol")


ggplot(Tirol_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Tirol (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )
############################
##Vorarlberg

Vorarlberg_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Vorarlberg")


ggplot(Vorarlberg_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Vorarlberg (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )
######################
##Wien

Wien_data <- aggregated_data %>%
  filter(BUNDESLAND_NAME == "Wien")


ggplot(Wien_data, aes(
  x = JAHR,
  y = Gesamteinschlag,
  color = ERHEBUNGSBEZIRK,
  group = ERHEBUNGSBEZIRK
)) +
  geom_line(alpha = 1, size = 0.7) +
  scale_x_continuous(breaks = 2000:2024) +
  labs(
    title = "Harvest Volume per District in Wien (2000–2024)",
    x = "Year",
    y = "Total Harvest Volume (1,000 EFM)",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor.x = element_blank() 
  )

})
