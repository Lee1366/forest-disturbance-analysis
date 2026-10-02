local({
# Cleaned copy of shapefile_changenames(1).R; formulas and research alternatives retained.
# See README.md and REVIEW_NOTES.md before interpreting outputs.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) stop("Run from the repository root.")
required_packages <- c("sf", "dplyr")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Run 00_setup_packages.R to install: ", paste(missing, collapse = ", "))
library(sf)
library(dplyr)

script_output <- file.path(project_root, "outputs", "00b_harmonize_district_boundaries")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

## shape file name changes
##12.06.2025


shp <- st_read(file.path(project_root, "data/shapefiles", "STATISTIK_AUSTRIA_POLBEZ_20250101.shp"))

names(shp)  # Find the correct name column, e.g., "district" or "NAME"
unique(shp$g_name)  # Replace "district_name" with the actual column name

# Step 2: Rename and merge district names
shp <- shp %>%
  mutate(g_name = case_when(
    g_name %in% c("Eisenstadt(Stadt)", "Eisenstadt-Umgebung", "Rust(Stadt)") ~ "Eisenstadt (Bezirk, Stadt und Rust)",
    g_name %in% c("Klagenfurt Land", "Klagenfurt Stadt") ~ "Klagenfurt",
    g_name %in% c("Villach Land", "Villach Stadt") ~ "Villach",
    g_name %in% c("Krems an der Donau(Stadt)", "Krems(Land)")  ~ "Krems",
    g_name %in% c("Wiener Neustadt(Land)", "Wiener Neustadt(Stadt)") ~ "Wr. Neustadt",
    g_name %in% c("Tulln", "Korneuburg") ~ "Korneuburg und Tulln",
    g_name %in% c("Hollabrunn", "Horn") ~ "Horn und Hollabrunn",
    g_name %in% c("Waidhofen an der Thaya", "Gmünd") ~ "Gmünd und Waidhofen_Thaya",
    g_name %in% c("Gänserndorf", "Mistelbach") ~ "Gänserndorf und Mistelbach",
    g_name %in% c("Mödling", "Bruck an der Leitha") ~ "Bruck und Mödling",
    g_name %in% c("Sankt Pölten(Land)", "Sankt Pölten(Stadt)") ~ "St. Pölten",
    g_name %in% c("Salzburg(Stadt)", "Salzburg-Umgebung") ~ "Salzburg",
    g_name %in% c("Graz(Stadt)", "Graz-Umgebung") ~ "Graz",
    g_name %in% c("Stadt Linz", "Linz-Land") ~ "Linz",
    g_name %in% c("Waidhofen an der Ybbs(Stadt)", "Amstetten") ~ "Amstetten",
    g_name %in% c("Innsbruck-Land", "Innsbruck-Stadt") ~ "Innsbruck",
    g_name %in% c("Wien  1.,Innere Stadt", "Wien  2.,Leopoldstadt", "Wien  3.,Landstraße",
                  "Wien  4.,Wieden", "Wien  5.,Margareten", "Wien  6.,Mariahilf", "Wien  7.,Neubau", 
                  "Wien  8.,Josefstadt", "Wien  9.,Alsergrund", "Wien 10.,Favoriten",
                  "Wien 11.,Simmering", "Wien 12.,Meidling", "Wien 13.,Hietzing", "Wien 14.,Penzing",
                  "Wien 15.,Rudolfsheim-Fünfhaus", "Wien 16.,Ottakring", "Wien 17.,Hernals",
                  "Wien 18.,Währing", "Wien 19.,Döbling", "Wien 20.,Brigittenau",
                  "Wien 21.,Floridsdorf", "Wien 22.,Donaustadt", "Wien 23.,Liesing", "Wien(Stadt)") ~ "Wien",
    g_name %in% c("Stadt Steyr", "Steyr-Land") ~ "Steyr",
    g_name %in% c("Stadt Wels", "Wels-Land") ~ "Wels",
    TRUE ~ g_name
  ))


# Add "m" to g_id only for names that were merged (i.e. appear more than once)
shp <- shp %>%
  group_by(g_name) %>%
  mutate(
    g_id = if (n() > 1) paste0("m", min(as.numeric(g_id))) else g_id
  ) %>%
  ungroup()


shp_merged <- shp %>%
  group_by(g_name, g_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")


dir.create(file.path(project_root, "outputs", "shapefiles"), recursive = TRUE, showWarnings = FALSE)
st_write(shp_merged, file.path(project_root, "outputs", "shapefiles", "merged_STATISTIK_AUSTRIA_POLBEZ_Neww3.shp"))

##########################################################
})
