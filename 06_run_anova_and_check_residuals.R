local({
# Cleaned copy of Anova_Completed_Data.R
# Research working script: alternative sections are retained. See README.md and REVIEW_NOTES.md.
# Run from the repository root, preferably one section at a time in RStudio.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "README.md"))) {
  stop("Set the working directory to the repository folder before running this script.")
}
required_packages <- c("readxl", "dplyr", "writexl", "Cairo")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages using 00_setup_packages.R: ", paste(missing_packages, collapse = ", "))
library(readxl)
library(dplyr)
library(writexl)
library(Cairo)

dir.create(file.path(project_root, "outputs"), recursive = TRUE, showWarnings = FALSE)
script_output <- file.path(project_root, "outputs", "anova_diagnostics")
dir.create(script_output, recursive = TRUE, showWarnings = FALSE)
previous_wd <- getwd()
on.exit(setwd(previous_wd), add = TRUE)
setwd(script_output)

##Anova check for conpleteed data
##10.09.2025


bk1 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK1") %>%
  mutate(BK = 1)

# Load BK2 and tag it
bk2 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK2") %>%
  mutate(BK = 2)

# Load BK3 and tag it
bk3 <- read_excel(file.path(project_root, "outputs", "2000_24_BK_with_deltas.xlsx"), sheet = "BK3") %>%
  mutate(BK = 3)


################


# Step 2: Add BK label to each
bk1$BK <- "BK1"  # Kleinwald
bk2$BK <- "BK2"  # Großwald
bk3$BK <- "BK3"  # ÖBF

# Step 3: Combine all into one dataframe
df_anova <- bind_rows(bk1, bk2, bk3)

# Step 4: Run ANOVA models (interaction)
anova_this <- aov(Delta_ThisYear ~ Neighbor_Category * BK, data = df_anova)
anova_next <- aov(Delta_NextYear ~ Neighbor_Category * BK, data = df_anova)


# Step 5: Extract ANOVA summary tables
extract_anova_table <- function(summary_obj) {
  df <- as.data.frame(summary_obj[[1]])
  df$Term <- rownames(df)
  rownames(df) <- NULL
  df <- df[, c("Term", names(df)[1:(ncol(df)-1)])]
  return(df)
}

anova_this_df <- extract_anova_table(summary(anova_this))
anova_this_df$Year <- "This Year"

anova_next_df <- extract_anova_table(summary(anova_next))
anova_next_df$Year <- "Next Year"

# Step 6: Combine and save results
anova_combined <- bind_rows(anova_this_df, anova_next_df)

print(anova_combined)

write_xlsx(anova_combined, file.path(project_root, "outputs", "ANOVA_complete_BK_results.xlsx"))


##check residuals normality 

# For This Year
residuals_this <- residuals(anova_this)
qqnorm(residuals_this)
qqline(residuals_this, col = "red")
shapiro.test(residuals_this)  # formal test for normality

# For Next Year
residuals_next <- residuals(anova_next)
qqnorm(residuals_next)
qqline(residuals_next, col = "red")
shapiro.test(residuals_next)

##################################
##for shapiro saving

# Install package if needed


shapiro_this <- shapiro.test(residuals_this)
shapiro_next <- shapiro.test(residuals_next)

# Create a data frame with results
shapiro_results <- data.frame(
  Year = c("Disturbance Year (dt)", "Post-Disturbance Year (dt+1)"),
  W = c(shapiro_this$statistic, shapiro_next$statistic),
  p_value = c(shapiro_this$p.value, shapiro_next$p.value)
)

# Write to Excel
write_xlsx(shapiro_results, file.path(project_root, "outputs", "Shapiro_Results_complete.xlsx"))


#############################
##for paper
## it wont be shown here in r, but it will be saved


CairoPNG("QQplots_single2.png", width = 3000, height = 1500, res = 300)

# Layout: 1 row, 2 columns
par(mfrow = c(1,2), mar = c(1,2,1,1), oma = c(5,5,2,1))

# Plot 1: remove ylab if you want only shared
qqnorm(residuals_this, main = "Disturbance Year (dt)",
       ylab = "", xlab = "", 
       cex.main = 1, cex.lab = 1, cex.axis = 1)
qqline(residuals_this, col = "red")

# Plot 2
qqnorm(residuals_next, main = "Post-Disturbance Year (dt+1)",
       ylab = "", xlab = "", 
       cex.main = 1, cex.lab = 1, cex.axis = 1)
qqline(residuals_next, col = "red")

# Shared axis labels
mtext("Theoretical Quantiles", side = 1, outer = TRUE, line = 2.5, cex = 1, font = 2)
mtext("Sample Quantiles", side = 2, outer = TRUE, line = 2.5, cex = 1, font = 2)

# Close device
dev.off()
########################################################

})
