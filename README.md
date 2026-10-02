# Austrian forest harvest research scripts (2000–2024)

R working scripts for processing HEM harvest records, classifying district-year damage, identifying spatial neighbors, calculating changes in regular harvest, and producing figures and exploratory statistical diagnostics.

These are cleaned copies of research working scripts. Alternative and historical sections are retained. They are not a validated end-to-end replication package, and they do not establish the final published results. No synthetic data have been substituted for the research code.

## Files

| Original filename | GitHub filename |
| --- | --- |
| Harvest_ab2000(1).R | 01_explore_harvest_trends.R |
| category_for_damage.R | 02a_classify_damage_all_ownerships.R |
| Austria_districts.R | 07_create_district_maps_and_animation.R |
| BK.R | 02b_classify_damage_by_ownership.R |
| category_hotspotneighbor.R | 03a_identify_neighbors_and_calculate_deltas_all_ownerships.R |
| BK_neighborigSituation.R | 03b_identify_neighbors_and_calculate_deltas_by_ownership.R |
| high_damage(1).R | 04a_classify_high_damage_neighbors_all_ownerships.R |
| HighDamage_BKs(1).R | 04b_classify_high_damage_neighbors_by_ownership.R |
| histogram_2(1).R | 05_check_delta_distributions.R |
| Anova_Completed_Data.R | 06_run_anova_and_check_residuals.R |
| hiebssatz(1).R | 08_explore_harvest_baseline_plot_variants.R |

## Data and setup

The underlying workbooks and district boundaries are not included. Add them locally only. `data/` and `outputs/` are excluded by `.gitignore`.

1. Extract this folder and open it in RStudio.
2. Set the working directory to this folder, where README.md is located.
3. Run `source("00_setup_packages.R")` once to install missing dependencies.
4. Put source Excel workbooks in `data/` and shapefile components in `data/shapefiles/`. Keep matching `.shp`, `.shx`, `.dbf`, and `.prj` files together.
5. Read REVIEW_NOTES.md before calculating or interpreting results.
6. Run a chosen file with, for example, `source("01_explore_harvest_trends.R")`. The local block restores the working directory when it finishes or errors. Plots assigned to named objects are explicitly printed where the original code did so; other interactive ggplot expressions may require selecting and running those expressions in RStudio. The scripts keep their variables in a local scope.

Shared generated workbooks are saved directly in `outputs/`. Figures and other relative outputs are saved in `outputs/<script_name>/`. Scripts can overwrite their generated workbooks. Do not keep your only copy of an existing workbook in outputs/.

## Dependencies between scripts

- `01_explore_harvest_trends.R` reads `data/Final_data_2000_2024.xlsx`, sheet `final2`.
- `02a_classify_damage_all_ownerships.R` creates `outputs/2000_24_damage_withoutBK.xlsx`.
- `02b_classify_damage_by_ownership.R` creates `outputs/2000_24_damage_BK.xlsx` with BK1_Damage_Classes, BK2_Damage_Classes and BK3_Damage_Classes.
- `03b_identify_neighbors_and_calculate_deltas_by_ownership.R` reads that ownership classification workbook and creates `outputs/2000_24_BK_with_deltas.xlsx` with BK1, BK2 and BK3.
- `06_run_anova_and_check_residuals.R` reads this delta workbook.
- `03a_identify_neighbors_and_calculate_deltas_all_ownerships.R` still expects the historical input `data/final_damage_withoutBK.xlsx`; its equivalence to the newer classification workbook has not been assumed. It creates `outputs/neighbors_category.xlsx`, including the full delta sheet needed by `04a_classify_high_damage_neighbors_all_ownerships.R`.
- `05_check_delta_distributions.R` and `04b_classify_high_damage_neighbors_by_ownership.R` require the separate historical input `data/BK_final_with_deltas.xlsx`. No script supplied here creates that exact file.
- `07_create_district_maps_and_animation.R` requires `data/final_classified_damage2.xlsx` and the older Neww2 boundaries.
- `08_explore_harvest_baseline_plot_variants.R` includes alternatives needing an `All` sheet in the ownership workbook and `data/damage_withoutBKold.xlsx`. The supplied ownership classification script does not create the `All` sheet. Treat this as an archive of plot variants until those inputs are verified.

The main harvest workbook uses ERHEBUNGSBEZIRK, JAHR, BK, Gesamteinschlag, Schadholz, ZUORDNUNG and BUNDESLAND_NAME across these scripts. Spatial joins use district name `g_name`. Other workbooks need the exact columns and sheets requested in their scripts; similar filenames do not imply equivalent data.

## Upload to GitHub

Upload the extracted `.R` files, README.md, REVIEW_NOTES.md and .gitignore to the repository root. The ZIP is a transport bundle; extract it before uploading. If copying manually, create each file with exactly the supplied filename and paste its entire contents, including the local block and closing brace.

## Validation

The files were inspected and checked for retained personal paths, package-installation commands in analysis scripts, and balanced delimiters. R is not installed in the preparation environment, and the source datasets were not supplied. Neither R parsing nor execution nor numerical reproduction has been verified.

## Numbering guide

00 installs dependencies; 01 explores harvest trends; 02 classifies damage; 03 identifies neighbors and computes deltas; 04 explores high-damage neighbors; 05 checks distributions; 06 runs the supplied ANOVA diagnostics; 07 maps districts; 08 retains historical baseline plot variants.

The a/b suffix marks parallel branches: a combines ownership categories, b analyzes them separately. It does not mean every file must be executed in filename order. Some later files depend on historical workbooks or sheets not produced by these scripts, as documented above. Step 06 is ANOVA diagnostics, not the final ART analysis; step 08 is exploratory. Step 06b now supplies ART models and post-hoc tests; stages 09a and 09b explore recovery.

## Additional scripts

| Original | Added GitHub file |
| --- | --- |
| shapefile_changenames(1).R | 00b_harmonize_district_boundaries.R |
| Histogram(1).R | 05a_explore_salvage_distributions_and_thresholds.R |
| histogram_3(1).R | 05b_check_delta_distributions_excluding_hotspots.R |
| Non_parametric_complete(1).R | 06b_run_art_models_and_posthoc_tests.R |
| hotspot_recovery(1).R | 09a_explore_hotspot_recovery_baselines.R |
| hypo4(1).R | 09b_analyze_short_hotspot_episodes_and_recovery.R |

Keep the existing numbered files. The new a/b suffixes group related additions without requiring any renaming of files already uploaded. 05a explores harvest/salvage distributions; 05b excludes hotspot district-year observations from delta plots, while 05 retains the original full-data diagnostics.

00b reads the original STATISTIK_AUSTRIA_POLBEZ_20250101 shapefile from data/shapefiles/ and writes merged boundaries to outputs/shapefiles/. It does not overwrite source boundaries. Other scripts still read the established Neww3 file from data/shapefiles/: review the generated geometry and identifiers before intentionally using it there. The generated attributes are g_name, g_id and geometry; additional state attributes expected in some maps are not retained by the original aggregation.

05a requires data/final_classified_damage.xlsx (Sheet1), which is distinct from final_classified_damage2.xlsx, and outputs/neighbors_category.xlsx (Neighbors_names). 05b requires data/BK_final_with_deltas.xlsx (BK1–BK3). 06b and both 09 scripts use outputs/2000_24_BK_with_deltas.xlsx (BK1–BK3), produced by 03b. Run 00_setup_packages.R again to install added dependencies.

06b contains ART main models, art.con() main-effect post-hoc comparisons, and interaction plots. The fitted formulas are preserved; matching the paper's final numerical tables has not been verified. Both 09 scripts are exploratory recovery analyses, not corrections to the 03 delta formulas. 09b stops before its final summary/plot/post-hoc section because the original definition of year3_clean was not included. Restore that cleaning step deliberately rather than guessing its filtering rule.
