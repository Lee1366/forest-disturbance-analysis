# Review notes

## Changes made

All 11 originals were kept untouched. These renamed copies remove personal absolute Windows paths, put inputs and outputs in documented folders, consolidate package loading, move package installation into 00_setup_packages.R, and normalize text encoding/newlines. Analysis formulas, statistical alternatives and historical plotting sections are retained except for the specific fixes below.

- `01_explore_harvest_trends.R`: Restored total-harvest aggregation after salvage aggregation before state plots.
- `02a_classify_damage_all_ownerships.R`: Replaced missing-count fallback so absent damage categories display zero.
- `02b_classify_damage_by_ownership.R`: Replaced missing-count fallback so absent damage categories display zero.
- `03a_identify_neighbors_and_calculate_deltas_all_ownerships.R`: Restored commented-out filtering assignment, added missing full-delta sheet export, corrected category spelling. Historical hard-coded summaries are labeled.
- `03b_identify_neighbors_and_calculate_deltas_by_ownership.R`: Named BK1 export and standardized BK2 sheet name to BK2; delta formulas unchanged.
- `08_explore_harvest_baseline_plot_variants.R`: Disabled an incompatible exploratory join; corrected Allowable Annual Cut legend wording. Other legacy AAC/Hiebssatz object names remain historical, not estimates of permitted harvest.

## Scientific and reproducibility issues retained for review

1. **Baseline definitions differ.** In 03b_identify_neighbors_and_calculate_deltas_by_ownership.R, Regular_Mean uses observations i-9 through i: a ten-observation window including the current year. In 03a_identify_neighbors_and_calculate_deltas_all_ownerships.R, Regular_Mean uses all observations 1 through i after at least ten exist: an expanding mean including the current year. Neither is the preceding ten-year mean excluding year t. These formulas were deliberately not changed because doing so changes delta values and statistical results. A strictly preceding ten-year baseline with records starting in 2000 first becomes available in 2010; an inclusive ten-observation window first becomes available in 2009. Check the method intended for the paper against the actual final scripts and data.
2. **Calendar continuity is assumed.** Row-based windows and lead() treat the next observation as the next year; missing years and duplicate district-year records need checking. Zero baselines can yield non-finite deltas. Means and sums with na.rm=TRUE may hide incomplete observations.
3. **Damage classes are pooled across years.** Classification ranks absolute Total_Schadholz across district-year observations (separately within each ownership in the BK version). It does not select the top ten percent within each year. Ties are assigned by row order. This behavior is retained.
4. **Historical plot variants are not final methods.** In 08_explore_harvest_baseline_plot_variants.R, AAC and Hiebssatz are legacy object names. A moving average of observed harvest is not a regulatory allowable annual cut; a mean excluding hotspot observations is also not a regulatory cut. One mixed-ownership early rolling calculation groups only by district, so its window can mix ownership observations. Later raw-data variants assume rows are already at the intended district/year/ownership level. The final figure script mentioned in the original comments was not supplied.
5. **ANOVA is retained as diagnostics.** 06_run_anova_and_check_residuals.R runs aov() and residual checks. ARTool is loaded in a legacy plot file, but no art() model call was found in the supplied scripts. Do not describe this package as reproducing the final ART models. Shapiro tests need valid sample sizes and nonconstant residuals; no statistical assumptions have been certified here.
6. **Hard-coded historical results.** 03a_identify_neighbors_and_calculate_deltas_all_ownerships.R contains a plot built from manually entered means and confidence intervals. It is now explicitly labeled as historical and does not recompute those numbers from input data.
7. **Missing historical inputs.** README.md identifies workbook-name and sheet dependencies that cannot be confirmed without the original workbooks. Neww2 and Neww3 boundary files remain distinct. No arbitrary renaming or copying between historical workbooks was performed.
8. **Spatial joins and classifications need data checks.** District-name joins assume matching identifiers and unique geometries. In 07_create_district_maps_and_animation.R, the original name trimming happens after the join and trims NAME_2 rather than the join key g_name; inspect unmatched rows before interpreting a map. In the cleaned high-damage alternative, 'None' can include districts neighboring only hotspots; it is not automatically an unexposed control group. These scientific choices were not changed.
9. **Working-script behavior.** Multiple variants overwrite variables, repeat plots and append workbook sheets. Rerunning selected append-only sections against an already modified workbook can fail on duplicate sheet names. Long map/animation sections and high-resolution exports may take substantial time. The scope is code preparation for sharing, not a fully refactored pipeline.

## Before citing outputs as final results

Use the original data and final analysis version to verify baseline definitions, input identities, unique district-year keys, missing-year handling, statistical models, and agreement with your existing tables. Those checks require data not included with these attachments.
