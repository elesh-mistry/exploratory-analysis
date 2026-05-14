# Exploratory data analysis
This project contains functions for exploratory data analysis and basic statistics.

The sections below list out the functions available. The pre-requisite packages are packages that need to be called for the functions to work. <b>Almost all function will require packages tidyverse, scales, and gtools</b>. Note the functions under the section "Graph themes" will be required for any of the other graph output functions. This helps to ensure consistent formatting of charts.

Given the number of functions, descriptions of parameters have not been included below. Please see the example scripts for worked examples.

Functions included in this project:
- [Completeness, outliers, and duplicates](#completeness-outliers-and-duplicates)
- [Graph themes](#graph-themes-required-for-any-of-the-below-graphing-functions)
- [Time-series plots](#time-series-plots)
- [Scatterplots and pairwise distribution plots](#scatterplots-and-pairwise-distribution-plots)
- [Distribution plots](#distribution-plots)
- [Flow plots](#flow-plots)
- [Maps](#maps)
- [Correlations](#correlations)
- [Compare gaussian measurements](#compare-gaussian-measurements)
- [Compare non-gaussian measurements, ranks, and scores](#compare-non-gaussian-measurements-ranks-and-scores)
- [Compare categorical and binomial outcomes](#compare-categorical-and-binomial-outcomes)

---

## Completeness, outliers, and duplicates

See script `data_completeness`. Includes count and distribution of NAs, label frequency, basic stats, field and label alignment between dataframes, duplicate counts, and outliers filter.

<details>
  
| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| na_counts_total  | Sums the number of NA's for each field in a dataframe and return the number and percentage of NAs. | None | None |
| na_counts_split  | Sums the number of NA's for each field in a dataframe, split by another categorical field, and return the number and percentage of NAs. | None | None |
| na_graph | Three graphical representations of NA's in a dataframe, similar to Python's missingno. "Matrix" for a tile chart for each observation by field. "Summary" for percentage missing by field. "Correlation" to see which fields have pairwise commonly missing fields. | None | Function only allows a dataframe with a maximum 1,000,000 datapoints. This is because the "Matrix" option in particular can take a long time to render otherwise. |
| label_freq  | Sums and returns the 'x' most frequent labels for each character and factor field in a dataframe. Returns frequency number, percentage, and cumulative percentage. | None | None |
| basic_stats_total  | Returns numeric stats for all numeric fields. Stats returned: No. of Observations, NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, Median, Sum, Std Error of Mean, Lower CL Mean, Upper CL Mean, Variance, Std Dev, Skewness, and Kurtosis. | fBasics | Can take longer for large dataframes, but no upper limit set. If you are unlikely to use all of the stats above, you can simply use summary(df) instead. This will return NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, and Median. |
| basic_stats_split  | Returns numeric stats for all numeric fields, split by categorical field. Stats returned: No. of Observations, NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, Median, Sum, Std Error of Mean, Lower CL Mean, Upper CL Mean, Variance, Std Dev, Skewness, and Kurtosis. | fBasics | Can take longer for large dataframes, but no upper limit set. If you are unlikely to use all of the stats above, you can simply use summary(df) instead. This will return NAs, Min, Max, 1st Quartile, 3rd Quartile, Mean, and Median. Will require a loop to split by another field. |
| fields_alignment  |  Useful as a check before joining two dataframes. Compares the field names of two dataframes and returns fields in common, if common fields are the same class, and fields only in one dataframe. | None | None |
| label_alignment  |  Useful as a check before joining two dataframes. For the same categorical field in two dataframes, compares unique values and returns those in common and those in only one dataframe. | None | None |
| duplicates_count  |  Generates a dataframe containing all duplicates. Can specify which fields to check across, or for all fields use test.duplicated.rows = colnames(df) | None | Uses dplyr to filter for duplicates. This can take a long time to run for large dataframes. |
| outliers  | Returns a dataframe containing outliers for a numerical field in a dataframe. User can select either standard deviation (sd) or standard error (se) and the number of deviations from the mean. | None | None |

</details>

---

## Producing plots

### Graph themes (required for any of the below graphing functions)

See script `graphs_theme`. Includes palette generation and theme setting.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| pal_range_cats  | Generate a three-part colour range for a specified number of categories | grDevices | None |
| pal_range_interpolate  | Generate a three-part colour range, applied as a smooth function | grDevices | None |
| plot_theme  | Theme to be applied to graphs produced by other functions in this exploratory data analysis project | None | None |

</details>

### Time-series plots

See script `plots_time_series`. Includes basic line charts, box-plot over time, seasonal decomposition, Bai-Perron test, SPC charts, and theographs.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| plt_line  | Generate time-series line charts. Includes options for filling regions and rolling averages. | None | None |
| plt_box_ts  | Generate a series of box plots over time to see how distribution changes over time. | None | None |
| plt_seasonal  | Generate charts showing a seasonal decomposition of a variable using moving averages, from stats package. | None | Designed for quick analysis. If more detailed look at decomposition required, use stats::stl  |
| plt_bai_perron  | Generate time-series plots with Bai-Perron breakpoints, regressing only on the index | strucchange | Designed for quick analysis, hence only regression on index. If more detailed look at structural breaks required, see documentation for package "strucchange". |
| plt_spc  | Generate basic SPC charts. | NHSRplotthedots | Only very basic plots. For different chart types (e.g., p charts), break points, etc., see documentation for package "NHSRplotthedots" and other NHS resources. |
| plt_duration  | Generate theographs (aka Priestley duration plots). | vistime | None |

</details>
  
### Scatterplots and pairwise distribution plots

See script `plots_scatterplots`. Includes basic scatterplots, heatmaps, pairwise scatterplots if all numeric, pairwise mixture of plots if mixture of variables, and tabular heatmap.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| plt_scatter  | Generate scatter charts. Includes options for contour lines. | None | For a large number of datapoints or facet grids, options for contour lines and non-linear best fit lines will run slowly. |
| plt_heatmap  | Generate coloured hex-heatmap for two numeric variables. | None | None |
| plt_scatter_pairs  | Generates pairwise scatterplots and correlation coefficients for selected numeric fields. Significance: * < 0.05, ** < 0.01, *** < 0.005. | GGally | Distance correlation unavailable - use function stat_cor_distance. Colour parameters are uniform for all datapoints (e.g., cannot have separate colour by another categorical field). For a large number of datapoints, options for non-linear best fit lines will run slowly. |
| plt_pair_combo  | Pairwise plots and correlation coefficients/tests of differences. Allows for a mix of numeric and non-numeric. For continuous vs continuous, presenting Spearman's correlation coefficient. For categorical vs continuous, presenting p-value for Wilcoxon test if 2 categories, or Kruskal-Wallis test if > 2 categories. For categorical vs categorical, presenting Chi-squared test p-value. You can edit the function directly for alternative tests (e.g., switch to Pearson correlations and unpaired t-tests/one-way ANOVA if Gaussian population). Significance: * < 0.05, ** < 0.01, *** < 0.005. | GGally | Distance correlation unavailable - use function stat_cor_distance. Colour parameters are uniform for all datapoints (e.g., cannot have separate colour by another categorical field). For a large number of datapoints, function will run slowly. |
| plt_xyheatmap  | For a pair of categorical variables, generate a grid-style heatmap for a chosen numeric field. | None | None |

</details>
  
### Distribution plots

See script `plots_distributions`. Includes box plots, plots of means, violin plots, ridge plots, histograms, bar plots, and funnelplots.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| plt_box  | Generate box plots. Line = median, 'X' = mean, Box = inter-quartile range, Whisker = 1.5 x IQR, Dot = outlier. | None | Also see plt_box_ts for box-plots over time. |
| plt_means  | Generate plots of means.  | None | Choosing option "ci" implicitly assumes a Gaussian distribution or CLT applies. May not be appropriate for all data. |
| plt_violin  | Generate violin plots.  | None | None |
| plt_ridge  | Generate ridgeline plots.  | ggridges | None |
| plt_histogram  | Generate histograms.  | None | None |
| plt_bar  | Generate bar plots. Includes options for stacked plots.  | None | Recommend not showing labels for complex charts involving lots of stacking, facet grids, etc., as alignment often fails. |
| plt_funnel  | Generate funnel plots for count variables. The Poisson regression will check for overdispersion and if overdispersed, switches to use a negative binomial regression. Note that two plots are produced: the first looks at how well the Poisson regression fits the data, and the second is the funnel plot itself. Also review regression results in table res.reg.  | FunnelPlotR, AER, MASS, cowplot, broom. May require conflicted, then run conflicts_prefer(dplyr::filter(), dplyr::select()). | As with all regression models, performance likely to be slow with many datapoints. FunnelPlotR leaves on all labels for outliers as default, so if there are many groups, the labels may overlap. |

</details>
  
### Flow plots

See script `plots_flows`. Includes Sankey plots.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| plt_sankey  | Generate Sankey plots. | ggsankey, which is not available in CRAN. Use remotes::install_github("davidsjoberg/ggsankey, dependencies = TRUE) | Recommend keeping relatively simple given slow performance of the package. |

</details>
  
### Maps

See script `plots_maps`. Includes heatmaps by latitude/longitude and by LSOA, and pie charts by latitude/longitude.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| plt_map_latlong  | Generate a map using latitude/longitude and colour-coded for a chosen metric. | leaflet | None |
| plt_map_latlong_pie  | Generate a map using latitude/longitude and a pie chart showing distribution by geography.  | leaflet, leaflet.minicharts | Data needs pre-grouping before the dataframe is passed into this function. |
| plt_map_lsoa  | Generate a map using LSOAs and colour-coded for a chosen metric. Could also be used by MSOA, electoral wards, etc. | leaflet, sf | Requires having the necessary shape files imported and transformed. See package "sf" details. |

</details>
  
---

## Statistical tests

### Correlations

See script `stats_correlation`. Includes tables for Pearson, Spearman, Distance, and Phi correlations.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| stat_cor  | Generate a table of pearson or spearman correlation coefficients. Also an option for a plot of the coefficients. For a visualisation with both scatterplots and coefficients, use the function plt_scatter_pairs. Significance: * < 0.05, ** < 0.01, *** < 0.005. | Hmisc | None |
| stat_cor_distance  | Generate a table of distance correlation coefficients. Also an option for a plot of the coefficients. Significance: * < 0.05, ** < 0.01, *** < 0.005. | energy. If not parallel running, purrr. If parallel running, future and furrr.  | Confidence intervals are produced using bootstrapping. If the function is taking a long time to run, consider running calculations in parallel (set run.parallel = TRUE), or reduce the number of iterations used to calculate the confidence intervals (default n.iter = 10000). |
| stat_cor_phi  | Generate a table of phi correlation coefficients for binary variables. Also an option for a plot of the coefficients. Significance marked for chosen alpha only. | statpsych | None |

</details>
  
### Compare gaussian measurements

See script `stats_compare_gauss`. Includes t-tests, one-way ANOVA, and repeated-measure ANOVA.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| stat_t_test  | Generate a vector with t-test results. Includes options for paired data and unequal variances.  | None | None |
| stat_anova_oneway  | Generate a vector with one-way ANOVA test results.  | None | Data needs to be organised in long format. |
| stat_anova_rm  | Generate a vector with repeated measures ANOVA test results.  | None | Data needs to be organised in long format. |

</details>
  
### Compare non-gaussian measurements, ranks, and scores

See script `stats_compare_nongauss`. Includes Mann-Whitney-Wilcoxon, Kolmogorov-Smirnov, Kruskal-Wallis, and Friedman rank sum tests.

<details>

| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| stat_mww  | Generate a vector with Mann-Whitney-Wilcoxon test results. Includes options for paired data and unequal variances.  | For unequal variances, lawstat (to run Brunner-Munzel test). | None |
| stat_ks  | Generate a vector with Kolmogorov-Smirnov test results. Option to plot distributions.  | None | None |
| stat_kw  | Generate a vector with Kruskal-Wallis test results.  | None | Data needs to be organised in long format. |
| stat_friedman  | Generate a vector with Friedman rank sum test results.  | None | Data needs to be stored as a matrix in wide format with just the numeric values. |

</details>
  
### Compare categorical and binomial outcomes

See script `stats_compare_categorical`. Includes chi-squared and McNemar's tests.

<details>
  
| Function name  | Description | Pre-requisite packages | Performance / limitations notes |
| ------------- | ------------- | ------------- |------------- |
| stat_chisq  | Generate a vector with chi-squared test results. For small samples (any E < 5 and/or N < 60) in 2x2 tables, function will automatically attempt Fisher's test.  | None | Data needs to be stored as a contingency table of class matrix of actual values. |
| stat_mcnemar  | Generate a vector with McNemar's test results. For small samples (b + c < 35) in 2x2 tables, function will automatically attempt a binomial exact test.  | None | Data needs to be stored as a contingency table of class matrix of actual values. |

</details>
