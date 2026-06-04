# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**nmecr** is an R package providing peer-reviewed energy data analysis algorithms for site-specific Measurement & Verification (M&V) in commercial/institutional buildings. It implements 11 modeling algorithms (SLR, 3P/4P/5P changepoint, TOWT, TOW, HDD, CDD, HDD-CDD, Mean) across hourly, daily, and monthly time intervals, with ASHRAE Guideline 14-compliant savings and uncertainty calculations.

## Build & Development Commands

```r
# Install from source (in R console)
devtools::install()

# Build package
devtools::build()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test nmecr_overview.R")

# Regenerate documentation (NAMESPACE + man/*.Rd from roxygen2 comments)
devtools::document()

# R CMD check (comprehensive package validation)
devtools::check()

# Build vignettes
devtools::build_vignettes()
```

## Architecture

### Data Pipeline

The package follows a linear pipeline: **data preparation → model fitting → prediction → savings calculation**.

```
create_dataframe(eload, temp)     # Combines energy + temperature data
  └─> aggregate()                 # Core aggregation to desired interval

assign_model_inputs()             # Creates configuration list for model options

model_with_*()                    # Fits model (SLR, CP, HDD_CDD, TOWT, mean)
  └─> Returns list: {model, training_data, model_input_options, model_stats}

calculate_model_predictions()     # Generates predictions on new data
calculate_summary_statistics()    # R², CVRMSE, NDBE, MBE, DOF
calculate_savings_and_uncertainty()           # Site-specific savings
calculate_norm_savings_and_uncertainty()      # Weather-normalized savings (TMY)
```

### Key Design Patterns

- **No custom classes**: All functions return standard R objects (lm, data.frame, list). Model objects are named lists containing the fitted lm model, training data with predictions, configuration, and statistics.
- **Configuration via `assign_model_inputs()`**: Factory function that produces a validated options list (regression type, breakpoints, temperature knots, occupancy threshold, day normalization). The `regression_type` argument is the dispatch key for which `model_with_*()` to call and is matched with `match.arg()`, so exact strings matter. Accepted values (many have short aliases): `"TOWT"`, `"TOW"`, `"SLR"`, `"HDD-CDD Multivariate Regression"`/`"HDD-CDD"`, `"HDD Regression"`/`"HDD"`, `"CDD Regression"`/`"CDD"`, `"Three Parameter Cooling"`/`"3PC"`, `"Three Parameter Heating"`/`"3PH"`, `"Four Parameter Linear Model"`/`"4P"`, `"Five Parameter Linear Model"`/`"5P"`, `"Mean"`.
- **TOWT models have dedicated helpers**: `find_occ_unocc()` for automated occupancy detection, `calculate_temp_knots()` for temperature breakpoint selection, `fit_TOWT_reg()` for the regression implementation, and `calculate_TOWT_model_predictions()` for predictions.
- **Changepoint models use the `segmented` package**: `model_with_CP()` wraps segmented regression for 3P, 4P, and 5P models with automatic breakpoint estimation.

### Source Code Organization (R/)

| Category | Files | Description |
|----------|-------|-------------|
| Data prep | `create_dataframe.R`, `aggregate.R`, `align_data.R` | Input processing and time-interval aggregation |
| Models | `model_with_SLR.R`, `model_with_CP.R`, `model_with_HDD_CDD.R`, `model_with_TOWT.R`, `model_with_mean.R`, `model_demand_with_TOWT.R` | One file per algorithm family |
| Analysis | `calculate_model_predictions.R`, `calculate_summary_statistics.R`, `calculate_savings_and_uncertainty.R`, `calculate_coverage.R`, `calculate_norm_savings_and_uncertainty.R` | Post-model analysis |
| Helpers | `assign_model_inputs.R`, `fit_TOWT_reg.R`, `find_occ_unocc.R`, `calculate_temp_knots.R`, `create_temp_matrix.R` | Internal utilities |

### Testing

Tests use **testthat** with a reference-based (golden file) approach. Expected outputs are stored as `.rds` files in `tests/testthat/Expectations/` and `tests/testthat/Processed Data/`. Tests compare function output against these stored references using `expect_identical()`.

When modifying function behavior that changes outputs, you must regenerate the affected `.rds` expectation files.

Note: the five test files contain spaces in their names (e.g. `test nmecr_overview.R`, `test dataframes creation.R`), so quote the path when running a single file: `testthat::test_file("tests/testthat/test nmecr_overview.R")`.

### Bundled Datasets

Six datasets in `data/` are used in tests and vignettes: `eload`/`temp` (daily), `school_eload`/`school_temp`/`school_op_mode` (hourly), and `TMY3` (typical meteorological year). Documented in `R/data.R`.

## Key Dependencies

- **segmented**: Changepoint/piecewise regression
- **dplyr/magrittr**: Data manipulation and pipe operator (`%>%`)
- **lubridate**: Date-time operations
- **xts/zoo**: Time series handling in aggregation
- **purrr**: Functional iteration helpers

## Important Context

- The `assertive` package was recently removed (CRAN-archived) and replaced with base R equivalents (`is.logical()`, `is.numeric()`, etc.).
- Documentation is generated by roxygen2 — edit roxygen comments in R files, not `man/*.Rd` or `NAMESPACE` directly.
- The default temperature balance point is 65°F throughout the package.
- Vignettes live in `inst/vignettes/` (not the standard `vignettes/` location).
