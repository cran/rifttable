## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ex_continuous, message = FALSE-------------------------------------------
library(dplyr)
library(rifttable)
data(cancer, package = "survival")
cancer <- cancer |>
  filter(ph.ecog < 3) |>
  mutate(ph.ecog = factor(ph.ecog))
attr(cancer$ph.ecog, which = "label") <- "ECOG performance status"

tribble(
  ~label,                                       ~type,
  "**Absolute estimates**",                     "",
  "Observations",                               "total",
  "Sum",                                        "sum",
  "Range (i.e., min-max)",                      "range",
  "Mean",                                       "",
  "  Mean (i.e., arithmetic mean)",             "mean",
  "  Mean (95% CI)",                            "mean (ci)",
  "  Mean (standard deviation)",                "mean (sd)",
  "  Geometric mean",                           "geomean",
  "Median",                                     "median",
  "Median (interquartile range)",               "median (iqr)",
  "",                                           "",
  "**Comparative estimates**",                  "",
  "Mean difference (95% CI)",                   "diff",
  "Median difference (95% CI)",                 "quantreg",
  "Mean ratio",                                 "",
  "  of arithmetic means",                      "fold",
  "  of arithmetic means, empirical SE",        "irrrob",
  "  of geometric means",                       "foldlog"
) |>
  mutate(
    exposure = "ph.ecog",
    outcome = "age"
  ) |>
  rifttable(
    data = cancer,
    diff_digits = 1, # Suppress unnecessary precision in most estimates
    # Show extraneous digits to highlight (minor) differences in ratio  models:
    ratio_digits = 3,
    overall = TRUE
  ) |>
  rt_gt() # obtain formatted output

