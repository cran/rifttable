## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example, message = FALSE-------------------------------------------------
library(rifttable)
library(dplyr)
data(breastcancer, package = "risks")

tibble::tribble(
  ~label,                      ~type,
  "**Absolute estimates**",    "",
  "Observations",              "total",
  "Outcomes",                  "outcomes",
  "Outcomes/Total",            "outcomes/total",
  "Cases/Controls",            "cases/controls",
  "Risk",                      "risk",
  "Risk (95% CI)",             "risk (ci)",
  "Outcomes (Risk)",           "outcomes (risk)",
  "Outcomes/Total (Risk)",     "outcomes/total (risk)",
  "",                          "",
  "**Comparative estimates**", "",
  "Risk ratio (95% CI)",       "rr",
  "Risk difference (95% CI)",  "rd",
  "Odds ratio (95% CI)",       "or"
) |>
  mutate(
    exposure = "stage",
    outcome = "death"
  ) |>
  rifttable(
    data = breastcancer,
    overall = TRUE
  ) |>
  rt_gt() # Formatted output

