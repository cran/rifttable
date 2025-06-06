## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
library(rifttable)
data(cancer, package = "survival")

cancer <- cancer |>
  tibble::as_tibble() |>
  dplyr::mutate(
    # The exposure (here, 'sex') must be categorical (a factor)
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    time = time / 365.25, # transform to years
    status = status - 1
  )

tibble::tribble(
  ~label,                                  ~type,
  "**Absolute estimates**",                "",
  "*Counts and sums*",                     "",
  "  Observations, *N*",                   "total",
  "  Events, *n*",                         "events",
  "  Events/observations",                 "events/total",
  "  Events/person-years",                 "events/time",
  "*Follow-up*",                           "",
  "  Person-years",                        "time",
  "  Maximum follow-up, years",            "maxfu",
  "  Median follow-up, years",             "medfu",
  "  Median follow-up (IQR), years",       "medfu (iqr)",
  "*Rates*",                               "",
  "  Rate per 1000 person-years",          "rate",
  "  Rate per 1000 person-years (95% CI)", "rate (ci)",
  "  Events/py (rate per 1000 py)",        "events/time (rate)",
  "*Risks*",                               "",
  "  1-year survival",                     "surv",
  "  1-year survival (95% CI)",            "surv (ci)",
  "  1-year risk/cumulative incidence",    "cuminc",
  "  1-year risk (95% CI)",                "cuminc (ci)",
  "  Median survival, years",              "medsurv",
  "  Median survival (95 CI), years",      "medsurv (ci)",
  "",                                      "",
  "**Comparative estimates**",             "",
  "  1-year survival difference",          "survdiff",
  "  1-year risk difference",              "cumincdiff",
  "  1-year survival ratio",               "survratio",
  "  1-year risk ratio",                   "cumincratio",
  "  Hazard ratio (95% CI)",               "hr"
) |>
  dplyr::mutate(
    time = "time",
    event = "status",
    exposure = "sex",
    arguments = list(list(timepoint = 1))
  ) |>
  rifttable(
    data = cancer,
    overall = TRUE
  ) |>
  rt_gt()

