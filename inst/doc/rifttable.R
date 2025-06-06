## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rifttable)

## ----data2--------------------------------------------------------------------
data(cancer, package = "survival")

cancer <- cancer |>
  tibble::as_tibble() |>
  dplyr::mutate(
    # The exposure (here, 'sex') must be categorical
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    time = time / 365.25,
    status = status - 1
  )

cancer

## ----ex1a---------------------------------------------------------------------
design1 <- tibble::tibble(
  label = c(
    "Outcomes",
    "Total",
    "Outcomes/Total",
    "Risk",
    "Risk (CI)",
    "Outcomes (Risk)",
    "Outcomes/Total (Risk)",
    "RR",
    "RD"
  )
) |>
  dplyr::mutate(
    type = label,
    exposure = "sex",
    outcome = "status"
  )

## ----ex1b---------------------------------------------------------------------
rifttable(
  design = design1,
  data = cancer
)

## ----ex2a---------------------------------------------------------------------
rifttable(
  design = design1,
  data = cancer
) |>
  rt_gt()

## ----ex3----------------------------------------------------------------------
rifttable(
  design = design1 |>
    dplyr::filter(label %in% c(
      "Outcomes/Total (Risk)",
      "RR",
      "RD"
    )),
  data = cancer,
  layout = "cols"
) |>
  rt_gt()

## ----ex4a---------------------------------------------------------------------
design2 <- tibble::tribble(
  # Elements that vary by row:
  ~label,                       ~stratum, ~confounders, ~type,
  "**Overall**",                NULL,     "",           "blank",
  "  Events",                   NULL,     "",           "events",
  "  Person-years",             NULL,     "",           "time",
  "  Rate/1000 py (95% CI)",    NULL,     "",           "rate (ci)",
  "  Unadjusted HR (95% CI)",   NULL,     "",           "hr",
  "  Age-adjusted HR (95% CI)", NULL,     "+ age",      "hr",
  "",                           NULL,     "",           "blank",
  "**Stratified models**",      NULL,     "",           "",
  "*ECOG PS1* (events/N)",      1,        "",           "events/total",
  "  Unadjusted",               1,        "",           "hr",
  "  Age-adjusted",             1,        "+ age",      "hr",
  "*ECOG PS2* (events/N)",      2,        "",           "events/total",
  "  Unadjusted",               2,        "",           "hr",
  "  Age-adjusted",             2,        "+ age",      "hr",
  "",                           NULL,     "",           "",
  "**Joint model**, age-adj.",  NULL,     "",           "",
  "  ECOG PS1",                 1,        "+ age",      "hr_joint",
  "  ECOG PS2",                 2,        "+ age",      "hr_joint"
) |>
  # Elements that are the same for all rows:
  dplyr::mutate(
    exposure = "sex",
    event = "status",
    time = "time",
    effect_modifier = "ph.ecog"
  )

## ----ex4b---------------------------------------------------------------------
rifttable(
  design = design2,
  data = cancer |>
    dplyr::filter(ph.ecog %in% 1:2)
) |>
  rt_gt()

## ----ex5a---------------------------------------------------------------------
design3 <- tibble::tribble(
  ~label,     ~stratum, ~type,          ~type2,
  "ECOG PS1", 1,        "events/total", "hr",
  "ECOG PS2", 2,        "events/total", "hr"
) |>
  dplyr::mutate(
    exposure = "sex",
    event = "status",
    time = "time",
    confounders = "+ age",
    effect_modifier = "ph.ecog"
  )

rifttable(
  design = design3,
  data = cancer |>
    dplyr::filter(ph.ecog %in% 1:2)
) |>
  rt_gt()

## ----ex5b---------------------------------------------------------------------
rifttable(
  design = design3,
  data = cancer |>
    dplyr::filter(ph.ecog %in% 1:2),
  layout = "cols",
  type2_layout = "cols"
) |>
  rt_gt()

## ----ex6----------------------------------------------------------------------
tibble::tribble(
  ~label,                   ~stratum, ~type,        ~digits,
  "Marginal mean (95% CI)", NULL,     "mean (ci)",  1,
  "  Male",                 "Male",   "mean",       NA,
  "  Female",               "Female", "mean",       NA,
  "",                       NULL,     "",           NA,
  "Stratified model",       NULL,     "",           NA,
  "  Male",                 "Male",   "diff",       1,
  "  Female",               "Female", "diff",       1,
  "",                       NULL,     "",           NA,
  "Joint model",            NULL,     "",           NA,
  "  Male",                 "Male",   "diff_joint", NA,
  "  Female",               "Female", "diff_joint", NA
) |>
  dplyr::mutate(
    exposure = "ph.ecog_factor",
    trend = "ph.ecog",
    outcome = "age",
    effect_modifier = "sex"
  ) |>
  rifttable(
    data = cancer |>
      dplyr::filter(ph.ecog < 3) |>
      dplyr::mutate(ph.ecog_factor = factor(ph.ecog))
  ) |>
  rt_gt()

