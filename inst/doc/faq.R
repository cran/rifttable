## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic, message = FALSE---------------------------------------------------
library(rifttable)
library(dplyr) # for data management, e.g., mutate()
library(tibble) # for constructing a tibble, e.g. via tribble()
data(breastcancer, package = "risks")

design <- tribble(
  ~label,                       ~type,                   ~stratum,
  "**Overall**",                "",                      "",
  "  Deaths/N",                 "outcomes/total",        c("Low", "High"),
  "  Risk",                     "risk",                  c("Low", "High"),
  "  Risk ratio (95% CI)",      "rr",                    c("Low", "High"),
  "  Risk difference (95% CI)", "rd",                    c("Low", "High"),
  "",                           "",                      "",
  "**Low hormone receptor**",   "",                      "",
  "  Deaths/N (Risk)",          "outcomes/total (risk)", "Low",
  "  Risk difference (95% CI)", "rd",                    "Low",
  "**High hormone receptor**",  "",                      "",
  "  Deaths/N (Risk)",          "outcomes/total (risk)", "High",
  "  Risk difference (95% CI)", "rd",                    "High"
) |>
  mutate(
    exposure = "stage",
    outcome = "death",
    effect_modifier = "receptor"
  )

rifttable(
  design = design,
  data = breastcancer
) |>
  rt_gt() # obtain formatted output

## -----------------------------------------------------------------------------
design1 <- tribble(
  ~label,   ~exposure, ~outcome, ~type,
  "N",      "stage",   "death",  "total",
  "Deaths", "stage",   "death",  "outcomes"
)
design1
rifttable(
  design = design1,
  data = breastcancer
) |>
  rt_gt()

## -----------------------------------------------------------------------------
design2 <- tibble(
  label = c("N", "Deaths"),
  exposure = "stage",
  outcome = "death",
  type = c("total", "outcomes")
)
design2
rifttable(
  design = design2,
  data = breastcancer
) |>
  rt_gt()

## -----------------------------------------------------------------------------
design3 <- bind_rows(
  tibble( # row 1
label = "N",
type = "total"
  ),
  tibble( # row 2
label = "Deaths",
type = "outcomes"
  )
) |>
  mutate( # elements that are the same for all rows
exposure = "stage",
outcome = "death"
  )
design3
rifttable(
  design = design3,
  data = breastcancer
) |>
  rt_gt()

## -----------------------------------------------------------------------------
design4 <- breastcancer |>
  table1_design(
death, # the total count will automatically be included
by = stage
  )
design4
rifttable(
  design = design4,
  data = breastcancer
) |>
  rt_gt()

## ----overall------------------------------------------------------------------
rifttable(
  design = design,
  data = breastcancer,
  overall = TRUE
) |>
  rt_gt() # obtain formatted output

## ----trend--------------------------------------------------------------------
rifttable(
  design = design |>
    mutate(trend = "stage_numeric"),
  data = breastcancer |>
    mutate(stage_numeric = as.numeric(stage))
) |>
  rt_gt() # obtain formatted output

## ----multiple-----------------------------------------------------------------
breastcancer_2exposures <- breastcancer |>
  mutate(
    exposure2 = case_when(
      stage == "Stage I" |
        (stage == "Stage II" & receptor == "High") ~
        "Level 1",
      stage == "Stage III" |
        (stage == "Stage II" & receptor == "Low") ~
        "Level 2"
    )
  )

attr(breastcancer_2exposures$exposure2, which = "label") <- "Exposure"
attr(breastcancer_2exposures$stage, which = "label") <- "Exposure"

bind_rows(
  design |>
    mutate(exposure = "exposure2") |>
    slice(2:5) |>
    rifttable(
      data = breastcancer_2exposures,
      layout = "cols"
    ),
  design |>
    slice(2:5) |>
    rifttable(
      data = breastcancer_2exposures,
      layout = "cols"
    )
) |>
  rt_gt() # obtain formatted output

## ----rounding-----------------------------------------------------------------
design <- tribble(
  ~label,                     ~type,
  "Deaths/N",                 "outcomes/total",
  "Risk",                     "risk",
  "Risk ratio (95% CI)",      "rr",
  "Odds ratio (95% CI)",      "or",
  "Risk difference (95% CI)", "rd"
) |>
  mutate(
    exposure = "stage",
    outcome = "death"
  )

rifttable(
  design = design,
  data = breastcancer,
  ratio_digits = 3, # Many digits for ratios
  risk_digits = 1
) |> # Fewer digits for risks
  rt_gt() # obtain formatted output

## ----rounding_decrease--------------------------------------------------------
rifttable(
  design = design,
  data = breastcancer,
  ratio_digits = 3,
  ratio_digits_decrease = NULL, # Do not round high ratios more
  risk_digits = 1
) |>
  rt_gt() # obtain formatted output

## ----rounding_digits----------------------------------------------------------
tribble(
  ~label,                     ~type,            ~digits,
  "Deaths/N",                 "outcomes/total", NA, # Uses rifttable default
  "Risk",                     "risk",           NA, # Uses risk_digits below
  "Risk ratio (95% CI)",      "",               NA,
  "  Rounded to 1 digit",     "rr",             1,
  "  Rounded to 2 digits",    "rr",             2,
  "Risk difference (95% CI)", "rd",             3
) |> # Overrides risk_digits
  mutate(
    exposure = "stage",
    outcome = "death"
  ) |>
  rifttable(
    data = breastcancer,
    risk_digits = 1
  ) |> # Fewer digits for risks, unless specified by "digits"
  rt_gt() # obtain formatted output

## ----joint--------------------------------------------------------------------
tribble(
  ~label,                       ~type,      ~stratum,
  "**Overall**",                "rd",       c("Low", "High"),
  "",                           "",         "",
  "**Stratified models**",      "",         "",
  "  Low hormone receptor",     "rd",       "Low",
  "  High hormone receptor",    "rd",       "High",
  "",                           "",         "",
  "**Joint models**",           "",         "",
  "  Low hormone receptor",     "rd_joint", "Low",
  "  High hormone receptor",    "rd_joint", "High"
) |>
  mutate(
    exposure = "stage",
    outcome = "death",
    effect_modifier = "receptor"
  ) |>
  rifttable(data = breastcancer) |>
  rt_gt()

## ----joint_reorder------------------------------------------------------------
tribble(
  ~label,                       ~type,      ~stratum,
  "**Joint models**",           "",         "",
  "  Low hormone receptor",     "rd_joint", "Low",
  "  High hormone receptor",    "rd_joint", "High"
) |>
  mutate(
    exposure = "stage",
    outcome = "death",
    effect_modifier = "receptor"
  ) |>
  rifttable(
    data = breastcancer |>
      mutate(
        receptor = relevel(
          factor(receptor), # Make "receptor" a factor in the first place
          ref = "Low"
        )
      )
  ) |> # Set new reference category
  rt_gt()

## ----reorder_middle-----------------------------------------------------------
result_reordered <- tibble(
  label = "**RD (95% CI)**",
  type = "rd",
  exposure = "stage",
  outcome = "death"
) |>
  rifttable(
    data = breastcancer |>
      mutate(
        stage = relevel(
          stage,
          ref = "Stage II"
        )
      )
  )

result_reordered |>
  rt_gt()

## ----reorder_middle2----------------------------------------------------------
result_reordered |>
  select(stage, "Stage I", everything()) |>
  rt_gt()

## ----change_ci----------------------------------------------------------------
tribble(
  ~label,            ~type,                   ~ci,
  "Deaths/N (Risk)", "outcomes/total (risk)", NA,
  "Risk ratio",      "",                      NA,
  "  80% CI",        "rr",                    0.8,
  "  95% CI",        "rr",                    NA, # Defaults to 0.95
  "  99% CI",        "rr",                    0.99
) |>
  mutate(
    exposure = "stage",
    outcome = "death"
  ) |>
  rifttable(
    data = breastcancer,
    risk_percent = TRUE
  ) |>
  rt_gt() # obtain formatted output

## ----custom, message = FALSE--------------------------------------------------
data(cancer, package = "survival")
cancer <- cancer |>
  tibble::as_tibble() |>
  mutate(
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    )
  )

design <- tibble::tibble(
  type = "mean",
  exposure = "sex",
  outcome = "age",
  effect_modifier = "ph.ecog",
  stratum = 1:2,
  label = paste0("ECOG PS ", stratum, ": mean age")
)

design |>
  rifttable(
    data = cancer,
    overall = TRUE
  ) |>
  rt_gt()

## ----custom_fn----------------------------------------------------------------
estimate_my_mean <- function(data, ...) {
  data |>
    group_by(.exposure) |>
    summarize(
      res = paste(
        round(
          mean(.outcome),
          digits = 3
        ),
        "yrs"
      )
    )
}

## ----custom_use---------------------------------------------------------------
design |> # Edit the previous design
  mutate(
    type = "my_mean", # Replace built-in "mean" by custom "my_mean"
    label = paste0(label, " (custom)")
  ) |>
  rifttable(
    data = cancer,
    overall = TRUE
  ) |>
  rt_gt()

