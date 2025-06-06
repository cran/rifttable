## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rifttable)

## ----data, message = FALSE----------------------------------------------------
library(dplyr) # for data handling
data(cancer, package = "survival")
cancer <- cancer |>
  tibble::as_tibble() |>
  dplyr::mutate(
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    ph.ecog = factor(ph.ecog)
  )

## ----design-------------------------------------------------------------------
design <- cancer |>
  table1_design(
    age, ph.ecog, ph.karno, pat.karno, # leave empty to include all variables
    by = sex
  )

# Print the design for illustration purposes
design

## ----rifttable----------------------------------------------------------------
design |>
  rifttable() |>
  rt_gt() # obtain formatted output

## ----custom-------------------------------------------------------------------
# Alternative: set_variable_labels() from the {labelled} package
attr(cancer$age, "label") <- "Age, years" # At diagnosis? Unclear (Loprinzi 94)
attr(cancer$ph.ecog, "label") <- "Physician-rated ECOG score"
attr(cancer$ph.karno, "label") <- "Physician-rated Karnofsky score"
attr(cancer$pat.karno, "label") <- "Patient-rated Karnofsky score"
attr(cancer$sex, "label") <- "Sex"

design <- cancer |>
  table1_design(
    age, ph.ecog, ph.karno, pat.karno,
    by = sex,
    continuous_type = "mean (sd)"
  ) |> # default: "median (iqr)"
  mutate( # rounding specifically for the "age" variable
    digits = if_else(
      outcome == "age",
      true = 1,
      false = NA
    )
  )

design |>
  rifttable(
    diff_digits = 0, # rounding for continuous variables other than age
    overall = TRUE
  ) |> # add unstratified "overall" column
  rt_gt() |> # obtain formatted output
  gt::tab_footnote(
    footnote = "Data shown are count (percent) or mean (standard deviation)."
  )

## ----morestats----------------------------------------------------------------
design_new <- design |>
  mutate( # create three rows, not one, for "age"
    copy = if_else(
      outcome == "age",
      true = 3,
      false = 1
    )
  ) |>
  tidyr::uncount(copy, .id = "copy") |>
  mutate( # set new labels and types for the three rows on age
    label = case_when(
      outcome == "age" & copy == 2 ~ "  Mean (SD)",
      outcome == "age" & copy == 3 ~ "  Range",
      TRUE ~ label
    ),
    type = case_when(
      outcome == "age" & copy == 1 ~ "",
      outcome == "age" & copy == 2 ~ "mean (sd)",
      outcome == "age" & copy == 3 ~ "range",
      TRUE ~ type
    )
  )
design_new
design_new |>
  rifttable(
    diff_digits = 0,
    overall = TRUE
  ) |>
  rt_gt() |>
  gt::tab_footnote(
    footnote = "Data shown are count (percent), unless indicated otherwise."
  )

