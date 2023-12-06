library(tidyverse)
library(gtsummary)
library(here)
library(janitor)
library(unheadr)
library(rio)




# Ex-1 ------------------------------------------------------------

# linelist_raw <- import(here("Copenhagen_raw.xlsx"))
# linelist_raw <- import(here("data-raw/Copenhagen_raw.xlsx"))
# linelist_raw <- import(here("../data-raw/Copenhagen_raw.xlsx"))
# linelist_raw <- import("data-raw/Copenhagen_raw.xlsx")
# linelist_raw <- import("data-raw/Copenhagen_raw.xlsx")
# linelist_raw <- import("../Copenhagen_raw.xlsx")
# linelist_raw <- read(here("Copenhagen_raw.xlsx"))
# linelist_raw <- read(here("data-raw/Copenhagen_raw.xlsx"))
# linelist_raw <- read(here("../data-raw/Copenhagen_raw.xlsx"))
# linelist_raw <- read("data-raw/Copenhagen_raw.xlsx")
# linelist_raw <- read("data-raw/Copenhagen_raw.xlsx")
# linelist_raw <- read("../Copenhagen_raw.xlsx")

linelist_raw




# Ex-2 ------------------------------------------------------------

# read and import the dataset
# use the working instructions used previously
# In the first line, create the path to the data set only
linelist <- ___("___") |>  # here is the path only
  # import the data
  # don't consider the first row (headers' groups) as column names!
  ___(col_names = FALSE) |>
  # mash together the four rows composing the headers
  # merge the groups' headers with the proper headers in a single row
  # (tip.: use a function in the `{unheadr}`)
  ___(
    n_name_rows = ___,  # how many rows has to be mashed?
    keep_names = FALSE,  # do we count/consider current colnames?
    sliding_headers = ___  # are there grouping headers'name (1st row)
  ) |>
  # remove empty cols (tip.: use a function in the `{janitor}`)
  ___(which = c("___")) |>
  # clean all the names
  # convert them using standard cammel_case convention
  # (tip.: use a function in the `{janitor}`)
  ___() |>
  # fill the `sex` column to have a complete data set (remind that we
  # have mashed the groups'heaaders with the columns'names)
  ___(demo_sex) |>
  # Because of our importing strategy, all columns are pure characters.
  # We can use the `parse_guess` function of the {readr} package to
  # infer a suitable proper type for each column.
  mutate(
    across(everything(), \(x) parse_guess(x, guess_integer = TRUE))
  )

linelist




# Update linelist, just execute the code below --------------------
linelist <- linelist |>
  mutate(
    time_day_onset = dmy(time_day_onset, tz = "UTC"),
    time_datetime_onset = paste(time_day_onset, time_start_hour) |>
      ymd_h(truncated = 2),
    demo_sex = factor(demo_sex),
    demo_group = factor(demo_group, labels = c("teacher", "student")),
    demo_class = factor(demo_class),
    demo_age = case_when(
      demo_age == 16 & demo_group == "teacher" ~ 61,
      demo_age %in% c(8, 180) & demo_group == "student" ~ 18,
      TRUE ~ demo_age
    ),
    across(where((\(x) all(x %in% c(0, 1, NA)))), as.logical),
    meal_datetime = ymd_h("2006-11-11 18"),
    symptoms_gastro = (
      symptoms_diarrhoea | symptoms_bloody | symptoms_vomiting
    ) |> replace_na(FALSE)
  ) |>
  rowwise() |>
  mutate(
    ate_anything = c_across(starts_with("food") & !ends_with("_d")) |>
      reduce(`|`) |>
      replace_na(FALSE)
  ) |>
  ungroup() |>
  mutate(
    food_meal = if_else(!food_meal & ate_anything, TRUE, food_meal),
    case = case_when(
      !food_meal | is.na(food_meal) ~ NA,
      symptoms_gastro &
        !is.na(time_datetime_onset) &
        (time_datetime_onset >= meal_datetime) ~ TRUE,
      TRUE ~ FALSE,
    ),
    incubation = if_else(case, time_datetime_onset - meal_datetime, NA),
    case = case & (time_datetime_onset <= (meal_datetime + days(2)))
  ) |>
  drop_na(case)

linelist


# Ex-3 ------------------------------------------------------------
tbl_symptoms_case <- linelist |>
  # Select symptom columns:
  select(case, starts_with("symptoms")) |>
  drop_na() |>

  # Reshape the dataset from wide to long
  ___(
    -___,
    names_to = "___",
    ___ = "value",
    values_drop_na = TRUE
  ) |>
  # Keep only TRUE values:
  filter(value) |>
  count(symptoms, case)

tbl_symptoms_case
