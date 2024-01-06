library(tidyverse)
library(gtsummary)
library(here)
library(janitor)
library(unheadr)
library(rio)


# Ex1 -------------------------


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

# Ex2 -------------------------
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
  # mutate every columns to a suitable proper type across each one.
  ___(
    ___(everything(), \(x) parse_guess(x, guess_integer = TRUE))
  )



linelist


skimr::skim(linelist)

# Ex3 -------------------------
# Update linelist:
linelist <- linelist |>
  ___(
    time_day_onset = ___(time_day_onset, tz = "UTC"),
    time_datetime_onset = paste(time_day_onset, time_start_hour) |>
      ymd_h(truncated = 2) # <1>
  )

linelist


# Ex4 -------------------------
linelist <- linelist |>
  ___(
    demo_sex = factor(demo_sex),
    demo_group = ___(demo_group, labels = c("teacher", "student")),
    demo_class = factor(demo_class)
  )
linelist


linelist |>
  tbl_cross(demo_age, demo_group)

# Ex5 -------------------------
linelist <- linelist |>
  ___(
    demo_age = case_when(
      demo_age == 16 & demo_group == "teacher" ~ 61,
      demo_age %in% c(8, 180) & demo_group == "student" ~ 18,
      TRUE ~ demo_age
    )
  )
linelist


linelist |>
  tbl_cross(demo_age, demo_group)

# Ex6 -------------------------
# Update linelist
linelist <- linelist |>
  ## Please note: the following sequence of three `___` is a one of the
  ## most useful and used standard patters in R managing data in the
  ## tidyverse
  ___(
    ___(
      ___(\(x) all(x %in% c(0, 1, NA))),
      as.logical
    )
  )
linelist

# Update linelist
linelist <- linelist |>
  mutate(
    across(
      where(\(x) all(x %in% c(0, 1, NA))),
      as.logical
    )
  )
linelist

# Ex7 -------------------------

linelist <- linelist |>
  ___(
    meal_datetime = ymd_h("2006-11-11 18"),
    symptoms_gastro = (
      symptoms_diarrhoea | symptoms_bloody | symptoms_vomiting
    ) |>
      # Replace missing values with FALSE
      ___(FALSE)
  )
linelist

# Ex8 -------------------------
linelist <- linelist |>
  rowwise() |>
  ___(
    # Consider only vars that start with "food" and do not end with "_d"
    ate_anything = c_across(___("food") & !___("_d")) |>
      reduce(`|`) |>
      # Replace missing values with FALSE
      ___(FALSE)
  ) |>
  ungroup()
linelist


tbl_ate_meal <- linelist |>
  tbl_cross(food_meal, ate_anything)
tbl_ate_meal

# Ex9 -------------------------

linelist <- linelist |>
  # modify the food_meal column for people who ate something but didn't
  # have a meal
  ___(
    food_meal = if_else(
      !food_meal & ate_anything,
      true = TRUE,
      false = food_meal
    )
  )
linelist


tbl_ate_meal <- linelist |>
  tbl_cross(food_meal, ate_anything)
tbl_ate_meal

# Ex10 -------------------------
# Start with linelist:
linelist <- linelist |>
  # Create case definition:
  ___(
    # Define cases as those with any gastro symptoms with onset after
    # the meal:
    case = case_when(
      # Define excluded individuals as those with no record of a meal:
      !food_meal | is.na(food_meal) ~ NA,

      # Cases have any gastro symptoms with onset after the meal:
      symptoms_gastro &
        !is.na(time_datetime_onset) &
        (time_datetime_onset >= meal_datetime)
      ~ TRUE,

      # All the rest (i.e., `TRUE` evaluates as `TRUE` for all the cases
      # that didn't match a previous condition!) marked as FALSE,
      # i.e., non cases have no gastro symptoms but ate a meal at the party:
      # !symptoms_gastro |
      #   (symptoms_gastro & (time_datetime_onset < meal_datetime))
      TRUE ~ FALSE
    ),
  )
linelist

# Ex11 -------------------------
linelist <- linelist |>
  # Update incubation to be NA if case is not TRUE:
  ___(
    incubation = if_else(case, time_datetime_onset - meal_datetime, NA)
  )
linelist


# Update the case definition to limit to onset three days after meal
linelist <- linelist |>
  ___(
    case = case & (time_datetime_onset <= (meal_datetime + ___(2)))
  )
linelist


linelist |>
  tbl_cross(time_datetime_onset, case)

# Ex12 -------------------------
linelist <- linelist |>
  # Remove rows where case is NA:
  drop_na(___)
linelist

# Ex13 -------------------------
incplot <- linelist |>

  # Remove missing values from incubation column:
  ___(___) |>

  # Create an empty ggplot frame:
  ggplot(aes(x = incubation)) +

  # Add a histogram of incubation:
  geom_histogram(
    # Set bin widths to 6 hours:
    binwidth = 6
  ) +

  # Adapt scale to better fit data
  scale_x_continuous(breaks = seq(0, 48, 6)) +

  # Label x and y axes:
  labs(
    x = "Incubation period in 6-hour bins",
    y = "Number of cases"
  )

# Print plot:
incplot

# Ex14 -------------------------
# Fetch data:
epicurve_date <- linelist |>

  # Filter for cases where dayonset is not missing:
  ___(case, !is.na(time_day_onset)) |>  #<1>

  # Add dayonset to ggplot aesthetic:
  ggplot(aes(x = time_day_onset)) +

  # Add the proper `geom_*` for a barplot:
  geom_bar() +

  # Update x and y axis labels:
  labs(x = "Date of onset",
       y = "Number of cases") +

  # Remove unnecessary grid lines:
  theme_bw()

# Print epicurve:
epicurve_date


# Ex15 -------------------------
# Fetch data:
epicurve_hour <- linelist |>

  # Filter for cases where dayonset is not missing:
  ___(___, !is.na(time_datetime_onset)) |>

  # Add onset_datetime to ggplot aesthetic:
  ggplot(aes(x = time_datetime_onset)) +

  # Add the proper `geom_*` for a barplot:
  geom_bar() +

  # Adjust x axis scales to a suitable unit:
  scale_x_datetime(
    date_breaks = "6 hours",
    labels = scales::label_date_short()) +

  # Update x and y axis labels:
  labs(
    x = "Date and time of onset",
    y = "Number of cases"
  ) +

  # Remove unnecessary grid lines:
  theme_bw()

# Print epicurve:
epicurve_hour


# Ex16 -------------------------
# Fetch data:
epicurve_strata <- linelist |>

  # Filter for cases where onset_datetime is not missing:
  ___(___, !is.na(time_datetime_onset)) |>

  # Add factor onset_day to ggplot aesthetic:
  ggplot(aes(x = time_datetime_onset)) +

  # Add the proper `geom_*` for an "overall" barplot:
  geom_bar() +

  # Superimpose the same `geom_*` for a barplot
  # stratified by sex:
  geom_bar(aes(fill = demo_sex), position = "dodge") +

  # Adjust x axis scales to a suitable unit:
  scale_x_datetime(
    date_breaks = "6 hours",
    labels = scales::label_date_short()) +

  # Stratify by group:
  facet_wrap("demo_group", nrow = 2) +

  # Update x and y axis labels:
  labs(
    x = "Date and time of onset",
    y = "Number of cases",
    fill = "Sex",
    title = "Epicurve of the outbreak, overall (gray bars) and stratified by sex (coloured bars)",
    subtitle = str_glue(
      "Copenhagen, November 2006, N = {sum(linelist$case)}"
    )
  ) +

  # Add theme:
  theme_bw()

# Print epicurve:
epicurve_strata


linelist |>
  tbl_cross(case, demo_sex, percent = "row", digits = c(0, 2))

linelist |>
  tbl_cross(case, demo_group, percent = "row", digits = c(0, 2))

linelist |>
  tbl_cross(case, demo_class, percent = "row", digits = c(0, 2))

tbl_symptoms <- linelist |>
  tbl_summary(
    by = case,
    include = starts_with("symptoms"),
    percent = "column",
    # Create nice labels:
    label  = list(
      symptoms_diarrhoea   ~ "Diarrhoea",
      symptoms_bloody      ~ "Dysentary",
      symptoms_vomiting    ~ "Vomiting",
      symptoms_abdo        ~ "Abdominal pain",
      symptoms_nausea      ~ "Nausea",
      symptoms_fever       ~ "Fever",
      symptoms_headache    ~ "Headache",
      symptoms_joint_pain   ~ "Joint pain"
    )
  ) |>
  add_overall() |>
  add_p() |>
  bold_labels() |>
  italicize_labels() |>
    # Modify header:
  modify_header(
    stat_1 = "**Non-case**\n *N* = {n}",
    stat_2 = "**Case**\n *N* = {n}",
    p.value = "**P value**"
    )
tbl_symptoms

# Ex17 -------------------------
tbl_symptoms_case <- linelist |>
  # Select all symptoms columns, i.e., those starting with "symptoms":
  ___(case, ___("symptoms")) |>
  # Remove rows with missing values:
  ___() |>

  # Reshape the dataset from wide to long
  ___(
    -___,
    names_to = "___",
    ___ = "value",
    values_drop_na = TRUE
  ) |>
  # Keep only TRUE `value`s:
  ___(value) |>
  # Count the number of cases with each symptom:
  count(symptoms, case)

tbl_symptoms_case
