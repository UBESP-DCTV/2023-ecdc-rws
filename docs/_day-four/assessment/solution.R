library(tidyverse)
library(gtsummary)
library(here)
library(janitor)
library(unheadr)
library(rio)
library(skimr)

linelist_raw <- import(here("data-raw/Copenhagen_raw.xlsx"))
linelist_raw

# read and import the dataset
# use the working instructions used previously
# In the first line, create the path to the data set only
linelist <- here("data-raw/Copenhagen_raw.xlsx") |>
  # import the data
  # don't consider the first row (headers' groups) as column names
  import(col_names = FALSE) |>
  # mash together the four rows composing the headers
  # merge the groups' headers with the proper headers in a single row
  # (tip.: use a function in the `{unheadr}`)
  mash_colnames(                                                  #  <1>
    n_name_rows = 4,  # how many rows has to be mashed?
    keep_names = FALSE, # do we count/consider current colnames?
    sliding_headers = TRUE  # are there grouping headers'name (1st row)
  ) |>
  # remove empty columns (tip.: use a function in the `{janitor}`)
  remove_empty(which = c("cols")) |>
  # clean all the names
  # convert them using standard cammel_case convention
  # (tip.: use a function in the `{janitor}`)
  clean_names() |>
  # fill the `sex` column to have a complete data set (remind that we
  # have mashed the groups'heaaders with the columns'names)
  fill(demo_sex) |>
  # Because of our importing strategy, all columns are pure characters.
  # We can use the `parse_guess` function of the {readr} package to
  # mutate every columns to a suitable proper type across each one.
  mutate(
    across(everything(), \(x) parse_guess(x, guess_integer = TRUE))
  )

linelist




# Ex-1 ------------------------------------------------------------

___(linelist)


# Update linelist:
linelist <- linelist |>
  mutate(
    time_day_onset = dmy(time_day_onset, tz = "UTC"),
    time_datetime_onset = paste(time_day_onset, time_start_hour) |>
      ymd_h(truncated = 2), # <1>
    demo_sex = factor(demo_sex),
    demo_group = factor(demo_group, labels = c("teacher", "student")),
    demo_class = factor(demo_class)
  )

linelist




# Ex-2 ------------------------------------------------------------

linelist |>
  # create a contingency table of `demo_age` and `demo_group`
  ___(demo_age, demo_group)


linelist <- linelist |>
  mutate(
    demo_age = case_when(
      demo_age == 16 & demo_group == "teacher" ~ 61,
      demo_age %in% c(8, 180) & demo_group == "student" ~ 18,
      TRUE ~ demo_age
    )
  )
linelist




# Ex-3 ------------------------------------------------------------

linelist |>
  # create a contingency table of `demo_age` and `demo_group`
  ___(demo_age, demo_group)


# Update linelist
linelist <- linelist |>
  mutate(
    across(
      where(\(x) all(x %in% c(0, 1, NA))),
      as.logical
    ),
    meal_datetime = ymd_h("2006-11-11 18"),
    symptoms_gastro = (
      symptoms_diarrhoea | symptoms_bloody | symptoms_vomiting
    ) |>
      # Replace missing values with FALSE
      replace_na(FALSE)
  ) |>
  rowwise() |>
  mutate(
    # Consider only vars that start with "food" and do not end with "_d"
    ate_anything = c_across(starts_with("food") & !ends_with("_d")) |>
      reduce(`|`) |>
      # Replace missing values with FALSE
      replace_na(FALSE)
  ) |>
  ungroup()

linelist




# Ex-4 ------------------------------------------------------------

tbl_ate_meal <- linelist |>
  # Create a cross table of people who ate something without having the meal
  ___(food_meal, ate_anything)
tbl_ate_meal


# Start with linelist:
linelist <- linelist |>
  # modify the food_meal column for people who ate something but didn't
  # have a meal
  mutate(
    food_meal = if_else(
      !food_meal & ate_anything,
      true = TRUE,
      false = food_meal
    )
  )
linelist




# Ex-5 ------------------------------------------------------------

tbl_ate_meal <- linelist |>
  # Create a cross table of people who ate something without having the meal
  ___(food_meal, ate_anything)
tbl_ate_meal


# Start with linelist:
linelist <- linelist |>
  # Create case definition:
  mutate(
    case = case_when(
      # Define excluded individuals as those with no record of a meal:
      !food_meal | is.na(food_meal) ~ NA,

      # Cases have any gastro symptoms with onset after the meal:
      symptoms_gastro &
        !is.na(time_datetime_onset) &
        (time_datetime_onset >= meal_datetime)  ~ TRUE,

      # All the rest (i.e., `TRUE` evaluates as `TRUE` for all the cases
      # that didn't match a previous condition!) marked as FALSE,
      # i.e., non cases have no gastro symptoms but ate a meal at the party:
      # !symptoms_gastro |
      #   (symptoms_gastro & (time_datetime_onset < meal_datetime))
      TRUE ~ FALSE
    ),
    incubation = if_else(case, time_datetime_onset - meal_datetime, NA),
    case = case & (time_datetime_onset <= (meal_datetime + days(2)))
  )
linelist




# Ex-6 ------------------------------------------------------------

linelist |>
  # Show a cross table of onset date/time and case status:
  ___(time_datetime_onset, case)


linelist <- linelist |>
  # Remove rows where case is NA:
  drop_na(case)
linelist




# Ex-7 ------------------------------------------------------------

linelist |>
  # Show a cross table of cases and sex, with percentages by row.
  # Use 0 decimal places for counts and 2 decimal places for percentages.
  ___(case, demo_sex, ___ = "row", digits = c(0, ___))




# Ex-8 ------------------------------------------------------------

linelist |>
  # Show a cross table of cases and group, with percentages by row.
  # Use 0 decimal places for counts and 2 decimal places for percentages.
  ___(case, demo_group, ___ = "row", digits = c(___, ___))




# Ex-9 ------------------------------------------------------------

linelist |>
  # Show a cross table of cases and class, with percentages by row.
  # Use 0 decimal places for counts and 2 decimal places for percentages.
  ___(case, demo_class, ___ = "row", digits = c(___, ___))




# Ex-10 ------------------------------------------------------------

tbl_symptoms <- linelist |>
  # Create a table of symptoms, stratified by case status.
  # consider percentages by column.
  ___(
    ___ = case,
    include = starts_with("symptoms"),
    percent = "___",
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
  # add a total column.
  ___() |>
  # add p values.
  ___() |>
  # make the labels bold and italic.
  ___() |>
  ___() |>
    # Modify header:
  modify_header(
    stat_1 = "**Non-case**\n *N* = {n}",
    stat_2 = "**Case**\n *N* = {n}",
    p.value = "**P value**"
    )
tbl_symptoms




# Ex-11 -----------------------------------------------------------

linelist |>
  # Calculate attack proportion using the `case` variable and the function `tbl_summary`
  ___(include = ___)




# Ex-12 -----------------------------------------------------------

tbl_attack_case_group <- linelist |>
  # Calculate attack proportion using the `case` variable, by `demo_group`
  # the function `tbl_summary`
  ___(___ = case, ___ = demo_group) |>
  # Add overall attack proportion
  ___()
tbl_attack_case_group




# Ex-13 -----------------------------------------------------------

linelist |>
  # Calculate attack proportion using the `demo_*` variables
  # by `case`. Exsclude `demo_id` and `demo_age`.
  ___(
    ___ = c(starts_with("demo"), -demo_id, -___),
    label = list(
      demo_class ~ "Class",
      demo_group ~ "Group",
      demo_sex ~ "Sex"
    ),
    by = case
  ) |>
  # Add overall attack proportion
  ___() |>
  modify_header(
    stat_1 = "**Non-case**<br> *N* = {n}",
    stat_2 = "**Case**<br> *N* = {n}"
  ) |>
  # Add p-values
  ___() |>
  # Make variable names bold and italics
  ___() |>
  ___() |>
  sort_p()




# Ex-14 -----------------------------------------------------------

# Pipe in the data:
rrtab <- linelist |>
  # Calculate risk ratios selecting the appropriate `{gtsummary}` fun.
  # use the `case` (y) variable and all the `food_*` variables (xs)
  # (excluding the dose ones, i.e., the ones ending with `_d`)
  ___(
    ___ = c(starts_with("food"), -ends_with("_d"), case),
    label = list(
      food_pasta ~ "Pasta",
      food_veal ~ "Veal",
      food_champagne ~ "Champagne",
      food_sauce ~ "Sauce",
      food_shrimps ~ "Shrimps",
      food_beer ~ "Beer",
      food_rocket ~ "Rocket",
      food_bread ~ "Bread",
      food_tuna ~ "Tuna",
      food_red_wine ~ "Red wine",
      food_green ~ "Green",
      food_white_wine ~ "White wine"
    ),

    # Choose the model (generalised linear model)
    method = glm,

    # Use the `case` variable as the outcome:
    y = ___,

    # Choose the model family:
    method.args = list(
      family = binomial(link = "log"),
      na.action = na.exclude
    ),

    # Exponentiate the results:
    exponentiate = ___,

    # Show results for binary variables on a single row:
    show_single_row = c(starts_with("food"), -ends_with("_d"))

  ) |>

  # Sort the table by p-values:
  sort_p()

# Print the results table:
rrtab




# Ex-15 -----------------------------------------------------------

# Pipe in the data:
drtab <- linelist |>
  # Calculate risk ratios and tabulate results
  # Calculate risk ratios through the `tbl_uvregression` function,
  # using the `case` variable and all the `food_*_d` variables
  ___(
    ___ = ends_with("_d"),
    label = list(
      food_pasta_d ~ "Pasta",
      food_veal_d ~ "Veal",
      food_champagne_d ~ "Champagne",
      food_sauce_d ~ "Sauce",
      food_shrimps_d ~ "Shrimps",
      food_beer_d ~ "Beer",
      food_rocket_d ~ "Rocket",
      food_bread_d ~ "Bread",
      food_tuna_d ~ "Tuna",
      food_red_wine_d ~ "Red wine",
      food_green_d ~ "Green",
      food_white_wine_d ~ "White wine"
    ),

    # Choose the model (generalised linear model)
    method = glm,

    # Use the `case` variable as the outcome:
    y = case,

    # Choose the model family:
    method.args = list(
      family = binomial(link = "log"),
      na.action = na.exclude
    ),

    # Exponentiate the results:
    ___ = TRUE

  ) |>

  # Sort the table by p-values:
  sort_p()

# Print the results table:
drtab




# Ex-16 -----------------------------------------------------------

tbl_glm <- glm(
    case ~
      demo_sex + demo_group + demo_age +
      food_pasta_d + food_veal_d + food_champagne_d +
      food_sauce_d + food_shrimps_d + food_beer_d +
      food_rocket_d + food_bread_d + food_tuna_d +
      food_red_wine_d + food_green_d + food_white_wine_d,
    data = linelist,
    family = binomial,
    na.action = na.exclude
  ) |>
  # Use the appropriate function to create a table of results:
  ___(
    label = list(
      food_pasta_d ~ "Pasta",
      food_veal_d ~ "Veal",
      food_champagne_d ~ "Champagne",
      food_sauce_d ~ "Sauce",
      food_shrimps_d ~ "Shrimps",
      food_beer_d ~ "Beer",
      food_rocket_d ~ "Rocket",
      food_bread_d ~ "Bread",
      food_tuna_d ~ "Tuna",
      food_red_wine_d ~ "Red wine",
      food_green_d ~ "Green",
      food_white_wine_d ~ "White wine",
      demo_sex = "Sex",
      demo_group ~ "Group"
    ),
    exponentiate = TRUE,
    show_single_row = c(demo_sex, demo_group)
  ) |>
  # sort by p-values
  sort_p() |>
  # adjust for multiple comparisons
  add_q() |>
  # add number of observations
  add_n()

tbl_glm
