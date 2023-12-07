# ex-1 ------------------------------------------------------------
___(tidyverse)
___(here)
___(rio)


linelist <- here("data-raw/Copenhagen_clean.xlsx") ___
  import() ___
  mutate(across(where(is.character), fct))

skimr::skim(linelist)




# ex-2 ------------------------------------------------------------

incplot <- linelist |>

  # Remove missing values:
  drop_na(incubation) |>

  # Create an empty ggplot frame setting `incubation` as aesthetic:
  ___(___(x = ___)) +

  # Add a histogram of incubation using the appropriate geom_*:
  ___(
    # Set bin widths to 6 hours:
    binwidth = 6
  ) +

  # Adapt scale to better fit data
  scale_x_continuous(breaks = seq(0, 48, 6)) +

  # Set the required labs to the x and y axes:
  ___(
    x = ___,
    y = ___
  )

# Print plot:
incplot



# ex-3 ------------------------------------------------------------

# Fetch data:
epicurve_date <- linelist |>

  # Filter for cases where dayonset is not missing:
  ___(
    ___ == TRUE,
    !is.na(___)
  ) ___

  # Add dayonset to ggplot aesthetic:
  ggplot(aes(x = dayonset)) ___

  # Add the proper `geom_*` for a barplot:
  ___() +

  # Adapt scale to data and adjust axis label angle:
  scale_x_datetime(
    date_breaks = "1 day",
    labels = scales::label_date_short()) +

  # Update x and y axis labels:
  ___(
    ___ = "Date of onset",
    ___ = "Number of cases"
  ) +

  # Remove unnecessary grid lines:
  theme_bw()

# Print epicurve:
epicurve_date


# ex-4 ------------------------------------------------------------

# Fetch data:
epicurve_hour <- linelist |>

  # Filter for cases where onset_datetime is not missing:
  filter(
    ___ == TRUE,
    !___(onset_datetime)
  ) ___

  # Add onset_datetime to ggplot aesthetic:
  ggplot(aes(___ = onset_datetime)) ___

  # Add the proper `geom_*` for a barplot:
  ___() +

  # Adjust x axis scales to a suitable unit:
  scale_x_datetime(
    date_breaks = "6 hours",
    labels = scales::label_date_short()) +

  # Update x and y axis labels:
  ___(
    ___ = "Date and time of onset",
    ___ = "Number of cases"
  ) +


  # Remove unnecessary grid lines:
  theme_bw()

# Print epicurve:
epicurve_hour


# ex-5 ------------------------------------------------------------

# Fetch data:
epicurve_strata <- linelist |>

  # Filter for cases where onset_datetime is not missing:
  filter(
    ___ == TRUE,
    !___(___)
  ) |>

  # Add factor onset_day to ggplot aesthetic:
  ggplot(aes(___ = onset_datetime)) +

  # Add the proper `geom_*` for an "overall" barplot:
  ___() +

  # Superimpose the same `geom_*` for a barplot
  # stratified by sex:
  ___(___(fill = ___), position = "dodge") +

  # Adjust x axis scales to a suitable unit:
  scale_x_datetime(
    date_breaks = "6 hours",
    labels = scales::label_date_short()) +

  # Stratify by group:
  facet_wrap("group", nrow = 2) +

  # Update x and y axis labels:
  ___(
    ___ = "Date and time of onset",
    ___ = "Number of cases",
    ___ = "Sex",
    title = "Epicurve of the outbreak, overall (gray bars) and stratified by sex (coloured bars)",
    subtitle = str_glue(
      "Copenhagen, November 2006, N = {sum(linelist$case)}"
    )
  ) +

  # Add theme:
  theme_bw()

# Print epicurve:
epicurve_strata
