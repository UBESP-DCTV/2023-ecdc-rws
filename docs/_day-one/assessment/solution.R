# ex1 -------------------------


___(tidyverse)
___(here)
___(rio)

# the file to be imported is in the data-raw folder and it is called Copenhagen_clean.xlsx
linelist <- here("data-raw", "Copenhagen_clean.xlsx") |>
  import() |>
  mutate(across(where(is.character), fct))
