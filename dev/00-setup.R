pkgs <- c(
  "tidyverse", "ggplot2", "readr", "readxl", "writexl", "haven",  "rio",
  "tidyr", "janitor", "dplyr", "forcats", "glue", "magrittr",
  "lubridate", "stringr", "renv", "here", "gtsummary", "flextable",
  "rmarkdown", "knitr", "quarto", "devtools", "spelling", "rmarkdown",
  "knitr"
)
install.packages(pkgs)

purrr::walk(pkgs, usethis::use_package, type = "Suggests")


spelling::spell_check_package()
spelling::update_wordlist()

renv::status()
renv::snapshot()
