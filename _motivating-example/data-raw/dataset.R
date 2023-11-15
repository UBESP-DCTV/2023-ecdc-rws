library(here)
library(rio)

source(here("R/simulate_outbreak_data.R"))

random_wedding <- simulate_outbreak_data()
random_labs <- simulate_clinical_laboratory_data(random_wedding)

export(random_wedding, here("data-raw/random_wedding.csv"))
export(random_labs, here("data-raw/random_labs.csv"))


