sampling_logical <- function(n, p_true = 0.5) {
  sample(
    c(FALSE, TRUE),
    size = n,
    replace = TRUE,
    prob = c(1 - p_true, p_true)
  )
}


simulate_outbreak_data <- function(n = 127, .seed = 20231003) {
  set.seed(.seed)

  tibble::tibble(
    GuestID = seq_len(n),
    WeddingCohort = TRUE,
    Eligible = sampling_logical(n, 0.97),
    Interviewed = sampling_logical(n, 0.93),
    NotEaten = sampling_logical(n),
    RefusedToParticipate = sampling_logical(n, 0.05) & !Interviewed,
    UnableToLocate = !(Eligible | RefusedToParticipate),
    Age = sample(c(
      sample(15:64, size = floor(n * 0.86), replace = TRUE),
      sample(5:14, size = floor(n * 0.02), replace = TRUE),
      sample(65:90, size = floor(n * 0.05), replace = TRUE),
      rep(NA, n - (floor(n * 0.86) + floor(n * 0.02) + floor(n * 0.05)))
      )),
    Gender = sample(c("Male", "Female"), size = n, replace = TRUE),
    Illness = sampling_logical(n),
    MainMealOnly = sampling_logical(n),
    DateOfIllness = sample(
      seq(
        from = lubridate::ymd_hm("1996-08-21 00:00"),
        to = lubridate::ymd_hm("1996-08-25 23:59"),
        by = "hours"
      ),
      size = n,
      replace = TRUE
    ),
    SeatingArrangements = sample(c(
      sample(
        paste("Table", 1:12),
        size = floor(n * 0.98),
        replace = TRUE,
        prob = c(
          c(3, 3, 5, 2, 7, 4, 4, 4, 2, 3, 8, 5) /
            sum(c(3, 3, 5, 2, 7, 4, 4, 4, 2, 3, 8, 5))
        )
      ),
      rep(NA_character_, n - floor(n * 0.98))
    )) |>
      (\(x) dplyr::if_else(
        MainMealOnly,
        true = x,
        false = NA_character_
      ))()
  )
}


simulate_clinical_laboratory_data <- function(
  base_db = simulate_outbreak_data(),
  .seed = 20231003
) {
  ill_db <- base_db |>
    dplyr::filter(.data[["Illness"]]) |>
    dplyr::select(dplyr::all_of(c("GuestID")))

  n_ill <- nrow(ill_db)

  ill_db |>
    dplyr::mutate(
      Diarrhoea = sampling_logical(n_ill, 1),
      FeelingFaverish = sampling_logical(n_ill, 0.51),
      AchesAndPains = sampling_logical(n_ill, 0.5),
      Nausea = sampling_logical(n_ill, 0.46),
      AbdominalCramps = sampling_logical(n_ill, 0.28),
      Vomiting = sampling_logical(n_ill, 0.27),
      Headaches = sampling_logical(n_ill, 0.16),
      BloodSeenInOnStool = sampling_logical(n_ill, 0.04),
      GPVisit = sampling_logical(n_ill, 0.67),
      Hospitalization = sampling_logical(n_ill, 0.12),
      StoolSamplesObtained = sampling_logical(n_ill, 0.81),
      StoolSamplePlusVeForSalmonellaTyphimurium =
        sampling_logical(n_ill, 39/57) & StoolSamplesObtained,
      TimeInHospital = sample(6:312, size = n_ill, replace = TRUE),
      DurationOfIllness = sample(2:312, size = n_ill, replace = TRUE),
      IncubationPeriod = sample(5:72, size = n_ill, replace = TRUE)
    )
}

