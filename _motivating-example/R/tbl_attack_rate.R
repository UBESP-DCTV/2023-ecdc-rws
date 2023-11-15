tbl_attack_rate <- function(db, var) {
  if (var == "illness") {
    return(
      db |>
        tbl_summary(
          include = illness,
          type = illness ~ "continuous",
          statistic = list(illness ~ "{sum}/{N_nonmiss} ({p_attack}%)")
        )
    )
  }

  var_label <- str_replace_all(var, "_", " ") |>
    str_to_sentence()

  db <- db |> convert_factor_missing(var)
  var_levels <- levels(db[[var]])

  db |>
    select(guest_id, illness, all_of(var)) |>
    convert_to_dummy(var) |>
    mutate(
      across(
        -all_of(c("guest_id", "illness")),
        ~if_else(.x == 0L, NA_integer_, if_else(illness, .x, 0L))
      )
    ) |>
    tbl_summary(
      include = -c(guest_id, illness),
      type = list(
        all_dichotomous() ~ "continuous"
      ),
      statistic = list(all_continuous() ~ "{sum}/{N_nonmiss} ({p_attack}%)"),
      missing = "no"
    ) |>
    bstfun::add_variable_grouping(
      {{var_label}} := as.character(var_levels)
    )
}




convert_factor_missing <- function(db, var) {
  db[[var]] <- factor(db[[var]])

  if (any(is.na(db[[var]]))) {
    db[[var]] <- db[[var]] |>
      fct_na_value_to_level(glue("(Missing {var})")) |>
      fct_relevel(glue("(Missing {var})"), after = Inf)
  }
  db
}


p_attack <- function(x) {
  round(
    100*(sum(x, na.rm = TRUE) / sum(!is.na(x)))
  )
}



convert_to_dummy <- function(db, var) {
  db <- db |> convert_factor_missing(var)
  var_levels <- levels(db[[var]])

  db |>
    rowwise() |>
    mutate(
      dummy = 1L
    ) |>
    pivot_wider(
      id_cols = .data[["guest_id"]],
      names_from = .data[[var]],
      values_from = .data[["dummy"]],
      values_fill = 0L,
      names_sort = TRUE
    ) |>
    right_join(db) |>
    relocate(all_of(var_levels), .after = .data[[var]]) |>
    select(-.data[[var]])
}
