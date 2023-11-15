gt_theme_ecdc <- function(gt_tbl) {
  gt_tbl |>
    gt::fmt_markdown() |>
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen", weight = "bold"),
      locations = gt::cells_title()
    ) |>
    gt::tab_options(
      table_body.border.top.style = "solid",
      table_body.hlines.style = "none",
      table_body.hlines.width = gt::px(0),
      table_body.border.bottom.style = "solid",
    ) |>
    gt::opt_vertical_padding(0.25)
}
