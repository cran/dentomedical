#' Crosstabulation with Counts and Percentages
#'
#' Creates a cross-tabulation (contingency table) between two categorical variables
#' and returns a publication-ready table as a \code{flextable}. The table displays
#' cell counts with optional percentages (cell-wise, row-wise, or column-wise),
#' and includes row totals (right) and column totals (bottom).
#'
#' @param data A data frame containing the variables to be cross-tabulated.
#' @param row_var A character string specifying the row variable (categorical).
#' @param col_var A character string specifying the column variable (categorical).
#' @param percent Type of percentage to display alongside counts. One of
#'   \code{"none"} (counts only), \code{"cell"} (percentage of total),
#'   \code{"row"} (row percentage), or \code{"col"} (column percentage).
#' @param digits Integer specifying the number of decimal places for percentages.
#'
#' @return A \code{flextable} object containing the formatted cross-tabulation
#'   with counts, optional percentages, and marginal totals.
#'
#' @export
#'
#' @examples
#' data(CO2)
#'
#' sum_crosstab(
#'   data = CO2,
#'   row_var = "Treatment",
#'   col_var = "Type",
#'   percent = "cell",
#'   digits = 2
#' )
sum_crosstab <- function(data, row_var, col_var,
                                percent = c("none", "cell", "row", "col"),
                                digits = 2) {

  percent <- match.arg(percent)

  # Base table
  tab <- table(data[[row_var]], data[[col_var]])

  # Percent calculations
  cell_p <- prop.table(tab) * 100
  row_p  <- prop.table(tab, 1) * 100
  col_p  <- prop.table(tab, 2) * 100

  # Build formatted matrix
  out <- matrix("", nrow = nrow(tab), ncol = ncol(tab),
                dimnames = dimnames(tab))

  for (i in seq_len(nrow(tab))) {
    for (j in seq_len(ncol(tab))) {

      txt <- as.character(tab[i, j])

      if (percent == "cell") {
        txt <- paste0(txt, " (", round(cell_p[i, j], digits), "%)")
      }
      if (percent == "row") {
        txt <- paste0(txt, " (", round(row_p[i, j], digits), "%)")
      }
      if (percent == "col") {
        txt <- paste0(txt, " (", round(col_p[i, j], digits), "%)")
      }

      out[i, j] <- txt
    }
  }

  # Convert to data frame
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  df[[row_var]] <- rownames(df)
  df <- df[, c(row_var, colnames(out))]

  # Add row totals
  df$Total <- rowSums(tab)

  # Add column totals
  total_row <- c("Total", colSums(tab), sum(tab))
  df <- rbind(df, total_row)

  # Flextable formatting
  ft <- flextable(df)
  ft <- autofit(ft)
  ft <- align(ft, align = "center", part = "all")
  ft <- bold(ft, part = "header")
  ft <- valign(ft, valign = "middle", part = "all")

  return(ft)
}
