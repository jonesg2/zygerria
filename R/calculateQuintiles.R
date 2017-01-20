#' Calculate quintiles
#'
#' Calculate the quintiles of a given column in a dataframe.
#'
#' @param data The dataframe
#' @param col The column on which to do the calculation
#'
#' @return The function returns the original dataframe with an additional column
#' containing a factor variables of the quintile ranges.
#'
#' @author Nathan Eastwood
#'
#' @importFrom stats quantile as.formula
#'
#' @export
calculateQuintiles <- function(data, col) {
  quints <- c(
    min(data[, col], na.rm = TRUE),
    quantile(data[, col], probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE),
    max(data[, col], na.rm = TRUE)
  )
  data$quints <- factor(
    ifelse(
      is.na(data[, col]),
      NA,
      ifelse(
        data[, col] >= quints[1] & data[, col] < quints[2],
        paste0(quints[1], " - ", quints[2] - 0.01),
        ifelse(
          data[, col] >= quints[2] & data[, col] < quints[3],
          paste0(quints[2], " - ", quints[3] - 0.01),
          ifelse(
            data[, col] >= quints[3] & data[, col] < quints[4],
            paste0(quints[3], " - ", quints[4] - 0.01),
            ifelse(
              data[, col] >= quints[4] & data[, col] < quints[5],
              paste0(quints[4], " - ", quints[5] - 0.01),
              paste0(quints[5], " - ", quints[6]))
          )
        )
      )
    ),
    levels = c(
      paste0(quints[1], " - ", quints[2] - 0.01),
      paste0(quints[2], " - ", quints[3] - 0.01),
      paste0(quints[3], " - ", quints[4] - 0.01),
      paste0(quints[4], " - ", quints[5] - 0.01),
      paste0(quints[5], " - ", quints[6])
    )
  )
  data
}
