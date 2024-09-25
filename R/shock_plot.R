#' Plot structural shocks for one identified shock.
#'
#' @param narrSign_model A model object coming from narrsign()
#' @param whichShock A string with the name of shock to be plotted
#' @param narr A Boolean whether shocks identified by narrative restrictions
#' should be used. If False only traditional
#' @param prob Argument indicating the quantile.
#'
#' @import ggplot2
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' shock_plot(model, 1, narr = TRUE, prob = 0.8)
#' }
shock_plot <- function(narrSign_model = NULL,
                       whichShock = NULL,
                       narr = FALSE,
                       prob = 0.5) {
  if (narr == TRUE) {
    mod <- narrSign_model$narr
  } else {
    mod <- narrSign_model$trad
  }

  dates <- narrSign_model$dates
  lag <- narrSign_model$lag

  # median shocks
  shocks <- mod$SHOCKS[, , whichShock]
  shocks <- apply(shocks, 2, quantile, probs = c(prob))
  shocks <- data.frame(dates = dates[-c(1:lag), ], shocks = shocks)

  ggplot(shocks, aes(x = dates, y = shocks)) +
    geom_line() +
    ylab(paste("Shock:", narrSign_model$shocknames[whichShock])) +
    xlab("Time") +
    theme_minimal()
}
