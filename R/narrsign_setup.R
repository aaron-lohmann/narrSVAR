#' Setup narrative sign restrictions
#'
#' This functions takes the user input about narrative sign restrictions and
#' sets up the arrays necessary for evaluation. Two types of restrictions are
#' possible and should be noted carefully. One can either place a restriction
#' on the sign of a specific shock or on the historic contribution. See package
#' documentation and the examples which should be self explanatory.
#'
#' @param shock_names A vector of shock names on which narrative restrictions
#' are placed
#' @param shock_type A vector of type of shocks which corresponds to the length
#' of shock_names
#' @param shock_dates A matrix of dimensions 2 x (length of shock_names) which
#' contains start and end date of narrative restriction
#' @param dates A vector of dates based on the original data set
#' @param relevant_variable An integer or string
#'  the name of variable on which the narrative restriction is placed
#' @param shock_size A string which further specifies the kind of shock
#' @param allshocknames A vector containing all named shocks in the system
#' @param shock_sign Either 1 or -1 depdning on the sign of the shock
#' @param lag A numeric for the amount of lags in the system
#' @param data Data used for estimation. Note: Without the dates!
#'
#' @return A list of arrays which contains the narrative restrictions
#' @export
#'
#' @references AR18.
#'
#' @examples
#' \dontrun{
#' varnames_uhlig <- colnames(uhlig)
#' allshocknames <- c("MP")
#' shock_names <- c("MP", "MP")
#' shock_type <- c("sign", "contribution")
#' shock_dates <- matrix(c("1979-10-01", "1979-10-01", "1979-10-01", "1979-10-01"), 2, 2)
#' dates <- uhlig[, 1]
#' relevant_variable <- c(NA, varnames_uhlig[7])
#' shock_sign <- c(1, 1)
#' shock_size <- c(NA, "strong")
#' lag <- 12
#' data <- uhlig[, -1]
#'
#' uhlig_narr_rest <- narrsign_setup(
#'   allshocknames = allshocknames,
#'   shock_names = shock_names,
#'   shock_type = shock_type,
#'   shock_dates = shock_dates,
#'   dates = dates,
#'   relevant_variable = relevant_variable,
#'   shock_sign = shock_sign,
#'   shock_size = shock_size,
#'   lag = lag,
#'   data = data
#' )
#' }
narrsign_setup <- function(allshocknames = NULL,
                           shock_names = NULL,
                           shock_type = NULL,
                           shock_dates = NULL,
                           dates = NULL,
                           relevant_variable = NULL,
                           shock_sign = NULL,
                           shock_size = NULL,
                           lag = NULL,
                           data = NULL) {
  varnames <- colnames(data)


  Ns <- matrix(0, dim(data)[1], dim(data)[2])
  Nc <- array(0, dim = c(dim(data)[1], dim(data)[1], dim(data)[2], dim(data)[2]))


  for (shock in 1:length(shock_names)) {
    if (shock_type[shock] == "sign") {
      Ns[which(dates == shock_dates[shock, 1]), which(allshocknames == shock_names[shock])] <- as.numeric(shock_sign[shock])
    } else {
      if (shock_size[shock] == "strong") {
        if (shock_sign[shock] == 1) {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- 2
        } else {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- -1
        }
      } else {
        if (sign(NSR[[rest]][[6]] == 1)) {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- 1
        } else {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- -2
        }
      }
    }
  }

  Ns <- Ns[(lag + 1):nrow(Ns), ]
  Nc <- Nc[(lag + 1):dim(Nc)[1], (lag + 1):dim(Nc)[2], , ]

  narrative_restr <- list(narrative_sign = Ns, narrative_contribution = Nc, dates = dates)

  narrative_restr
}
