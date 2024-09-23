#' Define the elasticity bound restrictions.
#'
#' ebr_setup is used to define the elasticity restrictions to be applied. Note,
#' that these elasticities are computed as fractions.
#'
#' @param shock_names A vector of shock names on which elasticity bounds are
#' placed. Note, each shock can be mentioned multiple times.
#' @param num_variable A vector of variable indicators which are in the numerator.
#' Must have the same length as shock_names. Order of reference is in order of shock_names.
#' @param denom_variable A vector of variable indicators which are in the denominator
#' Must have the same length as shock_names. Order of reference is in order of shock_names.
#' @param horizon A vector of horizons for which elasticity bound restrictions
#' should hold. This should have the same length as shock_names.
#' @param bounds A vector of numerics which specify the elasticity bounds. This
#' should have the same length as shock_names.
#'
#' @return A list which contains the elasticity bound restrictions.
#'
#' @export
#'
#' @examples
#'
#' data(kilian_2009)
#' varnames_kilian_2009 <- colnames(kilian_2009)
#'
#'
#'
#' ebr_kilian_2009_shocknames <- c("Aggregate Demand", "Oil specific Demand")
#' ebr_kilian_2009_first <- c(varnames_kilian_2009[2], varnames_kilian_2009[2])
#' ebr_kilian_2009_second <- c(varnames_kilian_2009[3], varnames_kilian_2009[3])
#' ebr_kilian_2009_horizon <- c(1, 1)
#' ebr_kilian_2009_maxbounds <- c(0.0258, 0.0258)
#'
#'
#' ebr_kilian_2009 <- ebr_setup(
#'   shock_names = ebr_kilian_2009_shocknames,
#'   num_variable = ebr_kilian_2009_first,
#'   denom_variable = ebr_kilian_2009_second,
#'   horizon = ebr_kilian_2009_horizon,
#'   bounds = ebr_kilian_2009_maxbounds
#' )
#'
#' ebr_kilian_2009
#'
ebr_setup <- function(shock_names = NULL,
                      num_variable = NULL,
                      denom_variable = NULL,
                      horizon = NULL,
                      bounds = NULL) {
  # Check if all inputs are provided and of the same length
  if (is.null(shock_names) || is.null(num_variable) || is.null(denom_variable) ||
      is.null(horizon) || is.null(bounds)) {
    stop("All input vectors must be provided.")
  }

  if (!all(length(shock_names) == length(num_variable),
           length(num_variable) == length(denom_variable),
           length(denom_variable) == length(horizon),
           length(horizon) == length(bounds))) {
    stop("All input vectors must have the same length.")
  }

  # Create a list for output
  list(
    shock_names,
    num_variable,
    denom_variable,
    horizon,
    bounds
  )
}
