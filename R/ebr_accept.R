#' Check whether elasticity bounds are satisfied
#'
#' ebr_accept takes values for IRFs which are based on current draws, computes
#' elasticity and assesses whether the elasticity bound restriction is
#' satisified. Note that elasticities are computed here as fractions. This
#' approach is likely only appropriate for a limited amount of application.
#'
#' @param ebr Object of ebr_setup.
#' @param impulses Computed impulses based on parameter draws.
#' @param Q A rotation matrix.
#' @param allshocknames Vector of all shocknames which are identified in the
#' the system.
#' @param data A data frame or tibble containg the data
#'
#' @return A Boolean whether elasticity bounds are fulfilled
#'
#' @references Diaz Ramirez (2018)

ebr_accept <- function(ebr = NULL,
                       impulses = NULL,
                       Q = NULL,
                       allshocknames,
                       data = data) {
  # Exit function if no elasticity bound is to be considered
  if (is.null(ebr)) {
    check <- 1
    return(check)
  }

  shock_names <- ebr[[1]]
  first_variable <- ebr[[2]]
  second_variable <- ebr[[3]]
  horizon <- ebr[[4]]
  bounds <- ebr[[5]]


  # Define elasticity matrix which stores which of the elasticity bounds
  # are satisfieed
  E <- matrix(NA, length(shock_names) , 1)

  for (b in 1:length(shock_names)) {
    # Pick the column which corresponds to the shock in question
    q <- Q[, which(allshocknames == shock_names[b]), drop = FALSE]

    # compute impulse response
    ik_1 <- impulses[horizon[b], , ] %*% q
    # extract value for the first relevant variable
    ik_1 <- ik_1[first_variable[b], ]

    # compute impulse response
    ik_2 <- impulses[horizon[b], , ] %*% q
    # extract value for the second relevant variable
    ik_2 <- ik_2[second_variable[b], ]

    # compute elasticity as share of response of first variable divided by the
    # response of the second variable
    elasticity <- ik_1 / ik_2

    # Check whether the computed elasticity falls into the respective bound and
    # save results as a boolean value
    E[b, ] <- elasticity < bounds[b]
  }
  # Check whether all of the elasticities fall into their respective bounds
  check <- prod(E)

  check
}
