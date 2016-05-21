
##' clausius_mossotti
##'
##' lorentzian polarizability of a molecule
##' @title clausius_mossotti
##' @param alpha in nm3
##' @param N in nm-3
##' @return relative dielectric function
##' @export
##' @family user_level utility
##' @author baptiste Auguie
clausius_mossotti <- function(alpha =1, N=1e-1)
  (2*N*alpha + 1) / (1 - N*alpha)

##' epsilon_qd
##'
##' dielectric function representing a quantum dot
##' @title epsilon_qd
##' @param wavelength in nm
##' @param ... passed to polarizability_dye
##' @return dielectric function
##' @export
##' @family user_level utility polarizability
##' @author baptiste Auguie
epsilon_qd <- function(wavelength, ...)
  clausius_mossotti(polarizability_dye(wavelength, ...))

