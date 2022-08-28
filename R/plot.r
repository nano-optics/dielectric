##' Conversion to long format data.frame for plotting
##'
##' Conversion to long format data.frame for plotting
##' @title dielectric2plot
##' @param m data.frame with wavelength and complex epsilon
##' @return long format data.frame
##' @author baptiste Auguie
##' @export
dielectric2plot <- function(m){

  data.frame(wavelength = rep(m$wavelength, 2),
             value = c(Re(m$epsilon), Im(m$epsilon)),
             variable = rep(c("real", "imag"), each=length(m$wavelength)))

}
