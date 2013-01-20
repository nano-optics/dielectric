\name{epsAg}
\alias{epsAg}
\title{epsAg}
\usage{
  epsAg(wavelength, epsilon.inf = 4, lambda.p = 282,
    mu.p = 17000)
}
\arguments{
  \item{wavelength}{wavelength in nm}

  \item{epsilon.inf}{background dielectric constant}

  \item{lambda.p}{plasma wavelength}

  \item{mu.p}{damping constant}
}
\value{
  data.frame
}
\description{
  permittivity silver
}
\details{
  analytical dielectric function of Silver (Drude model)
}
\examples{
require(dielectric) ; data(AgPalik)
wvl <- seq(300, 900)
silver <- epsAg(wvl)

matplot(silver$wavelength, cbind(Re(silver$epsilon), Im(silver$epsilon)),
t="l", lty=1, xlab = "wavelength / nm", ylab = "Dielectric function")
matpoints(AgPalik$wavelength, cbind(Re(AgPalik$epsilon), Im(AgPalik$epsilon)), pch=1)
}
\author{
  baptiste Auguie
}
\references{
  Principles of surface-enhanced Raman spectroscopy and
  related plasmonic effects Eric C. Le Ru and Pablo G.
  Etchegoin, published by Elsevier, Amsterdam (2009).
}
\seealso{
  Other user_level permittivity: \code{\link{epsAu}}
}

