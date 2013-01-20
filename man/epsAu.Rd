\name{epsAu}
\alias{epsAu}
\title{epsAu}
\usage{
  epsAu(wavelength, epsilon.infty = 1.54, lambda.p = 177.5,
    mu.p = 14500, A1 = 1.27, phi1 = -pi/4, lambda1 = 470,
    mu1 = 1900, A2 = 1.1, phi2 = -pi/4, lambda2 = 325,
    mu2 = 1060)
}
\arguments{
  \item{wavelength}{wavelength in nm}

  \item{epsilon.infty}{background dielectric constant}

  \item{lambda.p}{plasma wavelength}

  \item{mu.p}{damping constant}

  \item{A1}{A1}

  \item{phi1}{phi1}

  \item{lambda1}{lambda1}

  \item{mu1}{mu1}

  \item{A2}{A2}

  \item{phi2}{phi2}

  \item{lambda2}{lambda2}

  \item{mu2}{mu2}
}
\value{
  data.frame
}
\description{
  permittivity gold
}
\details{
  analytical dielectric function of Au (Drude model +
  interband transitions)
}
\examples{
require(dielectric) ; data(AuJC)
wvl <- seq(300, 900)
gold <- epsAu(wvl)

matplot(gold$wavelength, cbind(Re(gold$epsilon), Im(gold$epsilon)),
t="l", lty=1, xlab = "wavelength / nm", ylab = "Dielectric function")
matpoints(AuJC$wavelength, cbind(Re(AuJC$epsilon), Im(AuJC$epsilon)), pch=1)
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
  Other user_level permittivity: \code{\link{epsAg}}
}

