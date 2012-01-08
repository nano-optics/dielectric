\name{AuJC}
\alias{AuJC}
\docType{data}
\title{ AuJC }
\description{
  Gold dielectric function from Johnson and Christy
}
\usage{data(AuJC)}
\source{
  P. B. Johnson and R. W. Christy. Optical constants of the noble metals. Phys. Rev. B, 6:4370-4379, 1972.
  luxpop.com
}
\examples{
data(AuJC)
str(AuJC$raw())
AuJC$comment
\dontrun{demo(AuJC)}
}
\keyword{datasets}
