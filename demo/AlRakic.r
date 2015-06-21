library(ggplot2)
library(dielectric)

data(AlRakic)

AlRakic$set_span(200, 800)
raw <- dielectric2plot(AlRakic$raw())
al <- AlRakic$predict(n=300, all.knots=TRUE)

d <- dielectric2plot(al)

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=raw)

