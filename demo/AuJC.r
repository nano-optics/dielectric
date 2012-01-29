library(dielectric)
library(ggplot2)

data(AuJC)


AuJC$set_span(0.3, 0.8)
raw <- dielectric2plot(AuJC$raw())
silver <- AuJC$predict(n=300, all.knots=TRUE)

d <- dielectric2plot(silver)

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=raw)



