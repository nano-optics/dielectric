library(ggplot2)
library(dielectric)
# http://www.luxpop.com/Material/
d <- read.table("AuPalik.txt", header = T)
comment(d) <- c(source = "Au, Palik [http://www.luxpop.com/Material/]", 
                          units = "nanometres")

Au <- with(d, new("dielectric", wavelength=wavelength, 
                                 epsilon=epsilon, 
                                 span=range(wavelength), 
                                 comment=as.list(comment(d))))

save(Au, file="AuPalik.rda")

Au$set_span(300, 1000)
raw <- dielectric2plot(Au$raw())
al <- Au$predict(n=300, all.knots=TRUE)
write.table(al, file='AuPalik_interp.txt', row.names = FALSE)

d <- dielectric2plot(al)

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=raw)

