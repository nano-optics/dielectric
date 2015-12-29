library(ggplot2)
library(dielectric)

## http://refractiveindex.info/?shelf=main&book=Al&page=Rakic
ld <- readLines("Rakic.txt")
split <- grep("wl", ld)
dn <- read.table("Rakic.txt",head=TRUE, nrows = split[2]-3)
dk <- read.table("Rakic.txt",head=TRUE, skip = split[2]-2)

AlRakic_raw <- merge(dn, dk)

comment(AlRakic_raw) <- c(source = "Aluminum, Rakic [http://refractiveindex.info/?shelf=main&book=Al&page=Rakic]", 
                          units = "nanometres")

AlRakic <- with(AlRakic_raw, new("dielectric", wavelength=wl*1e3, 
                                 epsilon=(n+1i*k)^2, 
                                 span=range(wl*1e3), 
                                 comment=as.list(comment(AlRakic_raw))))

save(AlRakic, file="AlRakic.rda")

AlRakic$set_span(200, 800)
raw <- dielectric2plot(AlRakic$raw())
al <- AlRakic$predict(n=300, all.knots=TRUE)

d <- dielectric2plot(al)

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=raw)

