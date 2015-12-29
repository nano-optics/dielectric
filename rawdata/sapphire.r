require(dielectric)
setwd("~/Documents/github/dielectric/inst/rawdata")
test <- read.table("Sapphire-Malitson-o.txt", head=TRUE)
test <- transform(test, epsilon = complex(real = n,  imag = 0)^2,  
                  wavelength=wl*1e3)
## dput(test[, c("wavelength", "epsilon")])
sapphire_raw <- test[,c("wavelength", "epsilon")]

comment(sapphire_raw) <- c(source = "Sapphire, ordinary index,  http://refractiveindex.info/?shelf=main&book=Al2O3&page=Malitson-o",  units = "nanometres")

sapphire <- with(sapphire_raw, new("dielectric", 
                                       wavelength=wavelength, 
                                       epsilon=epsilon, span=range(wavelength), 
                                       comment=as.list(comment(sapphire_raw))))

save(sapphire, file="sapphire.rda")
