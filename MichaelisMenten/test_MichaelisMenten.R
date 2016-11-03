## testing michaelis-menten functions

setwd('~/Dropbox/Research/R_help/MichaelisMenten')

## load functions
source('MichaelisMenten.R')

## load data
datatop <- read.csv('datatop.csv')
x <- datatop[datatop$Site == 'A' & datatop$SRR > 0, ]

mmsol(x$SRR, x$Avg_sulfate_conc_in_FTR)

vOverS <- x$SRR / x$Avg_sulfate_conc_in_FTR
SoverV <- x$Avg_sulfate_conc_in_FTR / x$SRR
v <- x$SRR
S <- x$Avg_sulfate_conc_in_FTR

plot(vOverS, v, col = as.factor(x$Sulfate_txmt))
plot(v, vOverS, col = as.factor(x$Sulfate_txmt))
plot(S, SoverV, col = as.factor(x$Sulfate_txmt))
plot(S, v)

mmboot(x$SRR, x$Avg_sulfate_conc_in_FTR)
