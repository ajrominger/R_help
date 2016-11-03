## testing michaelis-menten functions

setwd('~/Dropbox/Research/R_help/MichaelisMenten')

## load functions
source('MichaelisMenten.R')

## load data
datatop <- read.csv('datatop.csv')
x <- datatop[datatop$Site == 'A' & datatop$SRR > 0, ]

mmsol(x$SRR, x$Avg_sulfate_conc_in_FTR)

mmboot(x$SRR, x$Avg_sulfate_conc_in_FTR)

mmttest(datatop$SRR[datatop$Site == 'B' & datatop$SRR > 0], 
        datatop$Avg_sulfate_conc_in_FTR[datatop$Site == 'B' & datatop$SRR > 0],
        datatop$SRR[datatop$Site == 'C' & datatop$SRR > 0],
        datatop$Avg_sulfate_conc_in_FTR[datatop$Site == 'C' & datatop$SRR > 0],
        n = 5000)

mmbootP(datatop$SRR[datatop$Site == 'A' & datatop$SRR > 0], 
        datatop$Avg_sulfate_conc_in_FTR[datatop$Site == 'A' & datatop$SRR > 0],
        datatop$SRR[datatop$Site == 'B' & datatop$SRR > 0],
        datatop$Avg_sulfate_conc_in_FTR[datatop$Site == 'B' & datatop$SRR > 0], 
        n = 5000)
