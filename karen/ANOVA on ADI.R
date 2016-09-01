## set wd to where analyses will be saved
setwd('/Volumes/Big Island Soundscapes/R - Acoustic Diversity')

## get all ADI file names
files <- list.files('../ADI Results -45 dB freq_step 500')

## read in all data into data.frame called dat
dat <- read.csv(paste('../ADI Results -45 dB freq_step 500', files[1], sep='/'))

for(i in 2:length(files)) {
  dat <- rbind(dat, read.csv(paste('../ADI Results -45 dB freq_step 500', files[i], sep='/')))
}

## sites and ages in order of file name (needs to be in order of file name so it matches the files as we read them in)
sites <- c('73_107', '73_84', 'Es87', 'Es89', 'KO6', 'KO8', 'KY7', 'KY8', 'LH6', 'LH9', 'LL1', 'LL7', 'O1', 'O12', 'Th1', 'Th8')

dat$site <- rep(sites, each=144)
ages <- c(42, 42, 200, 200, 500000, 500000, 175000, 175000, 40000, 40000, 90000, 90000, 7500, 7500, 400, 400)
dat$ages <- rep(ages, each=144)

## extract the hour from the file name
dat$hour <- as.numeric(substring(gsub('.*_|.wav', '', dat$FILENAME), 1, 2))


## break up hours into periods of the day
period <- as.character(cut(dat$hour, breaks=c(0, 5, 10, 15, 20, 23), labels=c('night1', 'morning', 'mid', 'evening', 'night2'), include.lowest=TRUE))

## edit the period vector so there's only one 'night'
period <- gsub('night.*', 'night', period)
dat$period <- period


## simplify 'dat' so it's just needed information
dat <- dat[, c('site', 'ages', 'period', 'LEFT_CHANNEL')]
names(dat)[4] <- 'ADI'

## order sites by age
dat$site <- reorder(dat$site, dat$ages)

## run the ANOVA
mod <- aov(ADI ~ site * period, data=dat)
summary(mod)

## post hoc test
tukey <- TukeyHSD(mod)

## which sites are different
nrow(tukey[[1]][tukey[[1]][, 4] <= 0.05, ])
tukey[[1]][tukey[[1]][,4] < 0.05, ] # which ones are different

## make letters designating site differences (to be added to plot below)
library(multcompView)
tukey.letters <- multcompLetters(tukey[[1]][, 4])$Letters[levels(dat$site)]

## plot by site and period
pdf('ADI boxplot .pdf', width=5, height=5)
par(mfrow=c(4, 1), mar=rep(0.5, 4), oma=c(4, 4, 1, 1)+0.1)
boxplot(ADI~site, data=dat[dat$period=='morning', ], xaxt='n')
mtext(tukey.letters, at=1:length(unique(dat$site))) # add significance letters
boxplot(ADI~site, data=dat[dat$period=='mid', ], xaxt='n')
boxplot(ADI~site, data=dat[dat$period=='evening', ], xaxt='n')
boxplot(ADI~site, data=dat[dat$period=='night', ])

mtext('Site', side=1, outer=TRUE, line=2)
mtext('Acoustic Diversity', side=2, outer=TRUE, line=2)

dev.off()
