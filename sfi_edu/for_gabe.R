library(plyr)
library(countrycode)
library(maps)

## load data
dat <- read.csv('sfi_edu/users-2018_04_16.csv', as.is = TRUE)

## extract year from date
dat$year <- as.integer(substr(dat$Registered.on, 1, 4))

## count up students enrolled in each year
studentByYear <- ddply(dat, 'year', function(x) data.frame(num_students = length(unique(x$Name))))

## plot it
pdf('sfi_edu/fig_studentsByYear.pdf', width = 4, height = 4)
par(mar = c(3, 3, 0, 0) + 0.5, mgp = c(2, 0.5, 0), cex.lab = 1.2)
plot(studentByYear, xlab = 'Year', ylab = 'Students enrolled')
dev.off()


## clean up countries
try1 <- countrycode(dat$Country, 'country.name', 'country.name')
try2 <- countrycode(dat$Country, 'iso2c', 'country.name')
try3 <- countrycode(dat$Country, 'iso3c', 'country.name')

dat$countryName <- ifelse(!is.na(try1), try1, ifelse(!is.na(try2), try2, try3))

## fix some by hand
fbh <- read.csv('sfi_edu/fix_by_hand.csv', as.is = TRUE)
dat$countryName[is.na(dat$countryName)] <- fbh$right[match(dat$Country, fbh$wrong)][is.na(dat$countryName)]

## match by state

statetry1 <- state.abb[match(dat$State, state.name)]
statetry2 <- state.abb[match(dat$State, state.abb)]

canada <- read.csv('sfi_edu/canada.csv', as.is = TRUE)
statetry3 <- canada$abb[match(dat$State, canada$name)]
statetry4 <- canada$abb[match(dat$State, canada$abb)]

stateName <- ifelse(!is.na(statetry1), 
                    statetry1, 
                    ifelse(!is.na(statetry2), 
                           statetry2, 
                           ifelse(!is.na(statetry3), 
                                  statetry3, 
                                  statetry4)))

countryByState <- ifelse(!is.na(statetry1), 'United States', ifelse(!is.na(statetry3), 'Canada', NA))
dat$countryName[is.na(dat$countryName)] <- countryByState[is.na(dat$countryName)]


## match by city (US/Canada)

cityName <- paste(dat$City, stateName)
countryByCity <- ifelse(cityName %in% canada.cities$name, 
                        'Canada', 
                        ifelse(cityName %in% us.cities$name, 
                               'United States', 
                               NA))

dat$countryName[is.na(dat$countryName)] <- countryByCity[is.na(dat$countryName)]


## match by city (world)

countryByWorld <- world.cities$country.etc[match(dat$City, world.cities$name)]
countryByWorld[countryByWorld == 'USA'] <- 'Inited States'
countryByWorld[countryByWorld == 'UK'] <- 'United Kingdom'
countryByWorld[countryByWorld == 'Korea South'] <- 'South Korea'

dat$countryName[is.na(dat$countryName)] <- countryByWorld[is.na(dat$countryName)]

## count up students by country

studentsByCountry <- ddply(dat, 'countryName', function(x) data.frame(num_students = length(unique(x$Name))))
studentsByCountry <- studentsByCountry[order(studentsByCountry$num_students), ]

pdf('sfi_edu/fig_studentsByCountry.pdf', width = 12, height = 16)
par(mar = c(0, 10, 0, 1) + 0.1, oma = c(4, 0, 0, 0), mfrow = c(1, 2))
with(studentsByCountry[studentsByCountry$num_students >= 10, ],  
     barplot(num_students, names.arg = countryName, 
             horiz = TRUE, cex.axis = 0.1, xaxt = 'n', las = 1, 
             xlim = range(studentsByCountry$num_students)))
axis(1)
box()

with(studentsByCountry[studentsByCountry$num_students < 10, ],  
     barplot(num_students, names.arg = countryName, 
             horiz = TRUE, cex.axis = 0.1, xaxt = 'n', las = 1, 
             xlim = range(studentsByCountry$num_students)))
axis(1)
box()

mtext('Students enrolled', side = 1, outer = TRUE, line = 3)
dev.off()
