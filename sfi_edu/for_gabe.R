library(plyr)
library(countrycode)
library(maps)
library(viridis)

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

write.csv(studentByYear, file = 'sfi_edu/studentsByYear.csv', row.names = FALSE)

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
studentsByCountry$countryName[is.na(studentsByCountry$countryName)] <- 'blank'

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

write.csv(studentsByCountry, file = 'sfi_edu/studentsByCountry.csv', row.names = FALSE)


## students by country and year

studentsByYearCountry <- ddply(dat, 'countryName', function(x) {
    x$year <- factor(as.character(x$year), levels = sort(unique(dat$year)))
    return(rbind(tapply(x$Name, x$year, function(X) length(unique(X)), default = 0)))
})

studentsByYearCountry$countryName[is.na(studentsByYearCountry$countryName)] <- 'blank'

write.csv(studentsByYearCountry, file = 'sfi_edu/studentsByYearCountry.csv', row.names = FALSE)

row.names(studentsByYearCountry) <- studentsByYearCountry$countryName
studentsByYearCountry <- as.matrix(studentsByYearCountry[, -1])
studentsByYearCountry <- studentsByYearCountry[studentsByCountry$countryName, ]

foo <- t(studentsByYearCountry[studentsByCountry$num_students >= 50 & 
                                   studentsByCountry$countryName != 'blank', ])
foo <- log(foo)
foo[!is.finite(foo)] <- NA

pdf('sfi_edu/fig_studentsByYearCountry.pdf', width = 5, height = 20)
par(mar = c(2, 10, 3, 1), cex.axis = 0.9)
image(x = sort(unique(dat$year)), y = 1:ncol(foo), z = foo, col = rev(viridis(50)), 
      axes = FALSE, frame.plot = TRUE, xlab = '', ylab = '')
text(expand.grid(as.integer(rownames(foo)), 1:ncol(foo)), 
     labels = ifelse(is.na(as.vector(foo)), 0, exp(as.vector(foo))), 
     col = ifelse(is.na(as.vector(foo)), 'black', 'white'))
axis(2, at = 1:ncol(foo), labels = colnames(foo), las = 1)
axis(3, las = 3)
dev.off()

