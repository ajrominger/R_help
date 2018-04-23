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


## how did they learn about it


dat$whereLearn <- tolower(dat$Where.did.you.hear.about.Complexity.Explorer.)

dat$whereLearn <- gsub('^a |^my |^an |^on a |^in a', '', dat$whereLearn)
dat$whereLearn <- trimws(dat$whereLearn, which = 'both')
dat$whereLearn[grep('googl|internet|search|online|browsing|surfing|stummble|stumble|study of |self study|studying|self-interest|on a site|looking|looked|link|just found it', dat$whereLearn)] <- 'web search'
dat$whereLearn[grep('^web$|^net$', dat$whereLearn)] <- 'web search'
dat$whereLearn[grep('^website$', dat$whereLearn)] <- 'web search'
dat$whereLearn[grep('class-central|class central|classcentral|course central', dat$whereLearn)] <- 'class-central.com'
dat$whereLearn[grep('friend|collegue|supervisor|colleague|someone|people|peer', dat$whereLearn)] <- 'friend or colleague'
dat$whereLearn[grep('mooc', dat$whereLearn)] <- 'mooc-list.com'
dat$whereLearn[grep('economist', dat$whereLearn)] <- 'the economist'
dat$whereLearn[grep('teacher|professor|lecturer|classroom|university|instructor|college|advisor|staff|tutor|lecture|school', dat$whereLearn)] <- 'teacher or professor'
dat$whereLearn[grep('^class$', dat$whereLearn)] <- 'teacher or professor'
dat$whereLearn[grep('learning how to learn', dat$whereLearn)] <- 'learning how to learn newsletter'
dat$whereLearn[grep('^book|some books', dat$whereLearn)] <- 'book'
dat$whereLearn[grep('youtube|you tube', dat$whereLearn)] <- 'youtube'
dat$whereLearn[grep('zhihu|知乎', dat$whereLearn)] <- 'zhihu.com'
dat$whereLearn[grep('netlogo|net logo', dat$whereLearn)] <- 'netlogo website'
dat$whereLearn[grep('melanie mitchell', dat$whereLearn)] <- 'melanie mitchell'
dat$whereLearn[grep('newscientist', dat$whereLearn)] <- 'new scientist'
dat$whereLearn[grep('radiolab|radio lab', dat$whereLearn)] <- 'radiolab'
dat$whereLearn[grep('amazon', dat$whereLearn)] <- 'amazon'
dat$whereLearn[grep('federico ii', dat$whereLearn)] <- 'university of naples federico ii'
dat$whereLearn[grep('wiki', dat$whereLearn)] <- 'wikipedia'
dat$whereLearn[grep('medium', dat$whereLearn)] <- 'medium'
dat$whereLearn[grep('coursera', dat$whereLearn)] <- 'coursera'
dat$whereLearn[grep('hindustan', dat$whereLearn)] <- 'hindustan times'
dat$whereLearn[grep('openculture|open culture', dat$whereLearn)] <- 'openculture.com'
dat$whereLearn[grep('news paper', dat$whereLearn)] <- 'newspaper'
dat$whereLearn[grep('quora', dat$whereLearn)] <- 'quora'
dat$whereLearn[grep('article|text citations|scientific journal|scientific literature|scholarly work|references in journals|referenced in emerald|paper', dat$whereLearn)] <- 'scientific article'
dat$whereLearn[grep('scientific am.*|scientific an.*|sci.*am', dat$whereLearn)] <- 'scientific american'
dat$whereLearn[grep('^focus$', dat$whereLearn)] <- 'focus magazine'
dat$whereLearn[grep("don't", dat$whereLearn)] <- 'blank'
dat$whereLearn[grep('maybe|not sure|none of the above|n/a', dat$whereLearn)] <- 'blank'
dat$whereLearn[grep('summercourse|summer school', dat$whereLearn)] <- 'csss'
dat$whereLearn[grep('sanida', dat$whereLearn)] <- 'sanida'
dat$whereLearn[grep('sam.*harris', dat$whereLearn)] <- 'sam harris'
dat$whereLearn[grep('reddit', dat$whereLearn)] <- 'reddit'
dat$whereLearn[grep('melanie', dat$whereLearn)] <- 'melanie mitchell'
dat$whereLearn[grep('npr|public radio', dat$whereLearn)] <- 'npr'
dat$whereLearn[grep('psu', dat$whereLearn)] <- 'psu'
dat$whereLearn[grep('omsi', dat$whereLearn)] <- 'omsi'
dat$whereLearn[grep('pdx', dat$whereLearn)] <- 'pdx.edu'
dat$whereLearn[grep('nautilus|nautil.us', dat$whereLearn)] <- 'nautilus'
dat$whereLearn[grep('edx', dat$whereLearn)] <- 'edx'
dat$whereLearn[grep('^dad$|^son|grandson|daughter|family|father|brother|sister', dat$whereLearn)] <- 'family'
dat$whereLearn[grep('habrahabr.ru', dat$whereLearn)] <- 'habrahabr.ru'
dat$whereLearn[grepl('email', dat$whereLearn) & 
                   grepl('sfi|santa fe|complexity explorer|csss', dat$whereLearn)] <- 'email from sfi edu'
dat$whereLearn[grep('learn.*learn', dat$whereLearn)] <- 'coursera'
dat$whereLearn[dat$whereLearn == ''] <- 'blank'


## sort and organize

foo <- sort(table(dat$whereLearn), TRUE)
dat$whereLearn <- factor(dat$whereLearn, levels = names(foo))
n <- sum(foo >= 5)

whereLearn <- ddply(dat, 'countryName', function(x) {
    o <- tapply(x$Name, x$whereLearn, function(X) length(unique(X)), default = 0)
    return(rbind(o[1:n]))
})

whereLearn$countryName[is.na(whereLearn$countryName)] <- 'blank'

write.csv(whereLearn, file = 'sfi_edu/whereLearn.csv', row.names = FALSE)

row.names(whereLearn) <- whereLearn$countryName
whereLearn <- as.matrix(whereLearn[, -1])
whereLearn <- whereLearn[studentsByCountry$countryName, ]

foo <- t(log(whereLearn[nrow(whereLearn) - (99:0), ]))

pdf('sfi_edu/fig_whereLearn.pdf', width = 4, height = 8)
par(mar = c(1, 3, 3, 1), cex.axis = 0.9)
image(1:n, 1:ncol(foo), foo, col = rev(viridis(50)), 
      axes = FALSE, frame.plot = TRUE, xlab = '', ylab = '')
mtext('Top 100 countries sorted most enrolled to least', side = 2, line = 1)
mtext('Top 50 ways to hear about complexity explorer\nsorted most to least', side = 3, line = 1)
dev.off()
