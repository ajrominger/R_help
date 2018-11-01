library(maps)
library(countrycode)
library(viridis)

foo <- require(socorro)
if(!foo) {
    foo2 <- require(devtools)
    if(!foo2) install.packages(devtools)
    devtools::install_github('ajrominger/socorro')
    library(socorro)
}

# make country names just one vector
dat <- read.csv('~/Desktop/users_country.csv', as.is = TRUE)
dat <- dat$country


# clean up
dat <- gsub('\\.', '', dat)
dat <- gsub('/.*', '', dat)
dat <- gsub('The', '', dat)
dat <- trimws(dat)
dat[dat == 'NULL'] <- NA
dat[dat %in% c('A1', 'A2', 'XX')] <- NA
dat[dat == '中国'] <- 'China'
dat[dat == 'México'] <- 'Mexico'
dat[dat == 'Brasília'] <- 'Brazil'
dat <- gsub('Brasil', 'Brazil', dat, ignore.case = TRUE)
dat[grep('United State|U.S.', dat, ignore.case = TRUE)] <- 'USA'
dat[grep('United King', dat, ignore.case = TRUE)] <- 'UK'
dat[grep('China', dat, ignore.case = TRUE)] <- 'China'
dat[dat == 'Los Alamos'] <- 'USA'
dat[dat == 'Italia'] <- 'Italy'
dat[dat == 'Milano'] <- 'Italy'
dat[dat == 'Wandsworth'] <- 'UK'
dat[dat == 'England'] <- 'UK'
dat[dat == 'Perú'] <- 'Peru'
dat[dat == 'España'] <- 'Spain'
dat[dat == 'Deutschland'] <- 'Germany'
dat[dat == 'Holland'] <- 'Netherlands'
dat[dat == 'Argentna'] <- 'Argentina'
dat[dat == 'CS'] <- 'Serbia'
dat <- toupper(dat)
dat[dat == 'US'] <- 'USA'

# convert 2 letter codes to full names
dat[nchar(dat) == 2 & !is.na(dat)] <- 
    countrycode(dat[nchar(dat) == 2 & !is.na(dat)], 'iso2c', 'country.name')

# change back stupid UK and USA
dat[grep('United King', dat, ignore.case = TRUE)] <- 'UK'
dat[grep('United State|U\\.S\\.', dat, ignore.case = TRUE)] <- 'USA'

# clean up rest
dat[grep('China|Hong Kong', dat, ignore.case = TRUE)] <- 'China'
dat[dat == 'Czechia'] <- 'Czech Republic'
dat[grep('Palestin', dat, ignore.case = TRUE)] <- 'Palestine'
dat[dat == 'Congo - Kinshasa'] <- 'Democratic Republic of the Congo'
dat[dat == 'Côte d’Ivoire'] <- 'Ivory Coast'
dat[dat == 'Réunion'] <- 'Reunion'
dat[dat == 'Curaçao'] <- 'Curacao'
dat[dat == 'TUNISIE'] <- 'Tunisia'
dat[dat == 'POLSKA'] <- 'Poland'
dat[dat == 'NORGE'] <- 'Norway'
dat[dat == 'SVERIGE'] <- 'Sweden'
dat[dat == 'Bosnia & Herzegovina'] <- 'Bosnia and Herzegovina'
dat <- gsub(' [^[:alnum:]].*', '', dat)

dat <- toupper(dat)

# country names on the map
m <- map('world', plot = FALSE)
countryNames <- toupper(gsub(':.*', '', m$names))


# get counts of students for each country
x <- table(dat)


# match colors to counts
ncol <- 20
pal <- viridis(ncol)
cols <- quantCol(x, pal = pal, trans = 'log')
names(cols) <- names(x)
cols <- cols[countryNames]
cols[is.na(cols)] <- 'gray'



# make the final map
pdf('~/Desktop/ce_users_map.pdf', width = 6, height = 3)
layout(matrix(1:2, nrow = 1), widths = c(6, 1))
par(mar = rep(0, 4), oma = rep(0, 4))
map('world', col = cols, fill = TRUE, lwd = 0.1)


# the legend
par(mar = c(3, 0, 3, 3.25), mgp = c(2, 0.75, 0))
plot(as.integer(x), xlim = c(0, 1), log = 'y', type = 'n', axes = FALSE, 
     xlab = '', ylab = '')

n <- length(unique(cols))
yy <- exp(seq(log(min(x)), log(max(x)), length.out = ncol + 1))
rect(xleft = 0, xright = par('usr')[2], ybottom = yy[-length(yy)], ytop = yy[-1], 
     col = pal, border = NA)
rect(0, yy[1], par('usr')[2], yy[length(yy)])

logAxis(4, expLab = TRUE)
mtext('Numbers of students', side = 4, line = 2)
dev.off()
