# setwd("~/Dropbox/Analyses/SF_Nesting")
setwd('~/Dropbox/Rscripts')
library(spectralGP)
library(spatstat)
# library(geoR)

grid.res <- 2^9
mu <- 4
sig2 <- 1.5 # S var
phi <- 0.15	# range
kappa <- 1
beta <- 1.5
tau2 <- 0	# nugget var


## mean density
## field size
## rate of foraging decay

## function to simulate locations of nests in field
simulate.nests <- function(la.mean, la.sd, dim=c(100, 300)) {
	## set up grid for Gaussian surface, needs to be square 
	## and of a power of 2 (i.e. 2^x) for spectral method
	# browser()
	gridsize <- rep(max(2^ceiling(log(dim)/log(2))), 2)
	
	## simulate Gaussian surface
	this.gp <- gp(gridsize=gridsize, specdens=matern.specdens, 
	              specdens.param=c(1, 0.5))
	simulate(this.gp)
	
	## create latent density by adding edge effect and turning spatially normal field
	## into a spatially log-normal field
	this.la <- as.im(predict(this.gp), 
	                 W=owin(c(0, max(dim)), c(0, max(dim))))
	this.la <- this.la[owin(c(0, dim[1]), c(0, dim[2]))] # clip to right dimensions
	
	## add edge effect. -0.445 is taken directy from NB GLMM model
	
	this.la$v <- this.la$v - 0.05*dist2edge(this.la)
	
	## exponentiate
	this.la$v <- exp(this.la$v)
	
	## re-scale density to conform to specified mean and sd
	this.la[] <- this.la[] / sum(this.la[drop=TRUE]) * la.mean * prod(dim)
	
	# image(this.la)
	
	## simulate actual nest locations
	these.nests <- rpoispp(this.la)
	pdf('nestPoints.pdf')
	plot(these.nests, main='', pch=16, cex=0.3)
	dev.off()
	return(these.nests)
}

## helper function to get nearest edge distance from im object
dist2edge <- function(im) {
	allcoord <- expand.grid(im$yrow, im$xcol)
	edge1 <- allcoord[, 2] - min(im$xrange)
	edge2 <- max(im$xrange) - allcoord[, 2]
	edge3 <- allcoord[, 1] - min(im$yrange)
	edge4 <- max(im$yrange) - allcoord[, 1]
	
	matrix(mapply(min, edge1, edge2, edge3, edge4), nrow=im$dim[1])
}

## function to calculate expected foraging given nests
expected.forage <- function(nests, disp) {
	pnts <- with(unclass(nests$window), 
	             expand.grid(seq(xrange[1], xrange[2], by=1), seq(yrange[1], yrange[2], by=1)))
	
	## helper function for calculating forage
	this.nest <- function(x, y) {
		d <- sqrt((pnts[, 1] - x)^2 + (pnts[, 2] - y)^2)
		exp(-disp*d)
	}
	
	## loop over nests, adding up their expected foraging
	out <- numeric(nrow(pnts))
	for(i in 1:length(nests$x)) {
		out <- out + this.nest(nests$x[i], nests$y[i])
	}
	
	## make foragin relative
	out <- out/max(out)
	
	with(unclass(nests$window),
	     im(matrix(out, nrow=floor(xrange[2])+1), 
	        xcol=seq(yrange[1], yrange[2], by=1),
	        yrow=seq(xrange[1], xrange[2], by=1)))
}

## plotting function
plot.forage <- function(x, file=NULL, ...) {
	if(is.null(file)) {
		if(is.null(dev.list())) quartz(width=8, height=3)
	} else {
		pdf(width=8, height=3, file=file)
	}
	
	par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 3))
	plot(x, main='', ribsep=0.05, zlim=0:1, ...)
	mtext('Potential relative foraging', side=4, line=1, cex=1.2, outer=TRUE)
	
	if(!is.null(file)) dev.off()
}


## test it
x <- simulate.nests(0.1, 1)
bla <- expected.forage(x, 1)
plot.forage(bla, file='fig_exampleForage.pdf')