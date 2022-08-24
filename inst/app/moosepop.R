#' Title
#'
#' @return
#' @export
#'
#' @examples
.First <- function(){
options(prompt="Rweb:> ")
}

# --------------------------------------------------------------------------
#  ESTIMATION GASAWAY MOOSEPOP CODE
# --------------------------------------------------------------------------

#' Title
#'
#' @param column.ana
#' @param strat
#' @param data
#' @param sampled
#' @param area
#' @param column.pred
#'
#' @return
#' @export
#'
#' @examples
moosepop<-
function(column.ana,strat,data,sampled,area,column.pred)
{
	data[,strat] <- as.factor(data[,strat])
	data.s <- data[!is.na(data[, sampled]) & data[, sampled] == 1, ]
	cds <- as.integer(data.s[,strat])
	cdu <- as.integer(data[,strat])
	lvs <- levels(data.s[,strat])
	nlvs <- max(cds)
	if(sum(lvs == "") > 0 | sum(is.na(lvs)) > 0)
		return(list(errstate = 1,
		errmessage = "Stratification has missing values",
		errextra = "")
	)
	if(length(unique(cds)) != length(unique(cdu)))
		return(list(errstate = 1,
		errmessage = "Some strata have not been sampled",
		errextra = data.frame(sampled = unique(as.character(data.s[,strat])),all = levels(data[,strat])))
	)
	means <- matrix(NA,nrow=nlvs,ncol=1)
	vars <- matrix(0,nrow=nlvs,ncol=1)
	n <- matrix(NA,nrow=nlvs,ncol=1)
	N <- matrix(NA,nrow=nlvs,ncol=1)
	areas <- matrix(NA,nrow=nlvs,ncol=1)
	areato <- matrix(NA,nrow=nlvs,ncol=1)
	counted <- matrix(NA,nrow=nlvs,ncol=1)
	i <- 1
	for (i in 1:nlvs) {
		ind.cds <- cds==i & data.s[,column.pred]==1
		ind.cdu <- cdu==i & data[,column.pred]==1
		counted[i] <- sum(data.s[ind.cds,column.ana])
		areas[i] <- sum(data.s[ind.cds,area])
		means[i] <- counted[i]/areas[i]
		N[i] <- length(data[ind.cdu,column.ana])
		n[i] <- length(data.s[ind.cds,column.ana])
		if(n[i] == 1 & N[i] > 1)
			return(list(errstate = 1,
			errmessage = "Cannot compute variance with only 1 sample in a stratum",
			errextra = data.frame(stratum = as.character(lvs[i]), n = n[i], N = N[i]))
		)
		vars[i] <- sum(data.s[ind.cds,column.ana]^2) -
			2*means[i]*sum(data.s[ind.cds,column.ana]*data.s[ind.cds,area]) +
			means[i]^2*sum(data.s[ind.cds,area]^2)
		if(n[i] > 1) vars[i] <- vars[i]/(n[i]-1)
		areato[i] <- sum(data[ind.cdu,area])
	}
	y.est.strat <- means*areato
	y.est <- sum(y.est.strat)
	y.est.strat.df <- data.frame(stratum=levels(data[,strat]),Estimate=y.est.strat)
	y.var.strat <- n*vars/areas^2*(1-n/N)*areato^2
	y.se <- sqrt( sum(y.var.strat) )
	y.var.strat.df <- data.frame(stratum=levels(data[,strat]),Variance=y.var.strat)
	n.strat.df <- data.frame(Stratum=levels(data[,strat]),n=n)
	n.strat.df <- rbind(n.strat.df,
		data.frame(Stratum="TOTAL", n = sum(n.strat.df[,2])))
	N.strat.df <- data.frame(Stratum=levels(data[,strat]),N=N)
	N.strat.df <- rbind(N.strat.df,
		data.frame(Stratum="TOTAL", N = sum(N.strat.df[,2])))
	mean.strat.df <- data.frame(Stratum=levels(data[,strat]),Density=means)
	areas.strat.df <- data.frame(Stratum=levels(data[,strat]),Area=areas)
	areas.strat.df <- rbind(areas.strat.df,
		data.frame(Stratum="TOTAL", Area = sum(areas.strat.df[,2])))
	areato.strat.df <- data.frame(Stratum=levels(data[,strat]),Area=areato)
	areato.strat.df <- rbind(areato.strat.df,
		data.frame(Stratum="TOTAL", Area = sum(areato.strat.df[,2])))
	counted.strat.df <- data.frame(Stratum=levels(data[,strat]),Counted=counted)
	counted.strat.df <- rbind(counted.strat.df,
		data.frame(Stratum="TOTAL", Counted = sum(counted.strat.df[,2])))
	v0 <- sum(y.var.strat)^2/sum(y.var.strat^2/(n-1), na.rm = TRUE)
	# create output
	ci80 <- c(y.est - y.se * qt(0.9,v0), y.est + y.se *
		qt(0.9,v0))
	cipm80 <- (y.se * qt(0.9,v0))/y.est
	ci90 <- c(y.est - y.se * qt(0.95,v0), y.est + y.se *
		qt(0.95,v0))
	cipm90 <- (y.se * qt(0.95,v0))/y.est
	ci95 <- c(y.est - y.se * qt(0.975,v0), y.est + y.se *
		qt(0.975,v0))
	cipm95 <- (y.se * qt(0.975,v0))/y.est
	outpt <- list(
		analysis.column = as.character(column.ana),
		total.estimate = as.numeric(y.est),
		total.standard.error = as.numeric(y.se),
		mean.density = mean.strat.df,
		stratum.estimates = y.est.strat.df,
		stratum.variances = y.var.strat.df,
		degrees.of.freedom = as.numeric(v0),
		confidence.interval.80.percent = ci80,
		conf.int.80.percent.proportion.of.mean = as.numeric(cipm80),
		confidence.interval.90.percent = ci90,
		conf.int.90.percent.proportion.of.mean = as.numeric(cipm90),
		confidence.interval.95.percent = ci95,
		conf.int.95.percent.proportion.of.mean = as.numeric(cipm95),
		sample.sizes = data.frame(n.strat.df, row.names = NULL),
		total.samples = data.frame(N.strat.df, row.names = NULL),
		moose.counted = data.frame(counted.strat.df, row.names = NULL),
		sampled.area = data.frame(areas.strat.df, row.names = NULL),
		total.area = data.frame(areato.strat.df, row.names = NULL)
		)
	outpt
}


# --------------------------------------------------------------------------
#  ESTIMATION GEO MOOSEPOP CODE
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
#         Latitude,Longitude to arbitrary UTM
#---------------------------------------------------------------------------

#------------------- Latitude,Longitude to arbitrary UTM

#' This function converts from Lat-Lon decimal degrees to the Universal
# Transverse Mercator Coordinates and returns the new coordinates in a
# 2-column matrix with x- and y- as columns.  In this program, the
# coordinates are calculated from a user supplied central meridian.
# Coordinates are returned in kilometers from the western-most
# longitude and the southern-most latitude observed in the data set.
#'
#' @param cm
#' @param lat
#' @param lon
#'
#' @return
#' @export
#'
#' @examples
LL.to.ARBUTM<-
function(cm,lat,lon)

{
# initialize some variables
	e2 <- 0.00676865799729
	a <- 6378206.4
	ep2 <- e2 / (1-e2)
	drc <- pi / 180
	sc <- 0.9996
	fe <- 500000
	ftm <- 0.30480371
#calculate some frequently used values
	lar <- lat * drc
	ls <- sin(lar)
	ls2 <- ls^2
	els2 <- ep2 * ls2
	lc <- cos(lar)
	lc2 <- lc^2
	lc3 <- lc^3
	lc5 <- lc^5
	elc2 <- ep2 * lc2
	lt2 <- tan(lar)^2
	lt4 <- lt2^2
# do the transformation
	v <- a/sqrt(1 - e2*ls2)
	p <- drc*(cm - lon)
	temp <- 5104.57388 - (lc2*(21.73607 - 0.11422*lc2))
	r1 <- 6367399.689*(lar - ls*lc*0.000001*temp)
	r2 <- (v*ls*lc*p^2)/2
	temp <- 5 - lt2 + 9*elc2 + (2*elc2)^2
	r3 <- (v*ls*lc3*p^4*temp)/24
	r4 <- v*lc*p
	temp <- 1 - lt2 + elc2
	r5 <- (v*lc3*p^3*temp)/6
	temp <- 61 - 58*lt2 + lt4 + 270*elc2 - 330*els2
	ra6 <- (v*ls*lc5*p^6*temp)/720
	temp <- 5 - 18*lt2 + lt4 + 14*elc2 - 58*els2
	rb5 <- (v*lc5*p^5*temp)/120
	northing <- sc*(r1 + r2 + r3 + ra6)
	easting <- -sc*(r4 + r5 + rb5)
	y<-(northing-min(northing))/1000
	x<-(easting-min(easting))/1000
	cbind(x,y)
}



# --------------------------------------------------------------------------
#         GENERALIZED INVERSE OF A MATRIX
#---------------------------------------------------------------------------

#------------------- GENERALIZED INVERSE OF A MATRIX

#' Title
#'
#' @param X
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
mginv <- function(X, tol = sqrt(.Machine$double.eps)) {
	dnx <- dimnames(X)
	if(is.null(dnx)) dnx <- vector("list", 2)
	s <- svd(X)
	nz <- s$d > tol * s$d[1]
	structure(
		if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
		dimnames = dnx[2:1])
}


# --------------------------------------------------------------------------
#         EMPIRICAL SEMIVARIOGRAM AND SEMICROSSVARIOGRAM FUNCTIONS
#---------------------------------------------------------------------------


#------------------- EMPIRICAL SEMIVARIOGRAM

#' Title
#'
#' @param data
#' @param x
#' @param y
#' @param var
#' @param nlag
#' @param directions
#' @param tolerance
#' @param inc
#' @param maxlag
#' @param nlagcutoff
#'
#' @return
#' @export
#'
#' @examples
empirical.semivariogram<-
function(data, x, y, var,
	nlag = 20, directions = c(0,45,90,135),
	tolerance = 22.5, inc = 0, maxlag = 1e32, nlagcutoff = 1)
# EMPIRICAL SEMIVARIOGRAM FUNCTION
# var1 is a matrix or data frame with x-coord in the first column
#                                     y-coord in the second column
#                                     z (response) in the third column
{
	n1 <- length(data[,1])
   # distance matrix among locations
	distance <- sqrt( ( matrix(data[,x],nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(data[,x],nrow=1,ncol=n1) )^2 +
		( matrix(data[,y],nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(data[,y],nrow=1,ncol=n1) )^2 )
	difx <- -(matrix(data[,y],nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(data[,y],nrow=1,ncol=n1))
	signind <- -(matrix(data[,x],nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(data[,x],nrow=1,ncol=n1)) < 0
	distance <- distance*1.0000000001
	theta.deg<-acos(difx/distance)*180/pi
   # matrix of degrees clockwise from north between locations
	theta.deg[signind] <- 360-theta.deg[signind]
	diff2 <- ( matrix(data[,var],nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(data[,var],nrow=1,ncol=n1) )^2
# convert to vectors
	distance <- matrix(distance, ncol = 1)
	theta.deg <- matrix(theta.deg, ncol = 1)
	diff2 <- matrix(diff2, ncol = 1)
# trim off values greater than maxlag
	indmax <- distance <= maxlag
	distance <- distance[indmax,]
	theta.deg <- theta.deg[indmax,]
	diff2 <- diff2[indmax,]

	maxd<-max(distance)
	if( inc <= 0) inc <- maxd/nlag
	ind <- distance==0
	ndir <- length(directions)
	store.results <- matrix(data = NA, ncol = 6,
		dimnames = list(NULL, c("distance", "gamma", "np", "azimuth", "hx", "hy")))
	for (j in 1:ndir) {
		for ( i in 1:nlag){
			if( (directions[j]-tolerance)<0 && (directions[j]+tolerance)>0 )
				ind1 <- theta.deg >= 360+directions[j]-tolerance |
					theta.deg < directions[j]+tolerance
			else if( (directions[j]+tolerance)>360 && (directions[j]-tolerance)<360 )
				ind1 <- theta.deg < directions[j]+tolerance-360 |
					theta.deg >= directions[j]-tolerance
			else
				ind1 <- theta.deg >= directions[j]-tolerance &
					theta.deg < directions[j]+tolerance
			ind<-distance>(i-1)*inc & distance<=i*inc &
				!is.na(theta.deg) & ind1
			nclass <- sum(ind)
			cv <- mean(diff2[ind])
			mean.dis <- mean(distance[ind])
			if(nclass > 0) store.results<-rbind(store.results,
				c(mean.dis,cv,nclass,directions[j],0,0))
		}
	}
	store.results[,"hx"]<-store.results[,"distance"]*sin(store.results[,"azimuth"]*pi/180)
	store.results[,"hy"]<-store.results[,"distance"]*cos(store.results[,"azimuth"]*pi/180)
	store.results[,"gamma"]<-store.results[,"gamma"]/2
	ind <- store.results[,"np"] >= nlagcutoff
	store.results <- store.results[ind,]
	ind <- !is.na(store.results[,"hx"])
	store.results <- store.results[ind,]
	as.data.frame(store.results)
}


#----------------------------------------------------------------------------------------
#            FUNCTIONS FOR FITTING THE SEMIVARIOGRAM MODEL TO VARIABLE 1
#----------------------------------------------------------------------------------------

#----------- EXPONENTIAL VARIOGRAM MODEL

#' Title
#'
#' @param h
#' @param nugget
#' @param parsil
#' @param range
#'
#' @return
#' @export
#'
#' @examples
exponential.variogram.model<-
function(h, nugget = 0, parsil = 1, range = 1)
{
	d <- sqrt(h[,1]^2 + h[,2]^2)
	ind <- d == 0
	v <- nugget + parsil*(1-exp(-d/range))
	v[ind] <- 0
	v
}


#----------- DATA COVARIANCE MATRIX BASED ON EXPONENTIAL VARIOGRAM MODEL

#' Title
#'
#' @param vcmatdata
#' @param x
#' @param y
#' @param nugget
#' @param parsil
#' @param range
#'
#' @return
#' @export
#'
#' @examples
exp.vc.matrix <- function(vcmatdata, x = "x", y = "y",
	nugget = nugget, parsil = parsil, range = range)
{
	n <- length(vcmatdata[,1])
	distance <- matrix(0, n, n)
	distance[lower.tri(distance)] <- dist(as.matrix(vcmatdata[,c(x,y)]))
	distance <- distance + t(distance)
	distance <- parsil*exp(-distance/range) + diag(nugget, nrow = n, ncol = n)
	distance
}


#------------ REML EQUATION TO MINIMIZE

#' Title
#'
#' @param theta
#' @param m2LLdata
#' @param X
#'
#' @return
#' @export
#'
#' @examples
m2LL <- function(theta, m2LLdata, X)
{
	nugget <- theta[1]
	parsil <- theta[2]
	range <- theta[3]
	z <- m2LLdata[,3]
	if(nugget <= 0 || parsil <= 0 || range <= 0)
		1e32
	else {
		n <- length(z)
		p <- length(X[1,])
		V <- exp.vc.matrix(vcmatdata = m2LLdata,
			nugget = nugget, parsil = parsil, range = range)
		Vi <- solve(V)
		b.hat <- mginv(t(X) %*% Vi %*% X) %*% t(X) %*% Vi %*% z
		f1 <- sum(log(svd(V)$d))
		f2 <- t(z - X %*% b.hat) %*% Vi %*% (z - X %*% b.hat)
		f3 <- sum(log(svd(t(X) %*% Vi %*% X)$d))
		f1 + f2 + f3 + (n - p) *log(2 * pi)
	}
}


#
# ---- GRAPH OF EMPIRICAL AND FITTED SEMIVARIOGRAM

#' Title
#'
#' @param emp.var
#' @param nugget
#' @param parsil
#' @param range
#'
#' @return
#' @export
#'
#' @examples
graph.variogram <-
function(emp.var, nugget, parsil, range)
{
	e.var <- cbind(emp.var, rep("empirical", times = nrow(
		emp.var)))
	f.var <- emp.var
	fitted.mod<-exponential.variogram.model(emp.var[,c("hx","hy")],
		nugget = nugget, parsil = parsil, range = range)
	f.var[, "gamma"] <- fitted.mod
	f.var <- cbind(f.var, rep("fitted", times = nrow(
		emp.var)))
	ef.var <- rbind(e.var, f.var)
	ef.var[,"azimuth"]<-as.factor(ef.var[,"azimuth"])
	trellis.device(graphsheet)
	strip.background <- trellis.par.get(
		"strip.background")
	strip.shingle <- trellis.par.get("strip.shingle")
	strip.background$col <- 0
	strip.shingle$col <- 0
	trellis.par.set("strip.background", strip.background)
	trellis.par.set("strip.shingle", strip.shingle)
	xyplot(ef.var[, "gamma"] ~ ef.var[, "distance"] | ef.var[, "azimuth"],
		groups = ef.var[, 7], panel = panel.superpose,
		type = c("p", "l"), col = c(1, 1), xlab =
		"distance", ylab = "semivariogram", as.table=T)
}




#----------------------------------------------------------------------------------------
#            BUILD MATRICES
#----------------------------------------------------------------------------------------


#
# ------- BUILDS THE SAMPLE VARIANCE-COVARIANCE MATRIX
#' Title
#'
#' @param data
#' @param nugget1
#' @param parsil1
#' @param range1
#' @param nugget2
#' @param parsil2
#' @param range2
#' @param sampled
#' @param strat
#'
#' @return
#' @export
#'
#' @examples
SS.mat <-
function(data, nugget1, parsil1, range1,
	nugget2, parsil2, range2, sampled, strat)
{
	sampled.ind <- !is.na(data[, sampled] == 1) & data[, sampled] == 1
	n <- sum(sampled.ind)
	x <- matrix(data[sampled.ind, "x"], nrow = n, ncol =
		1)
	y <- matrix(data[sampled.ind, "y"], nrow = n, ncol =
		1)
	x.mat <- matrix(rep(x, times = n), nrow = n,ncol = n)
	x.mat <- t(x.mat) - x.mat
	y.mat <- matrix(rep(y, times = n), nrow = n, ncol = n)
	y.mat <- t(y.mat) - y.mat
	c.mat <- matrix(rep(as.integer(data[sampled.ind, strat]), times = n),nrow = n,ncol = n)
	s11.ind <- matrix( c.mat == 1 & t(c.mat) == 1, nrow = n^2, ncol = 1)
	s22.ind <- matrix( c.mat == 2 & t(c.mat) == 2, nrow = n^2, ncol = 1)
	s12.ind <- matrix( c.mat == 1 & t(c.mat) == 2, nrow = n^2, ncol = 1)
	s21.ind <- matrix( c.mat == 2 & t(c.mat) == 1, nrow = n^2, ncol = 1)
	h <- cbind(matrix(x.mat, nrow = n^2, ncol = 1), matrix(y.mat, nrow = n^2, ncol = 1))
	gamma11 <- nugget1 + parsil1 - exponential.variogram.model(h, nugget = nugget1, parsil = parsil1,
		range = range1)
	gamma22 <- nugget2 + parsil2 - exponential.variogram.model(h, nugget = nugget2, parsil = parsil2,
		range = range2)
	gamma <- matrix(NA, nrow = n^2, ncol = 1)
	gamma[s11.ind] <- gamma11[s11.ind]
	gamma[s22.ind] <- gamma22[s22.ind]
	gamma[s12.ind] <- 0
	gamma[s21.ind] <- 0
	gamma <- matrix(gamma, nrow = n, ncol = n)
	gamma
}


#
# ------- BUILDS VARIANCE-COVARIANCE MATRIX BETWEEN SAMPLED AND UNSAMPLED
#' Title
#'
#' @param data
#' @param nugget1
#' @param parsil1
#' @param range1
#' @param nugget2
#' @param parsil2
#' @param range2
#' @param sampled
#' @param strat
#'
#' @return
#' @export
#'
#' @examples
SU.mat <-
function(data, nugget1, parsil1, range1,
	nugget2, parsil2, range2, sampled, strat)
{
	sampled.ind <- !is.na(data[, sampled] == 1) & data[, sampled] == 1
	unsampled.ind <- is.na(data[, sampled] == 1) | data[, sampled] != 1
	ns <- sum(sampled.ind)
	nu <- sum(unsampled.ind)
	xs <- matrix(data[sampled.ind, "x"], nrow = ns, ncol = 1)
	ys <- matrix(data[sampled.ind, "y"], nrow = ns, ncol = 1)
	xu <- matrix(data[unsampled.ind, "x"], nrow = nu, ncol = 1)
	yu <- matrix(data[unsampled.ind, "y"], nrow = nu, ncol = 1)
	ones <- matrix(1, nrow = ns, ncol = 1)
	oneu <- matrix(1, nrow = nu, ncol = 1)
	x.mat <- -matrix(rep(xs, times = nu), nrow = ns, ncol = nu) +
		t(matrix(rep(xu, times = ns), nrow = nu, ncol = ns))
	y.mat <- -matrix(rep(ys, times = nu), nrow = ns, ncol = nu) +
		t(matrix(rep(yu, times = ns), nrow = nu, ncol = ns))
	c.mats<-matrix(rep(as.integer(data[sampled.ind, strat]), times = nu), nrow = ns, ncol = nu)
	c.matu<-t(matrix(rep(as.integer(data[unsampled.ind, strat]), times = ns), nrow = nu, ncol = ns))
	s11.ind <- matrix( c.mats==1 & c.matu==1, nrow=ns*nu, ncol=1)
	s22.ind <- matrix( c.mats==2 & c.matu==2, nrow=ns*nu, ncol=1)
	s12.ind <- matrix( c.mats==1 & c.matu==2, nrow=ns*nu, ncol=1)
	s21.ind <- matrix( c.mats==2 & c.matu==1, nrow=ns*nu, ncol=1)
	h<-cbind(matrix(x.mat,nrow=ns*nu,ncol=1),matrix(y.mat,nrow=ns*nu,ncol=1))
	gamma11 <- nugget1 + parsil1 - exponential.variogram.model(h, nugget = nugget1, parsil = parsil1,
		range = range1)
	gamma22 <- nugget2 + parsil2 - exponential.variogram.model(h, nugget = nugget2, parsil = parsil2,
		range = range2)
	gamma<-matrix(NA, nrow = ns*nu, ncol = 1)
	gamma[s11.ind] <- gamma11[s11.ind]
	gamma[s22.ind] <- gamma22[s22.ind]
	gamma[s12.ind] <- 0
	gamma[s21.ind] <- 0
	gamma<-matrix(gamma, nrow = ns, ncol = nu)
	gamma
}

#
# ------- BUILDS THE UNSAMPLED VARIANCE-COVARIANCE MATRIX
#' Title
#'
#' @param data
#' @param nugget1
#' @param parsil1
#' @param range1
#' @param nugget2
#' @param parsil2
#' @param range2
#' @param sampled
#' @param strat
#'
#' @return
#' @export
#'
#' @examples
UU.mat<-
function(data, nugget1, parsil1, range1,
	nugget2, parsil2, range2, sampled, strat)
{
	sampled.ind <- is.na(data[, sampled] == 1) | data[, sampled] != 1
	n <- sum(sampled.ind)
	x <- matrix(data[sampled.ind, "x"], nrow = n, ncol =
		1)
	y <- matrix(data[sampled.ind, "y"], nrow = n, ncol =
		1)
	x.mat<-matrix(rep(x,times=n),nrow=n,ncol=n)
	x.mat<-t(x.mat)-x.mat
	y.mat<-matrix(rep(y,times=n),nrow=n,ncol=n)
	y.mat<-t(y.mat)-y.mat
	c.mat<-matrix(rep(as.integer(data[sampled.ind,strat]),times=n),nrow=n,ncol=n)
	s11.ind <- matrix( c.mat==1 & t(c.mat)==1, nrow=n^2, ncol=1)
	s22.ind <- matrix( c.mat==2 & t(c.mat)==2, nrow=n^2, ncol=1)
	s12.ind <- matrix( c.mat==1 & t(c.mat)==2, nrow=n^2, ncol=1)
	s21.ind <- matrix( c.mat==2 & t(c.mat)==1, nrow=n^2, ncol=1)
	h<-cbind(matrix(x.mat,nrow=n^2,ncol=1),matrix(y.mat,nrow=n^2,ncol=1))
	gamma11 <- nugget1 + parsil1 - exponential.variogram.model(h, nugget = nugget1, parsil = parsil1,
		range = range1)
	gamma22 <- nugget2 + parsil2 - exponential.variogram.model(h, nugget = nugget2, parsil = parsil2,
		range = range2)
	gamma<-matrix(NA,nrow=n^2,ncol=1)
	gamma[s11.ind]<-gamma11[s11.ind]
	gamma[s22.ind]<-gamma22[s22.ind]
	gamma[s12.ind] <- 0
	gamma[s21.ind] <- 0
	gamma <- matrix(gamma, nrow = n,ncol = n)
	gamma
}

# --------------------------------------------------------------------------
#         ESTIMATION USING GEO METHOD - FINITE POPULATION BLOCK KRIGING
#---------------------------------------------------------------------------


#' Title
#'
#' @param column.ana
#' @param strat
#' @param data
#' @param sampled
#' @param area
#' @param column.pred
#' @param column.lat
#' @param column.lon
#'
#' @return
#' @export
#'
#' @examples
geo.moosepop <- function(column.ana, strat, data, sampled, area,
                         column.pred, column.lat, column.lon)
{

  data <- cbind(data,
                LL.to.ARBUTM(mean(data[,column.lon]), data[,column.lat],
                             data[,column.lon]) )
  data[,strat] <- as.factor(data[,strat])

  # ------------------ SUMMARY STATISTICS

  data.s <- data[!is.na(data[, sampled]) & data[, sampled] == 1, ]
  cds <- as.integer(data.s[,strat])
  cdu <- as.integer(data[,strat])
  lvs <- levels(data.s[,strat])
  nlvs <- max(cds)
  if(sum(lvs == "") > 0 | sum(is.na(lvs)) > 0)
    return(list(errstate = 1, errmessage = "Stratification has missing values",
                errextra = ""))
  if(nlvs != 2)
    return(list(errstate = 1, errmessage = "Stratification must have exactly 2 levels",errextra = ""))
  if(length(unique(cds)) != length(unique(cdu)))
    return(list(errstate = 1,
                errmessage = "Some strata have not been sampled",
                errextra = data.frame(sampled = unique(as.character(data.s[,strat])),all = levels(data[,strat])))
    )
  means <- matrix(NA,nrow=nlvs,ncol=1)
  vars <- matrix(NA,nrow=nlvs,ncol=1)
  n <- matrix(NA,nrow=nlvs,ncol=1)
  N <- matrix(NA,nrow=nlvs,ncol=1)
  n.pred <- matrix(NA,nrow=nlvs,ncol=1)
  N.pred <- matrix(NA,nrow=nlvs,ncol=1)
  areas <- matrix(NA,nrow=nlvs,ncol=1)
  areato <- matrix(NA,nrow=nlvs,ncol=1)
  counted <- matrix(NA,nrow=nlvs,ncol=1)
  areas.pred <-matrix(NA,nrow=nlvs,ncol=1)
  areas.tot.pred <- matrix(NA,nrow=nlvs,ncol=1)
  counted.pred <- matrix(NA,nrow=nlvs,ncol=1)
  i <- 1
  for (i in 1:nlvs) {
    ind.cds <- cds==i & data.s[,sampled]==1
    ind.cdu <- cdu==i
    ind.cds.pred <- cds==i & data.s[,column.pred]==TRUE       #
    ind.cdu.pred <- cdu==i & data[,column.pred]==TRUE         # JM edits to get densities for AA's
    areas.pred[i] <- sum(data.s[ind.cds.pred,area])             #
    areas.tot.pred[i] <- sum(data[ind.cdu.pred,area])         #
    counted[i] <- sum(data.s[ind.cds,column.ana],na.rm = TRUE)
    counted.pred[i] <- sum(data.s[ind.cds.pred,column.ana])   #
    areas[i] <- sum(data.s[ind.cds,area])
    means[i] <- counted[i]/areas[i]
    n[i] <- length(data.s[ind.cds,column.ana])
    N[i] <- length(data[ind.cdu,column.ana])
    n.pred[i] <- length(data.s[ind.cds.pred,column.ana]) #JM edits to get AA sample sizes
    N.pred[i] <- length(data[ind.cdu.pred,column.ana])   #
    # if(n[i] < 20 & N[i] != n[i])
    #   return(list(errstate = 1,
    #               errmessage = "Cannot estimate autocorrelation with < 20 samples in a stratum",
    #               errextra = data.frame(stratum = as.character(lvs[i]), n = n[i], N = N[i]))
    #   )
    areato[i] <- sum(data[ind.cdu,area])
  }
  n.strat.df <- data.frame(Stratum=levels(data[,strat]),n = n)
  n.strat.df <- rbind(n.strat.df,
                      data.frame(Stratum="TOTAL", n = sum(n.strat.df[,2])))
  N.strat.df <- data.frame(Stratum=levels(data[,strat]), N = N)
  N.strat.df <- rbind(N.strat.df,
                      data.frame(Stratum="TOTAL", N = sum(N.strat.df[,2])))
  n.pred.strat.df <- data.frame(Stratum=levels(data[,strat]),n = n.pred)          # JM Edits to report AA sample sizes
  n.pred.strat.df <- rbind(n.pred.strat.df,                                       #
                           data.frame(Stratum="TOTAL", n = sum(n.pred.strat.df[,2])))  #
  N.pred.strat.df <- data.frame(Stratum=levels(data[,strat]), N = N.pred)         #
  N.pred.strat.df <- rbind(N.pred.strat.df,                                       #
                           data.frame(Stratum="TOTAL", N = sum(N.pred.strat.df[,2])))  #
  areas.strat.df <- data.frame(Stratum=levels(data[,strat]),Area=areas)
  areas.strat.df <- rbind(areas.strat.df,
                          data.frame(Stratum="TOTAL", Area = sum(areas.strat.df[,2])))
  areato.strat.df <- data.frame(Stratum=levels(data[,strat]),Area=areato)
  areato.strat.df <- rbind(areato.strat.df,
                           data.frame(Stratum="TOTAL", Area = sum(areato.strat.df[,2])))
  areas.pred.df <- data.frame(Stratum=levels(data[,strat]),Area=areas.pred)                   # JM edits to report total area for AA's
  areas.pred.df <- rbind(areas.pred.df,                                                       #
                         data.frame(Stratum="TOTAL", Area = sum(areas.pred.df[,2])))        #

  areas.tot.pred.df <- data.frame(Stratum=levels(data[,strat]),Area=areas.tot.pred)                   # JM edits to report sampled area for AA's
  areas.tot.pred.df <- rbind(areas.tot.pred.df,                                                       #
                             data.frame(Stratum="TOTAL", Area = sum(areas.tot.pred.df[,2])))          #

  counted.strat.df <- data.frame(Stratum=levels(data[,strat]),Counted=counted)
  counted.strat.df <- rbind(counted.strat.df,
                            data.frame(Stratum="TOTAL", Counted = sum(counted.strat.df[,2])))
  counted.pred.strat.df <- data.frame(Stratum=levels(data[,strat]),Counted=counted.pred)          # JM edits to report counted moose for AA's
  counted.pred.strat.df <- rbind(counted.pred.strat.df,                                           #
                                 data.frame(Stratum="TOTAL", Counted = sum(counted.pred.strat.df[,2])))#

  ind1 <- as.integer(data[,strat])==1 & data[,sampled]==1 & !is.na(data[,sampled])
  ind2 <- as.integer(data[,strat])==2 & data[,sampled]==1 & !is.na(data[,sampled])
  den1 <- data[ind1,column.ana]/data[ind1,area]
  den2 <- data[ind2,column.ana]/data[ind2,area]
  strat.1 <- data.frame(x = data[ind1, "x"], y = data[ind1, "y"], var = den1)
  strat.2 <- data.frame(x = data[ind2, "x"], y = data[ind2, "y"], var = den2)

  #--------------- EMPIRICAL SEMIVARIOGRAMS AND SEMICROSSVARIOGRAMS

  emp.var1 <- empirical.semivariogram(data = strat.1, x = "x", y = "y", var = "var", nlag = 8,
                                      maxlag = 50, directions = c(0), tolerance = 180,
                                      nlagcutoff = 3)
  emp.var2 <- empirical.semivariogram(data = strat.2, x = "x", y = "y", var = "var", nlag = 8,
                                      maxlag = 50, directions = c(0), tolerance = 180,
                                      nlagcutoff = 3)

  #----------------------- FIT SEMIVARIOGRAMS

  nugget1i <- mean(emp.var1[,"gamma"])/4
  parsil1i <- mean(emp.var1[,"gamma"])
  range1i <- mean(emp.var1[,"distance"])
  theta <- c(nugget1i,parsil1i,range1i)
  X1 <- matrix(1, nrow = length(den1), ncol = 1)
  parmest1 <- optim(theta, m2LL, m2LLdata = strat.1, X=X1)$par

  nugget2i <- mean(emp.var2[,"gamma"])/4
  parsil2i <- mean(emp.var2[,"gamma"])
  range2i <- mean(emp.var2[,"distance"])
  theta <- c(nugget2i,parsil2i,range2i)
  X2 <- matrix(1, nrow = length(den2), ncol = 1)
  parmest2 <- optim(theta, m2LL, m2LLdata = strat.2, X=X2)$par

  nugget1 <- parmest1[1]
  parsil1 <- parmest1[2]
  range1  <- parmest1[3]
  nugget2 <- parmest2[1]
  parsil2 <- parmest2[2]
  range2  <- parmest2[3]
  parmest1 <- data.frame(nugget = parmest1[1], parsil = parmest1[2], range = parmest1[3])
  parmest2 <- data.frame(nugget = parmest2[1], parsil = parmest2[2], range = parmest2[3])

  if(sum(den1) == 0) {
    nugget1 <- 1e-6
    parsil1 <- 0
    range1 <- 1
  }
  if(sum(den2) == 0) {
    nugget2 <- 1e-6
    parsil2 <- 0
    range2 <- 1
  }

  #------------------------- BUILD MATRICES

  SS <- SS.mat(data, nugget1 = nugget1, parsil1 = parsil1, range1 = range1,
               nugget2 = nugget2, parsil2 = parsil2, range2 = range2,
               sampled = sampled, strat = strat)
  SU <- SU.mat(data, nugget1 = nugget1, parsil1 = parsil1, range1 = range1,
               nugget2 = nugget2, parsil2 = parsil2, range2 = range2,
               sampled = sampled, strat = strat)
  UU <- UU.mat(data, nugget1 = nugget1, parsil1 = parsil1, range1 = range1,
               nugget2 = nugget2, parsil2 = parsil2, range2 = range2,
               sampled = sampled, strat = strat)

  #------------------------- FINITE POPULATION BLOCK KRIGING

  ind.sa <- !is.na(data[, sampled] == 1) & data[, sampled] == 1
  ns <- sum(ind.sa)
  ind.un <- is.na(data[, sampled] == 1) | data[, sampled] != 1
  nu <- sum(ind.un)
  z <- matrix(data[ind.sa, column.ana], nrow = ns, ncol = 1)
  area.s <- matrix(data[ind.sa, area], nrow = ns, ncol = 1)
  z <- z/area.s
  area.tot <- matrix(data[, area], nrow = nu+ns, ncol = 1)

  X1 <- as.numeric(as.integer(data[,strat])==1)       #
  X2 <- as.numeric(as.integer(data[,strat])==2)       #
  X <- Xstrat <- cbind(rep(1,times=length(X1)),X1,X2) # Edit: variable Xstrat added here
  Xs <- X[ind.sa,]                                    #
  Xu <- X[ind.un,]                                    # Edit: These 5 lines were switched with the next 5 to use Xstrat below

  B <- data[, column.pred]             #
  B[is.na(B)] <- 0                     #
  B <- Xstrat * cbind(B,B,B)           # ME edits - predictions for total and both strata  (H,L,T)
  Bs <- B[ind.sa,]                     #
  Bu <- B[ind.un,]                     #

  SSi <- solve(SS)  ##solve(SS)


  # ----------------------- PREDICTIONS

  part1 <- Xs %*% mginv(t(Xs) %*% SSi %*% Xs)
  part2 <- t(Xu) - t(Xs) %*% SSi %*% SU
  D <- SU + part1 %*% part2
  FF <- SSi %*% D
  Ao <- Bs + FF %*% Bu
  y.est <- t(Ao) %*% z
  y.est <- mean(area.tot)*y.est

  # ---------------------- VARIANCE

  part1 <- t(FF) %*% SS %*% FF
  part2 <- t(FF) %*% SU
  y.var <- (t(Bu) %*% ( part1 - part2 - t(part2) + UU ) %*% Bu)[1:ncol(Bu)] #ME edits to get variance of each stratum
  y.se <- mean(area.tot)*sqrt(y.var)

  # ---------------------- CONFIDENCE INTERVALS

  ci80 <- c(y.est - y.se * qnorm(0.9), y.est + y.se *
              qnorm(0.9))
  cipm80 <- (y.se * qnorm(0.9))/y.est
  ci90 <- c(y.est - y.se * qnorm(0.95), y.est + y.se *
              qnorm(0.95))
  cipm90 <- (y.se * qnorm(0.95))/y.est
  ci95 <- c(y.est - y.se * qnorm(0.975), y.est + y.se *
              qnorm(0.975))
  cipm95 <- (y.se * qnorm(0.975))/y.est

  # ----------------------- POINT PREDICTIONS -- ME edits

  betaGLS <- mginv(t(Xs) %*% SSi %*% Xs) %*% t(Xs) %*% SSi %*% z
  muS <- Xs %*% betaGLS
  muU <- Xu %*% betaGLS
  zU<-t(SU) %*% SSi %*% (z - muS) + muU

  Zpred<-c(z,zU)[order(c(which(ind.sa), which(ind.un)))]
  Zpred<-area.tot*Zpred

  # ---------------------- POINT VARIANCE

  part1 <- t(FF) %*% SS %*% FF
  part2 <- t(FF) %*% SU

  ZvarS<- rep(0, ns)
  ZvarU<-diag( part1 - part2 - t(part2) + UU )

  Zvar<- c(ZvarS, ZvarU)[order(c(which(ind.sa), which(ind.un)))]
  Zse<-area.tot*sqrt(Zvar)


  outpt <- list(
    estimate.total = as.numeric(y.est),
    estimate.standard.error = as.numeric(y.se),
    mean.density=as.numeric(y.est/areas.tot.pred.df$Area[c(3,1,2)]),   # JM edit: changed "areato.strat.df" to "areas.pred.df" to get
    conf.int.80 = ci80,                                                # correct density estimates for analysis areas.
    ci.prop.mean.80 = as.numeric(cipm80),
    conf.int.90 = ci90,
    ci.prop.mean.90 = as.numeric(cipm90),
    conf.int.95 = ci95,
    ci.prop.mean.95 = as.numeric(cipm95),
    sample.sizes = data.frame(n.strat.df, row.names = NULL),
    total.samples = data.frame(N.strat.df, row.names = NULL),
    moose.counted = data.frame(counted.strat.df, row.names = NULL),
    sampled.area = data.frame(areas.strat.df, row.names = NULL),
    total.area = data.frame(areato.strat.df, row.names = NULL),
    AA.sample.sizes = data.frame(n.pred.strat.df, row.names = NULL),       # JM edit to report AA summary stats
    AA.total.samples = data.frame(N.pred.strat.df, row.names = NULL),      #
    AA.moose.counted = data.frame(counted.pred.strat.df, row.names = NULL),#
    AA.sampled.area = data.frame(areas.pred.df, row.names = NULL),         #
    AA.total.area = data.frame(areas.tot.pred.df, row.names = NULL),       #
    strat.1.name = as.character(levels(data[,strat])[1]),
    strat.2.name = as.character(levels(data[,strat])[2]),
    empirical.semivariogram.strat1 = emp.var1[,1:3],
    empirical.semivariogram.strat2 = emp.var2[,1:3],
    parmest1 = parmest1,
    parmest2 = parmest2,
    predictions = data.frame(data[,c(column.lat, column.lon, strat)],
                                                   Est=Zpred, SE=Zse,row.names=NULL))


  outpt
}




# --------------------------------------------------------------------------
#         COMPOSITION ESTIMATION USING GASAWAY METHODS
#---------------------------------------------------------------------------

#' Title
#'
#' @param column.numerator
#' @param column.denominator
#' @param strat
#' @param data
#' @param sampled
#' @param area
#' @param column.pred
#'
#' @return
#' @export
#'
#' @examples
moosepop.composition<-
function(column.numerator, column.denominator, strat, data,
	sampled, area, column.pred)
{
  data[,strat] <- as.factor(data[,strat])
	data.s <- data[!is.na(data[, sampled]) & data[, sampled] == 1, ]
	cds <- as.integer(data.s[,strat])
	cdu <- as.integer(data[,strat])
	lvs <- levels(data.s[,strat])
	nlvs <- max(cds)
	num.means <- matrix(NA,nrow=nlvs,ncol=1)
	den.means <- matrix(NA,nrow=nlvs,ncol=1)
	num.vars <- matrix(NA,nrow=nlvs,ncol=1)
	den.vars <- matrix(NA,nrow=nlvs,ncol=1)
	n <- matrix(NA,nrow=nlvs,ncol=1)
	N <- matrix(NA,nrow=nlvs,ncol=1)
	areas <- matrix(NA,nrow=nlvs,ncol=1)
	areato <- matrix(NA,nrow=nlvs,ncol=1)
	num.counted <- matrix(NA,nrow=nlvs,ncol=1)
	den.counted <- matrix(NA,nrow=nlvs,ncol=1)
	cross.vars <- matrix(NA,nrow=nlvs,ncol=1)
	i <- 1
	for (i in 1:nlvs) {
		ind.cds <- cds==i & data.s[,column.pred]==1
		ind.cdu <- cdu==i & data[,column.pred]==1
		num.counted[i] <- sum(data.s[ind.cds,column.numerator])
		den.counted[i] <- sum(data.s[ind.cds,column.denominator])
		areas[i] <- sum(data.s[ind.cds,area])
		num.means[i] <- num.counted[i]/areas[i]
		den.means[i] <- den.counted[i]/areas[i]
		n[i] <- length(data.s[ind.cds,column.numerator])
		num.vars[i] <- sum(data.s[ind.cds,column.numerator]^2) -
			2*num.means[i]*sum(data.s[ind.cds,column.numerator]*data.s[ind.cds,area]) +
			num.means[i]^2*sum(data.s[ind.cds,area]^2)
		num.vars[i] <- num.vars[i]/(n[i] - 1)
		den.vars[i] <- sum(data.s[ind.cds,column.denominator]^2) -
			2*den.means[i]*sum(data.s[ind.cds,column.denominator]*data.s[ind.cds,area]) +
			den.means[i]^2*sum(data.s[ind.cds,area]^2)
		den.vars[i] <- den.vars[i]/(n[i] - 1)
		cross.vars[i] <- sum(data.s[ind.cds,column.numerator]*data.s[ind.cds,column.denominator]) -
			den.means[i]*sum(data.s[ind.cds,column.numerator]*data.s[ind.cds,area]) -
			num.means[i]*sum(data.s[ind.cds,column.denominator]*data.s[ind.cds,area]) +
			num.means[i]*den.means[i]*sum(data.s[ind.cds,area]^2)
		cross.vars[i] <- cross.vars[i]/(n[i] - 1)
		N[i] <- length(data[ind.cdu,area])
		areato[i] <- sum(data[ind.cdu,area])
	}
	num.est.strat <- num.means*areato
	den.est.strat <- den.means*areato
	num.est <- sum(num.est.strat)
	den.est <- sum(den.est.strat)
	num.est.strat.df <- data.frame(stratum=levels(data[,strat]),total.estimate=num.est.strat)
	den.est.strat.df <- data.frame(stratum=levels(data[,strat]),total.estimate=den.est.strat)
	num.var.strat <- n*num.vars/areas^2*(1-n/N)*areato^2
	den.var.strat <- n*den.vars/areas^2*(1-n/N)*areato^2
	num.se <- sqrt( sum(num.var.strat) )
	den.se <- sqrt( sum(den.var.strat) )
	num.var.strat.df <- data.frame(stratum=levels(data[,strat]),total.variance=num.var.strat)
	den.var.strat.df <- data.frame(stratum=levels(data[,strat]),total.variance=den.var.strat)
	n.strat.df <- data.frame(stratum=levels(data[,strat]),sample.sizes=n)
	N.strat.df <- data.frame(stratum=levels(data[,strat]),sample.sizes=N)
	num.mean.strat.df <- data.frame(stratum=levels(data[,strat]),mean.density=num.means)
	den.mean.strat.df <- data.frame(stratum=levels(data[,strat]),mean.density=den.means)
	areas.strat.df <- data.frame(stratum=levels(data[,strat]),sampled.area=areas)
	areato.strat.df <- data.frame(stratum=levels(data[,strat]),total.area=areato)
	num.counted.strat.df <- data.frame(stratum=levels(data[,strat]),counted=num.counted)
	den.counted.strat.df <- data.frame(stratum=levels(data[,strat]),counted=den.counted)
	num.v0 <- sum(num.var.strat)^2/sum(num.var.strat^2/(n-1))
	den.v0 <- sum(den.var.strat)^2/sum(den.var.strat^2/(n-1))
	rat.est <- num.est/den.est
	cross.var.strat <- n*cross.vars/areas^2*(1-n/N)*areato^2
	cross.var.strat.df <- data.frame(stratum=levels(data[,strat]),covariance=cross.var.strat)
	ratio.var <- rat.est^2*( sum(num.var.strat)/num.est^2 +
		sum(den.var.strat)/den.est^2 -
		2*sum(cross.var.strat)/num.est/den.est )
	rat.se <- sqrt(ratio.var)
	rat.v0 <- min(den.v0,num.v0)
	# create output
	num.ci80 <- c(num.est - num.se * qt(0.9,num.v0), num.est + num.se *
		qt(0.9,num.v0))
	num.cipm80 <- (num.se * qt(0.9,num.v0))/num.est
	num.ci90 <- c(num.est - num.se * qt(0.95,num.v0), num.est + num.se *
		qt(0.95,num.v0))
	num.cipm90 <- (num.se * qt(0.95,num.v0))/num.est
	num.ci95 <- c(num.est - num.se * qt(0.975,num.v0), num.est + num.se *
		qt(0.975,num.v0))
	num.cipm95 <- (num.se * qt(0.975,num.v0))/num.est
	# create output
	den.ci80 <- c(den.est - den.se * qt(0.9,den.v0), den.est + den.se *
		qt(0.9,den.v0))
	den.cipm80 <- (den.se * qt(0.9,den.v0))/den.est
	den.ci90 <- c(den.est - den.se * qt(0.95,den.v0), den.est + den.se *
		qt(0.95,den.v0))
	den.cipm90 <- (den.se * qt(0.95,den.v0))/den.est
	den.ci95 <- c(den.est - den.se * qt(0.975,den.v0), den.est + den.se *
		qt(0.975,den.v0))
	den.cipm95 <- (den.se * qt(0.975,den.v0))/den.est
	# create output
	rat.ci80 <- c(rat.est - rat.se * qt(0.9,rat.v0), rat.est + rat.se *
		qt(0.9,rat.v0))
	rat.cipm80 <- (rat.se * qt(0.9,rat.v0))/rat.est
	rat.ci90 <- c(rat.est - rat.se * qt(0.95,rat.v0), rat.est + rat.se *
		qt(0.95,rat.v0))
	rat.cipm90 <- (rat.se * qt(0.95,rat.v0))/rat.est
	rat.ci95 <- c(rat.est - rat.se * qt(0.975,rat.v0), rat.est + rat.se *
		qt(0.975,rat.v0))
	rat.cipm95 <- (rat.se * qt(0.975,rat.v0))/rat.est
	outpt <- list(
		sample.sizes = n.strat.df,
		total.samples = N.strat.df,
		sampled.area = areas.strat.df,
		total.area = areato.strat.df,
		numerator = column.numerator,
		numerator.estimate = num.est,
		numerator.se = num.se,
		numerator.counted = num.counted.strat.df,
		numerator.mean.density = num.mean.strat.df,
		numerator.stratum.estimates = num.est.strat.df,
		numerator.stratum.variances = num.var.strat.df,
		numerator.degrees.of.freedom = num.v0,
		numerator.conf.int.80 = num.ci80,
		numerator.ci.prop.mean.80 = num.cipm80,
		numerator.conf.int.90 = num.ci90,
		numerator.ci.prop.mean.90 = num.cipm90,
		numerator.conf.int.95 = num.ci95,
		numerator.ci.prop.mean.95 = num.cipm95,
		denominator = column.denominator,
		denominator.estimate = den.est,
		denominator.se = den.se,
		denominator.counted = den.counted.strat.df,
		denominator.mean.density = den.mean.strat.df,
		denominator.stratum.estimates = den.est.strat.df,
		denominator.stratum.variances = den.var.strat.df,
		denominator.degrees.of.freedom = den.v0,
		denominator.conf.int.80 = den.ci80,
		denominator.ci.prop.mean.80 = den.cipm80,
		denominator.conf.int.90 = den.ci90,
		denominator.ci.prop.mean.90 = den.cipm90,
		denominator.conf.int.95 = den.ci95,
		denominator.ci.prop.mean.95 = den.cipm95,
		ratio.estimate = rat.est,
		ratio.se = rat.se,
		stratum.covariances = cross.var.strat.df,
		ratio.degrees.of.freedom = rat.v0,
		ratio.conf.int.80 = rat.ci80,
		ratio.ci.prop.mean.80 = rat.cipm80,
		ratio.conf.int.90 = rat.ci90,
		ratio.ci.prop.mean.90 = rat.cipm90,
		ratio.conf.int.95 = rat.ci95,
		ratio.ci.prop.mean.95 = rat.cipm95
		)
	outpt
}


#----------------------------------------------------------------------------------------
#            GRAPHICS FUNCTIONS
#----------------------------------------------------------------------------------------



#' Title
#'
#' @param data
#' @param column.ana
#' @param column.unitid
#' @param strat
#' @param sampled
#' @param col.strat.1
#' @param col.strat.2
#' @param column.spe
#' @param col.spe.strat.1
#' @param col.spe.strat.2
#'
#' @return
#' @export
#'
#' @examples
samp.strat.map <- function(data, column.ana = "TOTAL", column.unitid = "UNITID", strat = "STRAT",
	sampled = "SAMPLED",  col.strat.1 = "red", col.strat.2 = "green",
	column.spe = "NA", col.spe.strat.1 = "lightsalmon1", col.spe.strat.2 = "palegreen3")
{
	library(maptools)
	# get average latitude
	avey <- 0
	for(i in 1:length(data[,column.unitid])) {
		lowery <- floor(as.numeric(substring(as.character(data[i,column.unitid]),1,4))/100) +
			(as.numeric(substring(as.character(data[i,column.unitid]),1,4))/100 -
			floor(as.numeric(substring(as.character(data[i,column.unitid]),1,4))/100))/.6
		uppery <- lowery + 2/60
		avey <- avey + lowery + uppery
	}
	avey <- avey/(2*length(data[,column.unitid]))
	avey.radians <- avey/180*pi

	# create R Map object
	s.concat <- NULL
	xmin <- 1e32
	xmax <- -1e32
	ymin <- 1e32
	ymax <- -1e32
	for(i in 1:length(data[,column.unitid])) {
		lowery <- floor(as.numeric(substring(as.character(data[i,column.unitid]),1,4))/100) +
			(as.numeric(substring(as.character(data[i,column.unitid]),1,4))/100 -
			floor(as.numeric(substring(as.character(data[i,column.unitid]),1,4))/100))/.6
		rightx <- -(floor(as.numeric(substring(as.character(data[i,column.unitid]),6,10))/100) +
			(as.numeric(substring(as.character(data[i,column.unitid]),6,10))/100 -
			floor(as.numeric(substring(as.character(data[i,column.unitid]),6,10))/100))/.6)
		uppery <- lowery + 2/60
		leftx <- rightx - 5/60
		lowery <- lowery*1/cos(avey.radians)
		uppery <- uppery*1/cos(avey.radians)

		Pstart <- 0
		attr(Pstart,"row.names") <- "Ring"
		verts <- matrix(c(leftx,uppery,rightx,uppery,
			rightx,lowery,leftx,lowery,leftx,uppery),
			ncol = 2, byrow = TRUE)
		s.temp <- list(Pstart = Pstart, verts = verts)
		attr(s.temp,"shp.type") <- as.integer(5)
		attr(s.temp,"nVerts") <- as.integer(5)
		attr(s.temp,"nParts") <- as.integer(1)
		attr(s.temp,"RingDir") <- as.integer(1)
		attr(s.temp,"bbox") <- c(leftx,lowery,rightx,uppery)
		s.concat <- c(s.concat,list(s.temp))
		xmin <- min(xmin,leftx)
		xmax <- max(xmax,rightx)
		ymin <- min(ymin,lowery)
		ymax <- max(ymax,uppery)
	}


	attr(s.concat,"shp.type") <- "poly"
	attr(s.concat,"nshps") <- length(data[,column.unitid])
	attr(s.concat,"minbb") <- c(xmin, ymin, 0.0, 0.0)
	attr(s.concat,"maxbb") <- c(xmax, ymax, 0.0, 0.0)
	attr(s.concat,"class") <- "ShapeList"
	samp.strat <- list(Shapes = s.concat, att.data = data)
	attr(samp.strat,"class") <- "Map"




	# draw map
	nParts <- as.integer(samp.strat$att.data[,strat])
	cols <- c(col.strat.1, col.strat.2)
	if(column.spe != "NA") {
		nParts <- nParts + 2*data[,column.spe]
		cols <- c(col.strat.1, col.strat.2, col.spe.strat.1, col.spe.strat.2)
	}
	fgs <- cols[nParts]
#	postscript(outfilen, horizontal = FALSE)
	plot(samp.strat, fg=fgs, xlim= c(xmin,xmax), ylim= c(ymin,ymax),
		main = "Sampling and Stratification", cex.main = 3,
		xlab = "Longitude (Decimal Degrees)", ylab = "",
		cex.lab = 1.5, cex.axis = 1.2, bty = "n", yaxt = "n")
	for(i in 1:length(data[,column.unitid]))
		if(data[i,sampled] ==1) {
			lines(samp.strat$Shapes[[i]]$verts,col = "black", lwd = 3)
			centx <- (attr(samp.strat$Shapes[[i]],"bbox")[1] +
				attr(samp.strat$Shapes[[i]],"bbox")[3])/2
			centy <- (attr(samp.strat$Shapes[[i]],"bbox")[2] +
				attr(samp.strat$Shapes[[i]],"bbox")[4])/2
			text(centx,centy,
				as.character(data[i,column.ana]),
				cex = 0.75, col = "black")
		}






#	dev.off(as.vector(2:max(dev.list()))[names(dev.list())=="postscript"])
}

#----------------------------------------------------------------------------------------
#           LIKELIHOOD FUNCTION FOR CROSS CORRELATION OF NUGGET EFFECT
#----------------------------------------------------------------------------------------

#' Title
#'
#' @param rho
#' @param theta1
#' @param theta2
#' @param m2LLdata
#' @param X
#'
#' @return
#' @export
#'
#' @examples
m2LL.cross <- function(rho, theta1, theta2, m2LLdata, X)
{
	nugget1 <- theta1[1]
	parsil1 <- theta1[2]
	range1 <- theta1[3]
	nugget2 <- theta2[1]
	parsil2 <- theta2[2]
	range2 <- theta2[3]

	z1 <- m2LLdata[,3]
	z2 <- m2LLdata[,4]
	z <- matrix(c(z1,z2), ncol = 1)

	X <- diag(2) %x% X

		n <- length(z1)
		p <- length(X[1,])
		V1 <- exp.vc.matrix(vcmatdata = m2LLdata,
			nugget = nugget1, parsil = parsil1, range = range1)
		V2 <- exp.vc.matrix(vcmatdata = m2LLdata,
			nugget = nugget2, parsil = parsil2, range = range2)
		V <- matrix(0, nrow = 2*n, ncol = 2*n)
		V[1:n,1:n] <- exp.vc.matrix(vcmatdata = m2LLdata,
			nugget = nugget1, parsil = parsil1, range = range1)
		V[(n+1):(2*n),(n+1):(2*n)] <- exp.vc.matrix(vcmatdata = m2LLdata,
			nugget = nugget2, parsil = parsil2, range = range2)
		Z <- rbind(diag(n), diag(n))
		V[1:n,(n+1):(2*n)] <- rho*sqrt(nugget1*nugget2)*diag(n)
		V[(n+1):(2*n),1:n] <- rho*sqrt(nugget1*nugget2)*diag(n)
		Vi <- solve(V)
		b.hat <- mginv(t(X) %*% Vi %*% X) %*% t(X) %*% Vi %*% z
		f1 <- sum(log(svd(V)$d))
		f2 <- t(z - X %*% b.hat) %*% Vi %*% (z - X %*% b.hat)
		f3 <- sum(log(svd(t(X) %*% Vi %*% X)$d))
		f1 + f2 + f3 + (n - p) *log(2 * pi)

}




#----------------------------------------------------------------------------------------
#            COMPOSITION ESTIMATION USING GEO METHOD - FINITE POPULATION BLOCK KRIGING
#----------------------------------------------------------------------------------------

#' Title
#'
#' @param column.numerator
#' @param column.denominator
#' @param strat
#' @param data
#' @param sampled
#' @param area
#' @param column.pred
#' @param column.lat
#' @param column.lon
#'
#' @return
#' @export
#'
#' @examples
geo.moosepop.composition<-
function(column.numerator, column.denominator, strat, data,
	sampled, area, column.pred, column.lat, column.lon)
{
  data[,strat] <- as.factor(data[,strat])
	data <- cbind(data,
		LL.to.ARBUTM(mean(data[,column.lon]), data[,column.lat],
		data[,column.lon]) )
	data[,strat] <- as.factor(data[,strat])
	data.s <- data[!is.na(data[, sampled]) & data[, sampled] == 1, ]
	data.u <- data[is.na(data[, sampled] == 1) | data[, sampled] != 1, ]
	cds <- as.integer(data.s[,strat])
	cda <- as.integer(data[,strat])
	cdu <- as.integer(data.u[,strat])
	lvs <- levels(data.s[,strat])
	nlvs <- max(cds)
	if(sum(lvs == "") > 0 | sum(is.na(lvs)) > 0)
		return(list(errstate = 1, errmessage = "Stratification has missing values"))
	if(nlvs != 2)
		return(list(errstate = 1, errmessage = "Stratification must have exactly 2 levels"))
	if(length(unique(cds)) != length(unique(cda)))
		return(list(errstate = 1,
		errmessage = "Some strata have not been sampled"),
		errextra = list(sampled = unique(as.character(data.s[,strat])),all = levels(data[,strat]))
	)
	num.means <- matrix(NA,nrow=nlvs,ncol=1)
	den.means <- matrix(NA,nrow=nlvs,ncol=1)
	num.vars <- matrix(NA,nrow=nlvs,ncol=1)
	den.vars <- matrix(NA,nrow=nlvs,ncol=1)
	n <- matrix(NA,nrow=nlvs,ncol=1)
	N <- matrix(NA,nrow=nlvs,ncol=1)
	areas <- matrix(NA,nrow=nlvs,ncol=1)
	areato <- matrix(NA,nrow=nlvs,ncol=1)
	num.counted <- matrix(NA,nrow=nlvs,ncol=1)
	den.counted <- matrix(NA,nrow=nlvs,ncol=1)
	cross.vars <- matrix(NA,nrow=nlvs,ncol=1)
	for (i in 1:nlvs) {
		ind.cds <- cds==i
		ind.cda <- cda==i
		num.counted[i] <- sum(data.s[ind.cds,column.numerator])
		den.counted[i] <- sum(data.s[ind.cds,column.denominator])
		areas[i] <- sum(data.s[ind.cds,area])
		num.means[i] <- num.counted[i]/areas[i]
		den.means[i] <- den.counted[i]/areas[i]
		n[i] <- length(data.s[ind.cds,column.numerator])
		num.vars[i] <- sum(data.s[ind.cds,column.numerator]^2) -
			2*num.means[i]*sum(data.s[ind.cds,column.numerator]*data.s[ind.cds,area]) +
			num.means[i]^2*sum(data.s[ind.cds,area]^2)
		num.vars[i] <- num.vars[i]/(n[i] - 1)
		den.vars[i] <- sum(data.s[ind.cds,column.denominator]^2) -
			2*den.means[i]*sum(data.s[ind.cds,column.denominator]*data.s[ind.cds,area]) +
			den.means[i]^2*sum(data.s[ind.cds,area]^2)
		den.vars[i] <- den.vars[i]/(n[i] - 1)
		cross.vars[i] <- sum(data.s[ind.cds,column.numerator]*data.s[ind.cds,column.denominator]) -
			den.means[i]*sum(data.s[ind.cds,column.numerator]*data.s[ind.cds,area]) -
			num.means[i]*sum(data.s[ind.cds,column.denominator]*data.s[ind.cds,area]) +
			num.means[i]*den.means[i]*sum(data.s[ind.cds,area]^2)
		cross.vars[i] <- cross.vars[i]/(n[i] - 1)
		N[i] <- length(data[ind.cda,area])
		areato[i] <- sum(data[ind.cda,area])
	}
	n.strat.df <- data.frame(stratum=levels(data[,strat]),sample.sizes=n)
	n.strat.df <- rbind(n.strat.df,
		data.frame(stratum="TOTAL", sample.sizes = sum(n.strat.df[,2])))
	N.strat.df <- data.frame(stratum=levels(data[,strat]),sample.sizes=N)
	N.strat.df <- rbind(N.strat.df,
		data.frame(stratum="TOTAL", sample.sizes = sum(N.strat.df[,2])))
	areas.strat.df <- data.frame(stratum=levels(data[,strat]),sampled.area=areas)
	areas.strat.df <- rbind(areas.strat.df,
		data.frame(stratum="TOTAL", sampled.area = sum(areas.strat.df[,2])))
	areato.strat.df <- data.frame(stratum=levels(data[,strat]),total.area=areato)
	areato.strat.df <- rbind(areato.strat.df,
		data.frame(stratum="TOTAL", total.area = sum(areato.strat.df[,2])))

	ind1 <- as.integer(data[,strat])==1 & data[,sampled]==1 & !is.na(data[,sampled])
	ind2 <- as.integer(data[,strat])==2 & data[,sampled]==1 & !is.na(data[,sampled])
	den1.num <- data[ind1,column.numerator]/data[ind1,area]
	den1.den <- data[ind1,column.denominator]/data[ind1,area]
	ind.NA <- is.na(den1.num)
	den1.num[ind.NA] <- 0
	ind.NA <- is.na(den1.den)
	den1.den[ind.NA] <- 0
	den2.num <- data[ind2,column.numerator]/data[ind2,area]
	den2.den <- data[ind2,column.denominator]/data[ind2,area]
	ind.NA <- is.na(den2.num)
	den2.num[ind.NA] <- 0
	ind.NA <- is.na(den2.den)
	den2.den[ind.NA] <- 0
	strat.1.num <- data.frame(x = data[ind1, "x"], y = data[ind1, "y"],
		var = den1.num)
	strat.1.den <- data.frame(x = data[ind1, "x"], y = data[ind1, "y"],
		var = den1.den)
	strat.1 <- data.frame(x = data[ind1, "x"], y = data[ind1, "y"],
		var.num = den1.num, var.den = den1.den)
	strat.2.num <- data.frame(x = data[ind2, "x"], y = data[ind2, "y"],
		var = den2.num)
	strat.2.den <- data.frame(x = data[ind2, "x"], y = data[ind2, "y"],
		var = den2.den)
	strat.2 <- data.frame(x = data[ind2, "x"], y = data[ind2, "y"],
		var.num = den2.num, var.den = den2.den)

	#--------------- EMPIRICAL SEMIVARIOGRAMS AND SEMICROSSVARIOGRAMS

	emp.var1.num <- empirical.semivariogram(data = strat.1.num, x = "x", y = "y", var = "var", nlag = 8,
			maxlag = 50, directions = c(0), tolerance = 180,
			nlagcutoff = 3)
	emp.var1.den <- empirical.semivariogram(data = strat.1.den, x = "x", y = "y", var = "var", nlag = 8,
			maxlag = 50, directions = c(0), tolerance = 180,
			nlagcutoff = 3)
	emp.var2.num <- empirical.semivariogram(data = strat.2.num, x = "x", y = "y", var = "var", nlag = 8,
			maxlag = 50, directions = c(0), tolerance = 180,
			nlagcutoff = 3)
	emp.var2.den <- empirical.semivariogram(data = strat.2.den, x = "x", y = "y", var = "var", nlag = 8,
			maxlag = 50, directions = c(0), tolerance = 180,
			nlagcutoff = 3)

	#----------------------- FIT SEMIVARIOGRAMS

	nugget1i <- mean(emp.var1.num[,"gamma"])/4
	parsil1i <- mean(emp.var1.num[,"gamma"])
	range1i <- mean(emp.var1.num[,"distance"])
	theta <- c(nugget1i,parsil1i,range1i)
	X1 <- matrix(1, nrow = length(den1.num), ncol = 1)
	parmest1.num <- optim(theta, m2LL, m2LLdata = strat.1.num, X=X1)$par

	nugget1i <- mean(emp.var1.den[,"gamma"])/4
	parsil1i <- mean(emp.var1.den[,"gamma"])
	range1i <- mean(emp.var1.den[,"distance"])
	theta <- c(nugget1i,parsil1i,range1i)
	parmest1.den <- optim(theta, m2LL, m2LLdata = strat.1.den, X=X1)$par

	nugget2i <- mean(emp.var2.num[,"gamma"])/4
	parsil2i <- mean(emp.var2.num[,"gamma"])
	range2i <- mean(emp.var2.num[,"distance"])
	theta <- c(nugget2i,parsil2i,range2i)
	X2 <- matrix(1, nrow = length(den2.num), ncol = 1)
	parmest2.num <- optim(theta, m2LL, m2LLdata = strat.2.num, X=X2)$par

	nugget2i <- mean(emp.var2.den[,"gamma"])/4
	parsil2i <- mean(emp.var2.den[,"gamma"])
	range1i <- mean(emp.var2.den[,"distance"])
	theta <- c(nugget2i,parsil2i,range2i)
	parmest2.den <- optim(theta, m2LL, m2LLdata = strat.2.den, X=X2)$par

	nugget1.num <- parmest1.num[1]
	parsil1.num <- parmest1.num[2]
	range1.num  <- parmest1.num[3]
	nugget1.den <- parmest1.den[1]
	parsil1.den <- parmest1.den[2]
	range1.den  <- parmest1.den[3]
	nugget2.num <- parmest2.num[1]
	parsil2.num <- parmest2.num[2]
	range2.num  <- parmest2.num[3]
	nugget2.den <- parmest2.den[1]
	parsil2.den <- parmest2.den[2]
	range2.den  <- parmest2.den[3]

	if(sum(den1.num) != 0 &&
	   sum(den1.den) != 0) {
		rho1 <- optimize(m2LL.cross, lower = 0, upper = 1, theta1 = parmest1.num,
			theta2 = parmest1.den, m2LLdata = strat.1, X=X1)$minimum

	}
	if(sum(den2.num) != 0 &&
	   sum(den2.den) != 0) {
		rho2 <- optimize(m2LL.cross, lower = 0, upper = 1, theta1 = parmest2.num,
			theta2 = parmest2.den, m2LLdata = strat.2, X=X2)$minimum
	}

	if(sum(den1.num) == 0) {
		nugget1.num <- 1e-6
		parsil1.num <- 0
		range1.num <- 1
		rho1 <- 0
	}
	if(sum(den1.den) == 0) {
		nugget1.den <- 1e-6
		parsil1.den <- 0
		range1.den <- 1
		rho1 <- 0
	}
	if(sum(den2.num) == 0) {
		nugget2.num <- 1e-6
		parsil2.num <- 0
		range2.num <- 1
		rho2 <- 0
	}
	if(sum(den2.den) == 0) {
		nugget2.den <- 1e-6
		parsil2.den <- 0
		range2.den <- 1
		rho2 <- 0
	}

	#------------------------- BUILD MATRICES

	SS.num <- SS.mat(data, nugget1 = nugget1.num, parsil1 = parsil1.num, range1 = range1.num,
		nugget2 = nugget2.num, parsil2 = parsil2.num, range2 = range2.num,
		sampled = sampled, strat = strat)
	SU.num <- SU.mat(data, nugget1 = nugget1.num, parsil1 = parsil1.num, range1 = range1.num,
		nugget2 = nugget2.num, parsil2 = parsil2.num, range2 = range2.num,
		sampled = sampled, strat = strat)
	UU.num <- UU.mat(data, nugget1 = nugget1.num, parsil1 = parsil1.num, range1 = range1.num,
		nugget2 = nugget2.num, parsil2 = parsil2.num, range2 = range2.num,
		sampled = sampled, strat = strat)

	SS.den <- SS.mat(data, nugget1 = nugget1.den, parsil1 = parsil1.den, range1 = range1.den,
		nugget2 = nugget2.den, parsil2 = parsil2.den, range2 = range2.den,
		sampled = sampled, strat = strat)
	SU.den <- SU.mat(data, nugget1 = nugget1.den, parsil1 = parsil1.den, range1 = range1.den,
		nugget2 = nugget2.den, parsil2 = parsil2.den, range2 = range2.den,
		sampled = sampled, strat = strat)
	UU.den <- UU.mat(data, nugget1 = nugget1.den, parsil1 = parsil1.den, range1 = range1.den,
		nugget2 = nugget2.den, parsil2 = parsil2.den, range2 = range2.den,
		sampled = sampled, strat = strat)

	n.S <- length(SS.num[,1])
	n.U <- length(UU.num[,1])

	diag.SS <- rep(0, times = n.S)
	diag.SS[cds == 1] <- rho1*sqrt(nugget1.num*nugget1.den)
	diag.SS[cds == 2] <- rho2*sqrt(nugget2.num*nugget2.den)
	diag.SS <- diag(diag.SS, nrow = n.S, ncol = n.S)
	SS <- rbind(cbind(SS.num, diag.SS), cbind(diag.SS, SS.den))

	zero.SU <- matrix(0, nrow = n.S, ncol = n.U)
	SU <- rbind(cbind(SU.num, zero.SU), cbind(zero.SU,SU.den))

	diag.UU <- rep(0, times = n.U)
	diag.UU[cdu == 1] <- rho1*sqrt(nugget1.num*nugget1.den)
	diag.UU[cdu == 2] <- rho2*sqrt(nugget2.num*nugget2.den)
	diag.UU <- diag(diag.UU, nrow = n.U, ncol = n.U)
	UU <- rbind(cbind(UU.num, diag.UU), cbind(diag.UU, UU.den))

	#------------------------- FINITE POPULATION BLOCK KRIGING

	ind.sa <- !is.na(data[, sampled] == 1) & data[, sampled] == 1
	ns <- sum(ind.sa)
	ind.un <- is.na(data[, sampled] == 1) | data[, sampled] != 1
	nu <- sum(ind.un)
	z <- rbind(matrix(data[ind.sa, column.numerator], nrow = ns, ncol = 1),
		matrix(data[ind.sa, column.denominator], nrow = ns, ncol = 1))
	ind.NA <- is.na(z)
	z[ind.NA] <- 0
	area.s <- matrix(data[ind.sa, area], nrow = ns, ncol = 1)
	z <- z/rbind(area.s,area.s)
	area.tot <- matrix(data[, area], nrow = nu+ns, ncol = 1)
	B <- matrix(data[, column.pred], ncol = 1)
	B[is.na(B)] <- 0
	Bs <- diag(2) %x% B[ind.sa]
	Bu <- diag(2) %x% B[ind.un]
	X1 <- as.numeric(as.integer(data[,strat])==1)
	X2 <- as.numeric(as.integer(data[,strat])==2)
	X <- cbind(rep(1,times=length(X1)),X1,X2)
	Xs <- diag(2) %x% X[ind.sa,]
	Xu <- diag(2) %x% X[ind.un,]
	SSi <- solve(SS)

	# ----------------------- PREDICTIONS

	part1 <- Xs %*% mginv(t(Xs) %*% SSi %*% Xs)
	part2 <- t(Xu) - t(Xs) %*% SSi %*% SU
	D <- SU + part1 %*% part2
	FF <- SSi %*% D
	Ao <- Bs + FF %*% Bu
	y.est <- t(Ao) %*% z
	y.est <- mean(area.tot)*y.est
	num.est <- y.est[1]
	den.est <- y.est[2]

	# ---------------------- COVARIANCE MATRIX

	part1 <- t(FF) %*% SS %*% FF
	part2 <- t(FF) %*% SU
	y.var <- t(Bu) %*% ( part1 - part2 - t(part2) + UU ) %*% Bu * mean(area.tot)^2
	num.se <- sqrt(y.var[1,1])
	den.se <- sqrt(y.var[2,2])

	# ---------------------- RATIO ESTIMATE AND VARIANCE

	rat.est <- y.est[1]/y.est[2]
	derv <- matrix(c(1/y.est[2],-y.est[1]/y.est[2]^2), ncol = 1)
	rat.se <- sqrt(t(derv) %*% y.var %*% derv)

	# ---------------------- CONFIDENCE INTERVALS

	num.ci80 <- c(num.est - num.se * qnorm(0.9), num.est + num.se *
		qnorm(0.9))
	num.cipm80 <- (num.se * qnorm(0.9))/num.est
	num.ci90 <- c(num.est - num.se * qnorm(0.95), num.est + num.se *
		qnorm(0.95))
	num.cipm90 <- (num.se * qnorm(0.95))/num.est
	num.ci95 <- c(num.est - num.se * qnorm(0.975), num.est + num.se *
		qnorm(0.975))
	num.cipm95 <- (num.se * qnorm(0.975))/num.est

	den.ci80 <- c(den.est - den.se * qnorm(0.9), den.est + den.se *
		qnorm(0.9))
	den.cipm80 <- (den.se * qnorm(0.9))/den.est
	den.ci90 <- c(den.est - den.se * qnorm(0.95), den.est + den.se *
		qnorm(0.95))
	den.cipm90 <- (den.se * qnorm(0.95))/den.est
	den.ci95 <- c(den.est - den.se * qnorm(0.975), den.est + den.se *
		qnorm(0.975))
	den.cipm95 <- (den.se * qnorm(0.975))/den.est

	rat.ci80 <- c(rat.est - rat.se * qnorm(0.9), rat.est + rat.se *
		qnorm(0.9))
	rat.cipm80 <- (rat.se * qnorm(0.9))/rat.est
	rat.ci90 <- c(rat.est - rat.se * qnorm(0.95), rat.est + rat.se *
		qnorm(0.95))
	rat.cipm90 <- (rat.se * qnorm(0.95))/rat.est
	rat.ci95 <- c(rat.est - rat.se * qnorm(0.975), rat.est + rat.se *
		qnorm(0.975))
	rat.cipm95 <- (rat.se * qnorm(0.975))/rat.est

	outpt <- list(
		numerator = column.numerator,
		numerator.estimate = num.est,
		numerator.se = num.se,
		numerator.conf.int.80 = num.ci80,
		numerator.ci.prop.mean.80 = num.cipm80,
		numerator.conf.int.90 = num.ci90,
		numerator.ci.prop.mean.90 = num.cipm90,
		numerator.conf.int.95 = num.ci95,
		numerator.ci.prop.mean.95 = num.cipm95,
		denominator = column.denominator,
		denominator.estimate = den.est,
		denominator.se = den.se,
		denominator.conf.int.80 = den.ci80,
		denominator.ci.prop.mean.80 = den.cipm80,
		denominator.conf.int.90 = den.ci90,
		denominator.ci.prop.mean.90 = den.cipm90,
		denominator.conf.int.95 = den.ci95,
		denominator.ci.prop.mean.95 = den.cipm95,
		ratio.estimate = rat.est,
		ratio.se = rat.se,
		ratio.conf.int.80 = rat.ci80,
		ratio.ci.prop.mean.80 = rat.cipm80,
		ratio.conf.int.90 = rat.ci90,
		ratio.ci.prop.mean.90 = rat.cipm90,
		ratio.conf.int.95 = rat.ci95,
		ratio.ci.prop.mean.95 = rat.cipm95,
		sample.sizes = n.strat.df,
		total.samples = N.strat.df,
		sampled.area = areas.strat.df,
		total.area = areato.strat.df,
		strat.1.name = data.frame(Name = levels(data[,strat])[1]),
		strat.2.name = data.frame(Name = levels(data[,strat])[2]),
		numerator.counted = data.frame(stratum=levels(data[,strat]),counted=num.counted),
		denominator.counted = data.frame(stratum=levels(data[,strat]),counted=den.counted),
		numerator.empirical.semivariogram.strat1 = emp.var1.num[,1:3],
		numerator.empirical.semivariogram.strat2 = emp.var2.num[,1:3],
		denominator.empirical.semivariogram.strat1 = emp.var1.den[,1:3],
		denominator.empirical.semivariogram.strat2 = emp.var2.den[,1:3],
		numerator.covariance.parameters.strat1 = parmest1.num,
		numerator.covariance.parameters.strat2 = parmest2.num,
		denominator.covariance.parameters.strat1 = parmest1.den,
		denominator.covariance.parameters.strat2 = parmest2.den,
		ratio.cov=y.var[1,2]
	)
	outpt
}


# Get estimate for whole survey
#' Title
#'
#' @param data
#' @param AA
#' @param metric
#'
#' @return
#' @export
#'
#' @examples
results<-function(data,AA = NULL, metric = "totalmoose"){
  for (i in 1:ncol(data)) if(!is.factor(data[,i]) && !isUnknown(data[,i])) data[is.na(data[,i]),i] <- 0
  data$columnpred <- 1
  column.pred <- ifelse(is.null(AA), "columnpred", paste(AA))
  column.ana<-metric
  column.unitid<-"Intid"
  column.ana.formula<- paste("[",metric,"]", sep = "")
  strat<-"Strat"
  area<-"AreaMi"
  column.lat<-"centrlat"
  column.lon<-"centrlon"
  sampled<-"Counted"
  data$SurveyID <- 1000
  column.surveyid<-"SurveyID"
  column.pred<-make.names(column.pred)
  column.ana<-make.names(column.ana)
  strat<-make.names(strat)
  area<-make.names(area)
  column.lat<-make.names(column.lat)
  column.lon<-make.names(column.lon)
  sampled<-make.names(sampled)
  column.surveyid<-make.names(column.surveyid)

  GSPE_calc.out<-geo.moosepop(column.ana = column.ana, strat = strat,data = data,
                              sampled = sampled, area = area,column.pred = column.pred,
                              column.lat=column.lat, column.lon=column.lon)
  return(GSPE_calc.out)
}


################
# Get Bull:Cow Ratio and by area and for total area
################
#' Title
#'
#' @param data
#' @param AA
#' @param num
#' @param denom
#'
#' @return
#' @export
#'
#' @examples
results.comp<-function(data,AA = NULL,num,denom){
  for (i in 1:ncol(data)) if(!is.factor(data[,i]) && !isUnknown(data[,i])) data[is.na(data[,i]),i] <- 0
  data$columnpred <- 1
  column.pred <- ifelse(is.null(AA), "columnpred", paste(AA))
  column.ana<-"totalmoose"
  column.unitid<-"Intid"
  column.ana.formula<-"[totalmoose]"
  strat<-"Strat"
  area<-"AreaMi"
  column.lat<-"centrlat"
  column.lon<-"centrlon"
  sampled<-"Counted"
  data$SurveyID <- 1000
  column.surveyid<-"SurveyID"
  column.numerator <- paste(num)
  column.denominator <- paste(denom)
  column.numerator <- make.names(column.numerator)
  column.denominator <- make.names(column.denominator)
  column.pred<-make.names(column.pred)
  column.ana<-make.names(column.ana)
  strat<-make.names(strat)
  area<-make.names(area)
  column.lat<-make.names(column.lat)
  column.lon<-make.names(column.lon)
  sampled<-make.names(sampled)
  column.surveyid<-make.names(column.surveyid)

  gspe.comp <- geo.moosepop.composition(column.numerator=column.numerator, column.denominator=column.denominator,
                                        strat=strat, data=data, sampled=sampled, area=area,
                                        column.pred=column.pred, column.lat=column.lat, column.lon=column.lon)

  return(gspe.comp)
}

#' Makes abundance table
#'
#' @param data
#' @param column_names
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
AA_abundance <- function(data, column_names = NULL,...) {
  AA<-c("columnpred",column_names)
  out.all <- list()

  # Run through all AA's and get estimates
  for(i in 1:length(AA)){
    a <- results(data,paste(AA[i]), metric = ...)
    out.all[[i]] <- a
  }

  # Extract all the estimates for AA's and their standard errors
  aa.out <- matrix(nrow=length(AA),ncol=10)

  for(i in 1:length(AA)){
    aa.out[i,2:4] <- round(out.all[[i]]$estimate.total)
    aa.out[i,5:7] <- round(out.all[[i]]$estimate.standard.error)
    aa.out[i,8:10] <- round(out.all[[i]]$ci.prop.mean.90,3)
    aa.out[i,1] <- AA[i]
  }
  aa.out[1,1]<-"Total Survey Area"
  aa.out <- data.frame(aa.out)
  names(aa.out) <- c("Area","Total.Est","High.Est","Low.Est","Total.SE",
                     "High.SE","Low.SE","Tot.RP@90","High.RP@90","Low.RP@90")

  for(i in 2:10){
    aa.out[,i]<-as.numeric(as.character(aa.out[,i]))
  }

  list(table = aa.out,
       results_list = out.all)

}

# function to get Total survey and AA estimates and standard errors in tables
#' Title
#'
#' @param data
#' @param column_names
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
AA_tables <- function(data, column_names = NULL) {
  AA<-c("columnpred",column_names)


total<-AA_abundance(data, column_names, metric = "totalmoose")
bull<-AA_abundance(data, column_names, metric = "TotalBulls")
cow<-AA_abundance(data, column_names, metric = "TotalCows")
calf<-AA_abundance(data, column_names, metric = "TotalCalves")



  # Assign a column of ones to get total bull:cow ratio
  data$pred <- 1

  ########################
  # Get all bull:cow and calf:cow ratios using results.comp funciton
  ######################

  comp.all.bullcow <- list()

  # Run through all AA's and get estimates
  for(i in 1:length(AA)){
    a <- results.comp(data,paste(AA[i]),'TotalBulls','TotalCows')
    comp.all.bullcow[[i]] <- a
  }

  # Extract all the ratio estimates for AA's and their standard errors
  comp.out.bullcow <- matrix(nrow=length(AA),ncol=4)

  for(i in 1:length(AA)){
    comp.out.bullcow[i,2] <- round(comp.all.bullcow[[i]]$ratio.estimate,3)
    comp.out.bullcow[i,3] <- round(comp.all.bullcow[[i]]$ratio.se,3)
    comp.out.bullcow[i,4] <- round(comp.all.bullcow[[i]]$ratio.ci.prop.mean.90,3)
    comp.out.bullcow[i,1] <- AA[i]
  }
  comp.out.bullcow[1,1]<- "Total Survey Area"


  comp.out.bullcow <- data.frame(comp.out.bullcow)
  names(comp.out.bullcow) <- c("Area","Ratio.Est","Ratio.SE","Ratio.RP@90")

  for(i in 2:4){
    comp.out.bullcow[,i]<-as.numeric(as.character(comp.out.bullcow[,i]))
  }


  ########################
  # Get all calf:cow ratios using using results.comp funciton
  ######################
  comp.all.calfcow <- list()

  # Run through all AA's and get estimates
  for(i in 1:length(AA)){
    a <- results.comp(data,paste(AA[i]),'TotalCalves','TotalCows')
    comp.all.calfcow[[i]] <- a
  }

  # Extract all the ratio estimates for AA's and their standard errors
  comp.out.calfcow <- matrix(nrow=length(AA),ncol=4)

  for(i in 1:length(AA)){
    comp.out.calfcow[i,2] <- round(comp.all.calfcow[[i]]$ratio.estimate,3)
    comp.out.calfcow[i,3] <- round(comp.all.calfcow[[i]]$ratio.se,3)
    comp.out.calfcow[i,4] <- round(comp.all.calfcow[[i]]$ratio.ci.prop.mean.90,3)
    comp.out.calfcow[i,1] <- AA[i]
  }
  comp.out.calfcow[1,1]<- "Total Survey Area"

  comp.out.calfcow <- data.frame(comp.out.calfcow)
  names(comp.out.calfcow) <- c("Area","Ratio.Est","Ratio.SE","Ratio.RP@90")

  for(i in 2:4){
    comp.out.calfcow[,i]<-as.numeric(as.character(comp.out.calfcow[,i]))
  }
  return(list(total_abundance = total$table,
              bull_abundance = bull$table,
              cow_abundance = cow$table,
              calf_abundance = calf$table,

              total_results = total$results_list,
              bull_results = bull$results_list,
              cow_results = cow$results_list,
              calf_results = calf$results_list,

              bullcow = comp.out.bullcow,
              calfcow = comp.out.calfcow))
}

