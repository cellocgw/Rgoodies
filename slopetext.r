# Plot symbols oriented to local slope.
# Interesting problem: if underlying plot has some arbitrary aspect ratio,
# retrieve by doing: Josh O'B via SO:  
# myasp <- with(par(),(pin[2]/pin[1])/(diff(usr[3:4])/diff(usr[1:2])))
# so make that the default value of argument 'asp'
# Default is 'plotx' is vector of indices at which to 
# plot symbols.  If is_indices=FALSE, only then turn to splinefun to 
# calculate y-values and slopes; and user beware.
#
# 6 Feb 2014: added default col arg so can stick in a color vector if desired
# TODO
#
slopetext<-function(x,y,plotx, mytext, is_indices=TRUE, asp=with(par(), (pin[1]/pin[2])*(diff(usr[3:4])/diff(usr[1:2]))),offsetit= 0, col='black', ...) {
if (length(x) != length(y)) stop('data length mismatch')
if (!is.numeric(c(x,y,plotx) ) ) stop('data not numeric')
if(is_indices) {
	# plotting at existing points.
	if(any(plotx<=1) | any(plotx>= length(x))) {
		warning("can't plot endpoint; will remove")
		plotx<-plotx[(plotx>1 & plotx<length(x))]
	}
	lows<-plotx-1
	highs<-plotx+1
	# then interpolate low[j],high[j] to get slope at x2[j]
	slopes <- (y[highs]-y[lows])/(x[highs]-x[lows])  #local slopes
	# sign(highlow)  fix the rotation problem 
	angles <- 180/pi*atan(slopes/asp)  + 180*(x[lows] > x[highs] )
	intcpts <- y[highs]-slopes*x[highs]   
	ploty <- intcpts + x[plotx]*slopes
	# change name, so to speak, to simplify common plotting code
	plotx<-x[plotx]
	}else{
	#interpolating at plotx values
		if  (any(plotx<min(x)) | any(plotx>max(x)) ) {
			warning("can't plot extrapolated point; will remove")
			plotx<-plotx[(plotx>min(x) & plotx<max(x))]
		}
		spf<-splinefun(x,y)
		ploty<-spf(plotx)
		angles <- 180/pi * atan(spf(plotx,1)/asp) #getting first deriv, i.e. slope
	} #end of else
xlen<-length(plotx) # better match y and mytext
# The trouble is: srt rotates about some non-centered value in the text cell
# Dunno what to do about that.
dely <- offsetit*cos(angles)
delx <- offsetit*sin(angles)
# srt must be scalar
mytext<-rep(mytext,length=xlen)
col <- rep(col,length=xlen)
for (j in 1:xlen) text(plotx[j], ploty[j], labels=mytext[j], srt= angles[j], adj=c(delx,dely),col=col[j], ...)
}