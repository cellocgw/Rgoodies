# Let's wiggle a picture! 
# like swirl, but create a "memory flashback" effect.
# so here, rate is number of cycles per image height
# and mag is peak deflection, so to speak.
wiggle<-function(picture, rate=5, mag= .1, linear=TRUE, extrap=FALSE, ...) {
require(akima, quietly=TRUE)
picdim<-dim(picture)
#KISS-  first get the sin wave
sinvec<- sin(2*pi*rate/picdim[1]*(1:picdim[1]) )
# now adjust +/-1 to mag
sinvec<- sinvec*mag
#create an x,y,z dataset with coordinates 
# this is more sensible
wigx <- col(picture)
wigy<- row(picture)
for(j in 1:picdim[1]) wigx[j,]<-wigx[j,]+sinvec[j]
xscale<-trunc(min(wigx)):trunc(max(wigx))
yscale<-wigy[,1]  
#
#  better, embed the image in a white border and
# then cut it back afterwards.
# 
newimage<-interp(wigx, wigy, (picture), xscale, yscale, linear=linear, extrap=extrap, duplicate='mean',  ... ) 
newimage<-t(newimage)  #too lazy to fix it thefirst time
return(invisible(newimage))
}