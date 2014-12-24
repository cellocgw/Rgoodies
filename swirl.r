# Let's swirl a picture! 

# This version removes the interpolation in favor of 
# rounding or truncation of rotated coordinates.  It's 
# much faster (and is what's provided on some websites) but quality
# suffers for very small images
#
# side note for future barrel/pin tool: want quadratic delta magnif.

# TODO
#
#	2) allow 3D data, such as RGB images. Only the interp() needs to be done 3X; the
#		radmat and thetmat never change.
#	3) change max radius of twist (see notes at corner)
#	4) add option to twist max at radius R, with both edges and center
#		of image untwisted.
#	5) allow for hex-coded images. This could blow up into allowing
#		objects of class RGB, etc., but for starters do something like
#		rgbpixel<-hex2RGB(hexpixel); do averages or whatever on the
#		rgb values in rgbpixel@coords; newpix<-hex(rgbpixel) -- 
#		might be easiest to build r,g,b matrices and hack them separately
#
#
##  Inputs
#	picture: a 2D array to be manipulated. I need to fix things so
#		this works flawlessly with color values as well as numeric.
#	radius: if desire twist less than full-image, set radius < 1
#	maxrad: radius at which twisting is max. Set >0 to have a swirl
#		which decreases to zero both at edge and center
#	rate: crude measure of the max twist angle
#	linear: not used yet. Someday maybe will allow other delta twist funcs
#	extrap: not used yet-- I think it's a holdover from "swirl," which
#		did a lot of linear interpolation.
swirl<-function(picture, radius=1, maxrad=0, rate=2*pi, linear=TRUE, extrap=FALSE, ...) { 
corner<-dim(picture)
doit <- min(corner)
# when the code to rotate reduced range is implemented, turn next line on.
# Have to watch proper index ranges, etc.  
#turnit <- min(corner,corner*radius)

# extract largest square region.
fullpic<-picture
slice <- abs(corner[1]-corner[2])
if(slice) {
	left <- trunc(slice/2)
	right <- left + slice%%2  # in case odd number of rows or cols
	if (corner[1]>corner[2]) {
		# more rows than cols; note that if we got here, slice is nonzero
		picture<-picture[left:(left+doit-1),] 
		}else  picture <- picture[,left:(left+doit-1)]
	}
# The matrix center is at (dim + 1)/2 regardless of even or odd.
radmat<- abs(outer(0:(doit-1), 0:(doit-1), function(x,y) sqrt((x-(doit-1)/2)^2 + (y-(doit-1)/2)^2) )  )
#   convert image xy to r-theta.  theta is more or less
#  arctan( rownum-center,colnum-center) 
thetmat <- atan2(row(picture)-(doit+1)/2,col(picture)-(doit+1)/2)
 radedge <- round(min(dim(picture))/2)
#
# This applies zero rotation only at radmat==max(radmat). I want to 
# apply zero rotation whenever radmat >= edge/2 , 
#  this--> calc 'radedge' as min. of x,y edge,
#  allow twist to be max at non-origin.
# separate upgrade when implement "turnit" as the max radius of
# twisted region: I think just replacing 'radedge'  or
#  'maxrad' with min(radedge_or_maxrad,turnit)
#
###  ok, I fouled something up here - even w/ maxrad==0, i.e. 
### default, theoutput is borked.  Compare w/ cswirl code and fix that
### first.  Then fix the maxrad>0 code
if(maxrad) {
#	thetmat as in default case for radmat >= maxrad, and 
#	in reverse from maxrad to origin (so no twist at origin
	thetmat[radmat>=maxrad]<- thetmat[radmat>=maxrad] + rate * (pmax(0,(radedge-radmat[radmat>=maxrad])/max(radmat)) )
	thetmat[radmat<maxrad] <- thetmat[radmat<maxrad] +rate*(radmat[radmat<maxrad]/max(radmat))  
	}else {
	#do the default case
		thetmat <- thetmat + rate * (pmax(0,(radedge-radmat)/max(radmat)) )
	}

# Convert rho-theta back to x-y space. 
#  R-matrices are just vectors anyway
# drift: I think I need to apply a 0.5 offset when there's an odd 
# number of rows or columns. naaah, that only matters for very 
# small rotations on very small matrices (rounding error)
# aaaand: "x" is rows, so oddly enough that is cos(thetmat),
# not sin, I fear
newx<-radmat*sin(thetmat)
newy<-radmat*cos(thetmat)
# map the original image data to the new coordinates
#  do the coordinate truncate/rounding.  Load into a matrix that
# is initalized w/ NA to see if there are 'holes'. 
#  need to map where each pixel came
# from, which is the row/col index of newx/y , and move said pixel to the
# value in that index location (for x and y) .
# translate the origin back, since my output is +/- some value.
# put a border of a couple pixels to be "cautious" 
yrow <- doit + 1
xcol <- doit + 1
#  And pre-calc the "rounded" matrix
roundx <- round(newx)
roundy <- round(newy)
# hmmm... if NA is such a pain, how about setting to -1 and checking
# against zero?
symat<-matrix( -1,nr=yrow, nc=xcol )
# hmmm.. can't I do this double loop as a simple vector op?
# yes, using array
roundz <- cbind(as.vector(roundx),as.vector(roundy))
roundz <- roundz[(abs(roundz[,1]) <= doit & abs(roundz[,2]) <= doit),]
roundz <- roundz + round(min(corner)/2+1)  # I think that'll offset OK 
# so change this first
symat[roundz]<-as.vector((picture[1:min(corner),1:min(corner)] ))
# now "plug in" this symat into original picture
if(slice) {
	if (corner[1]>corner[2]) {
		# more rows than cols; note that if we got here, slice is nonzero
		# also remember symat is bigger by a row&column, so have to bind
		fullpic<-rbind(fullpic[1:left,], symat[,1:doit],fullpic[(corner[1]-right):corner[1],])
				#fullpic[left:(left+doit),] <-symat
		}else {
			fullpic<-cbind(fullpic[,1:left], symat[1:doit,],fullpic[,(corner[2]-right):corner[2]])
		} #fullpic[,left:(left+doit)] <- symat
	symat<-fullpic #just to simplify output code
}
# note that if they're equal, do nothing. 
#  Now just need to fill in the NA values.  aka the -1 vals
nalocs <- which(symat < 0 , arr.ind=TRUE)
#  nalocs<-which(is.na(symat),arr.ind=TRUE)
#damn edges...remove them from processing region
nalocs<-nalocs[(nalocs[,1]>1 & nalocs[,1]< nrow(symat)),]
nalocs<-nalocs[(nalocs[,2]>1 & nalocs[,2]< ncol(symat)),]
# rumor has it this is the slowest part of the whole function.
#   sum(x)/length(x) is 10x faster
# than mean(x) for small data sizes like this.
for(j in 1:nrow(nalocs)) {
	nax <- nalocs[j,2]
	nay <- nalocs[j,1]
# forgot about the actual count if there are NA values.
#
#but.... something goes "uh-uh" with my sum/length calculation vs
# the mean() calculation. yeah -- is.numeric doesn't do what I thought,
# to say the least.OK, with default values now -1 , fast
# comparison is x[x>=0]
#cleaner would be to feed a 4x2 matrix of coords, but not worth it.
#anyway, all this hacking is still faster than computing !is.na .
	naybor<-c(symat[nay-1,nax],symat[nay+1,nax],symat[nay,nax-1],symat[nay,nax+1])
	naybor<-naybor[naybor>=0]
# 2 diagnostic lines for debug only
#sum(naybor)/length(naybor)
#mean(c(symat[nay-1,nax],symat[nay+1,nax],symat[nay,nax-1],symat[nay,nax+1]), na.rm=TRUE)
	symat[nay,nax]<-sum(naybor)/length(naybor)
#symat[nay,nax]<-mean(c(symat[nay-1,nax],symat[nay+1,nax],symat[nay,nax-1],symat[nay,nax+1]), na.rm=TRUE)
	}
# But since I'd filled with a plottable value, need to get rid of it
symat[symat <0 ] <- NA
# Since mean(nothing_at_all) returns NaN,  let's get rid of those.
symat[is.nan(symat)]<-NA 
return(invisible(symat))
}