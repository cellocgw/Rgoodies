hasAnyPwr  <- function(maxn,  init = NULL ){
# feed previous seq as init
if (is(init,'NULL')){
	theseq = as.bigz(2) # 0, 1 are their own powers
	maxvec <- as.bigz(c(3:maxn) )  
}else{
#build unused out of what's not in init
	theseq <- as.bigz(init)
	maxvec <- as.bigz(3:maxn)
	if (length(theseq) >=length(maxvec)){
		return(invisible(list(theseq = init, unused = NULL)))
	}
	idx <- NULL
	for (jb in 1:length(theseq)){
		gotit <- which(maxvec == theseq[jb])
		idx <- c(idx,gotit)
	}
	 maxvec <- maxvec[-idx]
}
unused <- maxvec
finished = FALSE
while (length(unused) && !finished) {
	canlen = length(unused)
	for (jn in 1:canlen){
		addit = TRUE  # means the number meets the rules, add to seq 
	#  this is a 1,2,3,...N step lookback test.
		seqlen <- length(theseq)
		maybe <- unused[jn]  #next item to try
#  if maybe fails, remove it from unused since it'll always fail
		testout <- testb(factorize(maybe))
		if (testout$foundit){
			unused <- unused[-jn]
			break
		} 
		for (jtry in (seqlen):1 ){
			maybe <- maybe + theseq[jtry]
			testout <-testb(factorize(maybe))
			if(testout$foundit){
				addit=FALSE
				break  # to jn loop
			}
		} #jtry
		if(addit) {
			theseq <- c(theseq,unused[jn])
			unused <- unused[-jn]
# need this break to reset jn
			break # to while loop
		}
		if( jn == canlen) finished = TRUE 
	} #jn 
} #end while
#
return(invisible(list(theseq = theseq, unused = unused)))  #, thepowers=thepowers)))
}

# helper function
testb <- function(barf) {
barl <- rle(as.numeric(barf)) # already sorted
bart <-  barl$lengths 
thisprime = 2 
foundit = FALSE
while (!foundit) {
	if ( sum(floor(bart/thisprime) == bart/thisprime) == length(bart))  {
		foundit = TRUE
		} else {
			thisprime = as.numeric(nextprime(thisprime))
		}
	if (thisprime > min(bart)) break
}
return(invisible(list(foundit = foundit, thisprime=thisprime)))	
}