rankall <- function(outcome,num="best"){
	if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    		stop("invalid outcome")
	}
	res <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	if(class(num) == "character"){
   		 if (! (num == "best" || num == "worst")){
      		stop("invalid number")
    		}
  	}
	if(outcome=="heart attack"){
		res <- res[,c(2,7,11)]
	}
	else if(outcome=="heart failure"){
		res <- res[,c(2,7,17)]
	}
	else
		res <- res[,c(2,7,23)]
	names(res)[3]="Deaths"
	res[,3]=suppressWarnings(as.numeric(res[,3]))
	res <- res[!is.na(res$Deaths),]
	res_split=split(res,res$State)
	split_func <- function(x,num){
		x = x[order(x$Deaths, x$Hospital.Name),]
    		# Return
    		if(class(num) == "character") {
     			if(num == "best") {
       			return (x$Hospital.Name[1])
     			}
     			else if(num == "worst") {
       			return (x$Hospital.Name[nrow(x)])
      		}
    		}
    		else{
      		return (x$Hospital.Name[num])
    		}
	}
	ans=lapply(res_split,split_func,num)
	return (data.frame(hospital=unlist(ans),s	tate=names(ans)) )
}