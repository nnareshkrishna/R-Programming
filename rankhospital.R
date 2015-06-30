
rankhospital <- function(state, outcome, num="best"){
	if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    		stop("invalid outcome")
  	}
	res <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	res <- res[res$State==state,]
	if(! ( state %in% levels(factor(res$State)) ) ) {
 	   stop("invalid state") 
 	}
	if(outcome=="heart attack"){
		res <- res[,c(2,11)]	
	}
	else if(outcome=="heart failure"){
		res <- res[,c(2,17)]
	}
	else if(outcome=="pneumonia"){
		res <- res[,c(2,23)]
	}
	names(res)[2] <- "Deaths"
	res[,2] <- suppressWarnings(as.numeric(res[,2]))
	res <- res[!is.na(res$Deaths),]
	res = res[order(res$Deaths, res$Hospital.Name),]
	if(num=="best")
		res <- res[1,"Hospital.Name"]
	else if(num=="worst"){
		res <- tail(res,1)
		res <- res[1,"Hospital.Name"]
	}
	else
		res <- res[num,"Hospital.Name"]
	return (res)
}