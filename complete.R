complete <- function(directory , id) {
	nobs <- function(id){
		loc <- paste(directory,"/",sprintf("%03d",id),".csv", sep="")
		ans <- sum(complete.cases(read.csv(loc)))
		return (ans)
	}
	#return (count_true(loc[1]))	
	return (data.frame(id=id,nobs=sapply(id, nobs)))
}