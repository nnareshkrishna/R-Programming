corr <- function(directory,threshold=0) {
	file_corr <- function(fname){
		data<-read.csv(file.path(directory,fname))
		nobs<-sum(complete.cases(data))
		if(nobs > threshold){
			return (cor(data$nitrate,data$sulfate,use="complete.obs"))
		}
	}
	total_corr <- sapply(list.files(directory),file_corr)
	total_corr <- unlist(total_corr[!sapply(total_corr,is.null)])
	return (total_corr)
}