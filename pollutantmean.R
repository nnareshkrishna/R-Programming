pollutantmean <- function(directory,pollutant,id=1:332){
	q <- id[id>=100]
	q <- c(q,paste("0",id[id>=10 & id<100], sep=""))
	q <- c(q,paste("00",id[id<10], sep=""))
	q <- q[q!="00"]
	q <- q[q!="0"]
	loc <- paste(directory,"/",q,".csv", sep="")
	x1 <- lapply(loc,read.csv)
	x <- do.call(rbind,x1)
	y <- x[,pollutant]
	z <- y[!is.na(y)]
	ans <-mean(z)
	round(ans,3)
}
	