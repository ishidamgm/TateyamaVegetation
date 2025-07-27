###Å@Tateyama_BA_periods changes.R

BA_calc <-function(d,jj=1){
#ii<-1
	#d<-dd[[ii]]
	d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col[jj]]))
	ba <- pi*(d_$dbh/200)^2
	return(sum(ba)/plt$Area[ii]*10000)
}


BA <- matrix(0,8,6)
for (ii in 1:8){
  for(jj in 1:6){
	d<-dd[[ii]]
	BA[ii,jj] <- BA_calc(d,jj)
		   }
		  }
#### absolute
 
ii <- 1 ; plot(BA[1,],type="b" , lty=ii,col=ii,ylim=c(0,90))
for (ii in 2:8)lines(BA[ii,],type="b",lty=ii,col=ii)

#### ratio

BAr <- BA/BA[,1]

ii <- 1 ; plot(BAr[1,],type="b" , lty=ii,pch=ii,col=ii,ylim=c(.8,1.2),xlab="period",ylab="Basal area ratio")
abline(h=1)
for (ii in 2:8)lines(BAr[ii,],type="b",lty=ii,pch=ii,col=ii)


legend(1,0.95,plt$na,pch=1:8,col=1:8)



