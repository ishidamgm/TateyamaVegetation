###@Tateyama_DiametricSpecies_test.R

load("—§R–ˆ–Ø’²¸_dd_plt.Rdata")


##### diametric-species 


 diam_sp1<-function(sp,dbh){
  Species <- unique(sp)
  SpeciesNumber <- length(Species)
  cls <- seq(0,110,5)					# 5cm‚İ‚Ì’¼ŒaŠK‚ğì¬
  ds <- table(sp,cut(dbh,cls))				#@Še÷í‚Ì’¼ŒaŠK•Ê–{”‚Ìˆê——•\)
  DiametrinSpecies <- t(ds)
  DiametrinSpeciesNumber <- length(ds[ds!=0])
  df<-data.frame(SpeciesNumber ,DiametrinSpeciesNumber)	
  return(list(Species, DiametrinSpecies,df))
				}

diam_sp<-function(sp,dbh){

  d<-diam_sp1(sp,dbh)
  Species<-d$Species ; DiametrinSpecies<-d$DiametrinSpecies ;df <-d$df
  n <- length(sp)
  NN<-c()
  for (ii in 1:n){
  i<-sample(n,ii)
  NN<-rbind(NN,diam_sp1(sp[i],dbh[i])[[3]])
							
		    }

    return(list(sp=Species, DiametrinSpecies=DiametrinSpecies,N=df,NN=NN))
}


#z<-diam_sp(sp,dbh)$NN
#plot(z[,2],type="l")
#lines(z[,1],col="blue")

##### save(dd,file="dd.RData")

d$d06
names(d)

dbh_col="d05"

dset<-function(d){
#ii<-1
	#d<-dd[[ii]]
	d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col]))
	(dsp<-diam_sp(d_$sp,d_$dbh))
	return(dsp$NN)
}

NN<-dset(dd[[1]])

plot(NN[,2],type="l",col=1,xlim=c(0,700),ylim=c(0,90),
xlab="Individuals",ylab="Diametric Species",
 main="Diversity of Diametric Species (Gonzalo & Timo, 2020)")


for (ii in 2:length(dd)){
  d<-dd[[ii]]
  #j<-which(substr(names(d),1,1)=="D")[1]
  d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col]))
  dsp<-diam_sp(d_$sp,d_$dbh)
  NN<-dsp$NN
  lines(NN[,2],type="l",col=ii)
}





legend(400,50,paste0(plt$na,"_",plt$alt,"m"),lty=1,col=1:nrow(plt))








