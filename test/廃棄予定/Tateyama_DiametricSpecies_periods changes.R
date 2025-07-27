###@Tateyama_DiametricSpecies_test.R

#### 2022/7/8 google doc "Tateyama"
#### Ÿ‚Ì‰Û‘è


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


dset<-function(d,jj=1){
#ii<-1
	#d<-dd[[ii]]
	d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col[jj]]))
	(dsp<-diam_sp(d_$sp,d_$dbh))
	return(dsp$NN)
}


dbh_col=c("d01","d02","d03","d04","d05","d06")
th_lbl=c("1st","2nd","3rd","4th","5th","6th")

fig_DiametricSpecies_6periods<-function(ii=1){
  d<-dd[[ii]]
  plot(0,type="n",col=1,xlim=c(0,700),ylim=c(0,90),
  xlab="Individuals",ylab="Diametric Species",
  main=plt$na[ii])

  for (jj in 1:6){
    d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col[jj]]))
print(nrow(d_))
    dsp<-diam_sp(d_$sp,d_$dbh)
    NN<-dsp$NN
    lines(NN[,2],type="l",col=jj)
                  }
}

windows()
fig_DiametricSpecies_6periods(8)
text(seq(100,600,100),90,1:6,col=1:6)

##
windows()
par(mfrow=c(3,3))
for(ii in 1:8)fig_DiametricSpecies_6periods(ii)

plot(1:10,1:10,type="n",axes=F,xlab="",ylab="")
for (i in 1:6){
	lines(c(3,8),c(i+2,i+2),col=i)
	text(rep(1.5,6),i+2,th_lbl[i],col=i)
}


###################################################

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

###############
######
#2022/6/29.R
dbh_col="Bijodaira"

dset<-function(d){
#ii<-1
	#d<-dd[[ii]]
	d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col]))
	(dsp<-diam_sp(d_$sp,d_$dbh))
	return(dsp$NN)
}

NN<-dset(dd[3])

plot(NN[,2],type="l",col=1,xlim=c(0,700),ylim=c(0,90),
xlab="Individuals",ylab="Diametric Species",
 main="Diversity of Diametric Species (Gonzalo & Timo, 2020)")

for(ii in 4:length(dd)){
ddd<-dd$ "Bijodaira"[ii]
ddd_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col]))
  dsp<-diam_sp(d_$sp,d_$dbh)
  NN<-dsp$NN
  lines(NN[,2],type="l",col=ii)
}
legend(400,50,paste0(plt$na,"_",plt$alt,"m"),lty=1,col=1:nrow(plt))


########
names(dd)
dd$ "Bijodaira" 
d
names(d)


