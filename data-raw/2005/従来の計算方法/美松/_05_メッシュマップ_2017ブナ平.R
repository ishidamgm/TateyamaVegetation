######　メッシュマップと分布相関.R
#2015上ノ小平
#ワーキング・ディレクトリの設定
wd<-wd<-"C:/00D/00/立山植生モニタリング事業/第04期/平成28年度/植生調査/R/松尾峠"
　setwd(wd)
dir()
(f<-dir(,pattern="^被度表"))#　dir(,pattern=".csv$")
ddd<-c()
for (iii in 1:4)ddd<-c(ddd,list(read.csv(f[iii],as.is=TRUE)))
#####　マトリクス
dim(ddd[[3]])
names(ddd[[1]])
		##jj<-4:28	## データ部分の列範囲
 		## image(matrix(1:28,4,7,byrow=TRUE))		　### ブナ坂
		##mx<-function(vv)matrix(vv,4,7,byrow=TRUE)　　   ### ブナ坂

		##image(matrix(rev(1:25),5,5,byrow=FALSE))	      ### 上ノ小平
		##mx<-function(vv)matrix(rev(vv),5,5,byrow=FALSE)　　 ### 上ノ小平

		##image(matrix(1:25,5,5,byrow=FALSE))			### 鏡石
		##mx<-function(vv)matrix(vv,5,5,byrow=FALSE)　　 		### 

		jj<-4:23	## データ部分の列範囲
		image(matrix(1:20,2,10,byrow=TRUE))			### 松尾峠
		mx<-function(vv)matrix(vv,2,10,byrow=TRUE)　　 		### 

spmap<-function(sp,c,iii){###iii<-4;sp<-"チシマザサ";c<-"S"
	d<-ddd[[iii]]
	image(mx(-as.numeric(d[d$種名==sp & d$階層==c,jj])),main=paste(sp,iii,"期"),axes=FALSE)
	}

spmap2<-function(sp,c,iii){
	d<-ddd[[iii]]
	image(mx(-as.numeric(d[d$種名==sp & d$階層==c,jj])),main=paste(sp,iii,"期"),axes=FALSE)#,zlim=c(0,5)
	box()
	}


####　
windows()
par(mfrow=c(3,3),mar=c(2,4,2,1),mgp=c(2,1,1))###
for(iii in 2:4)spmap2("チシマザサ","S",iii)
for(iii in 2:4)spmap2("オオシラビソ","B2",iii)
for(iii in 2:4)spmap2("オオカメノキ","B2",iii)


for(iii in 2:4)spmap2("ウリハダカエデ","B2",iii)

iii<-2
par(mfrow=c(4,5),mar=c(2,4,2,0),mgp=c(2,1,0))###
spmap("ブナ","A2",iii)
spmap("ブナ","B1",iii)
spmap("ブナ","B2",iii)
spmap("スギ","B2",iii)
spmap2("ヤマソテツ","C",iii)
spmap("シノブカグマ","C",iii)
spmap("チシマザサ","S",iii)

#####  windows()
VVV<-c()
for (iii in 1:4){
d<-ddd[[iii]]
VVV<-c(VVV,list(
	data.frame(B2=colSums(d[d$階層=="B2",jj]),C=colSums(d[d$階層=="C",jj]),S=colSums(d[d$階層=="S",jj]))
		))}
VVV
par(mfrow=c(3,3),mar=c(2,4,2,0),mgp=c(2,1,0))###
for (iii in 2:4){
image(mx(VVV[[iii]]$S),main="S層",axes=FALSE);box()
image(mx(VVV[[iii]]$B2),main="B2層",axes=FALSE);box()
image(mx(VVV[[iii]]$C),main="C層",axes=FALSE);box()
		}

par(mfrow=c(3,3),mar=c(2,4,2,0),mgp=c(2,1,0))###
for (iii in 2:4){
image(mx(VVV[[iii]]$S),main="S層",zlim=c(0,50),axes=FALSE);box()
image(mx(VVV[[iii]]$B2),main="B2層",zlim=c(0,50),axes=FALSE);box()
image(mx(VVV[[iii]]$C),main="C層",zlim=c(0,10),axes=FALSE);box()
		}

par(mfrow=c(3,3),mar=c(2,4,2,0),mgp=c(2,1,0))###
for (iii in 2:4){
contour(mx(VVV[[iii]]$S),main="S層",axes=FALSE);box()
contour(mx(VVV[[iii]]$B2),main="B2層",axes=FALSE);box()
contour(mx(VVV[[iii]]$C),main="C層",axes=FALSE);box()
		}

lapply(VVV,colSums)

#### 分布相関
xy<-data.frame(v=cut(V1,breaks=seq(0,5,1),include.lowest =TRUE),V2)
xy
plot(xy)
hist(V2)

