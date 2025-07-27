####種数-面積.r
####################################################
############　　立山植生モニタリング調査集計 　　　　
####################################################

### ワーキング・ディレクトリの設定
wd<-"../../立山植生モニタリング事業/第03期/平成25年度/植生調査"
setwd(wd)
### 関連関数読み込み　Domin_Krajina他
source("../../00/立山植生モニタリング事業/第03期/平成24年度/植生調査/立山植生source.R")

##野帳ファイルリスト読み込み
dd<-read.csv("植生野帳01-10.csv",as.is=TRUE,fileEncoding = "shift-jis")
rn_plt<-which(substr(dd$プロット,1,1)=="_")
site_code<-substr(dd$プロット[rn_plt],2,999)

##########
ql<-c(10,10,10,10,5,10,10,5,3,10)
spn<-c();

for(ii in 1:10)
{

d2<-subset(dd,プロット==as.character(ii))
#nrow(d2);str(d)

subp<-unique(d2$サブプロット)

for (j in 1:10){
	subp_<-sample(subp,10000,replace = T)
	spn0<-c();spn1<-c()
	subpn<-length(subp)
	for (i in 1:length(subp)){
	spn0<-c(spn0,length(unique(d2$種名[is.element(d2$サブプロット,subp_[1:i])])))
					}
	spn1<-rbind(spn1,spn0)

		}

spn<-c(spn,list(cbind((1:subpn)*ql[ii]^2,apply(spn1,2,mean))))

}

#windows()
plot(0,type="n",xlim=c(0,3500),ylim=c(0,150),xlab="Area (m^2)",ylab="Number of Species")
for (i in 1:10)lines(spn[[i]],col=i)
for (i in 1:10)text(tail(spn[[i]],1)+c(300,0),site_code[i],col=i)

###### ササの合計面積


sasa3<-c();sasa2<-c();

for(ii in 1:10)
{
d2<-subset(dd,プロット==as.character(ii))
subpn<-length(unique(d2$サブプロット))
d2[is.na(d2)]<-0;d2[d2==""]<-0;d2[d2=="."]<-0
for (i in c(1,2,5,6,7))d2[,i]<-as.numeric(d2[,i])	### 被度データ数値化
sasa3<-c(sasa3,sum(d2[d2$種名=="チシマザサ"　|d2$種名=="クマイザサ",7])/subpn)
sasa2<-c(sasa2,sum(d2[d2$種名=="チシマザサ"　|d2$種名=="クマイザサ",6])/subpn)
}

#windows()
plot(sasa3)
for (i in 1:10)text(i,sasa3[i],site_code[i],col=i)
points(1:10,sasa2,col="red")


###### 解析対象種　最低1プロットで出現頻度25%以上を記録した種

spl<-table(dd$種名)
spl<-data.frame(sp=spl[order(spl,decreasing = TRUE)])
spl2<-rownames(spl[1:100,])

### 取組みまだ
l<-read.csv("C:/00D/00/vegetation/R/tateyama_flora.csv",as.is=TRUE)
names(l)
plot(table(l$form))
fm<-l$form[match(spl2,l$spj)]
table(fm)

spl3<-data.frame(sp=spl2,freq=as.vector(spl[1:100,]),fm)
spl3[order(spl3$fm),]





