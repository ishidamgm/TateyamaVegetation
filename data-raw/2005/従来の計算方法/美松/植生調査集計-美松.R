####################################################
############　　　　　　　野帳デバッグ→集計 　　　　#############


### ワーキング・ディレクトリの設定
wd<-"C:/00D/00/立山植生モニタリング事業/第03期/平成24年度/植生調査/美松"
setwd(wd)
dir()

### 関連関数読み込み　Domin_Krajina他
source("../立山植生source.R")

##データファイル読み込み   (複数ファイルを集計するコードを基にしているので冗長)
f<-c("06美松-植生野帳2012.csv")
(site_code<-matrix(unlist(strsplit(f,"-")),ncol=2,byrow=T)[,1])
iii<-1;
####
(d<-read.csv(f[iii],as.is=TRUE))
d[is.na(d)]<-""
d
names(d)

#### 基礎変数取り込み
	col<-c(4,5,6)   				#### I, II, III期のデータのコラム
	coln<-length(col)

	(yr<-substr(names(d)[col],3,999))　	##　DK1999	被度2005	被度2011
	##str(d)
	sp<-d$種名;cls<-d$階層;plt<-d$サブプロット
#####
(sp_cls_tab<-table((factor(sp)),(factor(cls))))


	##################################
	###　入力用マトリクス　m　の作成
	##################################

	sp_<-levels(factor(sp)); sp_<-sp_[sp_!=""]
	cls_<-levels(factor(cls)); cls_<-cls_[cls_!=""]
	plt_<-levels(factor(plt)); plt_<-plt_[plt_!=""]

	cls_sp_all<-c()
	for(i in 1:length(cls_)){
		cls_sp<-levels(factor(sp[cls==cls_[i]]))
		cls_sp<-cbind(rep(cls_[i],length(cls_sp)),cls_sp)
		cls_sp_all<-rbind(cls_sp_all,cls_sp)
				}

	cn<-length(plt_)
	rn<-nrow(cls_sp_all)
	nm<-c(c("階層","種名"),plt_)



	###   データ格納とファイル出力

	lab<-names(d)[col]


	###　第一期は被度のデータなくDomin_Krajina

	ML<-list()


	for(ii in 1:coln){
		### ii<-1
		fn<-lab[ii]
		cv<-as.numeric(d[,col[ii]]);cv[is.na(cv)]<-0
		m<-data.frame(cls_sp_all,matrix(0,rn,cn));names(m)<-nm
			for (i in 1:rn){
				r<-which(d$階層==m$階層[i] & d$種名==m$種名[i])
			for (j in 1:length(r)) m[i,nm==plt[r][j]]<-m[i,nm==plt[r][j]]+cv[r][j]
						}
			m[is.na(m)]<-0

			ML<-c(ML,list(m))
				}

	names(ML)<-lab

	###　Domin_Krajina　被度階級表の作成

	DKL<-ML

	for(ii in 2:coln){
		m_DK<-ML[[ii]]
		for (i in 1:rn){
			for (j in 3:length(nm)){
				m_DK[i,j]<-Domin_Krajina(m_DK[i,j])
							}
					}
			DKL[[ii]]<-m_DK
				}


	###　Domin_Krajina　被度階級表の集計追加

	DKL2<-DKL
	for(ii in 1:coln){
		m_DK<-DKL2[[ii]]
		j<-3:length(nm)
		頻度<-rep(0,rn)
		for (i in 1:rn)頻度[i]<-length(which(m_DK[i,j]!=0))/cn
		総合優占度<-rep(0,rn)
		for (i in 1:rn)総合優占度[i]<-sqrt(mean(as.numeric(m_DK[i,j]))*頻度[i])/sqrt(10)*100
		m_DK2<-data.frame(m_DK,頻度=unlist(lapply(頻度,hindo)) ,総合優占度)
		DKL2[[ii]]<-m_DK2
				}


	###### 総合優占度の変化

	total<-data.frame(階層=DKL2[[1]]$階層,種名=DKL2[[1]]$種名,
	頻度=DKL2[[1]]$頻度,総合優占度=DKL2[[1]]$総合優占度)

	for(ii in 2:coln){
		total<-data.frame(total,頻度=DKL2[[ii]]$頻度,総合優占度=DKL2[[ii]]$総合優占度)
			}


	names(total)[seq(3,2+coln*2,2)]<-paste("頻度",yr,"年",sep="")
	names(total)[seq(4,3+coln*2,2)]<-paste("総合優占度",yr,"年",sep="")

	## 
	total<-total[order(total$階層,- total[,ncol(total)]),]

	###### 被度表の保存
	for(ii in 1:coln){
　		(fn<-paste("被度表-",site_code[iii],"-",names(ML)[iii],".csv",sep=""))
	 	write.csv(ML[[ii]],fn)
				}

	###### 被度階級表の保存

	for(ii in 1:coln){
　		(fn<-paste("被度階級表-",site_code[iii],"-",names(DKL2)[ii],".csv",sep=""))
	 	write.csv(DKL2[[ii]],fn)
			}

	##### 総合優占度の保存
	write.csv(total,paste("総合優占度-",site_code[iii],".csv",sep=""))



#<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<  計算ループ終了
#<<<<<<<<<<<<<<<<<< 


####　出力ファイル確認

dir()

#### 総合優占度作図

##　フォルダ設定

## setwd("./出力結果")


(f<-dir(pattern="総合優占度*"))
(site_code<-substr(f,7,nchar(f)-4))
col<-c(5,7,9);coln<-length(col)


typ<-c("A1","A2","B1","B2","C","L","S")
typc<-c("red","orange","blue","skyblue","darkgreen","purple","green")


par(mfrow=c(1,1))

for (iii in 1:length(f)){
　	### iii<-2
	d<-read.csv(f[iii] )　
	sp<-d$種名;cls<-d$階層;cls_<-levels(factor(cls))
	yr<-substr(names(d)[col],6,10)
	dm<-c();for (i in col) dm<-cbind(dm,tapply(d[,i],d$階層,sum))
	colnames(dm)<-yr
	clsc<-cls_;for (i in 1:length(clsc)) clsc[i]<-typc[cls_[i]==typ] 
	w<-0.9;s<-.2
	barplot(dm,legend=rownames(dm),ylab="総合優占度",width =w,space=s,
		main=site_code[iii],col=clsc,xlim=c(0,coln+1.5))

if(0){
  	d.seg.xs <- rep((s + w) * 1:(ncol(dm) - 1), each=nrow(dm))     # 線分の始点のx座標
  	d.seg.xe <- rep((s + w) * 2:ncol(dm) - w, each=nrow(dm))       # 線分の終点のx座標
  	d.seg.ys <- apply(dm[,1:(ncol(dm) - 1)], 2, cumsum)            # 線分の始点のy座標
  	d.seg.ye <- apply(dm[,2:ncol(dm)], 2, cumsum)                  # 線分の終点のy座標
  	segments(d.seg.xs, d.seg.ys, d.seg.xe, d.seg.ye)
		}
	
				}


