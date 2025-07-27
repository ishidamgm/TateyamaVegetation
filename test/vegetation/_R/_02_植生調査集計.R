####################################################
############　 野帳デバッグ→集計　　　#############

#### 頻度_平均被度　追加　2015　10/25
#### 2020/9/13
rm(list=ls())
ls()
###################【設定】>>>>>>>>>>>>>>>>>>

### ワーキング・ディレクトリの設定
#wd<-"m:/00D/00/立山植生モニタリング事業/第04期/平成30年度/植生調査/R/美松"
#setwd(wd)
dir()

###　野帳データ
yachou_<-"04ブナ平2023.csv" 　#### デバック後


###################【設定】<<<<<<<<<<<<<<<<<<<


### 関連関数読み込み　Domin_Krajina他
source("../立山植生source.R",encoding="SHIFT-JIS")


##データファイル読み込み   (複数ファイルを集計するコードを基にしているので冗長)
f<-c(yachou_)
(site_code<-gsub(".csv","",f))



iii<-1;
####
(d<-read.csv(f[iii],as.is=TRUE))
d[is.na(d)]<-""
names(d)


#####　立山目録の追加と種の生活型
flora<-read.csv( "../第Ⅴ期植物目録.csv",as.is=TRUE, fileEncoding = "SHIFT-JIS")
names(flora)
i<-match(d$種名,flora$spj)
(qq<-d$種名[is.na(i)])　#### write.csv(qq,"立山目録にない種.csv")　<<<　解析に入る前になくす
flora$form[i]



#### 基礎変数取り込み
	col<-c(4,5,6,7,8)   				#### I, II, III,VI 期のデータのコラム
	coln<-length(col)

	(yr<-substr(names(d)[col],3,999))　	##　
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

###############   頻度_平均被度  #######################
t_<-ML[[1]]
form_<-flora$form[match(t_$種名,flora$spj)]
(t_1<-data.frame(種名=t_$種名,階層=t_$階層,form=form_))
### 頻度と平均被度の計算
freq<-function(v)100*sum(v!=0)/length(v)
for (ii in 1:length(yr)){
	m<-as.matrix(ML[[ii]][,c(-1,-2)])
	t_1<-data.frame(t_1,平均被度=apply(m,1,mean),頻度=apply(m,1,freq))
	names(t_1)[2*ii+2]<-paste("V",yr[ii],sep="")
	names(t_1)[2*ii+3]<-paste("F",yr[ii],sep="")
				}

######　解析対象
i<-t_1$階層=="B2" | t_1$階層=="C" | t_1$階層=="L" | t_1$階層=="S"
j<-rowSums(t_1==0)==0 #### Ⅰ～Ⅳ期続けて記録がある種
(解析対象<-ifelse(i & j,1,0)) ### 
################

頻度_平均被度<-data.frame(t_1,解析対象)

########################################################

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


	##names(total)[seq(3,2+coln*2,2)]<-paste("頻度",yr,"年",sep="")
	##names(total)[seq(4,3+coln*2,2)]<-paste("総合優占度",yr,"年",sep="")

	names(total)[seq(3,2+coln*2,2)]<-paste("f",yr,sep="")
	names(total)[seq(4,3+coln*2,2)]<-paste("v",yr,sep="")

	## 
	##total<-total[order(total$階層,- total[,ncol(total)]),]

	###### 被度表の保存
	for(ii in 1:coln){
　		(fn<-paste("被度表-",site_code[iii],"-",names(ML)[ii],".csv",sep=""))
	 	write.csv(ML[[ii]],fn)
				}

	###### 被度階級表の保存

	for(ii in 1:coln){
　		(fn<-paste("被度階級表-",site_code[iii],"-",names(DKL2)[ii],".csv",sep=""))
	 	write.csv(DKL2[[ii]],fn)
			}

	##### 総合優占度の保存
	write.csv(total,paste("総合優占度-",site_code[iii],".csv",sep=""))

	##### 総合優占度_頻度_平均被度 の保存
	total2<-data.frame(total,頻度_平均被度)
	write.csv(total2,paste("総合優占度_頻度_平均被度-",site_code[iii],".csv",sep=""))

#<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<  計算ループ終了
#<<<<<<<<<<<<<<<<<< 

dir()




