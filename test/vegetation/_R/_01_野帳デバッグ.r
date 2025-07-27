####################################################
###  野帳デバッグ.r

###  【目的】解析用・次回野帳データの作成
###　	種名入力ミスの確認　
###　	階層整理　
###  	重複種統合
###	ファイル保存

###################【設定】>>>>>>>>>>>>>>>>>>

### ワーキング・ディレクトリ
#wd<-"m:/00D/00/立山植生モニタリング事業/第04期/平成30年度/植生調査/R/美松"
#setwd(wd)
dir()

###　野帳データ
#yachou<-"R02植生調査野帳_ブナ坂.csv"   #### デバック前
#yachou2<-"02ブナ坂-植生野帳2020.csv" 　#### デバック後

#yachou<-"2021植生調査野帳_弥陀ヶ原.csv"   #### デバック前
#yachou2<-"05弥陀ヶ原2021.csv" 　#### デバック後

#yachou<-"2021植生調査野帳_美女平.csv"   #### デバック前
#yachou2<-"01美女平2021.csv" 　#### デバック後
dir()
yachou<-"R04植生調査野帳_ブナ平.csv"   #### デバック前
yachou2<-"04ブナ平2023.csv" 　#### デバック後


###################【設定】<<<<<<<<<<<<<<<<<<<

##野帳データファイル読み込み   
(d<-read.csv(yachou,as.is=TRUE,fileEncoding="SHIFT-JIS"))
#(d<-read.csv(yachou,as.is=TRUE)) #utf-8
#edit(d)
d[is.na(d)]<-""
data.frame(names(d))

##### 被度データの数値化
col<-c(4,5,6,7,8)   				#### I, II, III,VI 期のデータのコラム
names(d)[col]
nn<-d[,col]
nn[nn==""]<-0
for (i in 1:ncol(nn))nn[,i]<-as.numeric(nn[,i])
which(is.na(nn),arr.ind = TRUE)
nn[is.na(nn)]<-0					###　ピリオドやスペースになっているところを修正
str(nn)
d[,col]<-nn
str(d)							### 確認

### dに野帳のオリジナル通し番号をid_noteとして追加
d<-data.frame(d,id_note=1:nrow(d))
d$階層
##　
####　階層限定 新たな野帳データを　dd　とする
(dd<-subset(d, 階層!="A1" &  階層!="A2" &  階層!="B1" & 階層!="L"))
names(dd)
###############################################################
####　クマイザサ　→　チシマザサに統合 !!!!!!!!!!! 上の小平のみ
sp<-dd$種名
#dd$種名[sp=="クマイザサ"]<-"チシマザサ"
###############################################################


#####　立山植物目録と野帳の種名確認
flora<-read.csv( "../第Ⅴ期植物目録.csv",as.is=TRUE,fileEncoding="SHIFT-JIS")
names(flora)
spl<-data.frame(table(dd$種名))
spl<-data.frame(spl,spl=flora$sp[match(spl[,1],flora$spj)])
(spna<-spl[is.na(spl$spl),]) ### 長さ0であればすべて目録にあることを示す
## write.table(spna,"clipboad")　
(undit_sp <- as.vector(spna$Var1) )
dd<-dd[!is.element(sp,undit_sp ),] ###リストにない種は除外
sp<-dd$種名



###　種の生活型の入力
i<-match(dd$種名,flora$spj)
(spfm<-flora$form[i])
names(table(flora$form))
sptype<-data.frame(
コード=c("bl","bs","cds","cl","cs","ds","f","h","l","sasa","w"),
生活型=c("広葉樹高木","広葉樹低木","ハイマツ","針葉樹高木","針葉樹低木","矮性低木","シダ","草本","蔓","ササ","水面"),
階層=c("B2","B2","B2","B2","B2","C","C","C","L","S","w"))
dd$種名[which(is.na(match(spfm,sptype$コード)))] ###  character(0)　となるまで修正
(cls2<-sptype$階層[match(spfm,sptype$コード)])
(dd$階層<-cls2)
table(cls2)



####　サブプロット 	NA入力
(subp<-dd$サブプロット)
for (i in 1:length(subp)){if(subp[i]==""){subp[i]<-subp[i-1]}}
(dd$サブプロット<-subp)
names(dd)

## 並べ替え
(dd<-dd[order(as.numeric(dd$サブプロット),dd$階層,dd$種名),])

##　階層"L"種の除外　元野帳で"L"となっておらずsubsetのフィルタリングでもれたもの
(dd<-dd[dd$階層!="L",])



###　同一サブプロット内重複記載種の統合
###　基礎変数取り込み

(subp<-dd$サブプロット)
(sp<-dd$種名)
(subp_<-unique(subp))

for (ii in 1:length(subp_)){### ii<-25 length(subp_)
	(spsubp_tab<-table(sp[subp_[ii]==subp]))
	(spdupl<-names(spsubp_tab[spsubp_tab>1])) #### 一つのサブプロットに重複のある種
i<-1
  while (i<=length(spdupl)){## i<-2
	spdupl[i]
	(j<-which(subp==subp_[ii] & sp==spdupl[i]))	####　dd通しでの重複種の番号
	goukei<-colSums(dd[subp==subp_[ii] & spdupl[i]==sp,col])
	dd[j[1],col]<-goukei				### 重複データ先頭行j[1]に統合
	dd$id_note[j[1]]<-paste(dd$id_note[j],collapse="_")
	dd$備考[j[1]]<-paste(dd$備考[j],collapse="_")
	dd<-dd[-j[-1],] 				### 重複データ先頭行j[-1]以外の重複行の削除 
	subp<-dd$サブプロット
	sp<-dd$種名
	i<-i+1
				}
}

## 並べ替え
(dd<-dd[order(as.numeric(dd$サブプロット),dd$階層,dd$種名),])

##edit(dd)

####　write.csv(dd,yachou2,row.names = FALSE)

####  edit(read.csv(yachou2))

