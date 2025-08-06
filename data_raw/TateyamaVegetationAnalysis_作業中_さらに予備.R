# # TateyamaVegetationAnalysis.R

# library(TateyamaVegetation)
# library(dplyr)
# library(tidyr)

# usethis::use_data()




#' 生野帳データの階層クラスの確認、修正、未入力データの補完をします
#'　サブプロット、階層、種名の順でソートします。
#'　サブプロットに同一種が複数ある場合は被度を合算し統一します
#'
#' @param d
#' @param sp_colnm
#' @param KAISO_colnm
#'
#' @returns
#' @export
#'
#' @examples
#' d<-FieldNote_Arimine2019_raw
#' FieldNote_CheckCorrect(d)
#'
FieldNote_CheckCorrect<- function(d=FieldNote){

  #　サブプロット・欠落の補完
  i.na<-which(is.na(d$サブプロット))
  for(i in i.na)d$サブプロット[i]<-d$サブプロット[i-1]

  #階層の確認と補完
  fl.i<-match(d$種名,  flora$spj)
  d$階層<-sptype$階層[match(flora$form[fl.i],sptype$コード)]
  d<-d[order(d$サブプロット,d$階層,d$種名),]

  # 被度の数値化
  d <- d %>%
    mutate(across(matches("^(被度|DK)"), ~ as.numeric(trimws(.))))


  # union and combine coverage data of duplicated species in a subplot#

  d <- d %>%
    group_by(サブプロット, 階層, 種名) %>%
    summarise(
      across(matches("^(被度|DK)"), ~ sum(.x, na.rm = TRUE)),
      # X = first(X),
      # no = first(no),
      # II_ = first(II_),
      .groups = "drop"
    )


  return(d)
}


#' Title
#'
#' @param d
#' @param period
#'
#' @returns
#' @export
#'
#' @examples
#' d <- subset(vv,plot=="Arimine")
#' vt<-VegetationTable(d,period="c04")
#' head(vt)
#'
VegetationTable <- function(d=subset(vv,plot=="Arimine"),period="c04"){
  # 組成表　vt:VegetationTable ####
  vt <- d %>%
    select(subplot, sp,period) %>%
    tidyr::pivot_wider(
      names_from = subplot,
      values_from = period,
      values_fill = 0  # 欠損を0にする（合計表などに便利）
    )

      # 頻度　VegetationTable ####
      vt %>%
        rowwise() %>%
        mutate(
          平均被度 = mean(c_across(-sp), na.rm = TRUE),
          出現頻度 = mean(c_across(-sp) > 0, na.rm = TRUE),
          頻度 =hindo(出現頻度 ),
          総合優占度 = 100 * sqrt(平均被度 * 出現頻度) / sqrt(10)
        ) %>%
        ungroup() ->vt

  return(vt)

}


#
#
# # 組成表　Domin_Krajina変換　VegetationTable ####
#
#
#     VegetationTable<- vt %>%
#       mutate(across(-種名,‾ Domin_Krajina(.)))
#
#     VegetationTable
#
#     # 頻度　VegetationTable ####
#     VegetationTable <- VegetationTable %>%
#       rowwise() %>%
#       mutate(
#         平均被度 = mean(c_across(-種名), na.rm = TRUE),
#         出現頻度 = mean(c_across(-種名) > 0, na.rm = TRUE),
#         頻度 =hindo(出現頻度 ),
#         総合優占度 = 100 * sqrt(平均被度 * 出現頻度) / sqrt(10)
#       ) %>%
#       ungroup()



#' csvとして入力された植生野帳を整えます
#' 1.　スプレッドシートで入力された野帳のcsvデータ(utf-8)の読み込み
#' 2.　種名のチェック　
#' 3.　階層の自動入力(新規記入分は階層が記入されていないため自動入力)
#' 4.　重複して記載された酒を合算して統一
#' 5.　空白行の除去
#' 6.　解析用に整えられたcsvの保存
#'
#'
#'
#' @param InputFileName 　　野帳入力データのファイル名　(utf-8 のcsvファイルで保存してください)
#' @param OutputFileName 　 解析用に整えられた野帳データの保存　csvファイル名
#' @param
#'
#' @returns
#'
#' @export
#'
#' @importFrom dplyr group_by summarise across starts_with mutate
#'
#' @examples
#' #　ワーキング・ディレクトリをセットしてください
#' # setwd("2025/美松")
#' dir()
#'
#'
#'
#'
analysis000_FieldNoteInput <- function(InputFileName="H31植生調査野帳_有峰.csv",OutputFileName="06美松-植生野帳2018.csv"){
  # species name check ####


  InputFileName="H31植生調査野帳_有峰.csv"
  setwd("‾/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2025/TateyamaVegetation/data_raw/2025/有峰")

  d <- readr::read_csv("H31植生調査野帳_有峰.csv")
  # FieldNote_Arimine2019_raw<-d
  # save(FieldNote_Arimine2019_raw,file="FieldNote_Arimine2019_raw.RData")



  SpeciesNameCheck(d$種名)



# # 組成表　vt:VegetationTable ####
#      vt <- d %>%
#       select(サブプロット, 種名, 被度2019) %>%
#       tidyr::pivot_wider(
#         names_from = サブプロット,
#         values_from = 被度2019,
#         values_fill = 0  # 欠損を0にする（合計表などに便利）
#       )
#
#
#
#
# # 組成表　Domin_Krajina変換　VegetationTable ####
#
#
#     VegetationTable<- vt %>%
#       mutate(across(-種名,‾ Domin_Krajina(.)))
#
#     VegetationTable
#
#     # 頻度　VegetationTable ####
#     VegetationTable <- VegetationTable %>%
#       rowwise() %>%
#       mutate(
#         平均被度 = mean(c_across(-種名), na.rm = TRUE),
#         出現頻度 = mean(c_across(-種名) > 0, na.rm = TRUE),
#         頻度 =hindo(出現頻度 ),
#         総合優占度 = 100 * sqrt(平均被度 * 出現頻度) / sqrt(10)
#       ) %>%
#       ungroup()


}


#
# # 02_植生調査集計.R ######
# ## 野帳デバッグ→集計　　　#############
#
# ## 初期化　####
# rm(list=ls())
# ls()
# ##【設定 #################
#
# ### ワーキング・ディレクトリの設定　####
# #wd<-"m:/00D/00/立山植生モニタリング事業/第04期/平成30年度/植生調査/R/美松"
# #setwd(wd)
# dir()
#
# ###　野帳データ ####
# #yachou_<-"09浄土2024.csv" 　#### デバック後
#
# yachou_<-"04上ノ小平2024.csv" 　#### デバック後
#
# ## 関連関数読み込み　Domin_Krajina他 ####
# source("../立山植生source.R") #,encoding="SHIFT-JIS")
#
#
# ##データファイル読み込み ####  (複数ファイルを集計するコードを基にしているので冗長)
# f<-c(yachou_)
# (site_code<-gsub(".csv","",f))
#
#
#
# iii<-1;
# ####
# (d<-read.csv(f[iii],as.is=TRUE))
# d[is.na(d)]<-""
# names(d)
#
#
# #　立山目録の追加と種の生活型 ####
# flora<-read.csv( "../第5期植物目録.csv",as.is=TRUE)
# names(flora)
# i<-match(d$種名,flora$spj)
# (qq<-d$種名[is.na(i)])　#### write.csv(qq,"立山目録にない種.csv")　<<<　解析に入る前になくす
# flora$form[i]
#
#
#
# ## 基礎変数取り込み ####
# 	col<-c(4,5,6,7,8)   				#### I, II, III,VI ,???期のデータのコラム
# 	coln<-length(col)
#
# 	(yr<-substr(names(d)[col],3,999))　	##　
# 	##str(d)
# 	sp<-d$種名;cls<-d$階層;plt<-d$サブプロット
#
# (sp_cls_tab<-table((factor(sp)),(factor(cls))))
#
#
#
# ##　入力用マトリクス　m　の作成　####
#
# 	sp_<-levels(factor(sp)); sp_<-sp_[sp_!=""]
# 	cls_<-levels(factor(cls)); cls_<-cls_[cls_!=""]
# 	plt_<-levels(factor(plt)); plt_<-plt_[plt_!=""]
#
# 	cls_sp_all<-c()
# 	for(i in 1:length(cls_)){
# 		cls_sp<-levels(factor(sp[cls==cls_[i]]))
# 		cls_sp<-cbind(rep(cls_[i],length(cls_sp)),cls_sp)
# 		cls_sp_all<-rbind(cls_sp_all,cls_sp)
# 				}
#
# 	cn<-length(plt_)
# 	rn<-nrow(cls_sp_all)
# 	nm<-c(c("階層","種名"),plt_)
#
#
#
# 	###   データ格納とファイル出力 ####
#
# 	lab<-names(d)[col]
#
#
# 	###　第一期は被度のデータなくDomin_Krajina ####
#
# 	ML<-list()
#
#
# 	for(ii in 1:coln){
# 		### ii<-1
# 		fn<-lab[ii]
# 		cv<-as.numeric(d[,col[ii]]);cv[is.na(cv)]<-0
# 		m<-data.frame(cls_sp_all,matrix(0,rn,cn));names(m)<-nm
# 			for (i in 1:rn){
# 				r<-which(d$階層==m$階層[i] & d$種名==m$種名[i])
# 			for (j in 1:length(r)) m[i,nm==plt[r][j]]<-m[i,nm==plt[r][j]]+cv[r][j]
# 						}
# 			m[is.na(m)]<-0
#
# 			ML<-c(ML,list(m))
# 				}
#
# 	names(ML)<-lab
#
# #   頻度_平均被度  #####
# t_<-ML[[1]]
# form_<-flora$form[match(t_$種名,flora$spj)]
# (t_1<-data.frame(種名=t_$種名,階層=t_$階層,form=form_))
#
# ## 頻度と平均被度の計算
# freq<-function(v)100*sum(v!=0)/length(v)
# for (ii in 1:length(yr)){
# 	m<-as.matrix(ML[[ii]][,c(-1,-2)])
# 	t_1<-data.frame(t_1,平均被度=apply(m,1,mean),頻度=apply(m,1,freq))
# 	names(t_1)[2*ii+2]<-paste("V",yr[ii],sep="")
# 	names(t_1)[2*ii+3]<-paste("F",yr[ii],sep="")
# 				}
#
# ##　解析対象 ####
# i<-t_1$階層=="B2" | t_1$階層=="C" | t_1$階層=="L" | t_1$階層=="S"
# j<-rowSums(t_1==0)==0 #### ???〜??? W期続けて記録がある種
# (解析対象<-ifelse(i & j,1,0)) ###
#
# 頻度_平均被度<-data.frame(t_1,解析対象)
#
#
#
# ##　Domin_Krajina　被度階級表の作成 ####
#
# 	DKL<-ML
#
# 	for(ii in 2:coln){
# 		m_DK<-ML[[ii]]
# 		for (i in 1:rn){
# 			for (j in 3:length(nm)){
# 				m_DK[i,j]<-Domin_Krajina(m_DK[i,j])
# 							}
# 					}
# 			DKL[[ii]]<-m_DK
# 				}
#
#
# ##　Domin_Krajina　被度階級表の集計追加 ####
#
# 	DKL2<-DKL
# 	for(ii in 1:coln){
# 		m_DK<-DKL2[[ii]]
# 		j<-3:length(nm)
# 		頻度<-rep(0,rn)
# 		for (i in 1:rn)頻度[i]<-length(which(m_DK[i,j]!=0))/cn
# 		総合優占度<-rep(0,rn)
# 		for (i in 1:rn)総合優占度[i]<-sqrt(mean(as.numeric(m_DK[i,j]))*頻度[i])/sqrt(10)*100
# 		m_DK2<-data.frame(m_DK,頻度=unlist(lapply(頻度,hindo)) ,総合優占度)
# 		DKL2[[ii]]<-m_DK2
# 				}
#
#
# ## 総合優占度の変化 #####
#
# 	total<-data.frame(階層=DKL2[[1]]$階層,種名=DKL2[[1]]$種名,
# 	頻度=DKL2[[1]]$頻度,総合優占度=DKL2[[1]]$総合優占度)
#
# 	for(ii in 2:coln){
# 		total<-data.frame(total,頻度=DKL2[[ii]]$頻度,総合優占度=DKL2[[ii]]$総合優占度)
# 			}
#
#
# 	##names(total)[seq(3,2+coln*2,2)]<-paste("頻度",yr,"年",sep="")
# 	##names(total)[seq(4,3+coln*2,2)]<-paste("総合優占度",yr,"年",sep="")
#
# 	names(total)[seq(3,2+coln*2,2)]<-paste("f",yr,sep="")
# 	names(total)[seq(4,3+coln*2,2)]<-paste("v",yr,sep="")
#
#
# ## total<-total[order(total$階層,- total[,ncol(total)]),]
#
# ## 保存　#####
# ### 被度表の保存 #####
# 	for(ii in 1:coln){
# 　		(fn<-paste("被度表-",site_code[iii],"-",names(ML)[ii],".csv",sep=""))
# 	 	write.csv(ML[[ii]],fn)
# 				}
#
# ### 被度階級表の保存 #####
#
# 	for(ii in 1:coln){
# 　		(fn<-paste("被度階級表-",site_code[iii],"-",names(DKL2)[ii],".csv",sep=""))
# 	 	write.csv(DKL2[[ii]],fn)
# 			}
#
# 	##### 総合優占度の保存
# 	write.csv(total,paste("総合優占度-",site_code[iii],".csv",sep=""))
#
# 	##### 総合優占度_頻度_平均被度 の保存
# 	total2<-data.frame(total,頻度_平均被度)
# 	write.csv(total2,paste("総合優占度_頻度_平均被度-",site_code[iii],".csv",sep=""))
#
# #<<<<<<<<<<<<<<<<<<
# #<<<<<<<<<<<<<<<<<<  計算ループ終了
# #<<<<<<<<<<<<<<<<<<
#
# dir()
#
#
# #　03_総合優占度-頻度-平均被度図.R ####
#
# #ワーキング・ディレクトリの設定   ⇒　なし
# #　R ver.4 以降　utf-8　デフォルトとなり　RStudio をメインとする
# #　wd内のプロジェクト.Rpoj　起動でwdがカレントになるのでsetwd(wd)　必要なくなる
#
#
# ## 総合優占度_頻度_平均被度 の読み込み　##################
#
# (d<-read.csv(dir(".",pattern="総合優占度_頻度_平均被度*"),as.is=TRUE))
# #
# # (d<-read.csv("総合優占度_頻度_平均被度-09浄土山2024.csv",as.is=TRUE))
# (t1<-table(d$階層))
# paste(names(t1),"層",t1,"種",collapse =",",sep="")
#
# ## 報告書用作表 ####
# #(d<-read.csv(dir(".",pattern="総合優占度_頻度_平均被度*"),as.is=TRUE))
# (集計<-ifelse(d$解析対象==1 & !is.na(d$解析対象),"*",""))
# options(digits=2)
# data.frame(names(d))
# ## 階層	種名	生活型	出現頻度　(％) 1-5 頻度階級	1-5	平均被度　(％) 1-5	総合優占度1-5	  ####
#
# j<-c(2,3,16,18,20,22,24,26,4,6,8,10,12,17,19,21,23,25,5,7,9,11,13)
# #j<-c(2,3,8,10,12,14,16)##, 14,16,18,20,22,4,6,8,10,15,17,19,21,5,7,9,11,13)
# names(d)[j]
# (dtab<-data.frame(d[,j],集計))
# ## 集計 ####
# names(dtab)
# # i 今年度記録あり　TRUE
# i<-dtab[,(ncol(dtab)-1)]!=0; table(dtab[i,]$階層)
# # 　今年度記録なし
# data.frame(dtab$種名[!i],dtab$階層[!i])
#
# ## write "報告書用-頻度_被度_優占度表.csv" ########
# write.csv(dtab,"報告書用-頻度_被度_優占度表.csv")
#
# ## 解析　####
# # d0<-d
# d<-d[d$解析対象==1 & !is.na(d$解析対象),]
# t1 ## paste(names(t1),t1)
# (t2<-table(d$階層)) ## paste(names(t2),t2)
#
# col<-c(5,7,9,11,13)
# typ<-c("B2","C","L","S")
# typc<-c("skyblue","darkgreen","purple","green")
#
# cls<-d$階層;cls_<-levels(factor(cls))
# yr<-substr(names(d)[col],2,5)
# dm<-c();for (i in col) dm<-cbind(dm,tapply(d[,i],d$階層,sum))
# dm<-dm[c("B2","C","S"),]
#
# colnames(dm)<-yr
# #clsc<-cls_;for (i in 1:) clsc[i]<-typc[cls_[i]==typ]
# clsc<-typ;for (i in 1:length(clsc)) clsc[i]<-typc[typ[i]==typ]
# coln<-length(clsc)
#
#
# w<-0.9;s<-.2
# par(mfrow=c(1,1))
# barplot(dm,legend=rownames(dm),ylab="総合優占度",width =w,space=s,
#         col=clsc,xlim=c(0,coln+3))
#
#
# par(mfrow=c(1,3))
# main.txt <- c("低木層　(B2)","草本層　(C)","ササ　(S)")
# for (i in 1:nrow(dm))barplot(dm[i,],main=main.txt[i],ylab="総合優占度　(％)")		#rownames(dm)[i]
#
#
# # 04_集計.R ####
#
# # plot_name <- "松尾峠"
# # plot_name <- "浄土山"
# plot_name <- "上ノ小平"
#
# # OS <- "win" #"win"
# OS <- "linux" #"win"
#
# library("ggplot2")
# library("ggrepel")
#
#
#
# ### 年度を調査年度に変換すること　例　2017→2018 !!!!
#
# ### ワーキング・ディレクトリの設定
# #　wd<-"m:/00D/00/立山植生モニタリング事業/第04期/平成30年度/植生調査/R/美松"
# # setwd(wd)
# dir()
# ####
# # ifelse(OS=="wim",
# #   d0<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE),
# #   d0<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE,fileEncoding="CP932")  #,fileEncoding="SHIFT-JIS"
# # )
#
# d0<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE)
#
# d0<-d0[,-1] 		### write.csvで自動的に生成された1列目を削除  # edit(d0)
# (nm<-names(d0))	#edit(dd) ; data.frame(nm)
#
# ### 階層除外
#
# i<-d0$階層=="A1" | d0$階層=="A2" | d0$階層=="B1" | d0$階層=="L" | is.na(d0$form)
#
# d0<-d0[!i,]
#
# ##### どんな種が多いのか?
# ####平均優占度
# (typc<-levels(factor(d0$階層)))
# (typf<-levels(factor(d0$form)))
# Fc <- grep("F",nm) ; Vc<-grep("V",nm) ; vc<-grep("v",nm)
# mF<-apply(d0[,Fc],1,mean)		#
# mv<-apply(d0[,vc],1,mean)
# (mFv<-data.frame(mF,mv,sp=d0$種名))
# d0$種名[order(mv)] ##### 総合優占度の順位
#
# #####　II〜V期平均
# x12 <- range(mFv$mF);y12 <- range(mFv$mv)
# g<-ggplot(mFv, aes(x = mF, y = mv, label = sp )) +xlim(x12[1]-15,x12[2])+ylim(y12[1]-7,y12[2])+
#   #+xlim(-20,100)+ylim(-20,100)+
#   #theme(panel.border = element_blank(), axis.line = element_line())+
#   xlab("II - V 期平均出現頻度 (%)")+
#   ylab("II - V 期平均総合優占度 ")+
#   #scale_x_continuous(breaks=seq(40,100,20),labels=1:4)+scale_y_continuous(breaks=c(-40,20,80,120))+
#   geom_point(col="red") +
#   geom_text_repel(size=3,fontface="bold",max.overlaps=1000)#)
#
#
# g1<-g+theme(axis.title=element_text(size=18,face="bold"))
#
# g2<-g1+theme(axis.text=element_text(size=15))
#
# g3<- g2 +
#   geom_hline(yintercept=0,linetype="dashed",colour="blue") +
#   geom_vline(xintercept=0,linetype="dashed",colour="blue")
#
# g3+labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
#
#
#
#
# #####　最新期集計
# (Fc_last <- rev(Fc)[1])
# (Vc_last <- rev(Vc)[1])
# (vc_last <- rev(vc)[1])
#
#
# d　<-　d0[d0[,tail(Fc,1)]!=0,  ] #今季調査で記録のあったもの
#
#
# #### 種数集計
# (t2<-table(d$階層));
# paste(names(t2),"層",t2,"種",collapse =",",sep="")
# (t1<-table(d$form));
#
# fm<-d$form
# fm[fm=="bl" |fm=="cl"]<-"tree" ; fm[fm=="bs" |fm=="cs" | fm=="ds" ]<-"shrub" ;fm[fm=="h"]<-"herb" ;fm[fm=="f"]<-"fern"
# (tfm<-table(fm))
#
# ##
# ffm<-data.frame(form=c("針葉樹高木","広葉樹高木","針葉樹低木","広葉樹低木","矮性低木","草本","藤本","シダ類","ササ類"),
#                 code=c("cl","bl","cs","bs","ds","h","l","f","sasa"))
# ffm
# #### 種ごとに集計する
#
#
# (tfm2<-data.frame(form=ffm$form[match(names(t1),ffm$code)],t1))
# (tfm3<-paste(tfm2[,1],tfm2[,3],"種",collapse =",",sep=""))
# (tfm3_<-paste0(tfm2[,1],tfm2[,3],"種 (#)"))
#
#
#
# ### 構成種
# txt<-c()
# for(i in 1:nrow(tfm2)){#i=1
#   #print(as.vector(ffm$form[i]))
#   txt_<-paste(d$種名[d$form==tfm2[i,2]],collapse =",",sep="")
#   txt_<-gsub("#",txt_,tfm3_[i])
#   txt<-c(txt,txt_)
# }
#
# paste(txt,collapse =", ")　　　####　報告書貼り付け用
#
#
# ####　過去に記録し，今回記録されなかった種
#
# ( d_na　<-　d0[d0[,tail(Fc,1)]==0,] )
# table(d_na$階層)
#
# d_na[,c(1,2, grep("F",nm))]
# nm
# paste(d_na$種名,d_na$階層,collapse =",",sep="")
#
#
# #### 総合優占度 ####
# dd<-d
# data.frame(names(dd))
# i<-order(dd[,3],dd[,vc_last],decreasing=TRUE)
# data.frame(dd[i,c(1,2,3,vc_last)])
#
# #### 頻度　主要樹種 ####
# i<-order(dd[,Fc_last],decreasing=TRUE)
# j<-Fc
# z<-dd[i,c(1:3,Fc_last)]
# paste(head(z$種名,10),z[1:10,ncol(z)],collapse =",")
# paste(head(z$種名,10),"(",sprintf("%.1f",head(z[,ncol(z)],10)),"%",")",collapse =",",sep="")
#
#
#
# #### 被度
# V_last<-dd[,Vc_last]　# data.frame(names(dd))
# i<-order(V_last,decreasing=TRUE)
# z<-dd[i,c(1:3,Vc_last)]
# (Vtotal<-sum(V_last)) ### 全出現種の平均被度合計は99.0％であった
# (V<-cumsum(V_last))
# dd$種名[i]
# plot(V/Vtotal)
# sp10<-z$種名[1:10]
# hido10<-z[,ncol(z)][1:10]
# paste(head(z$種名,10),"(",head(sprintf("%.1f",z[,ncol(z)],10)),"%",")",collapse =",",sep="")
#
#
# #### ササ ####
# dd[dd$form=="sasa",c(1:3,Fc)]
#
#
# #### 頻度-被度-総合優占度 ####
# fmn<-match(dd$form,ffm$code)
#
# ####    "出現頻度(％)" - "平均被度(％) #######
#
# d_ <- data.frame(x=d[,Fc_last], y=d[,Vc_last], sp=d$種名)
# x12 <- range(d_$x);y12 <- range(d_$y)
# g<-ggplot(d_, aes(x, y , label = sp )) +xlim(x12[1]-15,x12[2])+ylim(y12[1]-7,y12[2])+
#   xlab("出現頻度(％)")+  ylab("平均被度(％)")+
#   geom_point(col="red") +
#   geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
#   geom_hline(yintercept=0,linetype="dashed",colour="blue") +
#   geom_vline(xintercept=0,linetype="dashed",colour="blue")+
#   labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
#
# g
#
# ####   "出現頻度(％)" - "総合優占度"   #######
#
# d_ <- data.frame(x=d[,Fc_last], y=d[,vc_last], sp=d$種名)
# x12 <- range(d_$x);y12 <- range(d_$y)
# g<-ggplot(d_, aes(x, y , label = sp )) +xlim(x12[1]-15,x12[2])+ylim(y12[1]-7,y12[2])+
#   xlab("出現頻度(％)")+  ylab("総合優占度")+
#   geom_point(col="red") +
#   geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
#   geom_hline(yintercept=0,linetype="dashed",colour="blue") +
#   geom_vline(xintercept=0,linetype="dashed",colour="blue")+
#   labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
#
# g
#
# ###########   "平均被度(％)",ylab="総合優占度"
#
# d_ <- data.frame(x=d[,Vc_last], y=d[,vc_last], sp=d$種名)
# x12 <- range(d_$x);y12 <- range(d_$y)
# g<-ggplot(d_, aes(x, y , label = sp )) +xlim(x12[1]-15,x12[2])+ylim(y12[1]-7,y12[2])+
#   xlab("平均被度(％)")+  ylab("総合優占度")+
#   geom_point(col="red") +
#   geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
#   geom_hline(yintercept=0,linetype="dashed",colour="blue") +
#   geom_vline(xintercept=0,linetype="dashed",colour="blue")+
#   labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
#
# g
#
# ## ササの被度の経年変化　####
#
# sasa_df <- data.frame(Year=c(2003,2009,2015,2024),Coverage=as.numeric(dd[dd$form=="sasa",15:18]))
#
#
# ggplot(sasa_df,aes(x = Year, y = Coverage ))      +
#   geom_line()+geom_point(col="red",size=3)  +
#   xlab("西暦年") + ylab("ササの平均被度 (%)") +
#   scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
#
#   theme(axis.title.x  = element_text(size = 15),
#         axis.title.y = element_text(size = 14),
#         axis.text.x  = element_text(size = 12),
#         axis.text.y = element_text(size = 12))
#
#
# # 以上　####
#
#
#
#
#
