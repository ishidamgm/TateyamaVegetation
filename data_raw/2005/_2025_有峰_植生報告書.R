# _2025_有峰_植生報告書.R ####

# ライブラリ・読み込み
library(TateyamaVegetation)
library(tidyverse)
library(ggplot2)
library(ggrepel)

# 各調査区の調査年度確認　####
#　調査年度の列ラベル名が正しく入力されているか確認してください。
#  スプレッドシートのデータ列となります

VegetationSurveyYears
# よろしいですか　?　良ければ次へ

# ワーキング・ディレクトリの設定 ####
# 要入力　【!!!】
save_file   = FALSE #以下の実行でファイルの書き出しをするかどうか?　
wd. <- "../2025/有峰"
year<-2025
plot_name_jp <- "有峰"

(calc <- CalculationInfo(year,plot_name_jp))


# plot_name.<-"Arimine"
# yachou  <- "10有峰-植生野帳2025.csv"
# yachou2 <- "10有峰-植生野帳2025-最新.csv"
# calc<-list(yachou=yachou,yachou2=yachou2,clm=clm)


#yachou  :エクセル等で入力したオリジナルの野帳ファイル名です
#         保存する際はこの名前と同じにしてください【.xlsxがこのルールで命名されています。
# .csvとして保存すると自動的にこの名前で保存されます】
#yachou2 :オリジナルを整理した野帳ファイル名です
#yachou :植生被度の列名となります

# よろしいですか　?　良ければ次へ



# 集計計算開始 ####




# ワーキング・ディレクトリの設定
#setwd("~/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2025/植生調査/有峰")
#setwd("~/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2025/R/TateyamaVegetation/data_raw/有峰")



# ファイル確認　####
setwd(wd. )
getwd() ; dir() #　ここで作業します

# オリジナル野帳の確認修正 ####
### csvファイルの読み込み ####
d<-read_csv(calc$yachou)
d

## 種名の確認 ####
table(d$種名)

SpeciesNameCheck(d$種名)
# ⇒ヒナウチワカエデ イカリソウ ホソバイヌワラビ ヤマシユキザサ タニギハョウ シダ? ヤヤカマド はリストにありません。
# 種名を修正するか、floraに新たな種を追加してください。

## 置換　####

sp_old<-c("ヒナウチワカエデ","イカリソウ","ホソバイヌワラビ","ヤマシユキザサ","タニギハョウ","シダ?","ヤヤカマド")
sp_new<-c("オオイタヤメイゲツ","イカリソウ","ホソバナライシダ","ヤマトユキザサ","タニギキョウ","ミヤマワラビ","ナナカマド")

j=0; for(i in sp_old){
  j <- j+1
  d$種名[d$種名==i]<-sp_new[j]
}

## 削除　####

d<-d[d$種名!="イカリソウ",]

## 再確認　####
SpeciesNameCheck(d$種名)
# ⇒入力されている種名はすべて目録に含まれています。

## 入力値の確認　####

d$被度2025<-as.numeric(d$被度2025) #オリジナルに1セル"."が入っていたのでnumericに修正
summary(d$被度2025)

## 空白欄の入力　####
d<-FieldNote_CheckCorrect(d)
d
## 修正野帳の保存　####

if(save_file)write_csv(d,file=yachou2)

# 保存確認
dir()
getwd()

# d.<-d # 念の為dをd.にバックアップ

# 修正最新野帳を再ロード
(d <- read_csv(calc$yachou2))




#' スプレッドシートで入力した野帳データ・フレーム(tibble)を
#'　列名の変更、項目追加し計算用に構成します
#'　
(d <- FieldNote_FormatArange(d,calc))

# ここまでは手作業が必要になりますが以下は自動計算します

# 自動集計計算>>> ####
##  被度表 ####
#clm2<-calc$colnames$calc[match(calc$clm,calc$colnames$yachou)]
# 初年度のみDomin-Krajina被度

vt<-c()
vt<-c(vt,list(VegetationTable(d, period = calc$clm2[1], DK = TRUE)))

for(i in calc$clm2[-1]){
  vt<-c(vt,list(VegetationTable(d, period = i, DK = FALSE)))
}

# 解析用に名前つけ直し
names(vt)<-calc$hidohyou

# ファイルとして保存
if(save_file)for(i in calc$hidohyou)write.csv(vt[[i]],file=i,, row.names = F)

names(vt)<-calc$vt_label

##  被度階級表 ####
vtDK<-c()
for(i in calc$clm2){
  vtDK<-c(vt,list(VegetationTable(d, period = i, DK = TRUE)))
}

names(vtDK)<-calc$hidokaikyuhyou

### ファイル保存 ####
if(save_file) for(i in calc$hidokaikyuhyou)write.csv(vtDK[[i]],file=i, row.names = F)

# 解析用に名前つけ直し
names(vtDK)<-calc$vt_label

## 被度・総合優占度時系列集計　####
(vc<-VegetationChronologyTable(plot_name="Arimine",vt,vtDK))
### ファイル保存 ####
if(save_file) write_csv(vc[[1]],file=calc$sougou)


## 時系列(報告書用)　####

vc_repo <- list(VegetationChronologyTable_report(plot_name="Arimine",vc))
names(vc_repo)<-calc$plot_name
vc_repo

### ファイル保存 ####
if(save_file) write_csv(vc_repo[[1]],file=calc$houkousho[[1]])


# 作図 ####
## 出現頻度-平均被度 (第5期)　####
Fig_FrequencyCoverage(plot_name="Arimine",x="f5",y="c5",data=vc_repo)

# 結果 ####
text_species(plot_name="Arimine",period=5,data=vc_repo)

## 出現頻度−総合優占度 (第5期) ####
Fig_FrequencyDominace(plot_name="Arimine",x="f5",y="di5",data=vc_repo)

## 総合優占度経年変化 ####
Fig_DominanceValue(plot_name="Arimine",vc_repo=vc_repo)

# <<<<<自動計算 ####

# 野帳　VV5　の更新 第5期立山植生モニタリング植生調査野帳データの更新　####

VV5[[calc$plot_name]]<-d
## 内容確認
names(VV5)
VV5[[calc$plot_name]]
## 保存　まずはカレントに保存、####
## パッケージに反映させる場合は手作業でdata/VV5のバックアップ後、上書きし再ビルド
if(save_file) save(VV5,file="VV5.rda")

# 以上 ####




