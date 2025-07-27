# TateyamaVegetation_RData.R


### ワーキング・ディレクトリの設定
wd<-"./test"
setwd(wd)
dir()

#
vv<-read.csv("TateyamaVegetation_fieldnote_2023.csv" )
# save(vv,file="../data/TateyamaVegetation_vv.RData")
load("../data/TateyamaVegetation_vv.RData")
table(vv$plot)
