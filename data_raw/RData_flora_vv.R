# RData_flora_vv.R
# 米倉浩司・梶田忠 (2003-) 「BG Plants 和名－学名インデックス」（YList），http://ylist.info」
#
#

# Data Backup ####
#　write.csv(flora,file="data_raw/flora_old.csv")　####　これまでのdata_raw/flora_old.csvとして保存
#  save(flora,file="data_raw/flora_old.RData")

#　write.csv(flora,file="data_raw/flora_old.csv")　####　これまでのdata_raw/flora_old.csvとして保存
#  save(flora,file="data_raw/flora_old.RData")


# YList読み込み ####
# 米倉浩司・梶田忠 (2003-) 「BG Plants 和名－学名インデックス」読み込み
YList<-read.csv("data_raw/20210514YList.csv")
names(YList)


# これまでの出現種 ####
(sp0<-unique(vv$sp))

#　YList$和名にない種
(sp_na <- sp0[!is.element(sp0,YList$和名)])

#同種異名 ####

(sp_na2 <- YList$和名[match(sp_na,YList$別名)])

(sp_exc <- data.frame(sp_na,sp_na2))

#　種名置換　####
sp_exc$sp_na2[1]<-"ハウチワカエデ"
sp_exc$sp_na2[4]<-"ヤマトユキザサ"
sp_exc$sp_na2[6]<-"ネズコ"
sp_exc$sp_na2[7]<-"ゼンテイカ"
sp_exc$sp_na2[12]<-"イグサ"
# 「水面」と「カヤツリグサ科」【未同定】はそのまま
sp_exc$sp_na2[8]<-"カヤツリグサ科"
sp_exc$sp_na2[11]<-"水面"




matrix(nc=2,
  c(
  "","ハウチワカエデ",
  "","ヤマトユキザサ",
  "","ネズコ",
  "","ゼンテイカ",
  "","イグサ",
  # 「水面」と「カヤツリグサ科」【未同定】はそのまま
  "カヤツリグサ科","カヤツリグサ科",
  "水面","水面"
)
)



sp_exc
vv_new<-vv
for(i in 1:nrow(sp_exc)){
  sp.<-sp_exc$sp_na[i]
  vv_new$sp[match(sp.,vv_new$sp)]<-sp_exc$sp_na2[i]
}



(sp0<-unique(vv_new$sp))


(sp_na <- sp0[!is.element(sp0,YList$和名)])





