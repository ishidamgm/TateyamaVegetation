# RData_flora_vv.R
# 米倉浩司・梶田忠 (2003-) 「BG Plants 和名－学名インデックス」（YList），http://ylist.info」
#
#

library(dplyr)


# YList読み込み ####
# 米倉浩司・梶田忠 (2003-) 「BG Plants 和名－学名インデックス」読み込み
YList<-read.csv("20210514YList.csv") #data_raw/
names(YList)


# ケヤマハンノキのfloraへの追加
(fl<-flora)
YList[which(YList$和名=="ケヤマハンノキ"),]

fl<-rbind(fl,data.frame(...1=999,id=787555070,form="bl",spj="ケヤマハンノキ",sp="Alnus hirsuta Turcz.  var. hirsuta",family_jp="カバノキ科",family="Betulaceae"))
tail(fl)
fl<-fl[order(fl$spj),]
fl<-fl[,-1]
flora<-fl
#readr::write_excel_csv(fl,file="flora_20250806.csv")
# usethis::use_data(flora,overwrite = TRUE)

# Data Backup ####
#　write.csv(flora,file="data_raw/flora_old.csv")　####　これまでのdata_raw/flora_old.csvとして保存
#  save(flora,file="data_raw/flora_old.RData")

#　write.csv(flora,file="data_raw/flora_old.csv")　####　これまでのdata_raw/flora_old.csvとして保存
#  save(flora,file="data_raw/flora_old.RData")





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





sp_exc
vv_new<-vv
for(i in 1:nrow(sp_exc)){
  sp.<-sp_exc$sp_na[i]
  vv_new$sp[sp.==vv$sp]<-sp_exc$sp_na2[i]
}



(sp0<-unique(vv_new$sp))


(sp_na <- sp0[!is.element(sp0,YList$和名)])

vv4<-vv
vv<-vv_new


vv<-tibble(vv)
#save(vv,file=("data/TateyamaVegetation_vv.RData"))
#save(sp_exc,file=("data/sp_exc.RData"))

#階層の確認と補完
fl.i<-match(vv$sp,  flora$spj)
vv$layer<-sptype$階層[match(flora$form[fl.i],sptype$コード)]


# union and combine coverage data of duplicated species in a subplot#
vv5<- vv %>%
  group_by(pn,plot,subplot,layer, sp) %>%
  summarise(
    across(matches("^(dk|c)"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

View(subset(vv5,plot=="Arimine"))
View(subset(vv5,plot=="Mimatsu"))
#save(vv5,file=("data/TateyamaVegetation_vv5.RData"))
vv<-vv5
View(subset(vv,plot=="Mimatsu"))
#save(vv,file=("data/TateyamaVegetation_vv.RData"))


detach("package:TateyamaVegetation", unload = TRUE)
library(TateyamaVegetation)
(sp0<-unique(vv$sp))
(sp_na <- sp0[!is.element(sp0,YList$和名)]) # ok !!!

unique(vv$plot)
unique(vv$subplot)
View(subset(vv,plot=="Arimine"))
#View(subset(vv5,plot=="Arimine"))
summary(vv)
sum(is.na(vv))
i<-is.na(vv$layer)
sum(i)
vv[i,]

vv$layer[i]<-sptype$階層[match(flora$form[match(vv$sp[i],flora$spj)],sptype$コード)]
sum(is.na(vv$layer)) #NA なくなる

sum(is.na(vv$dk01))
sum(is.na(vv$c02))
sum(is.na(vv$c03))
sum(is.na(vv$c04)) #NA なくなる



# floraの更新 #### 20250804
# エングラーのまま

(sp0<-unique(vv$sp))
(sp_na <- sp0[!is.element(sp0,YList$和名)]) # ok !!!
sp0[is.na(match(sp0,flora$spj))]
i<-match(sp0,YList$和名)
names(flora)
names(YList)

form<-flora$form[match(sp0,flora$spj)]


df<-YList[i,c("LAPG.no.","和名","学名.withAuthor","LAPG.科名","LAPG.Family" )]
df<-tibble(df) %>%
  mutate(form,.before="和名")

names(df)<-c("id","form","spj","sp","family_jp","family" )

# write.csv(df,"data_raw/flora_edit.csv",row.names = F)

df<-readr::read_csv("data_raw/flora_edit.csv")
df[order(df$LAPG.no.),]
unique(df$form)

flora4<-flora
# save(flora4,file="data_raw/flora4.RData")

flora<-df
# save(flora,file="data/flora.RData")

# flora をAPG　YList　にする　ラベル名は変更せず
# vvにもどり生活型NAの修正など




library(tidyverse)

VegetationSurveyYears %>%
  pivot_longer(-plot_name, names_to = "Survey", values_to = "YearText") %>%
  mutate(Year = as.integer(str_extract(YearText, "\\d{4}"))) %>%
  select(plot_name, Survey, Year) %>%
  pivot_wider(names_from = plot_name, values_from = Year) -> VegetationYearsMatrix

VegetationYearsMatrix
# usethis::use_data(VegetationYearsMatrix)

#　usethis::use_data(vv, overwrite = TRUE)
summary(vv)



### Joudosan
d<-vv5

### vv5 Arimine _c04 subplot 12 200
# vv5[vv5$c04==200,"c04"]<-20
# vv5[vv5$c05==200,"c05"]<-20
#　usethis::use_data(vv5, overwrite = TRUE)
vv[vv$c04==200,]



# vvに2024年の上ノ小平と浄土山の調査データが含まれていなかった問題 ####

# 2025/8/6
# オリジナルのバックアップ　####
# save(vv,file="vv_20250806.RData")


## Joudosan ####
# 確認　plotがJoudoになっていた。ここも修正
vv %>% filter(plot=="Joudosan")
nrow(subset(vv,plot=="Joudo"))
unique(vv$plot)

subset(vv,plot=="Kaminokodaira")

d_Joudosan<-readr::read_csv("/home/i/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2024/植生調査/浄土山/09浄土-植生野帳2024.csv")

nrow(d_Joudosan)
SpeciesNameCheck(d_Joudosan$種名)
flora
which(APG$種名=="コイワカガミ")
APG[which(APG$種名=="イワカガミ"),]
### イワカガミ・コイワカガミ両者の区別はしない　####
d_Joudosan$種名[d_Joudosan$種名=="コイワカガミ"]<-"イワカガミ"
d_Joudosan<-FieldNote_CheckCorrect(d_Joudosan)

summary(d_Joudosan)

names(vv)
names(d_Joudosan)<-names(vv)[-c(1,2)]


d_Joudosan<-tibble(pn=9,plot="Joudosan",d_Joudosan)


(rng12<-range(which(vv$plot=="Joudo"))-c(1,-1))

nrow(vv)

vv<-rbind(vv[1:rng12[1],],d_Joudosan,vv[rng12[2]:nrow(vv),])


nrow(vv)

# save(vv,file="vv_20250806.RData")
# usethis::use_data(vv,overwrite=T)


## Kaminokodaira ####
plnam<-"Kaminokodaira"
d_<-readr::read_csv("/home/i/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2025/植生調査/上ノ小平/04上ノ小平2024.csv")
SpeciesNameCheck(d_$種名)
SpeciesNameCorrect(d_$種名)
sp.<-SpeciesNameCorrect(d_$種名)
SpeciesNameCheck(sp.)
d_$種名<-sp.

d_<-FieldNote_CheckCorrect(d_)


names(d_)<-names(vv)[-c(1,2)]

d_<-tibble(pn=4,plot=plnam,d_[,1:8])


(rng12<-range(which(vv$plot==plnam))-c(1,-1))

nrow(vv)

vv<-rbind(vv[1:rng12[1],],d_,vv[rng12[2]:nrow(vv),])

nrow(vv)

vv |> filter(plot==plnam)

# usethis::use_data(vv,overwrite=T)
#save(vv5,file="vv5_20250806.RData")

vv5<-vv

vv5$c05[vv5$plot=="Arimine"]<-vv5$c04[vv5$plot=="Arimine"]
vv5$c05[vv5$plot=="Mimatsu"]<-vv5$c04[vv5$plot=="Mimatsu"]

# usethis::use_data(vv5,overwrite=T)
