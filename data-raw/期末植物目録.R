# 期末植物目録.R

#　2025/12/17-　>>>>>> ####

library(tidyverse)

# flora2の作り直し　#####

## 森林forest_code ####
forest_code
#ForestFloraMatrix() 参照

## 植生vegetation_code ####
if(0){
  m<-VegetationFloraMatrix(VCrepo)
  head(m)
  sp<-names(m)[-1]
  vc<-c(); for( i in sp)vc<-c(vc,VegetationFloraCode(cod=m[[i]]))
  vegetation_code<-data.frame(sp,vegetation_code=vc)
  # usethis::use_data(vegetation_code,overwrite = TRUE)
}

vegetation_code

## 外来植物 nonnative_code ####
# IVから調査地点など変更あり最初から作り直し
# オリジナルを転置しcsvで扱いやすいように編集
d<-read.csv("data-raw/Nonnative_第Ⅴ期報告書用全体表3-5-1.csv")
d[is.na(d)]<-0
dim(d)
names(d)
(sp<-names(d)[5:ncol(d)])

###　種名のチェック　 YPlant ####
SpeciesNameCheck(sp,spj00=APG$種名)
which(APG$種名=="コガネスゲ")
which(APG$種名=="オウゴンスゲ")
which(APG$種名=="タカネスイバ")
which(APG$種名=="エゾコヌカグサ")
### 地点リスト　no_chiten ####
no<-d$no
chiten<-d$地点名
(no_chiten<-data.frame(no=no[no!=0],chiten=chiten[chiten!=""]))

#### 空セル補完 ####
no[no == ""] <- NA
no <- zoo::na.locf(no)

chiten[chiten == ""] <- NA
chiten <- zoo::na.locf(chiten)

#### new data ; d2 ####
d2<-d
d2[,1]<-no
d2[,2]<-chiten
d2

### 第Ⅴ期抽出 ####
d2<-d2[is.element(d2$X,paste0("R",2:7)),]

### 地点別記録値平均 ####
d3<-c()
for(i in 1:length(sp)){
  d3<-cbind(d3,ceiling(tapply(d2[,names(d2)==sp[i]],d2$地点名,mean)))
}

d3<-data.frame(d3)
names(d3)<-sp
no_V<-no[match(rownames(d3),chiten)]

d3<-data.frame(no=no_V,d3)
d3<-d3[order(as.numeric(d3$no)),]

### write.csv(d3,file="data-raw/Nonnative_各種各調査地点V期平均値_期末植物目録.CSV") ####
d3<-read.csv("data-raw/Nonnative_各種各調査地点V期平均値_期末植物目録.CSV")

### nonnative_code ####
sp<-names(d3)[-c(1,2)]
sn<-list(弘追=1:5,弥美=6:14,天大=15:28,室=29:43,弥=44:48)

nn_code<-function(ii){
  s<-c()
  for (i in 1:length(sn)){
    s<-c(s,paste("-",names(sn)[i],paste(d3[sn[[i]],ii],collapse=""),sep=""))
  }
  paste(s,collapse="")
}

spc<-c();for (i in 3:ncol(d3))spc<-c(spc,paste(nn_code(i),sep=""));spc
spc<-substr(spc,2,999)
(spc<-data.frame(sp,spc))
nonnative_code<-spc

##　bacKup VI flora flora2 ####
flora_IV<-flora
#save(flora_IV,file="data/flora_IV.rda")
flora2_IV<-flora2
#save(flora2_IV,file="data/flora2_IV.rda")

# flora2_V  = flora2
d1<-forest_code
d2<-vegetation_code
d3<-nonnative_code
(sp00<-unique(c(d1$sp,d2$sp,d3$sp)))
(i.APG<-na.omit(match(sp00,APG$種名)))

(sp_na<-sp00[is.na(i.APG)])
(sp<-sp00[!is.na(i.APG)])
names(APG)
head(flora2)
head(d<-APG[i.APG,c("ID0","X.2","種名","学名","科名.和.","科名")])
names(d)<-names(flora2)[1:6]
# 植物相コード入力
d<-data.frame(d,forest_code="",vegetation_code="",nonnative_code="")
head(d)

head(flora2)
# Forest_code
d$forest_code[match(d1$sp,d$spj)]<-d1$forest_code
# Vegetation_code
d2$sp[is.na(match(d2$sp,d$spj))]
d2<-d2[!is.na(match(d2$sp,d$spj)),] #[1] "カヤツリグサ科" "水面"  除外
d2$sp[is.na(match(d2$sp,d$spj))] 　 #character(0)　除外確認
d$vegetation_code[match(d2$sp,d$spj)]<-d2$vegetation_code

# NonNative_code
# APG種名リストにないものは省く
i<-match(d3$sp,d$spj)
na_i<-is.na(i)
d3.<-d3[!na_i,]
d$nonnative_code[match(d3.$sp,d$spj)]<-d3.$spc

# 除外されたもの
(omit_nonnative<-d3[na_i,])
#save(omit_nonnative,file="data/omit_nonnative.rda")

## 生活型入力  ####
names(d)
d$form<-flora2$form[match(d$spj,flora2$spj)]
flora2_V<-d
flora_V<-d[,1:6]

#save(flora_V,file="data/flora_V.rda")
#save(flora2_V,file="data/flora2_V.rda")
library(dplyr)
flora<-tibble(flora_V)
flora2<-tibble(flora2_V)
#save(flora,file="data/flora.rda")
#save(flora2,file="data/flora2.rda")

# <<<<flora2# <<<< ####

# 目録作成 ####
d<-flora2
memo<-paste("【森】",d$forest_code,"【植】",d$vegetation_code,"【外】",d$nonnative_code)

(fl.<-  FloraListMaker(d$spj,memo))

# cat(fl.,file="data-raw/FloraList_V.html")  # html保存

# 以下参考　IV期　作業フアイル　########
# 森林調査 ####
# d<-tibble(dd5)
# names(d)
# unique(d$sp)
# SpeciesNameCheck(d$sp)
# d$sp[d$sp=="クロベ"]<-"ネズコ"
# SpeciesNameCheck(d$sp)
# dd5<-d
# usethis::use_data(dd5)
sum(is.na(d$plot))




# 各調査区胸高断面積合計
# ba_plot <- d %>% group_by(plot) %>% summarise(batotal=sum(ba,na.rm=T))
# ba_sp <- d %>% group_by(plot,sp) %>% summarise(batotal=sum(ba,na.rm=T))

#' Title
#'
#' @param d
#'
#' @returns
#' @export
#'
#' @examples
#' ForestFloraMatrix(dd5)
#'
ForestFloraMatrix <- function(d=dd5){

  #最終調査の生存木のみ対象
  d<-d[!is.na(d$f07>0),c("plot","sp","d07")]
  d$ba<-pi*(d$d07/2)^2

  # Bjodaira....の順番に並ぶように因子化
  d$plot <- factor(d$plot, levels = unique(d$plot))

  d.<-d %>%
    group_by(plot, sp) %>%
    summarise(batotal = sum(ba, na.rm = TRUE),.groups = "drop_last") %>%   #.groups = "drop_last"
    mutate(share = batotal / sum(batotal) * 100,cls=floor(share/10)+1)

  # matrix_cls ####
  m<- d. %>%
    select(plot, sp, cls) %>%
    pivot_wider(
      names_from = sp,     # 列見出しにする
      values_from = cls    # 値にする
    )

  m[is.na(m)]<-0

  return(m)
}


d


#' Title
#'
#' @param cod
#'
#' @returns
#' @export
#'
#' @examples
#'
#' ForestFloraCode(cod=m[["オオシラビソ"]])
#'
ForestFloraCode<-function(cod=m[["オオシラビソ"]]){
  cod[cod>9]<-"A"
  cod<-
    paste(
    paste0(cod[1:4],collapse =""),
    paste0(cod[5:7],collapse =""),
    paste0(cod[8:8],collapse =""),sep="-")
  return(cod)
}

paste0(m[["オオシラビソ"]],collapse ="")

d[which(is.na(d$plot)),]

d <- d |> #%>%
  dplyr::group_by(サブプロット, 階層, 種名) |> #%>%
  dplyr::summarise(
    across(matches("^(被度|DK)"), ~ sum(.x, na.rm = TRUE)),
    # X = first(X),
    # no = first(no),
    # II_ = first(II_),
    .groups = "drop"
  )


# 植生調査 ####
unique(vv5$sp)

# 外来植物調査 ####

# 以下オリジナル ####

# RData_flora.R
flora<-read.csv("data-raw/第5期植物目録.csv")
# save(flora,file="data/flora.RData")

l<-flora
#l<-read.csv("m:/00d/文献/Flora/種名.csv",as.is=TRUE)
names(l)
spj<-l$種名 #match("ブナ",spj))

#######################
f<-c("01美女平","02ブナ坂","03ブナ平","04上ノ小平","05弥陀ヶ原",
     "06美松","07松尾峠","08鏡石","09浄土","10有峰")

#setwd("m:/00D/00/立山植生モニタリング事業/_調査資料（生データ）/植生調査表/野帳/III")
#dir()
#dd<-read.csv("植生野帳01-10_IV.csv",as.is=TRUE)
dd<-vv
names(dd)
(sp<-dd$種名)
(t1<-data.frame(table(sp)))
sp3<-as.vector(t1[,1])
(t1<-data.frame(t1,spn=match(sp3,spj)))
(t2<-t1[is.na(t1$spn),])  #### これがなくなるまで種名の修正　(同定できなかった種、水面は別)
as.vector(t2[,1])

### 植生調査調査区別頻度

freq<-c()
for (ii in 1:10){  ##length(f)
  ##ii<-2;
  (d<-subset(dd,プロット==ii))
  (v<-as.numeric(d[,8]))	#第4期被度
  (d<-d[v!=0,])
  (nam<-names(d))
  (plt<-as.numeric(d$サブプロット))
  (plt_<-as.numeric(unique(plt)))
  (pn<-length(plt_))
  (plt_<-sprintf("%02.0f", plt_))

  (sp<-d$種名)

  ii_<-sprintf("%02.0f", ii)
  freq0<-ceiling(table(substr(unique(paste(sprintf("%02.0f",plt),sp)),4,999))/pn*10)
  dat<-data.frame(I(ii_),I(as.character(names(freq0))),I(as.character(freq0)))
  freq<-c(freq,list(dat))
}


names(freq)<-f
freq2<-c();for (ii in 1:length(f))freq2<-rbind(freq2,as.matrix(freq[[ii]]))
sp2<-freq2[,2]
freq2
str(freq2)
(tab1<-table(sp2,freq2[,1]))

pr<-which(tab1==1,arr.ind =TRUE)
tab2<-tab1
#### 01テーブルの1のところを頻度に換える
for (i in 1:nrow(pr)){
  ### i<-1
  tab2[pr[i,1],pr[i,2]]<-freq2[which(sp2==rownames(pr)[i] & freq2[,1]==colnames(tab2)[pr[i,2]]),3]
}


tab3<-tab2;tab3[tab3==10]<-"A"
sp3<-rownames(tab3)
####
spc<-c();for(i in 1:nrow(tab3)){
  s<-paste(paste(tab3[i,1:4],collapse=""),tab3[i,5],paste(tab3[i,6:9],collapse=""),tab3[i,10],sep="-")
  spc<-c(spc,s)
}
(vegetation_sp<-data.frame(sp3,spc))

### write.csv(vegetation_sp,file="第Ⅳ期植生調査出現種分布一覧.csv")

#########################
# 第Ⅳ期毎木樹木分布 #####
#########################

setwd("./stand")
(f<-dir())
BA<-c()
for (ii in 1:length(f)){	##length(f)
  ##ii<-1;
  (d<-read.csv(f[ii],as.is=TRUE))
  (nam<-names(d))
  (dbhc<-rev(which(substr(nam,1,1)=="D"))[1])
  sp<-d$sp;dbh<-d[,dbhc];dbh[is.na(dbh)]<-0
  (tab<-tapply(dbh^2,sp,sum))
  BA<-c(BA,list(data.frame(ii,tab,ba=ceiling(10*tab/sum(tab)))))
}

setwd("../")

names(BA)<-f
BA

BA2<-c();for (ii in 1:length(f))BA2<-rbind(BA2,as.matrix(BA[[ii]]))
sp2<-rownames(BA2)
BA2
(tab1<-table(sp2,BA2[,1]))

pr<-which(tab1==1,arr.ind =TRUE)
tab2<-tab1
for (i in 1:nrow(pr))tab2[pr[i,1],pr[i,2]]<-BA2[which(sp2==rownames(pr)[i] & BA2[,1]==pr[i,2]),3]
tab3<-tab2;tab3[tab3==10]<-"A"
sp3<-rownames(tab3)
####
spc<-c();for(i in 1:nrow(tab3)){
  s<-paste(paste(tab3[i,1:4],collapse=""),paste(tab3[i,5:7],collapse=""),tab3[i,8],sep="-")
  spc<-c(spc,s)
}
(forest_sp<-data.frame(sp3,spc))


### write.csv(forest_sp,file="第Ⅳ期毎木樹木分布.csv")


# 第Ⅳ期外来植物分布 #####



dir()
d<-read.csv("第Ⅳ期外来種調査報告書用全体表.csv",skip=1,as.is=TRUE, header = FALSE,fileEncoding="cp932")
d[is.na(d)]<-""
names(d)
(sp<-d[5:75,1])
(p<-d[2,])
j<-which(p!="")
d[1,j]
d[2,j]
d[3,j]
m<-matrix(0,ncol=length(j),nrow=length(sp))
rownames(m)<-sp
colnames(m)<-j#d[3,j]
m
row_<-5 ### データの始まり行
d2<-d[row_:(row_+length(sp)),2:234]
d2[d2==""]<-0
d3<-c()
for (i in 1:nrow(d2))d3<-rbind(d3,as.numeric(d2[i,]))
dim(d3)


for(ii in 1:length(sp)){
  for(i in 1:(length(j)-1))m[ii,i]<-ceiling(mean(d3[ii,(j[i]):(j[i+1]-1)]))
}
edit(m)
(site<-as.character(d[2,j]))

sn<-list(弘追=1:5,弥美=6:15,天大=16:28,室=29:47,弥=48:52)

(site2<-substr(site,1,1))
paste("追","-弥","-天","-室","-弥",sep="")

d2<-m

code1<-function(ii){
  ##ii<-2
  s<-c()
  for (i in 1:length(sn)){
    s<-c(s,paste("-",names(sn)[i],paste(d2[ii,sn[[i]]],collapse=""),sep=""))
  }
  paste(s,collapse="")
}

spc<-c();for (i in 1:nrow(d2))spc<-c(spc,paste(code1(i),sep=""));spc
spc<-substr(spc,2,999)
(spc<-data.frame(sp,spc))
# '---------------------------- ####
# NonNativePlants<-spc # usethis::use_data(NonNativePlants) ####
### write.csv(spc,file="第Ⅳ期_外来種分布一覧.csv")
### write.table(spc,"clipboard")
### write.csv(data.frame(site),file="2013外来種分布一覧_site.csv")
# NonNativePlants4<-NonNativePlants
# save(NonNativePlants4,file="data/NonNativePlants4.rda")

## 調査データ結合 #########
dir()
l1<-read.csv("第Ⅳ期毎木樹木分布.csv" ,as.is=TRUE)
l2<-read.csv("第Ⅳ期植生調査出現種分布一覧.csv",as.is=TRUE)
l3<-read.csv("第Ⅳ期_外来種分布一覧.csv" ,as.is=TRUE)
names(l1)
names(l2)
names(l3)

l1<-na.omit(l1)
l2<-na.omit(l2)
l3<-na.omit(l3)

(sp00<-sort(unique(c(l1$sp3,l2$sp3,l3$sp))))
spn<-length(sp00)
森林<-rep("",spn);森林[match(l1$sp3,sp00)]<-l1$spc
植生<-rep("",spn);植生[match(l2$sp3,sp00)]<-l2$spc
外来<-rep("",spn);外来[match(l3$sp,sp00)]<-l3$spc
(sp_dat<-data.frame(sp=sp00,森林,植生,外来))
edit(sp_dat)


### write.csv(sp_dat,file="第Ⅳ期_全調査出現種.csv")


#　Ⅲ期　+　Ⅳ期     ####

d1<-read.csv("第III期全植生調査出現種分布一覧.csv" ,as.is=TRUE)
d2<-read.csv("第Ⅳ期_全調査出現種.csv",as.is=TRUE)

(sp00<-sort(unique(c(d1$sp,d2$sp))))
spn<-length(sp00)

names(d1)
i<-match(d1$sp,sp00)
森林<-rep("",spn);森林[i]<-d1$森林
植生<-rep("",spn);植生[i]<-d1$植生
外来<-rep("",spn);外来[i]<-d1$外来
d3<-data.frame(sp=sp00,森林3=森林,植生3=植生,外来3=外来)
i<-match(d2$sp,sp00)
森林<-rep("",spn);森林[i]<-d2$森林
植生<-rep("",spn);植生[i]<-d2$植生
外来<-rep("",spn);外来[i]<-d2$外来
d3<-data.frame(d3,森林4=森林,植生4=植生,外来4=外来)
edit(d3)

### write.csv(d3,file="第Ⅲ―Ⅳ期_全調査出現種.csv")
d<-d3
names(d)
all<-paste("【森】",d$森林3,">>",d$森林4,"","【植】",d$植生3,">>",d$植生4,"【外】",d$外来3,">>",d$外来4)
d<-data.frame(d$sp,all)
edit(d)
### write.csv(data.frame(d3,all),file="第Ⅲ―Ⅳ期_全調査出現種.csv")

d<-read.csv("第Ⅲ―Ⅳ期_全調査出現種.csv",as.is=TRUE)
edit(d)
names(d)
mim<-substr(d$植生4,8,8)
i<-mim!="" & mim!="0" & d$外来4!=""
d$sp[i]

# 以上 ####################


#//////////////////////////////////////////////////////


dir()
l1<-read.csv("第III期毎木樹木分布一覧.csv" ,as.is=TRUE)
l2<-read.csv("第III期植生調査出現種分布一覧.csv",as.is=TRUE)
l3<-read.csv("第III期外来種分布一覧.csv" ,as.is=TRUE)
names(l1)
names(l2)
names(l3)
sp外<-substr(l3$x,58,999);dst外<-substr(l3$x,1,57)

(sp00<-sort(unique(c(l1$sp3,l2$sp3,sp外))))
spn<-length(sp00)
森林<-rep("",spn);森林[match(l1$sp3,sp00)]<-l1$spc
植生<-rep("",spn);植生[match(l2$sp3,sp00)]<-l2$spc
外来<-rep("",spn);外来[match(sp外,sp00)]<-dst外
(sp_dat<-data.frame(sp=sp00,森林,植生,外来))
edit(sp_dat)


### write.csv(sp_dat,file="2013全植生調査出現種分布一覧.csv")

}



# 2020立山植生モニタリング植物目録作成.R  //Ishiad ############################################
d<-read.csv("第Ⅲ―Ⅳ期_全調査出現種.csv",as.is=TRUE)
names(d)
edit(d)

sp<-d$sp
names(l)
####
(i<-match(sp,l$種名))
d<-data.frame(d,学名=l$学名[i],科名=l$科名[i],属名=l$属名[i])

sp
spn<-nrow(d);spn           ### 349種->394-4 (NA,水面,)
unique(d$科名)			### 78科->78
unique(d$属名)		### 200属->209
i<-!is.na(d$学名);sum(i)	### 367種　未同定　23種

ifor<-d$森林!=""
iveg<-d$植生!=""
inn<-d$外来!=""


lapply(list(ifor,iveg,inn),sum) ## 森林　33種、植生　297種、外来　63種　

d2<-subset(d,!is.na(学名))
nrow(d2)### のべ種数
i<-d2$森林3!="";j<-d2$植生3!="";k<-d2$外来3!=""
ii<-i | j | k
paste("森林",sum(i),"種;　植生",sum(j),"種;　外来",sum(k),"種")

i<-d2$森林4!="";j<-d2$植生4!="";k<-d2$外来4!=""
ii<-i | j | k
paste("森林",sum(i),"種;　植生",sum(j),"種;　外来",sum(k),"種")

paste(d2$sp[j & k],collapse = ", ")
data.frame(d2$sp[j & k],d2$植生4[j & k])



外来34<-(d2$外来3!="" |d2$外来4!="")

### 新たに記録された外来種
d2$sp[xor(外来34,d2$外来4!="")]


intersect(which(d2$外来4!=""),which(外来34))
sum(外来34)
sum(d2$外来3!="")
sum(d2$外来4!="")

is.na(match(which(外来34),which(d2$外来4!="")))
d2$sp[i]

#######
names(d)
d[1:5,]
source("m:/00D/文献/Flora/FloraListMaker_source.r")
ls.str(mode="function")
memo<-paste(d[,3],"/",d[,4],"/",d[,5])


memo<-d$all
FloraListMaker(sp,memo = memo, file = "flora_list.htm")



# folra_YList_check ####
i.na<-which(is.na(match(flora$spj,APG$種名)))
cat("以下の種はYListにありません")
spj.<-flora$spj[i.na]
spj.[-grep("科",spj.)]
# vv_YList_check ####
i.na<-which(is.na(match(unique(vv$sp),APG$種名)))
cat("以下の種はYListにありません")
spj.<-unique(vv$sp)[i.na]
spj.



