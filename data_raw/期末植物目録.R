# RData_flora.R
flora<-read.csv("data_raw/第5期植物目録.csv")
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
###### 第Ⅳ期毎木樹木分布
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

#########################
###### 第Ⅳ期外来植物分布
#########################


dir()
d<-read.csv("第Ⅳ期外来種調査報告書用全体表.csv",skip=1,as.is=TRUE, header = FALSE)
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

### write.csv(spc,file="第Ⅳ期_外来種分布一覧.csv")
### write.table(spc,"clipboard")
### write.csv(data.frame(site),file="2013外来種分布一覧_site.csv")


########### 調査データ結合
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


#####　Ⅲ期　+　Ⅳ期

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

##################### 以上


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

############################################

##### 2020立山植生モニタリング植物目録作成.R  //Ishiad
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
