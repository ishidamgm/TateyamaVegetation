#### 集計.R

plot_name <- "松尾峠"

OS <- "win" #"win"

library("ggplot2")
library("ggrepel")



### 年度を調査年度に変換すること　例　2017→2018 !!!!

### ワーキング・ディレクトリの設定
#　wd<-"m:/00D/00/立山植生モニタリング事業/第04期/平成30年度/植生調査/R/美松"
# setwd(wd)
dir()
####
# ifelse(OS=="wim",
#   d0<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE),
#   d0<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE,fileEncoding="CP932")  #,fileEncoding="SHIFT-JIS"
# )

d0<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE)

d0<-d0[,-1] 		### write.csvで自動的に生成された1列目を削除  # edit(d0)
(nm<-names(d0))	#edit(dd) ; data.frame(nm)

### 階層除外

i<-d0$階層=="A1" | d0$階層=="A2" | d0$階層=="B1" | d0$階層=="L" | is.na(d0$form)

d0<-d0[!i,]	

##### どんな種が多いのか?
####平均優占度
(typc<-levels(factor(d0$階層)))
(typf<-levels(factor(d0$form)))
Fc <- grep("F",nm) ; Vc<-grep("V",nm) ; vc<-grep("v",nm)
mF<-apply(d0[,Fc],1,mean)		#
mv<-apply(d0[,vc],1,mean)
(mFv<-data.frame(mF,mv,sp=d0$種名))
d0$種名[order(mv)] ##### 総合優占度の順位

#####　II～V期平均

g<-ggplot(mFv, aes(x = mF, y = mv, label = sp )) +xlim(-20,100)+ylim(-20,80)+
  #theme(panel.border = element_blank(), axis.line = element_line())+
  xlab("II - V 期平均出現頻度 (%)")+
  ylab("II - V 期平均総合優占度 ")+
  #scale_x_continuous(breaks=seq(40,100,20),labels=1:4)+scale_y_continuous(breaks=c(-40,20,80,120))+
  geom_point(col="red") +
  geom_text_repel(size=3,fontface="bold",max.overlaps=1000)#)


g1<-g+theme(axis.title=element_text(size=18,face="bold"))

g2<-g1+theme(axis.text=element_text(size=15))

g3<- g2 + 
     geom_hline(yintercept=0,linetype="dashed",colour="blue") +
     geom_vline(xintercept=0,linetype="dashed",colour="blue")

g3+labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))




#####　最新期集計
(Fc_last <- rev(Fc)[1])
(Vc_last <- rev(Vc)[1])
(vc_last <- rev(vc)[1])


d　<-　d0[d0[,tail(Fc,1)]!=0,  ] #今季調査で記録のあったもの


#### 種数集計
(t2<-table(d$階層));
paste(names(t2),"層",t2,"種",collapse =",",sep="")
(t1<-table(d$form));

fm<-d$form
fm[fm=="bl" |fm=="cl"]<-"tree" ; fm[fm=="bs" |fm=="cs" | fm=="ds" ]<-"shrub" ;fm[fm=="h"]<-"herb" ;fm[fm=="f"]<-"fern"
(tfm<-table(fm))

##
ffm<-data.frame(form=c("針葉樹高木","広葉樹高木","針葉樹低木","広葉樹低木","矮性低木","草本","藤本","シダ類","ササ類"),
		   code=c("cl","bl","cs","bs","ds","h","l","f","sasa"))
ffm
#### 種ごとに集計する


(tfm2<-data.frame(form=ffm$form[match(names(t1),ffm$code)],t1))
(tfm3<-paste(tfm2[,1],tfm2[,3],"種",collapse =",",sep=""))
(tfm3_<-paste0(tfm2[,1],tfm2[,3],"種 (#)"))



### 構成種
txt<-c()
for(i in 1:nrow(tfm2)){#i=1
	#print(as.vector(ffm$form[i]))
	txt_<-paste(d$種名[d$form==tfm2[i,2]],collapse =",",sep="")
	txt_<-gsub("#",txt_,tfm3_[i])
	txt<-c(txt,txt_)
}

paste(txt,collapse =", ")　　　####　報告書貼り付け用


####　過去に記録し，今回記録されなかった種

( d_na　<-　d0[d0[,tail(Fc,1)]==0,] )
table(d_na$階層)

  d_na[,c(1,2, grep("F",nm))]
nm
paste(d_na$種名,d_na$階層,collapse =",",sep="")


#######




#### 総合優占度
dd<-d
data.frame(names(dd))
i<-order(dd[,3],dd[,vc_last],decreasing=TRUE)
data.frame(dd[i,c(1,2,3,vc_last)])

#### 頻度　主要樹種
i<-order(dd[,Fc_last],decreasing=TRUE)
j<-Fc
z<-dd[i,c(1:3,Fc_last)]
paste(head(z$種名,10),z[1:10,ncol(z)],collapse =",")
paste(head(z$種名,10),"(",sprintf("%.1f",head(z[,ncol(z)],10)),"%",")",collapse =",",sep="")



#### 被度
V_last<-dd[,Vc_last]　# data.frame(names(dd))
i<-order(V_last,decreasing=TRUE)
z<-dd[i,c(1:3,Vc_last)]
(Vtotal<-sum(V_last)) ### 全出現種の平均被度合計は76.8％であった
(V<-cumsum(V_last))
dd$種名[i]
plot(V/Vtotal)
sp10<-z$種名[1:10]
hido10<-z[,ncol(z)][1:10]
paste(head(z$種名,10),"(",head(sprintf("%.1f",z[,ncol(z)],10)),"%",")",collapse =",",sep="")


## ササ
dd[dd$form=="sasa",c(1:3,Fc)]


##

#### 頻度-被度-総合優占度
fmn<-match(dd$form,ffm$code)

###########    "出現頻度(％)" - "平均被度(％)

d_ <- data.frame(x=d[,Fc_last], y=d[,Vc_last], sp=d$種名)
g<-ggplot(d_, aes(x, y , label = sp )) +xlim(-20,100)+ylim(-5,11)+
  xlab("出現頻度(％)")+  ylab("平均被度(％)")+
  geom_point(col="red") +
  geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
  geom_hline(yintercept=0,linetype="dashed",colour="blue") +
  geom_vline(xintercept=0,linetype="dashed",colour="blue")+
  labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))

g

###########    "出現頻度(％)" - "総合優占度"

d_ <- data.frame(x=d[,Fc_last], y=d[,vc_last], sp=d$種名)
g<-ggplot(d_, aes(x, y , label = sp )) +xlim(-20,100)+ylim(-7,75)+
  xlab("出現頻度(％)")+  ylab("総合優占度")+
  geom_point(col="red") +
  geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
  geom_hline(yintercept=0,linetype="dashed",colour="blue") +
  geom_vline(xintercept=0,linetype="dashed",colour="blue")+
  labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))

g

###########   "平均被度(％)",ylab="総合優占度"

d_ <- data.frame(x=d[,Vc_last], y=d[,vc_last], sp=d$種名)
g<-ggplot(d_, aes(x, y , label = sp )) +xlim(-15,20)+ylim(-7,75)+
  xlab("平均被度(％)")+  ylab("総合優占度")+
  geom_point(col="red") +
  geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
  geom_hline(yintercept=0,linetype="dashed",colour="blue") +
  geom_vline(xintercept=0,linetype="dashed",colour="blue")+
  labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))

g


#####  old
par(mfrow=c(1,3))

x<-dd[,Fc_last];y<-dd[,Vc_last]
plot(x,y,xlab="出現頻度(％)",ylab="平均被度(％)",pch=fmn,col=fmn)
i<-tail(order(y),9);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)


x<-dd[,Fc_last];y<-dd[,vc_last]
plot(x,y,xlab="出現頻度(％)",ylab="総合優占度",pch=fmn,col=fmn)
i<-tail(order(y),10);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)


x<-dd[,Vc_last];y<-dd[,vc_last]
plot(x,y,xlab="平均被度(％)",ylab="総合優占度",,pch=fmn,col=fmn)
i<-tail(order(y),15)[8:15];text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)
##i<-tail(order(x),12);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)
legend(20,40,ffm$form,pch=1:nrow(ffm),col=1:nrow(ffm),cex=0.8)

#### 総合優占度の経年変化
par(mfrow=c(1,1))
dd_<-subset(d,d$集計=="*")
(yr<-substr(nm[vc],2,5))

typc<-c("skyblue","darkgreen","purple")

v<-c()
for (i in 1:length(vc)){
	v <- rbind(v,tapply(dd_[,vc[i]],dd_$階層,sum))
}
v

data.frame(t(v))

barplot(t(v),name=yr,ylab="総合優占度",col=typc,xlim=c(0,nrow(v)+3),legend=colnames(v))

#### 総合優占度の経年変化
par(mfrow=c(1,3))
dat<- colSums( dd_[dd_$階層=="B2",vc])

barplot(dat,names=yr,ylab="総合優占度",main="低木層　(B2)")

dat <-  colSums( dd_[dd_$階層=="C",vc])
barplot(dat,names=yr,ylab="総合優占度",main="草本層 (C)")

dat <-  colSums( dd_[dd_$階層=="S",vc])
barplot(dat,names=yr,ylab="総合優占度",main="ササ (S)")


#######################################


ggplot(v, aes(fl, fill=drv)) + geom_bar(position="stack")




d_<-d[d$解析対象==1 & !is.na(d$解析対象),]
(t2<-table(d$階層)) ## paste(names(t2),t2)
#####
data.frame(nm)
col<-c(5,7,9,11,13)
typ<-c("B2","C","S")
typc<-c("skyblue","darkgreen","purple","green")




	cls<-d$階層;cls_<-levels(factor(cls))
	yr<-substr(names(d)[col],2,5)
	dm<-c();for (i in col) dm<-cbind(dm,tapply(d[,i],d$階層,sum))
	dm<-dm[c("B2","C","S"),]

	colnames(dm)<-yr
	#clsc<-cls_;for (i in 1:) clsc[i]<-typc[cls_[i]==typ] 
	clsc<-typ;for (i in 1:length(clsc)) clsc[i]<-typc[typ[i]==typ] 
	coln<-length(clsc)


	w<-0.9;s<-.2
	par(mfrow=c(1,1))
	barplot(dm,legend=rownames(dm),ylab="総合優占度",width =w,space=s,
		col=clsc,xlim=c(0,coln+3))


par(mfrow=c(1,3))
for (i in 1:nrow(dm))barplot(dm[i,],main=rownames(dm)[i],ylab="総合優占度　(％)")			


################################
names(d)
(xj<-grep("F",names(d))[-1])  ### Ⅰ期は除く
(yj<-grep("V",names(d))[-1])
####
par(mfrow=c(1,1))
t_1<-d
#####　高木性樹種
i<-t_1$階層=="B2" & (t_1$form=="bl" | t_1$form=="cl")
t_2<-t_1[i,]
x<-t_2[,tail(xj,1)];y<-t_2[,tail(yj,1)]
plot(x,y,pch="●",xlim=c(0,100),ylim=c(0,6),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="高木性樹種(B2層)")
text(x,y+1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])

#####　低木性樹種 + ササ
##windows()
i<-t_1$階層=="B2" & t_1$form=="bs" 
t_2<-t_1[i,]
x<-t_2[,tail(xj,1)];y<-t_2[,tail(yj,1)]
plot(x,y,pch="●",xlim=c(0,100),ylim=c(0,100),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="低木性樹種+ ササ(B2,S層)")
#text(x,y+0.3,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])
### ササ
i<-t_1$階層=="S"
t_2<-t_1[i,]
x<-t_2[,tail(xj,1)];y<-t_2[,tail(yj,1)]
points(x,y,pch="●",col="red",cex=2)
text(x,y-0.2,t_2$種名,cex=0.6,col="red",pos=2)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj],lw=4,col="red")



### つぶれた結果の表示
#windows()
i<-t_1$階層=="B2" & t_1$form=="bs" 
t_2<-t_1[i,]
x<-t_2[,tail(xj,1)];y<-t_2[,tail(yj,1)]
plot(x,y,pch="●",xlim=c(0,100),ylim=c(0,7),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="低木性樹種(B2)")
text(x,y+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])


### 条件抽出
(spn<-which(t_2$V2018>2)) ####<<<<<<<names(t_2)
t_3<-t_2[spn,];  t_3$種名
par(mfrow=c(1,3))
for (i in 1:length(spn)) barplot(as.matrix(t_3[i,yj]),ylab="平均被度 (％)",main=t_3$種名[i])
str(t_2[i,yj])

### 条件抽出
(spn<-which(t_2$V2016>2))
t_3<-t_2[spn,];  t_3$種名
par(mfrow=c(1,4),mar=c(2,4,2,0),mgp=c(2,1,0)) ###mar=c(0,0,0,0)
for (i in 1:length(spn)) barplot(as.matrix(t_3[i,xj]),ylab="出現頻度 (％)",main=t_3$種名[i])






#####　シダ
##windows()
i<- t_1$階層=="C" & t_1$form=="f"
t_2<-t_1[i,]
x<-t_2[,tail(xj,1)];y<-t_2[,tail(yj,1)]
plot(x,y,pch="●",xlim=c(0.0,100),ylim=c(0,0.5),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="シダ (C層)")
text(x,y+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])


#####　草本　
##windows()
i<- t_1$階層=="C" & t_1$form=="h"
t_2<-t_1[i,]
x<-t_2[,tail(xj,1)];y<-t_2[,tail(yj,1)]
plot(x,y,pch="●",xlim=c(0.0,100),ylim=c(0,1.5),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="草本")
text(x,y+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])

plot(x,y,pch="●",xlim=c(20,70),ylim=c(0,0.8),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="草本")
text(x,y+0.02,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])

plot(x,y,pch="●",xlim=c(0,30),ylim=c(0,0.3),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="草本")
text(x,y+0.02,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])






