######　総合優占度-頻度-平均被度図.R

#ワーキング・ディレクトリの設定   ⇒　なし
#　R ver.4 以降　utf-8　デフォルトとなり　RStudio をメインとする
#　wd内のプロジェクト.Rpoj　起動でwdがカレントになるのでsetwd(wd)　必要なくなる

 rm(list=ls())

# 総合優占度_頻度_平均被度 の読み込み　##################

(d<-read.csv(dir(".",pattern="総合優占度_頻度_平均被度*"),as.is=TRUE))
(t1<-table(d$階層))
paste(names(t1),"層",t1,"種",collapse =",",sep="")

# 報告書用作表 ####
(d<-read.csv(dir(".",pattern="総合優占度_頻度_平均被度*"),as.is=TRUE))
(集計<-ifelse(d$解析対象==1 & !is.na(d$解析対象),"*",""))
options(digits=2)
data.frame(names(d))
#階層	種名	生活型	出現頻度　(％) 1-5 頻度階級	1-5	平均被度　(％) 1-5	総合優占度1-5	  ####			

j<-c(2,3,16,18,20,22,24,26,4,6,8,10,12,17,19,21,23,25,5,7,9,11,13)
#j<-c(2,3,8,10,12,14,16)##, 14,16,18,20,22,4,6,8,10,15,17,19,21,5,7,9,11,13)
names(d)[j]
(dtab<-data.frame(d[,j],集計))
# 集計 ####
names(dtab)
i<-dtab[,(ncol(dtab)-1)]!=0; table(dtab[i,]$階層)
data.frame(dtab$種名[!i],dtab$階層[!i])

# write "報告書用-頻度_被度_優占度表.csv" ########
write.csv(dtab,"報告書用-頻度_被度_優占度表.csv")

# 解析　####
d<-d[d$解析対象==1 & !is.na(d$解析対象),]
t1 ## paste(names(t1),t1)
(t2<-table(d$階層)) ## paste(names(t2),t2)

col<-c(5,7,9,11,13)
typ<-c("B2","C","L","S")
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
main.txt <- c("低木層　(B2)","草本層　(C)","ササ　(S)")
for (i in 1:nrow(dm))barplot(dm[i,],main=main.txt[i],ylab="総合優占度　(％)")		#rownames(dm)[i]	


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








#### 出現


#### 以下参考コード
#######################
###### 対数軸の検討
#######################
#names(t_1) 
j<-seq(4,11,2); t_1[,j]<-log(t_1[,j],10)
j<-seq(5,12,2); t_1[,j]<-100*t_1[,j]

#####　高木性樹種
i<-t_1$階層=="B2" & (t_1$form=="bl" | t_1$form=="cl")
t_2<-t_1[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",lim=c(0,100),ylim=c(-2,0.6),
xlab="出現頻度　(％)",ylab="log(平均被度)",main="高木性樹種")
text(t_2$頻度.3,t_2$平均被度.3+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])

#####　高木性樹種
i<-t_1$階層=="B2" & (t_1$form=="bl" | t_1$form=="cl")
t_2<-t_1[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",xlim=c(0.0,100),ylim=c(0,4),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="高木性樹種(B2層)")
text(t_2$頻度.3,t_2$平均被度.3+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])

#####　低木性樹種 + ササ
##windows()
i<-t_1$階層=="B2" & t_1$form=="bs" 
t_2<-t_1[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",xlim=c(0.0,100),ylim=c(0,12),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="低木性樹種+ ササ(B2,S層)")
text(t_2$頻度.3,t_2$平均被度.3+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])
### ササ
i<-t_1$階層=="S"
t_2<-t_1[i,]
points(t_2$頻度.3,t_2$平均被度.3,pch="●",col="red",cex=2)
text(t_2$頻度.3,t_2$平均被度.3-0.2,t_2$種名,cex=0.6,col="red")
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj],lw=4,col="red")

#####　シダ
##windows()
i<- t_1$階層=="C" & t_1$form=="f"
t_2<-t_1[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",xlim=c(0.0,100),ylim=c(0,15),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="シダ　(C層)")
text(t_2$頻度.3,t_2$平均被度.3+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])


#####　草本　
##windows()
i<- t_1$階層=="C" & t_1$form=="h"
t_2<-t_1[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",xlim=c(30,60),ylim=c(0,2.0),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="草本　(C層)")
text(t_2$頻度.3,t_2$平均被度.3+0.05,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])

plot(t_2$頻度.3,t_2$平均被度.3,pch="●",xlim=c(0.0,61),ylim=c(0,0.7),
xlab="出現頻度　(％)",ylab="平均被度 (％)",main="草本　(C層)")
text(t_2$頻度.3,t_2$平均被度.3+0.05,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])

############　　以上本文

#######################
###### 対数軸の検討
#######################
#names(t_1)
tlog<-t_1 
j<-seq(4,11,2); tlog[,j]<-log(t_1[,j],10)

#####　草本
i<-t_1$階層=="C" & (t_1$form=="h" )
t_2<-tlog[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",xlim=c(0,65),ylim=c(-3,0.6),
xlab="出現頻度　(％)",ylab="log(平均被度)",main="草本　(C層)")
text(t_2$頻度.3,t_2$平均被度.3+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])


#####　高木性樹種
i<-t_1$階層=="B2" & (t_1$form=="bl" | t_1$form=="cl")
t_2<-tlog[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",lim=c(0,100),ylim=c(-2,0.6),
xlab="出現頻度　(％)",ylab="log(平均被度)",main="高木性樹種")
text(t_2$頻度.3,t_2$平均被度.3+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])


#####　低木性樹種 + ササ
##windows()
i<-t_1$階層=="B2" & t_1$form=="bs" 
t_2<-tlog[i,]
plot(t_2$頻度.3,t_2$平均被度.3,pch="●",xlim=c(0,100),ylim=c(-2,1.2),
xlab="出現頻度　(％)",ylab="log(平均被度)",main="低木性樹種+ ササ")
text(t_2$頻度.3,t_2$平均被度.3+0.1,t_2$種名,cex=0.6)
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj])
### ササ
i<-t_1$階層=="S"
t_2<-tlog[i,]
points(t_2$頻度.3,t_2$平均被度.3,pch="●",col="red",cex=2)
text(t_2$頻度.3,t_2$平均被度.3-0.2,t_2$種名,cex=0.6,col="red")
for (i in 1:nrow(t_2))lines(t_2[i,xj],t_2[i,yj],lw=4,col="red")








