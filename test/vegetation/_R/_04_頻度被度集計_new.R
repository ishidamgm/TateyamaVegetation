#### 集計.R
### 年度を調査年度に変換すること　例　2017→2023 !!!!
# 2023 記録ゼロがカウントされないように変更

Survey_Plot = "ブナ平"

# ライブラリ読み込み　####
library("ggplot2")
library("ggrepel")

#　野帳読み込み　####
dtab<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE)
data.frame(names(dtab))

dtab<-dtab[,-1] ### write.csvで自動的に生成された1列目を削除
nrow(dtab)
# 2023 記録ゼロがカウントされないように変更　####
dtab<-dtab[dtab[,(ncol(dtab)-1)] != 0, ] 
nrow(dtab)



i<-dtab$階層=="A1" | dtab$階層=="A2" | dtab$階層=="B1" | is.na(dtab$form)
dd<-dtab[!i,]	
nm<-names(dd);				#edit(dd)

#どんな種が多いのか? #### 
#平均優占度 ###
(typc<-levels(factor(dd$階層)))
(typf<-levels(factor(dd$form)))
mF <- apply(dd[,grep("F",nm)],1,mean)		#
mv <- apply(dd[,grep("v",nm)],1,mean)
mFv<- data.frame(mF,mv,sp=dd$種名)
mFv

#####　Ⅱ～Ⅳ期平均
par(mfrow=c(1,2))
plot(mF,mv,type="n",xlab="Ⅱ～Ⅴ期平均出現頻度",ylab="Ⅱ～Ⅴ期平均被度")
text(mF,mv,dd$種名,cex=.5)
plot(mF,mv,type="n",xlab="Ⅱ～Ⅴ期平均出現頻度",ylab="Ⅱ～Ⅴ期平均被度",ylim=c(0,10))
text(mF,mv,dd$種名,cex=.5)


g<-ggplot(mFv, aes(x = mF, y = mv, label = sp )) +xlim(-20,100)+ylim(-20,80)+
  #theme(panel.border = element_blank(), axis.line = element_line())+
  xlab("II - V 期平均出現頻度 (%)")+
  ylab("II - V 期平均被度 (%)")+
  #scale_x_continuous(breaks=seq(40,100,20),labels=1:4)+scale_y_continuous(breaks=c(-40,20,80,120))+
  geom_point(col="red") +
  geom_text_repel(size=3,fontface="bold",max.overlaps=1000)#)


g1<-g+theme(axis.title=element_text(size=18,face="bold"))

g2<-g1+theme(axis.text=element_text(size=15))

g3<- g2 + 
     geom_hline(yintercept=0,linetype="dashed",colour="blue") +
     geom_vline(xintercept=0,linetype="dashed",colour="blue")

g3+labs(title=Survey_Plot)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))


#####　Ⅳ期
plot(dd[,7],dd[,15],type="n",xlab="出現頻度　(％)",ylab="平均被度　(％)")
text(dd[,7],dd[,15],dd$種名,cex=.7)

dd$種名[order(mv)] # 総合優占度の順位 ####

# 種数集計 ####
# 階層別種数　######
(t2<-table(dtab$階層))
paste(names(t2),"層",t2,"種",collapse =",",sep="")
# 生活型別種数　######
(t1<-table(dtab$form))

fm<-dd$form
fm[fm=="bl" |fm=="cl"]<-"tree"
fm[fm=="bs" |fm=="cs" | fm=="ds" ]<-"shrub"
(tfm<-table(dd$form))


#　生活型別種数　######


ffm<-data.frame(form=c("針葉樹高木","広葉樹高木","針葉樹低木","広葉樹低木","矮性低木","草本","藤本","シダ類","ササ類"),
		   code=c("cl","bl","cs","bs","ds","h","l","f","sasa"))
tfm2<-data.frame(form=ffm$form[match(names(tfm),ffm$code)],tfm)
paste(tfm2[,1],tfm2[,3],"種",collapse =", ",sep=" ")

fm_sp <- c()
for(i in 1:nrow(tfm2)){
  fm_sp <- c(fm_sp,list(dd$種名[dd$form==as.vector(tfm2[i,2])]))
}
names(fm_sp)<-tfm2[,1]
for(i in 1:nrow(tfm2)){
  sp.. <- paste(fm_sp[[i]], collapse =", ")
  cat(paste (tfm2[i,1],tfm2[i,3],"種　 (", sp.. ,") 、　")) 
}



### 構成種




for(i in 1:nrow(ffm)){
print(as.vector(ffm$form[i]))
print(paste(dd$種名[dd$form==ffm$code[i]],collapse =",",sep=""))
}





#### 総合優占度
data.frame(names(dd))
i<-order(dd[,3],dd[,19],decreasing=TRUE)
data.frame(dd[i,c(1,2,3,19)])

#### 頻度　主要樹種
i<-order(dd$F2023,decreasing=TRUE)
j<-4:8
z<-data.frame(dd[i,c(1,2,3,j)])
paste(head(z$種名,10),collapse =",")
paste(head(z$種名,10),"(",head(sprintf("%.1f",z$F2023),10),"%",")",collapse =",",sep="")



#### 被度
i<-order(dd$V2023,decreasing=TRUE)
z<-dd[i,]
(Vtotal<-sum(dd$V2023)) ### 全出現種の平均被度合計は77％であった
(V<-cumsum(dd$V2023[i]))
dd$種名[i]
plot(V/Vtotal)
paste(head(z$種名,10),"(",head(sprintf("%.1f",z$V2023),10),"%",")",collapse =",",sep="")


# ササの被度の経年変化　####

sasa_df <- data.frame(Year=c(2005,2011,2017,2023),Coverage=as.numeric(dd[dd$form=="sasa",15:18]))



  ggplot(sasa_df,aes(x = Year, y = Coverage ))      + 
     geom_line()+geom_point(col="red",size=3)  +
    xlab("西暦年") + ylab("ササの平均被度 (%)")  


##

#### 頻度-被度-総合優占度
fmn<-match(dd$form,ffm$code)
par(mfrow=c(1,3))

x<-dd$F2023;y<-dd$V2023
plot(x,y,xlab="出現頻度(％)",ylab="平均被度(％)",pch=fmn,col=fmn)
i<-tail(order(y),9);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)


x<-dd$F2023;y<-dd$v2023
plot(x,y,xlab="出現頻度(％)",ylab="総合優占度",pch=fmn,col=fmn)
i<-tail(order(y),10);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)


x<-dd$V2023;y<-dd$v2023
plot(x,y,xlab="平均被度(％)",ylab="総合優占度",,pch=fmn,col=fmn)
i<-tail(order(y),15)[8:15];text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)
##i<-tail(order(x),12);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)
legend(20,40,ffm$form,pch=1:nrow(ffm),col=1:nrow(ffm),cex=0.8)

#### 出現






