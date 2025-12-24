#### 集計.R

### 年度を調査年度に変換すること　例　2017→2018 !!!!

### ワーキング・ディレクトリの設定
　wd<-"m:/00D/00/立山植生モニタリング事業/第04期/平成30年度/植生調査/R/美松"
setwd(wd)
dir()
####
dtab<-read.csv("報告書用-頻度_被度_優占度表.csv",as.is=TRUE)
dtab<-dtab[,-1] ### write.csvで自動的に生成された1列目を削除
data.frame(names(dtab))


i<-dtab$階層=="A1" | dtab$階層=="A2" | dtab$階層=="B1" | is.na(dtab$form)
dd<-dtab[!i,]
##### どんな種が多いのか?
####平均優占度
(typc<-levels(factor(dd$階層)))
(typf<-levels(factor(dd$form)))
(mF<-apply(dd[,4:7],1,mean))
(mv<-apply(dd[,16:19],1,mean))

(mF<-apply(dd[,5:7],1,mean))
(mv<-apply(dd[,13:15],1,mean))

#####　Ⅱ～Ⅳ期平均
plot(mF,mv,type="n",xlab="Ⅱ～Ⅳ期平均出現頻度",ylab="Ⅱ～Ⅳ期平均被度")
text(mF,mv,dd$種名,cex=.5)
plot(mF,mv,type="n",xlab="Ⅱ～Ⅳ期平均出現頻度",ylab="Ⅱ～Ⅳ期平均被度",ylim=c(0,10))
text(mF,mv,dd$種名,cex=.5)

(mF<-apply(dd[,5:7],1,mean))
(mv<-apply(dd[,13:15],1,mean))

#####　Ⅳ期
plot(dd[,7],dd[,15],type="n",xlab="出現頻度　(％)",ylab="平均被度　(％)")
text(dd[,7],dd[,15],dd$種名,cex=.7)

dd$種名[order(mv)] ##### 総合優占度の順位

#### 種数集計
(t2<-table(dtab$階層));paste(names(t2),"層",t2,"種",collapse =",",sep="")
(t1<-table(dtab$form));
dtab[-(1:31),]
table(dtab$form)

fm<-dd$form
fm[fm=="bl" |fm=="cl"]<-"tree"
fm[fm=="bs" |fm=="cs" | fm=="ds" ]<-"shrub"
(tfm<-table(dd$form))
##

ffm<-data.frame(form=c("針葉樹高木","広葉樹高木","針葉樹低木","広葉樹低木","矮性低木","草本","藤本","シダ類","ササ類"),
		   code=c("cl","bl","cs","bs","ds","h","l","f","sasa"))
tfm2<-data.frame(form=ffm$form[match(names(tfm),ffm$code)],tfm)
paste(tfm2[,1],tfm2[,3],"種",collapse =",",sep="")

### 構成種
for(i in 1:nrow(ffm)){
print(as.vector(ffm$form[i]))
print(paste(dd$種名[dd$form==ffm$code[i]],collapse =",",sep=""))
}

#######

paste(names(t2),"層",t2,"種",collapse =",",sep="")



#### 総合優占度
data.frame(names(dd))
i<-order(dd[,3],dd[,19],decreasing=TRUE)
data.frame(dd[i,c(1,2,3,19)])

#### 頻度　主要樹種
i<-order(dd$F2018,decreasing=TRUE)
j<-4:7
z<-data.frame(dd[i,c(1,2,3,j)])
paste(head(z$種名,10),collapse =",")
paste(head(z$種名,10),"(",head(sprintf("%.1f",z$F2018),10),"%",")",collapse =",",sep="")



#### 被度
i<-order(dd$V2018,decreasing=TRUE)
z<-dd[i,]
(Vtotal<-sum(dd$V2018)) ### 全出現種の平均被度合計は131.7692％であった
(V<-cumsum(dd$V2018[i]))
dd$種名[i]
plot(V/Vtotal)
paste(head(z$種名,10),"(",head(sprintf("%.1f",z$V2018),10),"%",")",collapse =",",sep="")


## ササ
dd[dd$form=="sasa",c("種名","F2018","V2018")]


##

#### 頻度-被度-総合優占度
fmn<-match(dd$form,ffm$code)
par(mfrow=c(1,3))

x<-dd$F2018;y<-dd$V2018
plot(x,y,xlab="出現頻度(％)",ylab="平均被度(％)",pch=fmn,col=fmn)
i<-tail(order(y),9);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)


x<-dd$F2018;y<-dd$v2018
plot(x,y,xlab="出現頻度(％)",ylab="総合優占度",pch=fmn,col=fmn)
i<-tail(order(y),10);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)


x<-dd$V2018;y<-dd$v2018
plot(x,y,xlab="平均被度(％)",ylab="総合優占度",,pch=fmn,col=fmn)
i<-tail(order(y),15)[8:15];text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)
##i<-tail(order(x),12);text(x[i],y[i],dd$種名[i],pos= 2,cex=0.6)
legend(20,40,ffm$form,pch=1:nrow(ffm),col=1:nrow(ffm),cex=0.8)

#### 出現






