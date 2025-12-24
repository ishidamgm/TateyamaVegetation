#### Wv.R

### ”N“x‚π’²Έ”N“x‚Ι•Ο·‚·‚ι‚±‚Ζ@—α@2017¨2018 !!!!

### ƒ[ƒLƒ“ƒOEƒfƒBƒƒNƒgƒ‚Μέ’θ

dir()
####
dtab<-read.csv("•ρ‘—p-•p“x_”ν“x_—Dθ“x•\.csv",as.is=TRUE)
dtab<-dtab[,-1] ### write.csv‚Ε©“®“I‚Ι¶¬‚³‚κ‚½1—ρ–Ϊ‚πν
data.frame(names(dtab))


i<-dtab$K‘w=="A1" | dtab$K‘w=="A2" | dtab$K‘w=="B1" | is.na(dtab$form)
dd<-dtab[!i,]
##### ‚Η‚ρ‚Θν‚ª‘½‚Ά‚Μ‚©?
####•½‹Ο—Dθ“x
(typc<-levels(factor(dd$K‘w)))
(typf<-levels(factor(dd$form)))
(mF<-apply(dd[,4:7],1,mean))
(mv<-apply(dd[,16:19],1,mean))

(mF<-apply(dd[,5:7],1,mean))
(mv<-apply(dd[,13:15],1,mean))

#####@‡U`‡Wϊ•½‹Ο
plot(mF,mv,type="n",xlab="‡U`‡Wϊ•½‹Οo»•p“x",ylab="‡U`‡Wϊ•½‹Ο”ν“x")
text(mF,mv,dd$ν–Ό,cex=.5)
plot(mF,mv,type="n",xlab="‡U`‡Wϊ•½‹Οo»•p“x",ylab="‡U`‡Wϊ•½‹Ο”ν“x",ylim=c(0,10))
text(mF,mv,dd$ν–Ό,cex=.5)

(mF<-apply(dd[,5:7],1,mean))
(mv<-apply(dd[,13:15],1,mean))

#####@‡Wϊ
plot(dd[,7],dd[,15],type="n",xlab="o»•p“x@(“)",ylab="•½‹Ο”ν“x@(“)")
text(dd[,7],dd[,15],dd$ν–Ό,cex=.7)

dd$ν–Ό[order(mv)] ##### ‘‡—Dθ“x‚Μ‡Κ

#### ν”Wv
(t2<-table(dtab$K‘w));paste(names(t2),"‘w",t2,"ν",collapse =",",sep="")
(t1<-table(dtab$form));
dtab[-(1:31),]
table(dtab$form)

fm<-dd$form
fm[fm=="bl" |fm=="cl"]<-"tree"
fm[fm=="bs" |fm=="cs" | fm=="ds" ]<-"shrub"
(tfm<-table(dd$form))
##

ffm<-data.frame(form=c("j—tχ‚–Ψ","L—tχ‚–Ψ","j—tχ’α–Ψ","L—tχ’α–Ψ","αβ«’α–Ψ","‘–{","“΅–{","ƒVƒ_—ή","ƒTƒT—ή"),
		   code=c("cl","bl","cs","bs","ds","h","l","f","sasa"))
tfm2<-data.frame(form=ffm$form[match(names(tfm),ffm$code)],tfm)
paste(tfm2[,1],tfm2[,3],"ν",collapse =",",sep="")

### \¬ν
for(i in 1:nrow(ffm)){
print(as.vector(ffm$form[i]))
print(paste(dd$ν–Ό[dd$form==ffm$code[i]],collapse =",",sep=""))
}

#######

paste(names(t2),"‘w",t2,"ν",collapse =",",sep="")



#### ‘‡—Dθ“x
data.frame(names(dd))
i<-order(dd[,3],dd[,19],decreasing=TRUE)
data.frame(dd[i,c(1,2,3,19)])

#### •p“x@ε—vχν
i<-order(dd$F2019,decreasing=TRUE)
j<-4:7
z<-data.frame(dd[i,c(1,2,3,j)])
paste(head(z$ν–Ό,10),collapse =",")
paste(head(z$ν–Ό,10),"(",head(sprintf("%.1f",z$F2019),10),"%",")",collapse =",",sep="")



#### ”ν“x
i<-order(dd$V2019,decreasing=TRUE)
z<-dd[i,]
(Vtotal<-sum(dd$V2019)) ### ‘So»ν‚Μ•½‹Ο”ν“x‡v‚Ν131.7692“‚Ε‚ ‚Α‚½
(V<-cumsum(dd$V2019[i]))
dd$ν–Ό[i]
plot(V/Vtotal)
paste(head(z$ν–Ό,10),"(",head(sprintf("%.1f",z$V2019),10),"%",")",collapse =",",sep="")


## ƒTƒT
dd[dd$form=="sasa",c("ν–Ό","F2019","V2019")]


##

#### •p“x-”ν“x-‘‡—Dθ“x
fmn<-match(dd$form,ffm$code)
par(mfrow=c(1,3))

x<-dd$F2019;y<-dd$V2019
plot(x,y,xlab="o»•p“x(“)",ylab="•½‹Ο”ν“x(“)",pch=fmn,col=fmn)
i<-tail(order(y),9);text(x[i],y[i],dd$ν–Ό[i],pos= 2,cex=0.6)


x<-dd$F2019;y<-dd$v2019
plot(x,y,xlab="o»•p“x(“)",ylab="‘‡—Dθ“x",pch=fmn,col=fmn)
i<-tail(order(y),10);text(x[i],y[i],dd$ν–Ό[i],pos= 2,cex=0.6)


x<-dd$V2019;y<-dd$v2019
plot(x,y,xlab="•½‹Ο”ν“x(“)",ylab="‘‡—Dθ“x",,pch=fmn,col=fmn)
i<-tail(order(y),15)[8:15];text(x[i],y[i],dd$ν–Ό[i],pos= 2,cex=0.6)
##i<-tail(order(x),12);text(x[i],y[i],dd$ν–Ό[i],pos= 2,cex=0.6)
legend(10,40,ffm$form,pch=1:nrow(ffm),col=1:nrow(ffm),cex=0.8)

#### o»






