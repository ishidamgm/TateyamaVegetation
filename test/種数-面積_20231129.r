####�퐔-�ʐ�.r
####################################################
############�@�@���R�A�����j�^�����O�����W�v �@�@�@�@
####################################################

### ���[�L���O�E�f�B���N�g���̐ݒ�
wd<-"../../���R�A�����j�^�����O����/��03��/����25�N�x/�A������"
setwd(wd)
### �֘A�֐��ǂݍ��݁@Domin_Krajina��
source("../../00/���R�A�����j�^�����O����/��03��/����24�N�x/�A������/���R�A��source.R")

##�쒠�t�@�C�����X�g�ǂݍ���
dd<-read.csv("�A���쒠01-10.csv",as.is=TRUE,fileEncoding = "shift-jis")
rn_plt<-which(substr(dd$�v���b�g,1,1)=="_")
site_code<-substr(dd$�v���b�g[rn_plt],2,999)

##########
ql<-c(10,10,10,10,5,10,10,5,3,10)
spn<-c();

for(ii in 1:10)
{

d2<-subset(dd,�v���b�g==as.character(ii))
#nrow(d2);str(d)

subp<-unique(d2$�T�u�v���b�g)

for (j in 1:10){
	subp_<-sample(subp,10000,replace = T)
	spn0<-c();spn1<-c()
	subpn<-length(subp)
	for (i in 1:length(subp)){
	spn0<-c(spn0,length(unique(d2$�햼[is.element(d2$�T�u�v���b�g,subp_[1:i])])))
					}
	spn1<-rbind(spn1,spn0)

		}

spn<-c(spn,list(cbind((1:subpn)*ql[ii]^2,apply(spn1,2,mean))))

}

#windows()
plot(0,type="n",xlim=c(0,3500),ylim=c(0,150),xlab="Area (m^2)",ylab="Number of Species")
for (i in 1:10)lines(spn[[i]],col=i)
for (i in 1:10)text(tail(spn[[i]],1)+c(300,0),site_code[i],col=i)

###### �T�T�̍��v�ʐ�


sasa3<-c();sasa2<-c();

for(ii in 1:10)
{
d2<-subset(dd,�v���b�g==as.character(ii))
subpn<-length(unique(d2$�T�u�v���b�g))
d2[is.na(d2)]<-0;d2[d2==""]<-0;d2[d2=="."]<-0
for (i in c(1,2,5,6,7))d2[,i]<-as.numeric(d2[,i])	### ��x�f�[�^���l��
sasa3<-c(sasa3,sum(d2[d2$�햼=="�`�V�}�U�T"�@|d2$�햼=="�N�}�C�U�T",7])/subpn)
sasa2<-c(sasa2,sum(d2[d2$�햼=="�`�V�}�U�T"�@|d2$�햼=="�N�}�C�U�T",6])/subpn)
}

#windows()
plot(sasa3)
for (i in 1:10)text(i,sasa3[i],site_code[i],col=i)
points(1:10,sasa2,col="red")


###### ��͑Ώێ�@�Œ�1�v���b�g�ŏo���p�x25%�ȏ���L�^������

spl<-table(dd$�햼)
spl<-data.frame(sp=spl[order(spl,decreasing = TRUE)])
spl2<-rownames(spl[1:100,])

### ��g�݂܂�
l<-read.csv("C:/00D/00/vegetation/R/tateyama_flora.csv",as.is=TRUE)
names(l)
plot(table(l$form))
fm<-l$form[match(spl2,l$spj)]
table(fm)

spl3<-data.frame(sp=spl2,freq=as.vector(spl[1:100,]),fm)
spl3[order(spl3$fm),]





