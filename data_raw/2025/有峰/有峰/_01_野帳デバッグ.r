####################################################
###  �쒠�f�o�b�O.r

###  �y�ړI�z��͗p�E����쒠�f�[�^�̍쐬
###�@	�햼���̓~�X�̊m�F�@
###�@	�K�w�����@
###  	�d���퓝��
###	�t�@�C���ۑ�

###################�y�ݒ�z>>>>>>>>>>>>>>>>>>

### ���[�L���O�E�f�B���N�g��
#wd<-"m:/dropbox/00D/00/���R�A�����j�^�����O����/��04��/����30�N�x/�A������/R/����"
setwd(wd)
dir()

###�@�쒠�f�[�^
#yachou<-"H30�A�������쒠_����.csv"   #### �f�o�b�N�O


yachou<-"H31�A�������쒠_�L��.csv"   #### �f�o�b�N�O
yachou2<-"10�L��-�A���쒠2019.csv" �@#### �f�o�b�N��


###################�y�ݒ�z<<<<<<<<<<<<<<<<<<<

##�쒠�f�[�^�t�@�C���ǂݍ���   
(d<-read.csv(yachou,as.is=TRUE))
#edit(d)
d[is.na(d)]<-""
names(d)

##### ��x�f�[�^�̐��l��
col<-c(4,5,6,7)   				#### I, II, III,VI ���̃f�[�^�̃R����
names(d)[col]
nn<-d[,col]
nn[nn==""]<-0
for (i in 1:ncol(nn))nn[,i]<-as.numeric(nn[,i])
which(is.na(nn),arr.ind = TRUE)
nn[is.na(nn)]<-0					###�@�s���I�h��X�y�[�X�ɂȂ��Ă���Ƃ�����C��
str(nn)
d[,col]<-nn
str(d)							### �m�F

### d�ɖ쒠�̃I���W�i���ʂ��ԍ���id_note�Ƃ��Ēǉ�
d<-data.frame(d,id_note=1:nrow(d))

####�@�K�w���� �V���Ȗ쒠�f�[�^���@dd�@�Ƃ���
(dd<-subset(d, �K�w!="A1" &  �K�w!="A2" &  �K�w!="B1" & �K�w!="L"))


#####�@���R�A���ژ^�Ɩ쒠�̎햼�m�F
flora<-read.csv( "../��W���A���ژ^.csv",as.is=TRUE)
spl<-data.frame(table(dd$�햼))
spl<-data.frame(spl,spl=flora$sp[match(spl[,1],flora$spj)])
(spna<-spl[is.na(spl$spl),]) ### ����0�ł���΂��ׂĖژ^�ɂ��邱�Ƃ������@
as.vector(spna$Var1)

###�@��̐����^�̓���
i<-match(dd$�햼,flora$spj)
(spfm<-flora$form[i])
names(table(flora$form))
sptype<-data.frame(
�R�[�h=c("bl","bs","cds","cl","cs","ds","f","h","l","sasa"),
�����^=c("�L�t������","�L�t�����","�n�C�}�c","�j�t������","�j�t�����","�␫���","�V�_","���{","��","�T�T"),
�K�w=c("B2","B2","B2","B2","B2","C","C","C","L","S"))
dd$�햼[which(is.na(match(spfm,sptype$�R�[�h)))] ###  character(0)�@�ƂȂ�܂ŏC��
(cls2<-sptype$�K�w[match(spfm,sptype$�R�[�h)])
(dd$�K�w<-cls2)
####�@�T�u�v���b�g 	NA����
(subp<-dd$�T�u�v���b�g)
for (i in 1:length(subp)){if(subp[i]==""){subp[i]<-subp[i-1]}}
(dd$�T�u�v���b�g<-subp)


## ���בւ�
(dd<-dd[order(as.numeric(dd$�T�u�v���b�g),dd$�K�w,dd$�햼),])

##�@�K�w"L"��̏��O�@���쒠��"L"�ƂȂ��Ă��炸subset�̃t�B���^�����O�ł��ꂽ����
(dd<-dd[dd$�K�w!="L",])

###############################################################
####�@�N�}�C�U�T�@���@�`�V�}�U�T�ɓ��� !!!!!!!!!!! ��̏����̂�
sp<-dd$�햼
dd$�햼[sp=="�N�}�C�U�T"]<-"�`�V�}�U�T"
###############################################################

###�@����T�u�v���b�g���d���L�ڎ�̓���
###�@��b�ϐ���荞��

(subp<-dd$�T�u�v���b�g)
(sp<-dd$�햼)
(subp_<-unique(subp))

for (ii in 1:length(subp_)){### ii<-25 length(subp_)
	(spsubp_tab<-table(sp[subp_[ii]==subp]))
	(spdupl<-names(spsubp_tab[spsubp_tab>1])) #### ��̃T�u�v���b�g�ɏd���̂����
i<-1
  while (i<=length(spdupl)){## i<-2
	spdupl[i]
	(j<-which(subp==subp_[ii] & sp==spdupl[i]))	####�@dd�ʂ��ł̏d����̔ԍ�
	goukei<-colSums(dd[subp==subp_[ii] & spdupl[i]==sp,col])
	dd[j[1],col]<-goukei				### �d���f�[�^�擪�sj[1]�ɓ���
	dd$id_note[j[1]]<-paste(dd$id_note[j],collapse="_")
	dd$���l[j[1]]<-paste(dd$���l[j],collapse="_")
	dd<-dd[-j[-1],] 				### �d���f�[�^�擪�sj[-1]�ȊO�̏d���s�̍폜 
	subp<-dd$�T�u�v���b�g
	sp<-dd$�햼
	i<-i+1
				}
}

####�@write.csv(dd,yachou2,row.names = FALSE)
##edit(dd)
