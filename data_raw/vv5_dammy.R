# vv5_dammy.R
subset(vv5,plot=="Arimine")$c05<-subset(vv5,plot=="Arimine")$c04


vv5[vv5$plot=="Arimine","c05"]<-vv5[vv5$plot=="Arimine","c04"]
vv5[vv5$plot=="Mimatsu","c05"]<-vv5[vv5$plot=="Mimatsu","c04"]
# usethis::use_data(vv5,overwrite = TRUE)

vv5[vv5$plot=="Mimatsu","c05"]
