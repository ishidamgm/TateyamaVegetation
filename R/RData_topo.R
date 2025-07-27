# RData_topo.R

#' RData_topo
#' 中島さんが測量した調査区内の小川や歩道，補足測量点のデータをtopoとしてまとめる
#'
#'
#' @return
#' @export
#'
#' @examples
#' # not run !!  see body of this function with type 'RData_topo'
#' topo
#'
RData_topo <- function(){
  print("not run !!  see body of this function with type 'RData_topo'")
  f <-dir() #"@Bijodaira.csv"     "@Bunadaira.csv"     "@Bunazaka.csv"      "@Kaminokodaira.csv" "@Mimatsu.csv"
  .<-c()
  for( i in 1:length(f)){
    pn<-substr(f[i],2,nchar(f[i])-4)
    .<-rbind2(.,data.frame(plot=pn,read.csv(f[i],fileEncoding = "cp932")))
  }

  (topo<-.)

  # save(topo,file="../../../data/topo.RData")
}



#　毎木調査データにzデータを追加する



#' data/RData
#'
#' @return
#' @export
#'
#' @examples
#' treesXYZ
#'
RData_treesXYZ <- function(){
  f<-c("Bijodaira2021.csv","Bunazaka2020.csv","Bunadaira2023.csv",
       "Kaminokodaira2024.csv","Matsuotoge2022.csv","Mimatsu2019.csv","Kagamiishi2022.csv","Arimine2019.csv")


  .<-c()
  for( i in 1:length(f)){
    .<-c(.,list(read.csv(f[i],fileEncoding = "cp932")))
  }

  names(.)<-plt$na

  lapply(.,names)
  # 鏡石z値暫定入力
  .k<-.$Kagamiishi
  .k$z<-2250+.k$y*tan(30*pi/180)
  .$Kagamiishi<-.k

  #ラベル，xyz　データ結合
  .xyz<-c()
  for( i in 1:length(f)){
    ..<-.[[i]]
    .xyz<-rbind(.xyz,data.frame(plot=plt$na[i],sp=..$sp,lb=..$lb,x=..$x,y=..$y,z=..$z))
  }

  .xyz

  treesXYZ<-.xyz

  #  save(treesXYZ,file="treeXYZ.RData")
}



#' data/RData
#'  毎木xyz + topo xyz(中島さん@plot.csv)
#'
#'
#' @return
#' @export
#'
#' @examples
#' allXYZ
#'
RData_allXYZ <- function(){
  .<-rbind2(topo[,-7],treesXYZ)
  .[order(.$plot,.$sp,.$lb),]
  allXYZ <-.
  View(allXYZ)
  # save(allXYZ,file="data/allXYZ.RData")
}

# Bijodaira.stream1 <- subset(topo,plot==pn & sp=="stream1")
#
# list(Bijodaira=c("stream1","walk1","walk2"),Bunadaira=c("walk1"),
#      Bunazaka=c("stream1","stream2"),     Kaminokodaira=c("stream1","stream2"),
#                                                           Mimatsu=c("stream1"
#                                                           ))
# i<-topo$plot=="Bijodaira" & topo$sp!="survey"
# unique(topo$sp[i])
#
# plot(topo$x[i],topo$y[i],type="o")
#
# pn<-"Bijodaira"
# i<-topo$plot==pn & topo$sp!="survey" & substr(topo$sp,1,4) =="stre"
# j<-topo$plot==pn & topo$sp!="survey" & substr(topo$sp,1,4) =="walk"
# stream.<-unique(topo$sp[i])
# walk.<-unique(topo$sp[j])
#
# stream_XY <-function(pn="Bijodaira"){
#   i<-topo$plot==pn & topo$sp!="survey" & substr(topo$sp,1,4) =="stre"
#   stream.<-unique(topo$sp[i])
#   XY<-c()
#   for (j in 1:length(stream.)){
#     XY<-c( XY,)
#   }
#
#   topo$sp[i]==stream.
# }
#
# sum(topo$plot==pn & topo$sp=="walk3")

#' Title
#'
#' @return
#' @export
#'
#' @examples
#' par(mfrow=c(3,3))
#' for(i in 1:nrow(plt)){
#'  contour(rasXYZ[[i]],main=plt$na[i])
#' }
#'ggplot() +
#'geom_contour(data =rasXYZ[[1]], aes(x = x, y = y, z = z), color = "blue")
#'
RData_rasXYZ <- function(){
  # library(akima)
  # .<-allXYZ[allXYZ$plot=="Bijodaira",c("x","y","z") ]
  # .<-na.omit(.)
  # interp(.)
  # contour(interp(.$x,.$y,.$z, duplicate = "strip"))
  #
  # ..<-c()
  # for(i in 1:nrow(plt)){
  #   .<-allXYZ[allXYZ$plot==plt$na[i],c("x","y","z") ]
  #   .<-na.omit(.)
  #   ..<-c(..,list(interp(.$x,.$y,.$z, duplicate = "strip")))
  # }
  # names(..)<-plt$na
  #
  # rasXYZ<-..
  #  save(rasXYZ,file="data/rasXYZ.RData")
}






