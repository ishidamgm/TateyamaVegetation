
#' クロステーブル作成　被度→被度階級
#'
#' @param cv
#'
#' @return
#' @export
#'
#' @examples
#'
#' Domin_Krajina(26.5)
#'
Domin_Krajina<-function(cv){
		if(cv==100) return(10)
		if(cv>=75)   return(9)
		if(cv>=50)   return(8)
		if(cv>=33)   return(7)
		if(cv>=20)   return(6)
		if(cv>=10)   return(5)
		if(cv>=5)   return(4)
		if(cv>=3)   return(3)
		if(cv>=2)   return(2)
		if(cv>=1)   return(1)
		if(cv>0)   return(0.5)
		if(cv==0)   return(0)
		}

#' Title
#'
#' @param r
#'
#' @return
#' @export
#'
#' @examples
#' sapply(seq(0,1,.1),hindo)
#'
hindo<-function(r) {## floor(r/0.2-0.00001)+1
	if(r>0.8)   return("Ⅴ")
	if(r>0.6)   return("Ⅳ")
	if(r>0.4)   return("Ⅲ")
	if(r>0.2)   return("Ⅱ")
	if(r>0)   	return("Ⅰ")
	if(r==0)   	return("-")
			}



#' 各調査区の1〜4期の出現種と各期被度合計
#' ※　1期については　Domin_Krajina
#'
#' @param 調査地   漢字で入力のこと
#'
#' @returns  data.frame
#'
#' @export
#'
#' @examples
#'
#' ( df <-　PlotSpecies_1to4 ("美松"))
#'  # write.csv(df, "data_raw/PlotSpecies美松.csv")
#'
#'  PlotSpecies_1to4 ("有峰")
#'  # write.csv(df, "data_raw/PlotSpecies美松.csv")
#'
PlotSpecies_1to4 <-function(調査地="有峰"){
  d<-subset(vv,plot=="Arimine")
  sp.<-unique(d$sp)

  return(
    data.frame(
    cbind(
      c01=tapply(d$dk01,d$sp,sum),
      c02=tapply(d$c02,d$sp,sum),
      c03=tapply(d$c04,d$sp,sum),
      c04=tapply(d$c04,d$sp,sum))
  )
  )
}




