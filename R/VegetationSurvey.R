
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
#' cover<-seq(0,100,1)
#' plot(cover, Domin_Krajina(cover),type="l")
#'
#'
Domin_Krajina <- function(cv) {
  ifelse(cv == 100, 10,
         ifelse(cv >= 75, 9,
                ifelse(cv >= 50, 8,
                       ifelse(cv >= 33, 7,
                              ifelse(cv >= 20, 6,
                                     ifelse(cv >= 10, 5,
                                            ifelse(cv >= 5,  4,
                                                   ifelse(cv >= 3,  3,
                                                          ifelse(cv >= 2,  2,
                                                                 ifelse(cv >= 1,  1,
                                                                        ifelse(cv >  0,  0.5,
                                                                               0)))))))))))
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
#' @param site　　ex.　"Arimine"
#'
#' @returns  data.frame
#'
#' @export
#'
#' @examples
#'
#' ( df <-　PlotSpecies_1to4 ("Mimatsu"))
#'  #
#'
#'  ( df2 <-　PlotSpecies_1to4 ("Arimine"))
#'  # write.csv(df2, "data_raw/PlotSpecies有峰.csv")
#'
PlotSpecies_1to4 <-function(site="Mimatsu"){
  d<-subset(vv,plot==site)
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



#' 特定種の野帳記録
#'
#' @param sp.
#'
#' @returns　データフレーム　特定種の野帳記録
#'
#'
#' @export
#'
#' @examples
#' vv_sp_check("ヒロバスゲ")
#'
#' vv_sp_check("ウダイカンバ")
#'
#' vv_sp_check("ミヤマイタチシダ")
#'
#'
#'
vv_sp_check<-function(sp.="ヒロバスゲ"){
  subset(vv,sp==sp.)
}


