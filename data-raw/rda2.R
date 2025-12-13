# APG ####
#'  data/APG　植物目録
#' 米倉浩司・梶田忠 (2007-) 植物和名ー学名インデックスYList を利用させていただきました。
#'
#' @docType data
#' @name APG
#' @usage data(APG)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' APG
"APG"


#' APG植物目録()
#' 米倉浩司・梶田忠 (2007-) 植物和名ー学名インデックスYList
#' を利用させていただきました
#'
#' @returns
#'
#' @export
#'
#' @examples
#' head(APG)
#' sp.<-c("コイワカガミ","オオバユキザサ","コガネギク","イ","コゴメグサ","タテヤマオウギ")
#' sp..<-c("イワカガミ","ヤマトユキザサ","アキノキリンソウ","イグサ","イブキコゴメグサ","イワオウギ")
#' cat("以下はAPGリストにありません")
#' data.frame(sp=sp.[!is.element(sp.,APG$種名)])
#' data.frame(sp=sp..[!is.element(sp.,APG$種名)])
#' is.element(c("コイワカガミ","オオバユキザサ","コガネギク","イ","コゴメグサ","タテヤマオウギ"),YList$別名)
#' is.element(c("イワカガミ","ヤマトユキザサ","アキノキリンソウ","イグサ","イブキコゴメグサ","イワオウギ"),YList$和名)
#'
#'
#'
#'
RData_APG<-function(){

}
