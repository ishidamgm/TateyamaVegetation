# TateyamaVegetation_RData.R


#' 種の生活型と階層
#'
#' @returns
#'
#' @export
#'
#' @examples
#' sptype
#'
RData_sptype <-function(){
  # sptype<-data.frame(
  # コード=c("bl","bs","cds","cl","cs","ds","f","h","l","sasa","w"),
  # 生活型=c("広葉樹高木","広葉樹低木","ハイマツ","針葉樹高木","針葉樹低木","矮性低木","シダ","草本","蔓","ササ","水面"),
  # 階層=c("B2","B2","B2","B2","B2","C","C","C","L","S","w"))
  # save(sptype,file="data/sptype.RData")
  sptype

}


#' data/RData　サブプロット
#'
#' @return
#'
#' @export
#'
#' @examples
#' .<-subplot_xy
#' head(.)
#' table(.$plot)
#' plot(1:100,type="n")
#' i<-.$plot=="Bunazaka"
#' text(.$x[i],.$y[i],.$subplot[i])
#' RData_subplot_xy()
#'
RData_subplot_xy <-function(){
  print("This is data !!")
}


#' data/RData 植生調査実施年
#'
#' @returns
#' @export
#'
#' @examples
#' VegetationSurveyYears
#'
RData_VegetationSurveyYears  <-function(){

  VegetationSurveyYears <- df %>%
    select(plot_name, Vegetation001:Vegetation005) %>%
    mutate(across(starts_with("Vegetation"),
                  ~ paste0(
                    if (cur_column() == "Vegetation001") "DK" else "被度",
                    substr(as.character(.), 1, 4)
                  )))

  VegetationSurveyYears
}



