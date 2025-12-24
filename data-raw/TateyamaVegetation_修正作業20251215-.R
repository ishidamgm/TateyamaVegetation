# TateyamaVegetation_修正作業20251215-.R


# 2025/12/17 flora list の作成 ####






# 全体データに美松・有峰のデータを追加する.R　####

VV5
vv5
#
subset(vv5,plot=="Arimine")
VV5.<-split(vv5,vv5$plot)

## c04 c05変わっていない ####

# vv5をどのように作ったか　→　VV5から####

VV5$Arimine

vv5<-dplyr::bind_rows(VV5, .id = "plot")
subset(vv5,plot=="Arimine")
#期末データ更新された
# 保存　####
#save(vv5,file="data/vv5.rda")

#save(VV5,file="data/VV5.rda")

#vv<-vv5
#save(vv,file="data/vv.rda")

# 2025/12/16
#VCrepo の美松・有峰　4期5期　同じ
View(VCrepo$Arimine)
View(VCrepo)

# TateyamaVegetationAnalysys.R のコードチェックと修正 >>>>>　####

# サンプル用　生データの更新　FieldNote_Arimine2019_raw  2025/12/15 #####
FieldNote_Arimine2019_raw #1            1 B2    アオダモ          0        1          1      1   NA        1   1
d <- read_csv("~/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2025/R/2025/有峰/10有峰-植生野帳2025-最新.csv")
d
# # A tibble: 936 × 8
# サブプロット 階層  種名           DK2000 被度2007 被度2013 被度2019 被度2025
# <dbl> <chr> <chr>           <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#   1            1 B2    アオダモ          0        1  1      1        0
# 計算用に整形して確認　####
FieldNote_FormatArange(d,calc= CalculationInfo(2025,"有峰"))
FieldNote_Arimine2019_raw<-d
#save(FieldNote_Arimine2019_raw,file="data/FieldNote_Arimine2019_raw.rda")　####

# VTとVTdk を作成　保存 ####
#' 報告書用植生表リスト　全調査区1-5期　(被度・ドミン-クラジナ被度　両方対応)
#' list of vegetation table of all plots and periods
#'
#' @returns
#'
#'
#' @export
#'
#' @examples
#'
#' (VT <- VegetationTableList(DK=FALSE))
#' (VTdk <- VegetationTableList(DK=TRUE))
#'
#' #usethis::use_data(VT,VTdk,overwrite = TRUE)
#'
#'
#'
#'


#  VCrepo 報告書用植生経年変化表 を作成　保存 ####

#' 報告書用植生経年変化表　
#'
#' @param plot_name
#'
#' @returns
#' @export
#'
#' @examples
#'
#' VegetationChronologyTable_report("Arimine")
#' VCrepo<-lapply(plt$plot_name,VegetationChronologyTable_report)
#'names(VCrepo)<-plt$plot_name
#'
#'VCrepo[["Kagamiishi"]]
#'VCrepo[["Arimine"]]
#'
#'# usethis::use_data(VCrepo,overwrite = TRUE)
#'
#'

# <<<<< 以上　TateyamaVegetationAnalysys.R のコードチェックと修正　####
