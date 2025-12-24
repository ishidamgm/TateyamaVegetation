# allXYZ ####
#' data/allXYZ　調査区ランドマーク座標　(Nakajima)
#'
#' @docType data
#' @name allXYZ
#' @usage data(allXYZ)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' allXYZ
#'
"allXYZ"



# FieldNote_Arimine2019_raw ####
#' data/整理前の野帳生データ(サンプル)　FieldNote_Arimine2019_raw
#'
#' @docType data
#' @name FieldNote_Arimine2019_raw
#' @usage data(FieldNote_Arimine2019_raw)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @examples
#' FieldNote_Arimine2019_raw
"FieldNote_Arimine2019_raw"


# flora ####
#' data/flora
#'
#' @docType data
#' @name flora
#' @usage data(flora)
#' @format An object of class `spec_tbl_df`.
#' @keywords datasets
#' @examples
#' flora
"flora"



# ForestTrees ####
#' data/ForestTrees
#'
#' @docType data
#' @name ForestTrees
#' @usage data(ForestTrees)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' str(ForestTrees)
#' head(ForestTrees)
#'
"ForestTrees"



# LandMarks ####
#' data/LandMarks　中島さんのデータ
#'
#' @docType data
#' @name LandMarks
#' @usage data(LandMarks)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' head(LandMarks)
"LandMarks"



# leg ####
#' data/leg　図凡例
#'
#' @docType data
#' @name leg
#' @usage data(leg)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' leg
"leg"





# plt ####
#' data/plt
#'
#' @docType data
#' @name plt
#' @usage data(plt)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' plt
"plt"



# rasXYZ ####
#' data/各調査区等高線用ラスタデータrasXYZ
#'
#' @docType data
#' @name rasXYZ
#' @usage data(rasXYZ)
#' @format An object of class `list`.
#' @keywords datasets
#' @examples
#' par(mfrow=c(3,3))
#' for(i in 1:nrow(plt)){
#'  contour(rasXYZ[[i]],main=plt$na[i])
#' }
#'
#'
#'
"rasXYZ"



# sp_exc ####
#' data/sp_exc　同種異名(シノニム)の変換表です
#' sp_naからsp_na2に統一しました
#'
#' @docType data
#' @name sp_exc
#' @usage data(sp_exc)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' sp_exc
"sp_exc"



# sptype ####
#' data/sptype コード、生活型、 階層の略号表
#'
#' @docType data
#' @name sptype
#' @usage data(sptype)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' sptype
"sptype"



# subplot_xy ####
#' data/ 各調査地におけるサブプロットの位置座標
#'
#' @docType data
#' @name subplot_xy
#' @usage data(subplot_xy)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' subplot_xy
"subplot_xy"



# TateyamaVegetationPlots ####
#' data/ 立山植生モニタリング調査区一覧表
#'
#' @docType data
#' @name TateyamaVegetationPlots
#' @usage data(TateyamaVegetationPlots)
#' @format An object of class `spec_tbl_df`.
#' @keywords datasets
#' @examples
#' TateyamaVegetationPlots
#'
"TateyamaVegetationPlots"



# topo ####
#' data/ 各調査区の沢や歩道の座標(中島さん)
#'
#' @docType data
#' @name topo
#' @usage data(topo)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' topo
"topo"



# treesXYZ ####
#' data/ 各調査区樹木位置座標
#'
#' @docType data
#' @name treesXYZ
#' @usage data(treesXYZ)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' treesXYZ
"treesXYZ"



# VegetationSurveyYears ####
#' data/ 植生調査年(スプレッドシート列ラベル)
#'
#' @docType data
#' @name VegetationSurveyYears
#' @usage data(VegetationSurveyYears)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @examples
#' VegetationSurveyYears
"VegetationSurveyYears"

# VegetationYearsMatrix ####
#' data/ 植生調査年
#'
#' @docType data
#' @name VegetationSurveyYears
#' @usage data(VegetationSurveyYears)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @source automatically generated skeleton
#' @examples
#' VegetationYearsMatrix
#' VegetationYearsMatrix[1:5,"Arimine"]
"VegetationYearsMatrix"



# vv ####
#' data/ 全期調査区サブプロット植生調査生データ
#' 5期末においてvv5(1-5期)と同じもの
#' @docType data
#' @name vv
#' @usage data(vv)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @examples
#' vv
#' # 調査区ごとの抽出
#' subset(vv,plot=="Arimine")
#' #listで分割
#'VV<-split(vv, vv5$plot)
#'#再結合
#'dplyr::bind_rows(VV, .id = "plot")
#'
"vv"



# vv4 ####
#' data/ 1-4期調査区サブプロット植生調査生データ
#'
#' @docType data
#' @name vv4
#' @usage data(vv4)
#' @format An object of class `data.frame`.
#' @keywords datasets
#' @examples
#' vv4
#'
"vv4"


# vv5 ####
#' data/ 1-5期調査区サブプロット植生調査生データ
#' vv5 for the period I-V (2020-2025)
#'
#' @docType data
#' @name vv5
#' @usage data(vv5)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @examples
#' # NOTE:
#'# vv5 is stored in long format.
#'# For manual editing, it is often convenient to split by plot,
#'# edit each subset, and then re-bind to vv5.
#' vv5
#' #listで分割
#'(VV5<-split(vv5, vv5$plot))
#'#再結合
#'dplyr::bind_rows(VV5, .id = "plot")
#'
#'# 調査区ごとの抽出
#'(arimine <- subset(vv5, plot == "Arimine"))
#'
#'
"vv5"




# dd5 ####
#' data/ 1-5期調査区サブプロット森林調査生データ
#' dd5 for the period I-V (2020-2025)
#'
#' @docType data
#' @name vv5
#' @usage data(vv5)
#' @format An object of class data.frame
#' @keywords datasets
#' @source 中島・石田　「森林調査」
#' @examples
#' head(dd5)
#'
"dd5"


# VT ####
#' data/ 植生調査票(2020-2025)
#' VT  Vegtaion cross table for the period V (2020-2025)
#'
#' @docType data
#' @name VT
#' @usage data(VT)
#' @format An object of class `list` of  `tbl_df`.
#' @keywords datasets
#' @source automatically generated skeleton
#' @seealso [VTdk]
#' @examples
#' names(VT)
#'  VT[["Arimine_c04"]]
#'  VTdk[["Arimine_c04"]]
"VT"


# VTdk ####
#' data/Domin-Kradina集計表
#'  VTdk Vegetation cross table (Domin-Kradina)
#'
#' @docType data
#' @name vv5
#' @usage data(vv5)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @examples
#' names(VT5)
#'  VT[["Arimine_c04"]]
#'  VTdk[["Arimine_c05"]]
"VTdk"


# VC ####
#' data/ Vegetation chronosequence
#'
#' @docType data
#' @name VC
#' @usage data(VC)
#' @format A named list of tibbles (one element per plot).
#' @keywords datasets
#' @seealso [VegetationChronologyTable()]
#' @examples
#' data("VC", package = "TateyamaVegetation")
#' VC
#' (d<- VC[["Arimine"]])
#' nm<-names(d)
#'  gsub("Mean","m",nm)
#'  sptype
#'  d$sp
#'  d.<-cbind(
#'  d[,c("sp","Frequencydk01","Frequencyc02","Frequencyc03","Frequencyc04","Frequencyc05")],
#'  d[,c("FreqCodedk01","FreqCodec02","FreqCodec03","FreqCodec04","FreqCodec05")],
#'  d[,c("Meandk01","Meanc02","Meanc03","Meanc04","Meanc05")],
#'  d[,c("DominatIndexdk01dk","DominatIndexc02dk","DominatIndexc03dk","DominatIndexc04dk","DominatIndexc05dk")]
#'  )
#'  names(d.)<- c("sp",paste0("f",1:5),paste0("F",1:5),paste0("c",1:5),paste0("di",1:5))
#'
#'  tibble(d.)
#'   d. %>%
#'     mutate()
#'
#'  d %>% select(sp, starts_with("Meanc"))
#'
"VC"

# VCrepo ####
#' data/ Vegetation chronosequence summary
#'
#' @docType data
#' @name VCrepo
#' @usage data(VCrepo)
#' @format A list of the objects of class `tbl_df`.
#' @keywords datasets
#' @seealso [VegetationChronologyTable()]
#' @examples
#' VCrepo
"VCrepo"


# vc5 ####
#' data/ Vegetation chronosequence summary (data frame)
#'
#' @docType data
#' @name vc5
#' @usage data(vc5)
#' @format A  `tbl_df`.
#' @keywords datasets
#' @examples
#' VCrepo
#' vc5
"vc5"


# vegetation_code ####
#' data/ 植物目録各調査区頻度分布コード
#' 植生モニタリング調査(小島覚・石田仁)
#'
#' @docType data
#' @name vegetation_code
#' @usage data(vegetation_code)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @examples
#'
#' vegetation_code
#'  SpeciesNameCheck(vegetation_code$sp)
#'
"vegetation_code"

# forest_code ####
#' data/ 植生モニタリング調査(石田仁・中島春樹)
#'
#' @docType data
#' @name forest_code
#' @usage data(forest_code)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @source
#'  @examples
#' forest_code
#'  SpeciesNameCheck(forest_code$sp)
"forest_code"


# NonNativePlants ####
#' data/ 立山外来植物調査(大田道人・山下寿之・吉田めぐみ)
#'
#' @docType data
#' @name NonNativePlants
#' @usage data(NonNativePlants)
#' @format An object of class `tbl_df`.
#' @keywords datasets
#' @source "第Ⅳ期外来種調査報告書用全体表.csv",
#'  @examples
#' NonNativePlants
#'  NonNativePlants$sp
#'  SpeciesNameCheck(NonNativePlants$sp)
"NonNativePlants"


