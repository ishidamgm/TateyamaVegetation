
#' csvファイルのエンコーディング変換
#'
#' 上書きします。必要に応じて事前にバックアップしておいてください。
#'
#' @param filename
#' @param en_old　　original fileEncoding　defaulut　"cp932"
#' @param en_new        new  fileEncoding  defaulut　"utf-8"
#'
#' @returns
#'
#' @export
#'
#' @examples
#'
#' #EncodingExchange(filename="H30植生調査野帳_美松.csv",en_old="cp932",en_new="utf-8")
#'
#'　#EncodingExchange(filename="H31植生調査野帳_有峰.csv",en_old="cp932",en_new="utf-8")
#'　
#'　
#'　
EncodingExchange<-function(filename="test.csv",en_old="cp932",en_new="utf-8"){
  d.<-read.csv(filename,fileEncoding=en_old)
  write.csv(d.,file=filename,fileEncoding=en_new,row.names = FALSE)
}

#' 種の和名が植物目録floraに記載されているか確認します
#'
#' @param spj　種名(和名)のベクトル
#'
#' @returns　記載の有無、記載のない入力種名
#'
#' @export
#'
#' @examples
#' SpeciesNameCheck(c("ブナ","スギ","クロベ"))
#' d<-FieldNote_Arimine2019_raw
#' head(d)
#' spj<-d$種名
#' SpeciesNameCheck(spj)
#'

#'
#'
SpeciesNameCheck<-function(spj){
  spj.<-spj
  fl.i<-match(spj,  flora$spj)
  if(anyNA(fl.i)){
    cat(unique(spj[is.na(fl.i)]), "はリストにありません。\n 種名を修正するか、floraに新たな種を追加してください。\n")
  } else {    cat("入力されている種名はすべて目録に含まれています。\n")}

}



#' Title
#'
#' @param spj
#'
#' @returns
#' @export
#'
#' @examples
#' d<-FieldNote_Arimine2019_raw
#' spj<-d$種名
#' SpeciesNameCheck(spj)
#' SpeciesNameCheck(SpeciesNameCorrect(spj))
#'
SpeciesNameCorrect<-function(spj){
  for(i in 1:nrow(sp_exc)){
        sp.<-sp_exc$sp_na[i]
         spj[sp.==spj]<-sp_exc$sp_na2[i]
  }
  return(spj)
}


#' Title
#'
#' @param d
#' @param period
#' @param DK  Domin-Krajina conversion of coverages
#' @returns
#' @export
#' @importFrom magrittr %>%
#' @examples
#' d <- subset(vv,plot=="Arimine")
#' head(VegetationTable(d,period="c04"))
#' head(VegetationTable(d,period="c04",DK=TRUE))
#'
#'
#'
#'


#' 生野帳データの階層クラスの確認、修正、未入力データの補完をします
#'　サブプロット、階層、種名の順でソートします。
#'　サブプロットに同一種が複数ある場合は被度を合算し統一します
#'
#' @param d
#' @param sp_colnm
#' @param KAISO_colnm
#'
#' @returns
#' @export
#'
#' @examples
#' d<-FieldNote_Arimine2019_raw
#' FieldNote_CheckCorrect(d)
#'
#'
#'
FieldNote_CheckCorrect<- function(d=FieldNote){

  # 種名のチェック

  #d$種名<-SpeciesNameCheck(d$種名)


  #　サブプロット・欠落の補完
  i.na<-which(is.na(d$サブプロット))
  for(i in i.na)d$サブプロット[i]<-d$サブプロット[i-1]

  #階層の確認と補完
  fl.i<-match(d$種名,  flora$spj)
  d$階層<-sptype$階層[match(flora$form[fl.i],sptype$コード)]
  d<-d[order(d$サブプロット,d$階層,d$種名),]

  # 被度の数値化
  d <- d |> #%>%
    dplyr::mutate(across(matches("^(被度|DK)"), ~ as.numeric(trimws(.))))


  # union and combine coverage data of duplicated species in a subplot#

  d <- d |> #%>%
    dplyr::group_by(サブプロット, 階層, 種名) |> #%>%
    dplyr::summarise(
      across(matches("^(被度|DK)"), ~ sum(.x, na.rm = TRUE)),
      # X = first(X),
      # no = first(no),
      # II_ = first(II_),
      .groups = "drop"
    )


  return(d)
}


#' Title
#'
#' @param d
#' @param period
#' @param DK
#'
#' @returns
#' @export
#'
#' @examples
#' VegetationTable()
#'
VegetationTable <- function(d=subset(vv,plot=="Arimine"),period="c04",DK=FALSE){
  # # 組成表　vt:VegetationTable ####
  vt <- d |>
    dplyr::select(subplot, sp,any_of(period)) |>
    tidyr::pivot_wider(
      names_from = subplot,
      values_from = period,
      values_fill = 0  # 欠損を0にする（合計表などに便利）
    )

  # # Domin_Krajina convert

  if(DK){
    if(period!="dk01")
    {vt<- vt |>
      dplyr::mutate(across(-sp,~ Domin_Krajina(.)))
    }
  }






  # # 頻度　VegetationTable ####
  vt |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Mean = mean(dplyr::c_across(-sp), na.rm = TRUE),
      Frequency = mean(dplyr::c_across(-sp) > 0, na.rm = TRUE),
      FreqCode =hindo(Frequency),
      DominatIndex = 100 * sqrt(Mean * Frequency) / sqrt(10)
    ) |>
    dplyr::ungroup() ->vt

  return(vt)

}


#' list of vegetation table of all plots and periods
#'
#' @returns
#'
#'
#' @export
#'
#' @examples
#'
#' VT <- VegetationTableList(DK=FALSE)
#' VTdk <- VegetationTableList(DK=TRUE)
#'
#' #usethis::use_data(VT,VTdk,overwrite = TRUE)
#'

#'
VegetationTableList<-function(DK=FALSE){
  #被度階級表(被度生データ)
  plot_name <- plt$plot_name
  period<- c("dk01","c02","c03","c04","c05")
  vt <- list()
  for (i in plot_name) {
    for (j in  period) {
      nm <- paste(i, j, sep = "_")
      print(nm)
      vt[[nm]] <- VegetationTable(subset(vv, plot == i),DK=DK, j)
    }
  }
  return(vt)
}

#' Title
#'
#' @param VT
#' @param VTdk
#'
#' @returns
#' @export
#'
#' @examples
#'
#' VC<-VegetationChronologyTable(VT=VT,VTdk=VTdk)
#' # usethis::use_data(VC,overwrite = TRUE)
#' VC$Arimine
#'
#'
VegetationChronologyTable<-function(VT=VT,VTdk=VTdk){

  plot_name <- plt$plot_name
  period<- c("dk01","c02","c03","c04","c05")

  vc <- list()

  for (i in plot_name) {

    vc_list <- lapply(period, function(j) {
      nm <- paste(i, j, sep = "_")
      print(nm)

      m  <- VT[[nm]]
      m2 <- VTdk[[nm]]
      cn <- c(1, (ncol(m) - 3):ncol(m))

      dplyr::full_join(
        m[, cn],
        m2[, cn],
        by = "sp",
        suffix = c(j, paste0(j, "dk"))
      )
    })

    # vc_list の中身を sp をキーに順次 full_join
    vc[[i]] <- Reduce(function(x, y) dplyr::full_join(x, y, by = "sp"), vc_list)
  }

  return(vc)
}


#' Title
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
#'
#'# usethis::use_data(VCrepo,overwrite = TRUE)
VegetationChronologyTable_report<-function(plot_name="Arimine"){
  # データ読み込み
  d<- VC[[plot_name]]

  # 選別・リネーム


  # 生活型・階層挿入・ソート
  form<-flora$form[match(d$sp,flora$spj)]
  layer<-sptype$階層[match(form,sptype$コード)]

  d_ <- dplyr::bind_cols(
    d %>% dplyr::select(
      sp, Frequencydk01, Frequencyc02, Frequencyc03, Frequencyc04, Frequencyc05,
    FreqCodedk01, FreqCodec02, FreqCodec03, FreqCodec04, FreqCodec05,
    Meandk01, Meanc02, Meanc03, Meanc04, Meanc05,
    DominatIndexdk01dk, DominatIndexc02dk, DominatIndexc03dk, DominatIndexc04dk, DominatIndexc05dk)
  ) %>%
    setNames(c("sp", paste0("f", 1:5), paste0("F", 1:5), paste0("c", 1:5), paste0("di", 1:5)))  %>%
    dplyr::mutate(layer,.before="sp") %>%
    dplyr::mutate(form,.after="sp") %>%
    arrange(layer, sp)

  # 1~5期出現した種を解析対象とする
  cal<-as.numeric((rowSums(d_[,paste0("f",1:5)]>0)==5))
  d_ <- tibble(d_,cal)

  return(d_)


}

#' Figure of chronosequence for dominant variance
#'
#' @param plot_name
#'
#' @returns
#' @export
#'
#' @examples
#' Fig_DominanceValue("Kaminokodaira")
#' Fig_DominanceValue("Arimine")
#' Fig_DominanceValue("Matsuotoge")
#'
#' Fig_DominanceValue(plt$plot_name[9])
#'
#' par(mfrow=c(3,4))
#' for(i in plt$plot_name)Fig_DominanceValue(i)
#'
#' par(mfrow=c(1,1))
#'
Fig_DominanceValue <- function(plot_name="Joudosan"){
  d <- VCrepo[[plot_name]]
  dv_cal <- d %>% filter(cal==1) %>%
    group_by(layer)  %>%
    summarize(across(starts_with("di"), sum))

  barplot(as.matrix(dv_cal[,-1]),main=plot_name)
}


