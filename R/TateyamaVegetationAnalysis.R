

#' スプレッドシートで入力した野帳データ・フレーム(tibble)を
#'　列名の変更、項目追加し計算用に構成します
#'　
#'　
#' @param d 野帳データ修正版(tibble)
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
#' #d                                                              #元の野帳データ
#' #FieldNote_FormatArange(d,calc= CalculationInfo(2025,"有峰"))   #計算用データ
#'
#'
FieldNote_FormatArange<-function(d=d,calc= CalculationInfo(2025,"有峰")){
  pn<- match(plot_name_jp,VegetationSurveyYears$plot_name_jp)
  plot_name <- VegetationSurveyYears$plot_name[match(plot_name_jp,VegetationSurveyYears$plot_name_jp)]
  d.<-d[,calc$colnames$yachou]
  names(d.)<-calc$colnames$calc
  tibble(pn,plot=plot_name,d.)
}



#' 集計計算に用いるファイル名・スプレッドシートの列名　調査年と調査区名から返します
#' VegetationSurveyYearsに基づきます
#' 各調査期の列名が正しく入力されていることを確認してください。
#'
#' @param year
#' @param plot_name_jp
#'
#' @returns
#' @export
#'
#' @seealso \code{\link{SpeciesNameCheck}}, \code{\link{SpeciesNameCorrect}}, \code{\link{FieldNote_CheckCorrect}}
#'
#' @examples
#' VegetationSurveyYears
#' #各調査期の列名が正しく入力されていることを確認してください!!!
#'
#' CalculationInfo(2025,"有峰")
#'
#' #yachou  :エクセル等で入力したオリジナルの野帳ファイル名です
#' #         保存する際はこの名前と同じにしてください【.xlsxがこのルールで命名されています。
#'  .csvとして保存すると自動的にこの名前で保存されます】
#' #yachou2 :オリジナルを整理した野帳ファイル名です
#' #yachou :植生被度の列名となります
#'
#' # 計算用データ・フレーム行列名対応表　yachou_calc.colnames
#'
CalculationInfo <-function(year=2025,plot_name_jp="有峰"){

  vy <- VegetationSurveyYears
  i<-which(vy$plot_name_jp==plot_name_jp)
  plot_name<-vy$plot_name[i]
  no<-vy$no[i]
  j<-grep(year,vy[i,])
  yachou  <- paste0(vy$no[i],vy$plot_name_jp[i],"-植生野帳",year,".csv")
  yachou2 <- paste0(vy$no[i],vy$plot_name_jp[i],"-植生野帳",year,"-最新",".csv")
  clm <- as.character(unlist(vy[i,grep("Vegetation",names(vy))]))

  # 計算用データ・フレーム行列名対応表　yachou_calc.colnames
  yachou.colnames<-c("サブプロット","階層","種名",clm)
  calc.colnames<-c("subplot", "layer",   "sp","dk01",paste0("c0",2:length(clm)) )
  colnames <- data.frame(yachou=yachou.colnames,calc=calc.colnames)
  clm2<-colnames$calc[match(clm,colnames$yachou)]
  vt_label<-paste0(plot_name,"_",clm2)

 # 被度表
  hidohyou<-paste0("被度表-",vy$no[i],vy$plot_name_jp[i],"-",clm,".csv")
  # 被度階級表
  hidokaikyuhyou<-paste0("被度階級表-",vy$no[i],vy$plot_name_jp[i],"-",clm,".csv")

  # 総合優占度_頻度_平均被度-10有峰.csv
  sougou<-paste0("総合優占度_頻度_平均被度-",vy$no[i],vy$plot_name_jp[i],".csv")

  #報告書用-頻度_被度_優占度表.csv
  houkousho<-paste0("報告書用-頻度_被度_優占度表-",vy$no[i],vy$plot_name_jp[i],".csv")


  #リストへ統合
  list( year=year,
        plot_name=  plot_name,
        plot_name_jp=  plot_name_jp,
        no=no,
       yachou=yachou,yachou2=yachou2,
       clm=clm,
       clm2=clm2,
       colnames=colnames,
       hidohyou= hidohyou,
       hidokaikyuhyou=hidokaikyuhyou,
       sougou=sougou,
       houkousho=houkousho,
       vt_label=vt_label
       )

}



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



#' 和名の置換
#' 本調査の種和名はYList(米倉浩司・梶田忠 2007-)の第一和名に基づきます。
#' 同種異名は本コマンドで置換表sp_excに基づき修正できます。
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


#' 野帳の整理をします。
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


#' 報告書用植生表(被度・ドミン-クラジナ被度　両方対応)
#'
#' @param d
#' @param period
#' @param DK
#'
#' @returns
#' @export
#'
#' @examples
#' VegetationTable(d=subset(vv,plot=="Arimine"),period="c04",DK=FALSE)
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



#' 植生経年変化表　(粗表)
#' @param plot_name vector of a plot name or plots names
#' @param vt　a tibble (or data.frame) of vegetation table
#' @param vtdk 　a tibble (or data.frame) of vegetation table
#'
#' @returns
#' @export
#'
#' @examples
#'
#' VegetationChronologyTable()
#'
#' VegetationChronologyTable(plot_name="Arimine")
#'
#'
#'
#'
VegetationChronologyTable<-function(plot_name=plt$plot_name,vt=VT,vtDK=VTdk,
                               period=c("dk01","c02","c03","c04","c05")){



  vc <- list()

  for (i in plot_name) {

    vc_list <- lapply(period, function(j) {
      nm <- paste(i, j, sep = "_")
      print(nm)

      m  <- vt[[nm]]
      m2 <- vtDK[[nm]]
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

  vc
}

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
#'
#'# usethis::use_data(VCrepo,overwrite = TRUE)
#'
VegetationChronologyTable_report<-function(plot_name="Arimine", vc=VC){
  # データ読み込み
  d<- vc[[plot_name]]

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



#' 種の被度の時系列変化　(例　ササの経年変化作図)
#' Change of coverage for floral types (life form species)
#'
#' @param data A tibble with columns `form`, `plot`, and `c1:c5`.
#' @return Summarised tibble.
#' @importFrom dplyr filter group_by summarise across
#' @importFrom magrittr %>%
#' @examples
#' library(magrittr)
#' library(tidyr)
#' library(dplyr)
#' library(ggplot2)
#'
#'  d<-plots_5periods_sum(data=vc5,condition=form == "sasa")
#' # c2〜c5をロング化
#'
#'
#' d_long <- d %>%
#'   select(plot, c2:c5) %>%
#'   pivot_longer(c2:c5, names_to = "period", values_to = "value") %>%
#'   mutate(period = factor(period, levels = c("c2","c3","c4","c5")))
#'
#' # 各plotを別パネル
#' ggplot(d_long, aes(x = period, y = value, group = plot)) +
#'   geom_line() +
#'   geom_point() +
#'   facet_wrap(~ plot, scales = "free_y") +
#'   labs(x = "Period", y = "Coverage", title = "Change in c2–c5 by plot") +
#'   theme_minimal()
#'
#'
#' # 1枚で表示
#' shapes <- c(15, 16, 17, 18, 19, 20, 21, 22, 23)
#'
#' ggplot(d_long, aes(x = period, y = value,
#'                    color = plot,
#'                    shape = plot,
#'                    group = plot)) +
#'   geom_line() +
#'   geom_point(size = 3) +
#'   scale_shape_manual(values = shapes) +
#'   labs(x = "Period", y = "Coverage",
#'        title = "Change in c2–c5 (all plots)") +
#'   theme_minimal()
#'
#' #'
plots_5periods_sum <- function(data=d,condition=form == "sasa") {
  data %>%
    filter({{condition}}) %>%
    group_by(plot) %>%
    summarise(across(c1:c5, sum, na.rm = TRUE), .groups = "drop")


}



#' 報告書用作図　総合優占度経年変化
#' Figure of chronosequence for dominant variance
#'
#' @param plot_name
#'
#' @returns
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggrepel)
#'
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
#' # 単年度版
#' Fig_DominanceValue(plot_name="Arimine",vc_repo=VCrepo)
#'
Fig_DominanceValue <- function(plot_name="Joudosan",vc_repo=VCrepo){
  d <- vc_repo[[plot_name]]
  dv_cal <- d %>% filter(cal==1) %>%
    group_by(layer)  %>%
    summarize(across(starts_with("di"), sum))

  barplot(as.matrix(dv_cal[,-1]),main=plot_name)
}



#' 報告書用作図　出現頻度−平均被度
#'
#' @param d data frame of tibble   VCrepo
#' @param plot_name
#' @param x column name of frequency
#' @param y column name of coverage
#' @param sp column name of species
#'
#' @returns
#'
#' @export
#'
#' @import ggplot2
#'
#'
#' @examples
#' library(ggplot2)
#' Fig_FrequencyCoverage(plot_name="Mimatsu")
#' Fig_FrequencyCoverage(plot_name="Arimine")
#' Fig_FrequencyCoverage(plot_name="Arimine",x="f5",y="c5")
#'
#'
#'
Fig_FrequencyCoverage<-function(plot_name="Arimine",x="f5",y="c5",sp="sp",data=VCrepo){
  d_<-data[[plot_name]][,c(sp,x,y)]
  names(d_)<-c("sp","x","y")
  d_$x<-d_$x*100
  x12 <- range(d_$x) ; y12 <- range(d_$y)
  g<-ggplot(d_, aes(x, y , label = sp )) + xlim(x12[1]-15,x12[2])+ylim(y12[1]-7,y12[2])+
    xlab("出現頻度(％)")+  ylab("平均被度(％)")+
    geom_point(col="red") +
    geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
    geom_hline(yintercept=0,linetype="dashed",colour="blue") +
    geom_vline(xintercept=0,linetype="dashed",colour="blue")+
    labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
g
  }


#' 報告書用作図　出現頻度−総合優占度
#'
#' @param data list of data frame (tibble)   default :   VCrepo
#' @param plot_name
#' @param x column name of frequency
#' @param y column name of coverage
#' @param sp column name of species
#'
#' @returns
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#'　library(ggplot2)
#'　library(ggrepel)
#'　
#' Fig_FrequencyDominace(plot_name="Mimatsu")
#' Fig_FrequencyDominace(plot_name="Arimine")
#' Fig_FrequencyDominace(plot_name="Arimine",x="f3",y="di3")
#' Fig_FrequencyDominace(plot_name="Arimine",x="f5",y="di5")
#'
Fig_FrequencyDominace<-function(plot_name="Arimine",x="f5",y="di5",data=VCrepo,sp="sp"){
  d_<-data[[plot_name]][,c(sp,x,y)]
  names(d_)<-c("sp","x","y")
  d_$x<-d_$x*100
  x12 <- range(d_$x) ; y12 <- range(d_$y)
  g<-ggplot(d_, aes(x, y , label = sp )) + xlim(x12[1]-15,x12[2])+ylim(y12[1]-7,y12[2])+
    xlab("出現頻度(％)")+  ylab("総合優占度")+
    geom_point(col="red") +
    geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
    geom_hline(yintercept=0,linetype="dashed",colour="blue") +
    geom_vline(xintercept=0,linetype="dashed",colour="blue")+
    labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
  g
}


# ####    "出現頻度(％)" - "平均被度(％) #######
#
# d_ <- data.frame(x=d[,Fc_last], y=d[,Vc_last], sp=d$種名)
# x12 <- range(d_$x);y12 <- range(d_$y)
# g<-ggplot(d_, aes(x, y , label = sp )) +xlim(x12[1]-15,x12[2])+ylim(y12[1]-7,y12[2])+
#   xlab("出現頻度(％)")+  ylab("平均被度(％)")+
#   geom_point(col="red") +
#   geom_text_repel(size=3,fontface="bold",max.overlaps=100)+
#   geom_hline(yintercept=0,linetype="dashed",colour="blue") +
#   geom_vline(xintercept=0,linetype="dashed",colour="blue")+
#   labs(title=plot_name)+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
#
# g
#



#' 報告書用植生調査集計表　Data Table of Vegetation Chronosequence
#'
#' @param plot_name
#'
#' @returns
#' @export
#'
#' @examples
#'
#' DT_VCrepo("Mimatsu")
#' DT_VCrepo("Arimine")
#'
#' # for (i in plt$plot_name) {DT_VCrepo(i)}
#'
DT_VCrepo<-function(plot_name="Mimatsu"){
  df=VCrepo[[plot_name]]
  DT::datatable(df, options = list(pageLength = 10),caption=plot_name) %>%
    DT::formatRound(columns = c(paste0("c",1:5),paste0("f",1:5)),digits = 2)%>%
    DT::formatRound(columns = paste0("di",1:5),digits = 1)

}



#' 各調査地の各調査期における出現種
#'
#' @param plot_name
#' @param period
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
#'
#' text_species(plot_name="Arimine",period=5)
#'
#'
text_species<-function(plot_name="Arimine",period=5,data=vc5){
  fclm <- paste0("f",period)
  cclm <-paste0("c",period)
  d<-data
  d. <- d %>% filter(plot==plot_name)

  tab <- table(d.$layer)
  typ <- sptype$階層2[match(names(tab),sptype$階層)]
  (typtab<-data.frame(typ,tab))


  txt0<-paste(plot_name,"第",period,"期に(", paste(typ,tab,"種",collapse = ", "),")を記録した。\n")

  cat(txt0)

  for(i in 1:nrow(typtab)){
    lifefom<-typtab$typ
    #layer<-typtab$Var1

    d. %>% filter(layer==names(tab)[i])%>%arrange(-.data[[fclm]]) -> d.freq
    d. %>% filter(layer==names(tab)[i])%>%arrange(-.data[[cclm]]) ->d.cov
    sp.freq<-paste(d.freq$sp,sprintf("%.1f%%",100*d.freq[[fclm]]),collapse = ", ")
    sp.cov <-paste(d.cov$sp,sprintf("%.1f%%",d.cov[[cclm]]),collapse = ", ")


    txt1<-paste(lifefom[i],"に関して\n")
    txt2<-paste("種別の出現頻度は多い順に",sp.freq,"であった。\n")
    txt3<-paste("また、種別の平均被度は多い順に",sp.cov,"であった。\n")

    cat(paste(txt1,txt2,txt3))

  }
}
