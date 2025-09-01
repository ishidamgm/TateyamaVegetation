# report_text.R
# どのプロットでいつ?
library(dplyr)

#' 各調査地の各調査期における出現種
#'
#' @param plot_name
#' @param period
#'
#' @returns
#' @export
#'
#' @examples
#'
#' text_species(plot_name="Arimine",period=5)
#'
#'
text_species<-function(plot_name="Arimine",period=5){
  fclm <- paste0("f",period)
  cclm <-paste0("c",period)
  d<-vc5
  d. <- d %>% filter(plot==plot_name)

  tab <- table(d.$layer)
  typ <- sptype$生活型[match(names(tab),sptype$階層)]
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


#--------------------------------------------
plot_name="Kaminokodaira"
period=5
fclm <- paste0("f",period)
cclm <-paste0("c",period)
d<-vc5
d. <- d %>% filter(plot==plot_name)

tab <- table(d.$layer)
typ <- sptype$生活型[match(names(tab),sptype$階層)]
(typtab<-data.frame(typ,tab))

cat(paste("今回の調査では", paste(typ,tab,"種",collapse = ", "),"を記録した。\n"))

for(i in 1:nrow(typtab)){
  lifefom<-typtab$typ
  #layer<-typtab$Var1
  cat(paste(lifefom[i],"に関しては\n"))
  d. %>% filter(layer==names(tab)[i])%>%arrange(-.data[[fclm]]) -> d.freq
  d. %>% filter(layer==names(tab)[i])%>%arrange(-.data[[cclm]]) ->d.cov
  sp.freq<-paste(d.freq$sp,sprintf("%.1f%%",100*d.freq[[fclm]]),collapse = ", ")
  sp.cov <-paste(d.cov$sp,sprintf("%.1f%%",d.cov[[cclm]]),collapse = ", ")

  txt1<-paste(lifefom[i],"に関しては\n")
  txt2<-paste("種別の出現頻度は多い順に",sp.freq,"であった。\n")
  txt3<-paste("また、種別の平均被度は多い順に",sp.cov,"であった。\n")

  cat(paste(txt1,txt2,txt3))

  # cat(paste(lifefom[i],"に関しては\n"))
  # cat(paste("種別の出現頻度は多い順に",sp.freq,"であった。\n"))
  # cat(paste("また、種別の平均被度は多い順に",sp.cov,"であった。\n"))

}

i<-1
cat(paste(names(tab)[i],"に関しては"))
d. %>% filter(layer==names(tab)[i])%>%arrange(-.data[[fclm]]) -> d.freq

d. %>% filter(layer==names(tab)[i])%>%arrange(-.data[[cclm]]) ->d.cov

sp.freq<-paste(d.freq$sp,sprintf("%.1f%%",100*d.freq[[fclm]]),collapse = ", ")

sp.cov <-paste(d.cov$sp,sprintf("%.1f%%",d.cov[[cclm]]),collapse = ", ")

paste("種別の出現頻度は多い順に",sp.freq,"であった。")
paste("また、種別の平均被度は多い順に",sp.cov,"であった。")


paste(d.cov$sp,sprintf("%.1f%%",100*d.freq[[cclm]]),d.freq[[cclm]])


subset(d.,layer==names(tab)[i])[]

d.
# write.csv(d,file="data_raw/集計_vc5.csv")

# VCre
