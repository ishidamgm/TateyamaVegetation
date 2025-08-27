# report_text.R
# どのプロットでいつ?
library(dplyr)
plot_name="Kaminokodaira"
period=5
fclm <- paste0("f",period)
cclm <-paste0("c",period)
d<-vc5
d. <- d %>% filter(plot==plot_name)

tab <- table(d.$layer)
typ <- sptype$生活型[match(names(tab),sptype$階層)]
data.frame(typ,tab)
paste("今回の調査では", paste(typ,tab,"種",collapse = ", "),"を記録した。")

paste("今回の調査では", paste(typ,tab,"種",collapse = ", ") ,"を記録した。")
d. %>% filter(layer==names(tab)[i])%>%arrange(-.data[[fclm]]) ->d.freq
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
