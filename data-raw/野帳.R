# 野帳.R
names(plt)
plt[plt$plot_name=="Arimine",c("Vegetation001","Vegetation002","Vegetation003","Vegetation004","Vegetation005")]
Arimine_lbl<-VegetationSurveyYears[plt$plot_name=="Arimine",c("Vegetation001","Vegetation002","Vegetation003","Vegetation004","Vegetation005")]
Mimatsu_lbl<-VegetationSurveyYears[plt$plot_name=="Mimatsu",c("Vegetation001","Vegetation002","Vegetation003","Vegetation004","Vegetation005")]

df<-vv[vv$plot=="Arimine",]
names(df)[6:10]<-Arimine_lbl

df2<-vv[vv$plot=="Mimatsu",]
names(df2)[6:10]<-Mimatsu_lbl


#####
library(openxlsx2)

# ワークブックとワークシートの作成
wb <- wb_workbook()
wb$add_worksheet("Arimine")
wb$add_worksheet("Mimatsu")

# データの書き込み
wb$add_data("Arimine", df, start_row = 1, start_col = 1)
wb$add_data("Mimatsu", df2, start_row = 1, start_col = 1)
# サブプロットが変わる位置で改ページ（行番号 + 1）
subplot_changes <- which(diff(df$subplot) != 0)
page_breaks <- subplot_changes + 1  # データ行に対して +1

# 改ページの設定（openxlsx2用）
wb_add_page_break(wb,  row = page_breaks)

# 保存
wb$save("立山植生野帳2025.xlsx", overwrite = TRUE)

