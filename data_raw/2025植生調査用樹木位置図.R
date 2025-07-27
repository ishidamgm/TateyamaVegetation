#　2025植生調査用樹木位置図.R
library(TateyamaVegetation)
help(package="TateyamaVegetation")
# 2025年植生調査用樹木位置図の描画 ####
treemap("Mimatsu")+ ylim(c(45, 105))
treemap("Arimine")+ xlim(c(-5, 55))+ ylim(c(-5, 60))
