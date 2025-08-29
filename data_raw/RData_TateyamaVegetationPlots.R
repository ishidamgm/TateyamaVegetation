# RData_TateyamaVegetationPlots.R
library(readr)
library(tibble)

#setwd("~/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2025/TateyamaVegetation/data_raw")
plt <- readr::read_csv("TateyamaVegetationPlots.csv")

names(df)

# save(TateyamaVegetationPlots,file="../data/TateyamaVegetationPlots.RData")

#usethis::use_data(plt,overwrite = TRUE)

plt$plot_name
expand.grid(plot = plt$plot_name, period= c("dk01","c02","c03","c04","c05"))


plt$no[]

VegetationSurveyYears$plot_name_jp<-plt$plot_name_jp
VegetationSurveyYears<- VegetationSurveyYears %>%
  relocate(plot_name_jp, .after = plot_name)
VegetationSurveyYears

VegetationSurveyYears %>%
  mutate(no=substr(101:110,2,3)) %>%
  relocate(no, .before = plot_name) ->VegetationSurveyYears

#usethis::use_data(VegetationSurveyYears,overwrite = TRUE)
