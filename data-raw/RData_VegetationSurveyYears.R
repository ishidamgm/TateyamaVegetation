# RData_VegetationSurveyYears.R



df<-TateyamaVegetationPlots
names(df)
df %>% select("Vegetation001":"Vegetation005" )

#colnames_被度 <-


VegetationSurveyYears <- df %>%
  select(plot_name, Vegetation001:Vegetation005) %>%
  mutate(across(starts_with("Vegetation"),
                ~ paste0(
                  if (cur_column() == "Vegetation001") "DK" else "被度",
                  substr(as.character(.), 1, 4)
                )))

VegetationSurveyYears

  # VegetationSurveyYears <- df %>%
  #   select(plot_name, Vegetation001:Vegetation005) %>%
  #   mutate(across(starts_with("Vegetation"),
  #                 ~ paste0("被度", substr(as.character(.), 1, 4))))


  #save(VegetationSurveyYears,file="../data/VegetationSurveyYears.RData")

