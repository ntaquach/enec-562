library(dplyr)
library(writexl)

mam.scores.veg <- mam.scores %>%
  mutate(MaxHerb = mam$MaxHerb, MaxShrub=mam$MaxShrub, 
         DensityS=mam$DensityS, DensityW=mam$DensityW,
         AvgDensity=mam$AvgDensity, WoodyDebri=mam$WoodyDebri,
         GroundCover=mam$GroundCover)

write_xlsx(mam.scores.veg,"NMDS vs. Veg data.xlsx")
write.csv(mam.scores.veg, "NMDS vs. Veg.csv")
