# Analyze data using non-metric multi-dimensional scaling (NMDS)

#NMDS is simply a multivariate analysis technique that simplify the data. In other words, instead of having 7-8D graph, NMDS will present us with a 2D graph that explains most of the variation in the dataset.

#mam2 <- as.matrix(decostand(mam1,method="total")) #calculate relative abundance from count data

setwd("C:/GitHub Projects/enec-562/Final Project")
set.seed(111)
mam_nmds <- metaMDS(mam1,distance="bray",maxit=999,trymax = 200,wascores = T,k=2,autotransform = F) #gotta put autotransform on FALSE so R won't transform your data when you don't want it to.

### Extract site and species scores

#site scores
mam.scores <- as.data.frame(scores(mam_nmds)$sites) #extract NMDS scores
mam.scores <- mam.scores %>% mutate(Trap = mam$Trap,Site = mam$Site, Restored = mam$Restored)

#species scores
spp.scores <- as.data.frame(scores(mam_nmds,display=c("species")))

library(writexl)
write_xlsx(mam.scores,"mam.scores.xlsx")
write_xlsx(spp.scores,"spp.scores.xlsx")