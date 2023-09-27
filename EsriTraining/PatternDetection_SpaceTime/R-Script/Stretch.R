library(arcgisbinding)
library(sf)
library(ggplot2)
library(tmap)
library(sfdep)
library(spdep)
library(tidyverse)

path = getwd()
file_ = st_read(paste0(path, "/EsriTraining/PatternDetection_SpaceTime/patterndetection_spacetime.gdb"),
                layer = 'US_Counties')

ggplot(data = file_, aes(x = SNAPRate)) +
  geom_histogram(aes(y=..density..), color="darkblue") +
  geom_density(alpha=.2)

tm_shape(file_) +
  tm_polygons('SNAPRate')

list_nb = poly2nb(file_, queen = TRUE)
empty_nb = which(card(list_nb) == 0)

Hot_spot = file_[-empty_nb, ]
empty_counties = file_[empty_nb,]

Hot_spot_nb = poly2nb(Hot_spot, queen = TRUE)
Hot_spot_binary <- nb2listw(Hot_spot_nb, style="B")
Hot_spot_lag <- lag.listw(Hot_spot_binary, Hot_spot$SNAPRate)

globalG.test(Hot_spot$SNAPRate, Hot_spot_binary)

HotSpotFinal = Hot_spot %>%
  mutate(
    nb = st_contiguity(Shape), # neighbors share border/vertex
    wt = st_weights(nb), # row-standardized weights
    tes_lag = st_lag(SNAPRate, nb, wt)) %>% # calculate spatial lag of SNAPRate
  mutate(Gi = local_g_perm(SNAPRate, nb, wt, nsim = 999)) %>%
  unnest(Gi)

tm_shape(HotSpotFinal) + tm_polygons('gi')
