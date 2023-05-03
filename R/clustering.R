library(spdep)
library(sf)
library(dplyr)
sf::sf_use_s2(FALSE)

pod <- readr::read_csv("data/table_pod.csv")
pod <- filter(pod, income_avg > 0, code_muni == 36)
zones <- st_read("data/table_pod.gpkg")
zones <- filter(zones, income_avg > 0, code_muni == 36)
wm_queen <- spdep::poly2nb(as_Spatial(zones), queen = TRUE)
rswm_queen <- spdep::nb2listw(wm_queen, style = "W", zero.policy = TRUE)

input_var <- "income_avg"

x <- as.numeric(scale(pod[[input_var]]))

localMI <- localmoran(x, rswm_queen)

zone_map <- select(zones, code_zone, name_zone, all_of(x))
zone_map <- cbind(zone_map, localMI)

library(tmap)
library(tmaptools)
tmap_mode(mode = "view")

tm_shape(zone_map) +
  tm_fill(
    col = "Ii",
    palette = "RdBu",
    id = "name_zone",
    popup.vars = c(input_var, "Ii")
    ) +
  tm_borders(col = "gray80", lwd = 1) +
  tm_basemap(server = "CartoDB.Positron") +
  tm_view(set.view = c(-46.659736, -23.554353, 12))


tm_shape(zone_map) +
  tm_fill(col = "Pr.z....E.Ii..",
          breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
          palette="-Blues",
          title = "local Moran's I p-values") +
  tm_borders(alpha = 0.5)



plot_moran <- moran.plot(x, rswm_queen)

library(ggplot2)
library(ggrepel)

plot_moran <- cbind(plot_moran, select(pod, name_zone))
plot_moran <- mutate(
  plot_moran,
  highlight = ifelse(is_inf, name_zone, NA)
  )

ggplot(plot_moran, aes(x, wx)) +
  geom_smooth(se = FALSE, method = "lm") +
  geom_point(aes(shape = is_inf), alpha = 0.7) +
  geom_text_repel(aes(label = highlight)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2)

quadrant <- vector(mode="numeric",length=nrow(localMI))
DV <- hunan$GDPPC2012 - mean(hunan$GDPPC2012)
C_mI <- localMI[,1] - mean(localMI[,1])
signif <- 0.05
quadrant[DV >0 & C_mI>0] <- 4
quadrant[DV <0 & C_mI<0] <- 1
quadrant[DV <0 & C_mI>0] <- 2
quadrant[DV >0 & C_mI<0] <- 3
quadrant[localMI[,5]>signif] <- 0

localMI

head(plot_moran)

spod <- pod |>
  select(contains("income")) |>
  mutate(across(everything(), \(x) as.numeric(scale(x))))

lcosts <- nbcosts(wm_queen, spod)

wm <- nb2listw(wm_queen, lcosts, style = "B")

mst <- mstree(wm)

clus4 <- skater(mst[, 1:2], spod, 6)

zone_map$cluster <- factor(clus4$groups)

tm_shape(zone_map) +
  tm_fill(col = "cluster") +
  tm_borders(alpha = 0.5)

library(dbscan)

input_var <- "income_avg"
zone_centers <- st_centroid(zones)
zone_centers <- st_coordinates(zone_centers)

X <- cbind(pod[[input_var]], zone_centers)
X <- pod[[input_var]]
X <- scale(X)

db_naive <- dbscan(X, eps = 0.05, minPts = 4)

clusters_zones <- zones |>
  mutate(cluster_id = db_naive$cluster) |>
  filter(cluster_id >= 1) |>
  st_as_sf()

tm_shape(clusters_zones) +
  tm_fill(col = "cluster_id")

db_naive$cluster























