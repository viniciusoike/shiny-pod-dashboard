library(tmap)
library(tmaptools)

tmap_mode(mode = "view")

pals <- c(
  "Paleta 1" = "viridis",
  "Paleta 2" = "BrBG",
  "Paleta 3" = "RdBu",
  "Paleta 4" = "Set3"
  )

styles <- c(
  "Quantis" = "quantile",
  "Quebras Naturais" = "jenks",
  "Desvio-Padrão" = "sd",
  "Cluster" = "hclust"
  )

tmap_options(check.and.fix = TRUE)

map_pod <- function(shp, x, pal = "Paleta 1", style = "Quantis", n = 7) {

  tm_shape(shp) +
    tm_fill(
      col = x,
      palette = pals[pal],
      style = styles[style],
      n = n,
      id = "name_zone") +
    tm_borders(col = "gray80", lwd = 1) +
    tm_basemap(server = "CartoDB.Positron") +
    tm_view(set.view = c(-46.659736, -23.554353, 12))

}
