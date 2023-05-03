library(ggplot2)
library(ggrepel)
library(readr)
library(sf)

pod <- read_csv("data/table_pod.csv")
shp <- st_read("data/table_pod.gpkg")

pod <- dplyr::filter(pod, code_zone <= 342)
shp <- dplyr::filter(shp, code_zone <= 342)
