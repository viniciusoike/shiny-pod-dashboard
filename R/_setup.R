library(ggplot2)
library(readr)
library(dplyr)
library(tmap)
library(tmaptools)
library(plotly)
tmap_mode(mode = "view")

sf::sf_use_s2(FALSE)

pod <- read_csv("data/table_pod.csv")
shp <- sf::st_read("data/table_pod.gpkg")
shp <- sf::st_make_valid(shp)

dictionary <- readxl::read_excel("data/dictionary.xlsx")

pod <- dplyr::filter(pod, code_zone <= 342)
shp <- dplyr::filter(shp, code_zone <= 342)

var_choices <- names(pod)[-c(1:9)]
