library(biscale)
library(sf)

shp <- st_make_valid(shp)

shp <- st_cast(shp, "MULTIPOLYGON")

map_biscale <- function(x, y) {

  as.character(substitute(x))

}

map_biscale(income_avg)

x <- dplyr::select(shp, income_avg, car_rate)

y <- na.omit(x$income_avg)

var_cut <- function(dat, x, y) {

  clean_dat <- shp %>%
    filter(!is.na(.data[[x]]), !is.na(.data[[y]]))

  clean_dat$cutx <- cut(
    clean_dat[[x]],
    breaks = classInt::classIntervals(clean_dat[[x]], style = "jenks", n = 3)$brks,
    include.lowest = TRUE,
    dig.lab = 3,
    na.rm = TRUE
  )

  clean_dat$cuty <- cut(
    clean_dat[[y]],
    breaks = classInt::classIntervals(clean_dat[[y]], style = "jenks", n = 3)$brks,
    include.lowest = TRUE,
    dig.lab = 3,
    na.rm = TRUE
  )

  # combine
  clean_dat$bi_class <- paste0(
    as.numeric(clean_dat$cutx), "-", as.numeric(clean_dat$cuty))

  # create levels
  out <- list(
    bi_x = levels(clean_dat$cutx),
    bi_y = levels(clean_dat$cuty)
  )

  return(clean_dat)
}

xx = var_cut(shp, "income_avg", "car_rate")

bi_class(shp, income_avg, car_rate, "jenks")

bi_levels_clean(xx, FALSE)


x1 <- cut(
  y,
  breaks = classInt::classIntervals(y, style = "jenks", n = 3)$brks,
  include.lowest = TRUE,
  dig.lab = 3,
  na.rm = TRUE
)

levels(x1)


# clean levels created by bi_var_cut
bi_levels_clean <- function(levels_list, split){

  # remove braces
  levels_list$bi_x <- gsub("[][()]", "", levels_list$bi_x)
  levels_list$bi_y <- gsub("[][()]", "", levels_list$bi_y)

  # add dashes
  levels_list$bi_x <- gsub(",", "-", levels_list$bi_x)
  levels_list$bi_y <- gsub(",", "-", levels_list$bi_y)

  # optionally split
  if (split == TRUE){

    levels_list$bi_x <- unique(unlist(strsplit(levels_list$bi_x, "-")))
    levels_list$bi_y <- unique(unlist(strsplit(levels_list$bi_y, "-")))

  }
  # return output
  return(levels_list)

}


classInt::classIntervals(na.omit(x$income_avg), n = 3, style = "jenks")$brks

mean_rm <- function(var) {
  var <- ensym(var)
  expr(mean(!!var, na.rm = TRUE))
}



map_biscale("income_avg")
library(rlang)
my_biclass <- function(df, x, y) {

  bi_class(df, .data[[x]], .data[[y]], style = "jenks")

}

my_biclass(shp, "income_avg", "car_rate")

my_mean <- function(x) {

  x_var <- ensym(x)

  mean(!!x_var)

}

my_mean("3")

my_biclass("income_avg", "car_rate")
bi_class(shp, income_avg, car_rate, style = "jenks")


map_biscale <- function(x, y) {

  clean_dat <- shp %>%
    filter(!is.na(.data[[x]]), !is.na(.data[[y]]))

  biclassed <- bi_class(
    clean_dat,
    x = rlang::ensym(x),
    y = rlang::ensym(y),
    style = "jenks",
    dim = 3
    )

  lvls = unique(biclassed$bi_class)
  lvls = lvls[order(lvls)]
  labels = biscale_labels[lvls]

  biclassed <- biclassed %>%
    select(name_zone, all_of(c(x, y)), bi_class) %>%
    mutate(
      bi_class_label = factor(bi_class, levels = lvls, labels = labels)
    )

  tm_shape(biclassed) +
    tm_fill(
      col = "bi_class_label",
      palette = grpink[lvls],
      id = "name_zone",
      popup.vars = c(
        "Cluster: " = "bi_class_label",
        "Income: " = "income_avg",
        "Car rate: " = "car_rate")
    ) +
    tm_borders(col = "white", lwd = 0.5)

}

map_biscale("income_avg", "car_rate")



clean_dat <- shp %>%
  filter(!is.na(income_avg), !is.na(car_rate)) %>%
  mutate(income_avg = log(income_avg))

biclassed <- bi_class(clean_dat, x = income_avg, y = car_rate, style = "jenks", dim = 3)

lvls = unique(biclassed$bi_class)
lvls = lvls[order(lvls)]
lvls <- c("1-1", "1-2", "2-1", "2-2", "2-3", "3-2", "3-3")

biscale_labels <- c(
  "1-1" = "Low-Low", "1-2" = "Low-Medium", "1-3" = "Low-High", "2-1" = "Medium-Low",
  "2-2" = "Medium-Medium", "2-3" = "Medium-High", "3-1" = "High-Low",
  "3-2" = "High-Medium", "3-3" = "High-High"
  )



grpink <- biscale::bi_pal("GrPink", dim = 3, preview = FALSE)

biclassed <- biclassed %>%
  select(name_zone, income_avg, car_rate, bi_class) %>%
  mutate(
    bi_class_label = factor(bi_class, levels = lvls, labels = labels)
    )

library(tmap)
library(tmaptools)
tmap_mode(mode = "view")

tm_shape(biclassed) +
  tm_fill(
    col = "bi_class_label",
    palette = grpink[lvls],
    id = "name_zone",
    popup.vars = c(
      "Cluster: " = "bi_class_label",
      "Income: " = "income_avg",
      "Car rate: " = "car_rate")
    ) +
  tm_borders(col = "white", lwd = 0.5)



map = ggplot() +
  geom_sf(
    data = biclassed,
    mapping = aes(fill = bi_class, text = paste(name_zone, round(income_avg, 2))),
    color = "white",
    size = 0.05) +
  bi_scale_fill(pal = "GrPink", dim = 3, labels = labels)

ggplotly(map)
