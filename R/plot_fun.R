
swap_variable <- function(x) {
  dictionary |> dplyr::filter(var_label_pt == x) |> dplyr::pull(var_name)
}

vl_variables <- dictionary$var_name
names(vl_variables) <- dictionary$var_label_pt

prep_pod_data <- function(x, y, z, logx = "", logy = FALSE) {

  # Swap variables
  x <- swap_variable(x)
  y <- swap_variable(y)
  size <- swap_variable(z)

  variables <- c(x, y, size)

  df <- pod |>
    dplyr::select(name_zone, name_district, dplyr::all_of(variables))

  if (logx == "Log") {
    df <- mutate(df, across(all_of(x), log))
  }

  if (logy == "Log") {
    df <- mutate(df, across(all_of(y), log))
  }

  return(df)

}


#
# df <- prep_pod_data("Razão de Dependência Total", "Carros por Família (média)", "Número de Empregos")
#
# ggplot(df, aes(x = ))
#
#
#
#
# ggplot(pod, aes(x = scale(fami, center = TRUE, scale = FALSE), y = jobs)) +
#   geom_point()
#
# plot_scatter <- function(x, y, z) {
#
#
#
#   ggplot(df, aes(x = x, y = y)) +
#     geom_point(aes(size = size), color = "#264653", alpha = 0.5) +
#     scale_size_continuous(range = c(1, 10)) +
#     theme_minimal() +
#     theme(legend.position = "bottom")
#
# }
#
#
#
#
#
# p <- ggplot(pod_data(), aes(x = .data[[x]], y = .data[[y]])) +
#   geom_point(aes(size = .data[[size]]), color = "#264653", alpha = 0.5) +
#   scale_size_continuous(
#     name = input$z_var,
#     range = c(1, 10)
#   ) +
#   scale_x_continuous(
#     labels = scales::label_number(big.mark = ".", decimal.mark = ",")
#   ) +
#   scale_y_continuous(
#     labels = scales::label_number(big.mark = ".", decimal.mark = ",")
#   ) +
#   labs(
#     x = input$x_var,
#     y = input$y_var
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
#
# if (input$is_trend) {
#   p <- p +
#     geom_smooth(color = "#e9c46a", method = "lm", se = FALSE)
# }
#
# p
