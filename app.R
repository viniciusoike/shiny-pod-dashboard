#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

sf::sf_use_s2(FALSE)

pod <- read_csv("data/table_pod.csv")
shp <- sf::st_read("data/table_pod.gpkg")
shp <- sf::st_make_valid(shp)

dictionary <- readxl::read_excel("data/dictionary.xlsx")

pod <- dplyr::filter(pod, code_zone <= 342)
shp <- dplyr::filter(shp, code_zone <= 342)

var_choices <- names(pod)[-c(1:9)]
var_choices


source(here::here("R/map.R"))

ui_data <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      titlePanel("titulo POD"),
      selectInput("x_var", "Choose X variable:", choices = dictionary$var_label_pt, selected = "Renda Familiar Média"),
      selectInput("y_var", "Choose Y variable:", choices = dictionary$var_label_pt, selected = "Carros por Família (média)"),
      selectInput("z_var", "Choose Z variable:", choices = dictionary$var_label_pt, selected = "Densidade Populacional"),
      checkboxInput("is_trend", "Include Trend", value = FALSE),
      "Transformar",
      selectInput("trans_x", "Transform X", choices = c("None", "Scale", "Log"), selected = "None"),
      selectInput("trans_y", "Transform Y", choices = c("None", "Scale", "Log"), selected = "None"),
      selectInput("geo", "Agregar por:", choices = c("Zona OD", "Distrito"), selected = "Zona OD")
    ),
    mainPanel(
      width = 9,
      plotOutput("scatter_pod")
    )
  )
)

ui_map <- fluidPage(
  fluidRow(column(12), tmapOutput("map_pod")),
  fluidRow(
    column(
      3,
      selectInput("map_variable", "Choose a variable to map", choices = colnames(shp), selected = "income_pc"),
      selectInput("map_style", "Type of map", choices = names(styles), selected = "Quantis"),
      numericInput("map_group", "Choose n", value = 5, min = 2, max = 10, step = 1)
    )
  )
)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  navbarPage(
    title = "Dashboard POD",
    tabPanel("Dados", ui_data),
    tabPanel("Mapa", ui_map)
  )
)

swap_variable <- function(x) {
  dictionary |> dplyr::filter(var_label_pt == x) |> dplyr::pull(var_name)
}

vl_variables <- dictionary$var_name
names(vl_variables) <- dictionary$var_label_pt

plot_scatter <- function(df) {

  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size), color = "#264653", alpha = 0.5) +
    scale_size_continuous(range = c(1, 10)) +
    theme_minimal() +
    theme(legend.position = "bottom")

}

# Define server logic required to draw a histogram
server <- function(input, output) {

  pod_data <- reactive({

    df <- pod

    # Swap variables
    x <- swap_variable(input$x_var)
    y <- swap_variable(input$y_var)
    size <- swap_variable(input$z_var)

    if (input$trans_x == "Log") {df <- pod |> mutate(across(x, log))}
    if (input$trans_y == "Log") {df <- pod |> mutate(across(y, log))}
    if (input$trans_x == "Scale") {df <- pod |> mutate(across(x, ~as.numeric(scale(.x))))}
    if (input$trans_y == "Scale") {df <- pod |> mutate(across(y, ~as.numeric(scale(.x))))}

    return(df)

  })


  output$scatter_pod <- renderPlot({

    # Swap variables
    x <- swap_variable(input$x_var)
    y <- swap_variable(input$y_var)
    size <- swap_variable(input$z_var)

    p <- ggplot(pod_data(), aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(aes(size = .data[[size]]), color = "#264653", alpha = 0.5) +
      scale_size_continuous(
        name = input$z_var,
        range = c(1, 10)
        ) +
      scale_x_continuous(
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      ) +
      scale_y_continuous(
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
        ) +
      labs(
        x = input$x_var,
        y = input$y_var
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    if (input$is_trend) {
      p <- p +
        geom_smooth(color = "#e9c46a", method = "lm", se = FALSE)
    }

    p


  }, res = 72, height = 600)

  output$map_pod <- renderTmap({
    map_pod(
      shp = shp,
      x = input$map_variable,
      style = input$map_style,
      n = input$map_group
      )
  })


}

# Run the application
shinyApp(ui = ui, server = server)
