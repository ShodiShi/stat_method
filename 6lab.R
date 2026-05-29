library(shiny)
library(ggplot2)
library(lmtest)

# Твой путь к файлу
PATH_TO_DATA <- "C:/Users/Мухаммадамин/Desktop/стат методы/flats R.csv"

ui <- fluidPage(
  titlePanel("Регрессионный анализ: Квартиры"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Фильтры данных"),
      selectInput("f_type", "Тип помещения:", choices = NULL),
      sliderInput("f_floor", "Этаж:", min = 1, max = 20, value = c(1, 20)),
      sliderInput("f_total", "Этажей в доме:", min = 1, max = 30, value = c(1, 30)),
      hr(),
      selectInput("model_type", "Модель регрессии:",
                  choices = list("Парная (Цена ~ Метраж)" = "simple",
                                 "Логарифмическая (ln(Цена) ~ Метраж)" = "log")),
      hr(),
      h4("Прогноз стоимости"),
      numericInput("in_space", "Площадь (м2):", 25),
      actionButton("predict_btn", "Рассчитать", class = "btn-success", style = "width: 100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Графики", 
                 br(),
                 fluidRow(
                   column(6, plotOutput("distPlot")),
                   column(6, plotOutput("scatterPlot"))
                 )),
        tabPanel("Статистика", 
                 verbatimTextOutput("summaryOut"),
                 h4("Тест на гетероскедастичность:"),
                 verbatimTextOutput("bpTest")),
        tabPanel("Обзор данных",
                 tableOutput("dataStat"),
                 tableOutput("rawTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Первичная загрузка для настройки фильтров
  full_data <- reactive({
    if(!file.exists(PATH_TO_DATA)) return(NULL)
    df <- read.csv(PATH_TO_DATA, sep = ";", fileEncoding = "cp1251")
    colnames(df) <- c("type", "price", "floor", "total_floors", "space", "furniture")
    df$space <- as.numeric(gsub(",", ".", df$space))
    df$log_price <- log(df$price)
    df
  })
  
  # Обновление лимитов в фильтрах на основе файла
  observe({
    df <- full_data()
    if(!is.null(df)) {
      updateSelectInput(session, "f_type", choices = c("Все", unique(as.character(df$type))))
      updateSliderInput(session, "f_floor", min = min(df$floor), max = max(df$floor), value = range(df$floor))
      updateSliderInput(session, "f_total", min = min(df$total_floors), max = max(df$total_floors), value = range(df$total_floors))
    }
  })
  
  # Отфильтрованные данные
  filtered_data <- reactive({
    df <- full_data()
    if(is.null(df)) return(NULL)
    
    if(input$f_type != "Все") df <- df[df$type == input$f_type, ]
    df <- df[df$floor >= input$f_floor[1] & df$floor <= input$f_floor[2], ]
    df <- df[df$total_floors >= input$f_total[1] & df$total_floors <= input$f_total[2], ]
    df
  })
  
  model_obj <- reactive({
    df <- filtered_data()
    if(nrow(df) < 2) return(NULL) # Нужно хотя бы 2 точки для модели
    if(input$model_type == "simple") return(lm(price ~ space, df))
    lm(log_price ~ space, df)
  })
  
  output$distPlot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = price)) + 
      geom_density(fill = "#5bc0de", alpha = 0.5) + theme_minimal() + labs(title="Плотность цены")
  })
  
  output$scatterPlot <- renderPlot({
    req(filtered_data())
    y_var <- if(input$model_type == "log") "log_price" else "price"
    ggplot(filtered_data(), aes_string(x = "space", y = y_var)) + 
      geom_point() + geom_smooth(method = "lm", col = "red") + 
      theme_minimal() + labs(title="Регрессия")
  })
  
  output$summaryOut <- renderPrint({ 
    req(model_obj())
    summary(model_obj()) 
  })
  
  output$bpTest <- renderPrint({ 
    req(model_obj())
    bptest(model_obj()) 
  })
  
  observeEvent(input$predict_btn, {
    output$summaryOut <- renderPrint({
      req(model_obj())
      pred <- predict(model_obj(), data.frame(space = input$in_space))
      if(input$model_type == "log") pred <- exp(pred)
      cat("ПРОГНОЗ ЦЕНЫ ДЛЯ ПЛОЩАДИ", input$in_space, ":", round(pred, 0), "руб.\n")
      cat("-------------------------------------------\n")
      summary(model_obj())
    })
  })
  
  output$dataStat <- renderTable({
    req(filtered_data())
    df <- filtered_data()
    data.frame(
      Параметр = c("Квартир после фильтра", "Средняя цена", "Макс. этаж в выборке"),
      Значение = c(nrow(df), round(mean(df$price)), max(df$total_floors))
    )
  })
  
  output$rawTable <- renderTable({ head(filtered_data(), 15) })
}

shinyApp(ui, server)