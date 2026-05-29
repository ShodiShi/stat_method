# Подключаем нужные библиотеки (как импорты в других языках)
library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Статистическое моделирование: Лаба №5"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("scenario", "Выберите ситуацию:",
                  choices = list("A: Мат.ожидания и дисперсии равны" = "a",
                                 "B: Мат.ожидания разные, дисперсии равны" = "b",
                                 "C: Мат.ожидания равны, дисперсии разные" = "c",
                                 "D: Все существенно различно" = "d")),
      hr(),
      helpText("Нажмите 'Запустить', чтобы обновить выборки"),
      actionButton("go", "Сгенерировать данные")
    ),
    
    mainPanel(

      tabsetPanel(
        tabPanel("Гистограммы", plotOutput("distPlot")),
        tabPanel("Коробки с усами", plotOutput("boxPlot")),
        tabPanel("Статистические тесты", verbatimTextOutput("stats"))
      )
    )
  )
)


server <- function(input, output) {
  
 
  data <- eventReactive(input$go, {
  
    s <- input$scenario
    n <- 100 
    
    # Создаем выборки в зависимости от сценария
    if (s == "a") {
      x1 <- rnorm(n, mean = 10, sd = 2) # rnorm -нормальное распределение
      x2 <- rnorm(n, mean = 10, sd = 2)
    } else if (s == "b") {
      x1 <- rnorm(n, mean = 10, sd = 2)
      x2 <- rnorm(n, mean = 25, sd = 2) # среднее сильно больше
    } else if (s == "c") {
      x1 <- rnorm(n, mean = 10, sd = 1)
      x2 <- rnorm(n, mean = 10, sd = 8) #  дисперсия сильно больше
    } else {
      x1 <- rnorm(n, mean = 10, sd = 2)
      x2 <- rnorm(n, mean = 30, sd = 10) 
    }
    

    df <- data.frame(
      Value = c(x1, x2),
      Group = rep(c("Выборка X1", "Выборка X2"), each = n)
    )
    return(list(df = df, x1 = x1, x2 = x2))
  })
  
  # Рисуем гистограмму
  output$distPlot <- renderPlot({
    d <- data()
    ggplot(d$df, aes(x = Value, fill = Group)) +
      geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
      geom_density(alpha = 0.2) +
      geom_vline(aes(xintercept = mean(d$x1)), color = "red", linetype = "dashed") +
      geom_vline(aes(xintercept = mean(d$x2)), color = "blue", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Гистограммы и плотности распределения")
  })
  
  # Рисуем "Коробки с усами"
  output$boxPlot <- renderPlot({
    d <- data()
    ggplot(d$df, aes(x = Group, y = Value, fill = Group)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Диаграмма 'Ящик с усами' (Boxplot)")
  })
  
  # Выводим результаты тестов
  output$stats <- renderPrint({
    d <- data()
    cat("--- Тест Шапиро-Уилка (Нормальность) ---\n")
    print(shapiro.test(d$x1))
    print(shapiro.test(d$x2))
    
    cat("\n--- Тест Стьюдента (Равенство средних) ---\n")
    print(t.test(d$x1, d$x2))
    
    cat("\n--- Тест Фишера (Равенство дисперсий) ---\n")
    print(var.test(d$x1, d$x2))
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)