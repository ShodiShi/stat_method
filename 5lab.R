library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Лабораторная работа №5: Корреляционный анализ"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Объем выборки (N):", 200),
      sliderInput("r1_target", "Заданный r1 (слабая связь):", 0.1, 0.2, 0.15, step = 0.01),
      sliderInput("r2_target", "Заданный r2 (сильная связь):", 0.6, 0.9, 0.75, step = 0.01),
      hr(),
      helpText("X2 и X3 генерируются на основе X1 по заданным формулам.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Диаграммы рассеяния", 
                 plotOutput("scatter1"),
                 plotOutput("scatter2")),
        tabPanel("Анализ и Статистика", 
                 verbatimTextOutput("statResults")),
        tabPanel("Доверительные интервалы",
                 verbatimTextOutput("fisherCI"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Генерируем данные реактивно
  data_lab <- reactive({
    set.seed(123) # Для воспроизводимости
    N <- input$n
    a <- 0; s <- 1 # Произвольные параметры
    
    X1 <- rnorm(N, a, s)
    
    # Формулы из задания:
    # X_new = r*X1 + sqrt(1 - r^2) * rnorm(N, a, s)
    X2 <- input$r1_target * X1 + sqrt(1 - input$r1_target^2) * rnorm(N, a, s)
    X3 <- input$r2_target * X1 + sqrt(1 - input$r2_target^2) * rnorm(N, a, s)
    
    data.frame(X1 = X1, X2 = X2, X3 = X3)
  })
  
  # 4. Диаграммы рассеяния
  output$scatter1 <- renderPlot({
    df <- data_lab()
    ggplot(df, aes(x = X1, y = X2)) + 
      geom_point(color = "blue", alpha = 0.6) +
      geom_smooth(method = "lm", color = "black") +
      labs(title = paste("Слабая корреляция (Задано r =", input$r1_target, ")"))
  })
  
  output$scatter2 <- renderPlot({
    df <- data_lab()
    ggplot(df, aes(x = X1, y = X3)) + 
      geom_point(color = "red", alpha = 0.6) +
      geom_smooth(method = "lm", color = "black") +
      labs(title = paste("Сильная корреляция (Задано r =", input$r2_target, ")"))
  })
  
  # 5 и 6. Коэффициенты и t-статистика
  output$statResults <- renderPrint({
    df <- data_lab()
    test1 <- cor.test(df$X1, df$X2)
    test2 <- cor.test(df$X1, df$X3)
    
    cat("--- Анализ X1 и X2 ---\n")
    cat("Выборочный r1_hat:", round(test1$estimate, 4), "\n")
    cat("t-статистика Стьюдента:", round(test1$statistic, 4), "\n")
    cat("p-value:", test1$p.value, "\n\n")
    
    cat("--- Анализ X1 и X3 ---\n")
    cat("Выборочный r2_hat:", round(test2$estimate, 4), "\n")
    cat("t-статистика Стьюдента:", round(test2$statistic, 4), "\n")
    cat("p-value:", test2$p.value, "\n")
  })
  
  # 7. Z-преобразование Фишера
  output$fisherCI <- renderPrint({
    df <- data_lab()
    r_val <- cor(df$X1, df$X3) # Берем для примера сильную связь
    n <- input$n
    
    # Z = 0.5 * ln((1+r)/(1-r))
    z <- 0.5 * log((1 + r_val) / (1 - r_val))
    se_z <- 1 / sqrt(n - 3)
    
    # Доверительный интервал для Z
    z_low <- z - 1.96 * se_z
    z_high <- z + 1.96 * se_z
    
    # Обратное преобразование в r
    r_low <- (exp(2 * z_low) - 1) / (exp(2 * z_low) + 1)
    r_high <- (exp(2 * z_high) - 1) / (exp(2 * z_high) + 1)
    
    cat("Доверительный интервал (95%) для r2 через Z-преобразование Фишера:\n")
    cat("Z-значение:", round(z, 4), "\n")
    cat("Нижняя граница r:", round(r_low, 4), "\n")
    cat("Верхняя граница r:", round(r_high, 4), "\n")
  })
}

shinyApp(ui, server)