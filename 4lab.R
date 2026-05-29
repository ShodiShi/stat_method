library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- Подготовка данных ---
pulse_data <- data.frame(
  CB = c(68,104,102,87,76,72,66,85,98,82,76,84,92,85,84,72,80,87,84,90,76,100,80,92,88,78,80,66,72,86, rep(NA, 20)),
  EB = c(76,85,90,94,68,80,98,88,72,90,104,80,78,84,100,78,80,78,80,82,98,72,72,68,88,72,80,86,86,80,85,80,104,68,82,76,84,82,72,76,100,68,80,72,94,76,88,78,82,68),
  CA = c(86,76,72,85,74,84,88,80,80,85,80,80,86,80,80,72,86,84,84,88,72,72,80,84,80,76,76,88,80,76, rep(NA, 20)),
  EA = c(64,68,64,80,72,60,78,84,64,74,72,84,66,88,66,90,60,64,68,70,60,60,60,80,68,72,66,70,60,70,56,68,60,60,82,64,60,62,60,60,86,72,64,82,82,60,56,68,68,66)
)

grades_raw <- data.frame(
  "Группа 1" = c(4,5,3,4,3,3,3,4,3,3,3,3,3,4,4,4,3,4,4,3,4,3,3,3,3,5,4,3,3,4),
  "Группа 2" = c(4,4,3,4,5,4,5,4,4,4,4,3,3,4,4,4,3,4,3,4,3,4,3,3,3,5,3,3,3,4),
  "Группа 3" = c(4,4,4,4,3,3,5,4,5,4,3,5,4,4,3,3,4,3,3,3,3,3,4,4,3,3,3,5,4,5),
  "Группа 4" = c(5,5,3,5,4,5,4,5,5,5,5,5,3,5,5,5,5,5,5,4,4,5,3,4,4,5,3,5,5,4),
  check.names = FALSE
)

# --- Интерфейс (UI) ---
ui <- fluidPage(
  titlePanel("Статистический анализ (Раздельные графики)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("task", "Выберите раздел анализа:", 
                  choices = c("Пульс: До vs После (п.2)" = "pulse_time",
                              "Пульс: Больные vs Здоровые (п.3)" = "pulse_groups",
                              "Анализ оценок (Задание 2)" = "grades")),
      hr(),
      helpText("Графики разделены строго по пунктам задания.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Визуализация", plotOutput("mainPlot")),
        tabPanel("Статистические тесты", verbatimTextOutput("statsOutput"))
      )
    )
  )
)

# --- Сервер ---
server <- function(input, output) {
  
  output$mainPlot <- renderPlot({
    if (input$task == "pulse_time") {
      # ПУНКТ 2: Сравнение До и После
      df_long <- stack(pulse_data) %>% na.omit()
      # Фильтруем для удобства по парам
      ggplot(df_long, aes(x = ind, y = values, fill = ind)) +
        geom_boxplot() +
        facet_wrap(~(ind %in% c("CB", "CA")), scales = "free_x", 
                   labeller = as_labeller(c("TRUE" = "Пациенты", "FALSE" = "Здоровые"))) +
        theme_bw() +
        labs(title = "Сравнение однородности: Состояние До vs После", x = "Этап", y = "Пульс")
      
    } else if (input$task == "pulse_groups") {
      # ПУНКТ 3: Сравнение Здоровых и Больных
      df_long <- stack(pulse_data) %>% na.omit()
      ggplot(df_long, aes(x = ind, y = values, fill = ind)) +
        geom_boxplot() +
        facet_wrap(~(ind %in% c("CB", "EB")), scales = "free_x", 
                   labeller = as_labeller(c("TRUE" = "Сравнение ДО", "FALSE" = "Сравнение ПОСЛЕ"))) +
        theme_light() +
        labs(title = "Сравнение групп: Больные vs Здоровые", x = "Группа", y = "Пульс")
      
    } else {
      # ЗАДАНИЕ 2: Оценки
      df_grades <- stack(grades_raw)
      ggplot(df_grades, aes(x = ind, fill = as.factor(values))) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Set1") +
        theme_minimal() +
        labs(title = "Распределение оценок", x = "Группа", y = "Количество", fill = "Оценка")
    }
  })
  
  output$statsOutput <- renderPrint({
    if (grepl("pulse", input$task)) {
      cat("КРИТЕРИЙ ШАПИРО-УИЛКА (Нормальность):\n")
      print(lapply(pulse_data, function(x) shapiro.test(na.omit(x))))
      
      cat("\nСРАВНЕНИЕ (T-TEST):\n")
      if (input$task == "pulse_time") {
        cat("Больные (До vs После):\n")
        print(t.test(pulse_data$CB, pulse_data$CA))
        cat("\nЗдоровые (До vs После):\n")
        print(t.test(pulse_data$EB, pulse_data$EA))
      } else {
        cat("До лечения (Больные vs Здоровые):\n")
        print(t.test(pulse_data$CB, pulse_data$EB))
        cat("\nПосле лечения (Больные vs Здоровые):\n")
        print(t.test(pulse_data$CA, pulse_data$EA))
      }
    } else {
      df_grades <- stack(grades_raw)
      tbl <- table(df_grades$ind, df_grades$values)
      cat("ТАБЛИЦА СОПРЯЖЕННОСТИ:\n")
      print(tbl)
      cat("\nКРИТЕРИЙ ХИ-КВАДРАТ:\n")
      print(chisq.test(tbl))
    }
  })
}

shinyApp(ui, server)