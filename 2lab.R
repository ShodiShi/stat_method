# ЛАБОРАТОРНАЯ РАБОТА №2: Интервальные оценки нормального распределения
# install.packages(c("shiny", "ggplot2", "gridExtra", "shinydashboard"))
library(shiny); library(ggplot2); library(gridExtra); library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "📊 Лаб. работа №2: Доверительные интервалы"),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Параметры", icon = icon("sliders-h"), startExpanded = TRUE,
               div(style = "padding: 15px;",
                   numericInput("a", "📍 Среднее (a):", 10, step = 0.5),
                   numericInput("sigma", "📏 СКО (σ):", 2, min = 0.1, step = 0.1),
                   sliderInput("alpha", "🎯 Уровень значимости α:", 
                               value = 0.05, min = 0.01, max = 0.10, step = 0.01,
                               animate = FALSE)
               )
      ),
      menuItem("Настройки графиков", icon = icon("chart-line"),
               div(style = "padding: 15px;",
                   checkboxInput("show_theory", "Показать теорию", TRUE),
                   checkboxInput("show_grid", "Показать сетку", TRUE)
               )
      ),
      div(style = "padding: 20px 15px;",
          actionButton("gen", "🎲 Генерировать выборки", 
                       class = "btn-success btn-lg btn-block",
                       style = "white-space: normal; height: auto; padding: 12px;")
      )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-top: 3px solid #3c8dbc; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .box-title { font-weight: bold; font-size: 16px; }
      .small-box { border-radius: 5px; }
      .small-box h3 { font-size: 28px; font-weight: bold; }
      .btn-success { 
        background-color: #00a65a !important; 
        border-color: #008d4c !important;
        font-size: 15px !important;
      }
      .btn-success:hover { background-color: #008d4c !important; }
      .sidebar-menu { padding-bottom: 20px; }
    '))),
    
    fluidRow(
      valueBoxOutput("box_n100", width = 4),
      valueBoxOutput("box_n500", width = 4),
      valueBoxOutput("box_n1000", width = 4)
    ),
    
    tabBox(width = 12, title = "Результаты анализа",
           tabPanel("📈 Q-Q графики", 
                    plotOutput("qq_plots", height = "550px")),
           tabPanel("📊 Гистограммы", 
                    plotOutput("hist_plots", height = "550px")),
           tabPanel("📋 Статистика", 
                    box(width = 12, solidHeader = TRUE, status = "primary",
                        verbatimTextOutput("stats"))),
           tabPanel("📉 Сходимость", 
                    box(width = 12, solidHeader = TRUE, status = "info",
                        plotOutput("conv_plot", height = "600px")))
    )
  )
)

server <- function(input, output) {
  data <- eventReactive(input$gen, {
    set.seed(42)
    N_vals <- c(100, 500, 1000)
    samples <- lapply(N_vals, function(n) rnorm(n, input$a, input$sigma))
    
    results <- lapply(1:3, function(i) {
      s <- samples[[i]]; N <- N_vals[i]
      x_bar <- mean(s); s2 <- var(s); s_sd <- sd(s)
      
      # ДИ при известной σ² (z-интервал)
      z <- qnorm(1 - input$alpha/2)
      me_known <- z * input$sigma / sqrt(N)
      ci_known <- c(x_bar - me_known, x_bar + me_known)
      
      # ДИ при неизвестной σ² (t-интервал)
      t <- qt(1 - input$alpha/2, df = N - 1)
      me_unknown <- t * s_sd / sqrt(N)
      ci_unknown <- c(x_bar - me_unknown, x_bar + me_unknown)
      
      list(sample = s, N = N, mean = x_bar, var = s2, sd = s_sd,
           ci_known = ci_known, ci_unknown = ci_unknown,
           me_known = me_known, me_unknown = me_unknown)
    })
    
    list(results = results, N_vals = N_vals, samples = samples)
  })
  
  output$box_n100 <- renderValueBox({
    if(input$gen == 0) return(valueBox("—", "N = 100", icon = icon("database"), color = "aqua"))
    d <- data()
    r <- d$results[[1]]
    valueBox(
      sprintf("%.3f", r$mean),
      subtitle = sprintf("N=100 | x̄ (σ=%.3f)", r$sd),
      icon = icon("chart-bar"),
      color = "aqua"
    )
  })
  
  output$box_n500 <- renderValueBox({
    if(input$gen == 0) return(valueBox("—", "N = 500", icon = icon("database"), color = "green"))
    d <- data()
    r <- d$results[[2]]
    valueBox(
      sprintf("%.3f", r$mean),
      subtitle = sprintf("N=500 | x̄ (σ=%.3f)", r$sd),
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$box_n1000 <- renderValueBox({
    if(input$gen == 0) return(valueBox("—", "N = 1000", icon = icon("database"), color = "purple"))
    d <- data()
    r <- d$results[[3]]
    valueBox(
      sprintf("%.3f", r$mean),
      subtitle = sprintf("N=1000 | x̄ (σ=%.3f)", r$sd),
      icon = icon("chart-area"),
      color = "purple"
    )
  })
  
  output$stats <- renderPrint({
    d <- data()
    cat("═══════════════════════════════════════════════════════════════\n")
    cat("  ТЕОРЕТИЧЕСКИЕ ПАРАМЕТРЫ\n")
    cat("═══════════════════════════════════════════════════════════════\n")
    cat(sprintf("a = %.2f | σ = %.2f | σ² = %.2f | α = %.2f\n\n", 
                input$a, input$sigma, input$sigma^2, input$alpha))
    
    for(i in 1:3) {
      r <- d$results[[i]]
      cat("═══════════════════════════════════════════════════════════════\n")
      cat(sprintf("  ВЫБОРКА N = %d\n", r$N))
      cat("═══════════════════════════════════════════════════════════════\n")
      cat(sprintf("x̄ = %.4f | s² = %.4f | s = %.4f\n\n", r$mean, r$var, r$sd))
      
      cat("─────────────────────────────────────────────────────────────\n")
      cat("  ДИ при ИЗВЕСТНОЙ σ² (z-интервал)\n")
      cat("─────────────────────────────────────────────────────────────\n")
      cat(sprintf("z(%.3f) = %.4f\n", 1-input$alpha/2, qnorm(1 - input$alpha/2)))
      cat(sprintf("Погрешность: ±%.4f\n", r$me_known))
      cat(sprintf("Интервал: [%.4f, %.4f]\n", r$ci_known[1], r$ci_known[2]))
      cat(sprintf("Ширина: %.4f\n", diff(r$ci_known)))
      covered <- input$a >= r$ci_known[1] && input$a <= r$ci_known[2]
      cat(sprintf("a %s\n\n", ifelse(covered, "✓ ПОПАЛО", "✗ НЕ ПОПАЛО")))
      
      cat("─────────────────────────────────────────────────────────────\n")
      cat("  ДИ при НЕИЗВЕСТНОЙ σ² (t-интервал)\n")
      cat("─────────────────────────────────────────────────────────────\n")
      cat(sprintf("t(%.3f, df=%d) = %.4f\n", 1-input$alpha/2, r$N-1, qt(1 - input$alpha/2, r$N-1)))
      cat(sprintf("Погрешность: ±%.4f\n", r$me_unknown))
      cat(sprintf("Интервал: [%.4f, %.4f]\n", r$ci_unknown[1], r$ci_unknown[2]))
      cat(sprintf("Ширина: %.4f\n", diff(r$ci_unknown)))
      covered <- input$a >= r$ci_unknown[1] && input$a <= r$ci_unknown[2]
      cat(sprintf("a %s\n\n", ifelse(covered, "✓ ПОПАЛО", "✗ НЕ ПОПАЛО")))
    }
  })
  
  output$qq_plots <- renderPlot({
    d <- data()
    colors <- c("#3498db", "#2ecc71", "#9b59b6")
    plots <- lapply(1:3, function(i) {
      s <- d$samples[[i]]; N <- d$N_vals[i]
      df <- data.frame(
        theoretical = qnorm(ppoints(N), input$a, input$sigma),
        sample = sort(s)
      )
      p <- ggplot(df, aes(x = theoretical, y = sample)) +
        geom_point(color = colors[i], size = 2.5, alpha = 0.6) +
        labs(title = sprintf("N = %d", N), 
             x = "Теоретические квантили", 
             y = "Выборочные квантили") +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 15, color = colors[i]),
          panel.border = element_rect(fill = NA, color = "gray70"),
          panel.grid.minor = element_blank()
        )
      if(input$show_theory) {
        p <- p + geom_abline(intercept = 0, slope = 1, 
                             color = "#e74c3c", size = 1.2, linetype = "dashed")
      }
      if(!input$show_grid) {
        p <- p + theme(panel.grid = element_blank())
      }
      p
    })
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], ncol = 3, 
                 top = grid::textGrob("Q-Q графики: проверка нормальности", 
                                      gp = grid::gpar(fontsize = 16, fontface = "bold")))
  })
  
  output$hist_plots <- renderPlot({
    d <- data()
    colors <- c("#3498db", "#2ecc71", "#9b59b6")
    plots <- lapply(1:3, function(i) {
      s <- d$samples[[i]]; N <- d$N_vals[i]
      p <- ggplot(data.frame(x = s), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, 
                       fill = colors[i], color = "white", alpha = 0.6) +
        stat_density(aes(color = "Оценка"), geom = "line", size = 1.5) +
        labs(title = sprintf("N = %d", N), 
             x = "Значение", y = "Плотность", color = "") +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 15, color = colors[i]),
          legend.position = "top",
          panel.border = element_rect(fill = NA, color = "gray70"),
          panel.grid.minor = element_blank()
        )
      if(input$show_theory) {
        p <- p + stat_function(fun = dnorm, 
                               args = list(mean = input$a, sd = input$sigma),
                               aes(color = "Теория"), size = 1.5) +
          scale_color_manual(values = c("Теория" = "#e74c3c", "Оценка" = "#34495e"))
      } else {
        p <- p + scale_color_manual(values = c("Оценка" = "#34495e"))
      }
      if(!input$show_grid) {
        p <- p + theme(panel.grid = element_blank())
      }
      p
    })
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], ncol = 3,
                 top = grid::textGrob("Гистограммы и плотности распределения", 
                                      gp = grid::gpar(fontsize = 16, fontface = "bold")))
  })
  
  output$conv_plot <- renderPlot({
    d <- data()
    N_vals <- d$N_vals
    means <- sapply(d$results, function(r) r$mean)
    ci_known_lower <- sapply(d$results, function(r) r$ci_known[1])
    ci_known_upper <- sapply(d$results, function(r) r$ci_known[2])
    ci_unknown_lower <- sapply(d$results, function(r) r$ci_unknown[1])
    ci_unknown_upper <- sapply(d$results, function(r) r$ci_unknown[2])
    
    # Создаём отдельные датафреймы для каждой линии
    df_mean <- data.frame(N = N_vals, Value = means, Type = "Точечная оценка x̄")
    df_known_lower <- data.frame(N = N_vals, Value = ci_known_lower, Type = "ДИ (известная σ): нижняя")
    df_known_upper <- data.frame(N = N_vals, Value = ci_known_upper, Type = "ДИ (известная σ): верхняя")
    df_unknown_lower <- data.frame(N = N_vals, Value = ci_unknown_lower, Type = "ДИ (неизвестная σ): нижняя")
    df_unknown_upper <- data.frame(N = N_vals, Value = ci_unknown_upper, Type = "ДИ (неизвестная σ): верхняя")
    
    df <- rbind(df_mean, df_known_lower, df_known_upper, df_unknown_lower, df_unknown_upper)
    df$Type <- factor(df$Type, levels = c("Точечная оценка x̄", 
                                          "ДИ (известная σ): нижняя", "ДИ (известная σ): верхняя",
                                          "ДИ (неизвестная σ): нижняя", "ДИ (неизвестная σ): верхняя"))
    
    ggplot(df, aes(x = N, y = Value, color = Type, shape = Type, linetype = Type)) +
      geom_line(size = 1.5) +
      geom_point(size = 5) +
      geom_hline(yintercept = input$a, linetype = "dotted", color = "black", size = 1.2) +
      annotate("text", x = min(N_vals) + 50, y = input$a, 
               label = sprintf("Истинное a = %.2f", input$a),
               vjust = -0.8, hjust = 0, size = 5.5, fontface = "bold", color = "black") +
      scale_color_manual(
        values = c("Точечная оценка x̄" = "#e74c3c", 
                   "ДИ (известная σ): нижняя" = "#3498db",
                   "ДИ (известная σ): верхняя" = "#3498db",
                   "ДИ (неизвестная σ): нижняя" = "#2ecc71",
                   "ДИ (неизвестная σ): верхняя" = "#2ecc71"),
        breaks = c("Точечная оценка x̄", 
                   "ДИ (известная σ): нижняя", "ДИ (известная σ): верхняя",
                   "ДИ (неизвестная σ): нижняя", "ДИ (неизвестная σ): верхняя")
      ) +
      scale_shape_manual(
        values = c(16, 17, 17, 15, 15),
        breaks = c("Точечная оценка x̄", 
                   "ДИ (известная σ): нижняя", "ДИ (известная σ): верхняя",
                   "ДИ (неизвестная σ): нижняя", "ДИ (неизвестная σ): верхняя")
      ) +
      scale_linetype_manual(
        values = c("solid", "dashed", "dashed", "dotdash", "dotdash"),
        breaks = c("Точечная оценка x̄", 
                   "ДИ (известная σ): нижняя", "ДИ (известная σ): верхняя",
                   "ДИ (неизвестная σ): нижняя", "ДИ (неизвестная σ): верхняя")
      ) +
      scale_x_continuous(breaks = N_vals) +
      labs(title = "Зависимость оценок и границ доверительных интервалов от объема выборки",
           x = "Объем выборки (N)", y = "Значение параметра", 
           color = "Тип оценки:", shape = "Тип оценки:", linetype = "Тип оценки:") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15, margin = margin(b = 15)),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.8, "cm"),
        panel.border = element_rect(fill = NA, color = "gray70"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold")
      )
  })
}

shinyApp(ui = ui, server = server)