# ЛАБОРАТОРНАЯ РАБОТА №1: Анализ распределений
# install.packages(c("shiny", "ggplot2", "moments"))
library(shiny); library(ggplot2); library(moments)

ui <- fluidPage(
  titlePanel("📊 Лаб. работа №1: Анализ распределений"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("dist", "Распределение:", c("Биномиальное", "Геометрическое", "Экспоненциальное", "Гамма")),
                 conditionalPanel(condition = "input.dist == 'Биномиальное'",
                                  numericInput("n_b", "n:", 20, 1), 
                                  numericInput("p_b", "p:", 0.3, 0.01, 0.99, 0.01)),
                 conditionalPanel(condition = "input.dist == 'Геометрическое'",
                                  numericInput("p_g", "p:", 0.25, 0.01, 0.99, 0.01)),
                 conditionalPanel(condition = "input.dist == 'Экспоненциальное'",
                                  numericInput("lam", "λ:", 2, 0.1, step = 0.1)),
                 conditionalPanel(condition = "input.dist == 'Гамма'",
                                  numericInput("alp", "α:", 3, 0.1, step = 0.1), 
                                  numericInput("bet", "β:", 2, 0.1, step = 0.1)),
                 numericInput("N", "Объем N:", 150, 100, 200),
                 actionButton("gen", "🎲 Сгенерировать", class = "btn-primary btn-block")
    ),
    mainPanel(width = 9, tabsetPanel(
      tabPanel("📈 Графики", fluidRow(column(6, plotOutput("p1", height = "350px")), column(6, plotOutput("p2", height = "350px")))),
      tabPanel("📋 Статистика", verbatimTextOutput("stats"))
    ))
  )
)

server <- function(input, output) {
  data <- eventReactive(input$gen, {
    set.seed(42); N <- input$N
    if(input$dist == "Биномиальное") {
      s <- rbinom(N, input$n_b, input$p_b); tm <- input$n_b * input$p_b; tv <- tm * (1 - input$p_b)
      pe <- mean(s) / input$n_b; o <- as.numeric(table(s)); v <- as.numeric(names(table(s)))
      e <- dbinom(v, input$n_b, pe) * N; vl <- e >= 5; cs <- sum((o[vl] - e[vl])^2 / e[vl])
      df <- sum(vl) - 2; pv <- 1 - pchisq(cs, df)
      list(s=s, tm=tm, tv=tv, pe=pe, pt=input$p_b, pn="p", cs=cs, df=df, pv=pv, tp="d")
    } else if(input$dist == "Геометрическое") {
      s <- rgeom(N, input$p_g); q <- 1 - input$p_g; tm <- q / input$p_g; tv <- q / input$p_g^2
      pe <- 1 / (mean(s) + 1); o <- as.numeric(table(s)); v <- as.numeric(names(table(s)))
      e <- dgeom(v, pe) * N; vl <- e >= 5; cs <- sum((o[vl] - e[vl])^2 / e[vl])
      df <- sum(vl) - 2; pv <- 1 - pchisq(cs, df)
      list(s=s, tm=tm, tv=tv, pe=pe, pt=input$p_g, pn="p", cs=cs, df=df, pv=pv, tp="d")
    } else if(input$dist == "Экспоненциальное") {
      s <- rexp(N, input$lam); tm <- 1 / input$lam; tv <- 1 / input$lam^2; le <- 1 / mean(s)
      br <- quantile(s, seq(0, 1, 0.1)); o <- hist(s, breaks = br, plot = F)$counts
      e <- diff(pexp(br, le)) * N; vl <- e >= 5; cs <- sum((o[vl] - e[vl])^2 / e[vl])
      df <- sum(vl) - 2; pv <- 1 - pchisq(cs, df)
      list(s=s, tm=tm, tv=tv, pe=le, pt=input$lam, pn="λ", cs=cs, df=df, pv=pv, tp="c")
    } else {
      s <- rgamma(N, input$alp, input$bet); tm <- input$alp / input$bet; tv <- input$alp / input$bet^2
      ae <- mean(s)^2 / var(s); be <- mean(s) / var(s)
      br <- quantile(s, seq(0, 1, 0.1)); o <- hist(s, breaks = br, plot = F)$counts
      e <- diff(pgamma(br, ae, be)) * N; vl <- e >= 5; cs <- sum((o[vl] - e[vl])^2 / e[vl])
      df <- sum(vl) - 3; pv <- 1 - pchisq(cs, df)
      list(s=s, tm=tm, tv=tv, pe=c(ae,be), pt=c(input$alp,input$bet), pn=c("α","β"), cs=cs, df=df, pv=pv, tp="c")
    }
  })
  
  output$stats <- renderPrint({
    d <- data()
    cat("═══════════════════════════════════════════════════════\n")
    cat("  ТЕОРЕТИЧЕСКИЕ ХАРАКТЕРИСТИКИ\n")
    cat("═══════════════════════════════════════════════════════\n")
    cat(sprintf("M[X] = %.4f | D[X] = %.4f\n\n", d$tm, d$tv))
    cat("═══════════════════════════════════════════════════════\n")
    cat("  ВЫБОРОЧНЫЕ ХАРАКТЕРИСТИКИ\n")
    cat("═══════════════════════════════════════════════════════\n")
    cat(sprintf("x̄ = %.4f (откл. %.2f%%) | s² = %.4f (откл. %.2f%%)\n", 
                mean(d$s), abs(mean(d$s)-d$tm)/d$tm*100, var(d$s), abs(var(d$s)-d$tv)/d$tv*100))
    cat(sprintf("s = %.4f | Me = %.4f\n", sd(d$s), median(d$s)))
    cat(sprintf("Асимметрия = %.4f | Эксцесс = %.4f\n\n", skewness(d$s), kurtosis(d$s)-3))
    cat("═══════════════════════════════════════════════════════\n")
    cat("  ОЦЕНКА ПАРАМЕТРОВ\n")
    cat("═══════════════════════════════════════════════════════\n")
    if(length(d$pn) == 1) {
      cat(sprintf("Оценка %s: %.4f | Истинное %s: %.4f | Откл: %.2f%%\n\n", 
                  d$pn, d$pe, d$pn, d$pt, abs(d$pe-d$pt)/d$pt*100))
    } else {
      cat(sprintf("Оценка α: %.4f | Истинное α: %.4f\n", d$pe[1], d$pt[1]))
      cat(sprintf("Оценка β: %.4f | Истинное β: %.4f\n\n", d$pe[2], d$pt[2]))
    }
    cat("═══════════════════════════════════════════════════════\n")
    cat("  КРИТЕРИЙ ХИ-КВАДРАТ\n")
    cat("═══════════════════════════════════════════════════════\n")
    cat(sprintf("χ² = %.4f | df = %d | p-value = %.4f\n", d$cs, d$df, d$pv))
    cat(sprintf("Гипотеза %s (α = 0.05)\n", ifelse(d$pv > 0.05, "ПРИНИМАЕТСЯ ✓", "ОТВЕРГАЕТСЯ ✗")))
  })
  
  output$p1 <- renderPlot({
    d <- data()
    if(d$tp == "d") {
      fd <- as.data.frame(table(d$s)); fd$Var1 <- as.numeric(as.character(fd$Var1))
      fd$freq <- fd$Freq / input$N
      if(input$dist == "Биномиальное") fd$th <- dbinom(fd$Var1, input$n_b, input$p_b)
      else fd$th <- dgeom(fd$Var1, input$p_g)
      ggplot(fd, aes(x = Var1)) +
        geom_line(aes(y = freq, color = "Эмпир."), size = 1.5) +
        geom_point(aes(y = freq, color = "Эмпир."), size = 3) +
        geom_line(aes(y = th, color = "Теор."), size = 1.5, linetype = "dashed") +
        geom_point(aes(y = th, color = "Теор."), size = 2.5) +
        scale_color_manual(values = c("Эмпир." = "#E63946", "Теор." = "#457B9D")) +
        labs(title = "Полигон частот", x = "Значение", y = "Вероятность", color = "") +
        theme_minimal(base_size = 13) + theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      p <- ggplot(data.frame(x = d$s), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#A8DADC", color = "white", alpha = 0.7) +
        stat_density(aes(color = "Оценка"), geom = "line", size = 1.5) +
        labs(title = "Гистограмма и плотность", x = "Значение", y = "Плотность", color = "") +
        theme_minimal(base_size = 13) + theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
      if(input$dist == "Экспоненциальное") {
        p + stat_function(fun = dexp, args = list(rate = input$lam), aes(color = "Теор."), size = 1.5) +
          scale_color_manual(values = c("Теор." = "#E76F51", "Оценка" = "#264653"))
      } else {
        p + stat_function(fun = dgamma, args = list(shape = input$alp, rate = input$bet), aes(color = "Теор."), size = 1.5) +
          scale_color_manual(values = c("Теор." = "#457B9D", "Оценка" = "#1D3557"))
      }
    }
  })
  
  output$p2 <- renderPlot({
    d <- data()
    if(d$tp == "d") {
      ec <- ecdf(d$s); xr <- seq(min(d$s)-1, min(max(d$s)+1, 25), by = 0.1)
      df <- data.frame(x = xr, emp = ec(xr))
      if(input$dist == "Биномиальное") df$th <- pbinom(xr, input$n_b, input$p_b)
      else df$th <- pgeom(xr, input$p_g)
      ggplot(df) + geom_step(aes(x = x, y = emp, color = "Эмпир."), size = 1.5) +
        geom_line(aes(x = x, y = th, color = "Теор."), size = 1.5, linetype = "dashed") +
        scale_color_manual(values = c("Эмпир." = "#E63946", "Теор." = "#457B9D")) +
        labs(title = "Функция распределения", x = "Значение", y = "F(x)", color = "") + ylim(0, 1) +
        theme_minimal(base_size = 13) + theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      if(input$dist == "Экспоненциальное") tq <- qexp(ppoints(input$N), rate = d$pe)
      else tq <- qgamma(ppoints(input$N), shape = d$pe[1], rate = d$pe[2])
      ggplot(data.frame(th = tq, sm = sort(d$s)), aes(x = th, y = sm)) +
        geom_point(color = "#A8DADC", size = 3, alpha = 0.6) +
        geom_abline(intercept = 0, slope = 1, color = "#E76F51", size = 1.5, linetype = "dashed") +
        labs(title = "Q-Q график", x = "Теоретические квантили", y = "Выборочные квантили") +
        theme_minimal(base_size = 13) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
}

shinyApp(ui = ui, server = server)