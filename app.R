# app.R
# Simple Shiny app to simulate "success" = (1-l)*H + l*L
# with social strata based structural luck
# Beginner-friendly.

library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("LUCK vs HARD WORK — Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("l", "Luck weight (l):", min = 0, max = 1, value = 0.05, step = 0.01),
      numericInput("n", "Sample size (n):", value = 100000, min = 1000, max = 1000000, step = 1000),
      numericInput("seed", "Random seed:", value = 22, min = 1),
      actionButton("run", "Run simulation"),
      helpText("Note: larger n->slower. Start with 100k; try 500k if your laptop is fast.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 verbatimTextOutput("summaryText"),
                 tableOutput("topComposition")
        ),
        tabPanel("Plots",
                 plotOutput("densityPlot", height = "300px"),
                 plotOutput("densityPlot.l", height = "300px"),
                 plotOutput("densityPlot.h", height = "300px"),
                 plotOutput("decilePlot", height = "300px"),
                 tableOutput("average_scores")
        ),
        tabPanel("Distributions Per decile",
                 paste("Hardwork score is in red and Luck score is in blue"),
                 plotOutput("Plot1", height = "200px"),
                 plotOutput("Plot2", height = "200px"),
                 plotOutput("Plot3", height = "200px"),
                 plotOutput("Plot4", height = "200px"),
                 plotOutput("Plot5", height = "200px"),
                 plotOutput("Plot6", height = "200px"),
                 plotOutput("Plot7", height = "200px"),
                 plotOutput("Plot8", height = "200px"),
                 plotOutput("Plot9", height = "200px"),
                 plotOutput("Plot10", height = "200px")),
        tabPanel("Raw (sample)", dataTableOutput("sampleTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive simulation runs when "Run simulation" is pressed
  sim <- eventReactive(input$run, {
    set.seed(input$seed)
    n <- as.integer(input$n)
    l <- input$l
    
    # 1. social strata assignment
    strata <- sample(c("Upper","Middle","Lower"), size = n, replace = TRUE, prob = c(0.2,0.5,0.3))
    # 2. structural luck S by strata
    S <- numeric(n)
    S[strata == "Upper"]  <- runif(sum(strata == "Upper"), min = 0.6, max = 1.0)
    S[strata == "Middle"] <- runif(sum(strata == "Middle"), min = 0.3, max = 0.7)
    S[strata == "Lower"]  <- runif(sum(strata == "Lower"), min = 0.0, max = 0.4)
    # 3. random luck epsilon
    eps <- runif(n)
    # 4. total luck L (you can change weights 0.7/0.3 in formula below)
    L <- 0.7 * S + 0.3 * eps
    # 5. hard work H
    H <- rbeta(n,0.6,3)
    # 6. success
    success <- (1 - l) * H + l * L
    
    df <- tibble(id = 1:n, strata = strata, S = S, eps = eps, L = L, H = H, success = success)
    # indicate whether hardwork > luck for each person
    df <- df %>% mutate(hw_dominant = ifelse(H > L, 1, 0))
    
    # rank and decile
    df <- df %>% arrange(desc(success)) %>%
      mutate(rank = row_number(),
             decile = ntile(desc(success), 10))  # 1 = top 10%
    
    df
  }, ignoreNULL = FALSE) # allow initial run when app opens if pressed
  
  # Summary text
  output$summaryText <- renderPrint({
    req(sim())
    df <- sim()
    l <- input$l
    cat("Simulation summary\n")
    cat("--------------\n")
    cat("Parameter l (luck weight):", l, "\n")
    cat("Sample size n:", nrow(df), "\n\n")
    
    top10 <- df %>% filter(decile == 1)
    cat("Top 10% (by success) — composition:\n")
    print(table(top10$strata) / nrow(top10))
    
    cat("\nOverall proportion where Hardwork > Luck (whole sample):",
        mean(df$hw_dominant), "\n")
    cat("Top 10% proportion where Hardwork > Luck:",
        mean(top10$hw_dominant), "\n")
  })
  
  # Table: composition of top 10% by strata (counts and %)
  output$topComposition <- renderTable({
    req(sim())
    df <- sim()
    top10 <- df %>% filter(decile == 1)
    comp <- top10 %>% group_by(strata) %>% summarise(count = n()) %>%
      mutate(percent = round(100 * count / sum(count), 2))
    comp
  })
  
  # Density plot of success
  output$densityPlot <- renderPlot({
    req(sim())
    df <- sim()
    ggplot(df, aes(x = success)) +
      geom_density() +
      ggtitle("Density of Success Score") +
      xlab("Success") + ylab("Density")
  })
  
  #average hardwork and luck score in each decile
  output$average_scores <- renderTable({
    req(sim())
    df <- sim()
    comp <- df %>% 
      group_by(decile) %>% 
      summarise(average.luck = round(mean(L),3), 
                average.hard.work = round(mean(H),3)
                )
    comp
  })
  
  # Density plot of luck
  output$densityPlot.l <- renderPlot({
    req(sim())
    df <- sim()
    ggplot(df, aes(x = L)) +
      geom_density() +
      ggtitle("Density of Luck Score") +
      xlab("Luck") + ylab("Density")
  })
  
  # Density plot of success
  output$densityPlot.h <- renderPlot({
    req(sim())
    df <- sim()
    ggplot(df, aes(x = H)) +
      geom_density() +
      ggtitle("Density of Hard Work Score") +
      xlab("Hard Work") + ylab("Density")
  })
  
  # Decile plot: proportion (hardwork>luck) across deciles
  output$decilePlot <- renderPlot({
    req(sim())
    df <- sim()
    dec <- df %>% group_by(decile) %>%
      summarise(prop_hw = mean(hw_dominant),
                count = n())
    ggplot(dec, aes(x = factor(decile), y = prop_hw)) +
      geom_col() +
      labs(x = "Decile (1 = top 10%)", y = "Proportion Hardwork > Luck",
           title = "Proportion with Hardwork > Luck by Decile")
  })

  
  
  # -------------------------
  # COMBINED DISTRIBUTION PLOTS (Luck + Hard Work)
  # -------------------------
  
  make_decile_plot <- function(df, decile_num) {
    dec <- df %>% filter(decile == decile_num)
    
    ggplot() +
      geom_density(data = dec, aes(x = L), 
                   color = "blue", fill = "blue", alpha = 0.3) +
      geom_density(data = dec, aes(x = H), 
                   color = "red", fill = "red", alpha = 0.3) +
      labs(
        title = paste("Decile", decile_num, ": Luck vs Hard Work Distribution"),
        x = "Score",
        y = "Density"
      ) +
      theme_minimal()
  }
  
  output$Plot1  <- renderPlot({ req(sim()); make_decile_plot(sim(), 1) })
  output$Plot2  <- renderPlot({ req(sim()); make_decile_plot(sim(), 2) })
  output$Plot3  <- renderPlot({ req(sim()); make_decile_plot(sim(), 3) })
  output$Plot4  <- renderPlot({ req(sim()); make_decile_plot(sim(), 4) })
  output$Plot5  <- renderPlot({ req(sim()); make_decile_plot(sim(), 5) })
  output$Plot6  <- renderPlot({ req(sim()); make_decile_plot(sim(), 6) })
  output$Plot7  <- renderPlot({ req(sim()); make_decile_plot(sim(), 7) })
  output$Plot8  <- renderPlot({ req(sim()); make_decile_plot(sim(), 8) })
  output$Plot9  <- renderPlot({ req(sim()); make_decile_plot(sim(), 9) })
  output$Plot10 <- renderPlot({ req(sim()); make_decile_plot(sim(), 10) })
  
  # Show a small sample of the raw data (first 1000 rows)
  output$sampleTable <- renderDataTable({
    req(sim())
    df <- sim()
    df %>% select(id, strata, S, eps, L, H, success, hw_dominant) %>% head(1000)
  }, options = list(pageLength = 10))
}

shinyApp(ui, server)
