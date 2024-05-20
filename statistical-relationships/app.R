#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(tidyverse)
set.seed(123)
pop <- sample(1:100, 100000, replace = T)

dnorm_limit <- function(x, htype, zscore) {
  y <- dnorm(x)
  if(htype==1){
    y[x>= -abs(zscore)&x<=abs(zscore)] <- NA
  }else if(htype==2){
    y[x<=zscore] <- NA
  }else{
    y[x>=zscore] <- NA
  }
  return(y)
}
pval_calc <- function(htype, zscore){
  if(htype==1){
    return(round(2*(1-pnorm(zscore)),3))
  } else if(htype==2){
    return(round((1-pnorm(zscore)), 3))
  } else{
    return(round(pnorm(zscore), 3))
  }
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statistical Relationships Among Data"),
    h1("Correlation Coefficients"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("varx", 
                        "Variance of X:", 
                        min = 0, 
                        max = 5, 
                        value = 1, 
                        step = 0.1), 
            sliderInput("vary", 
                        "Variance of Y:", 
                        min = 0, 
                        max = 5, 
                        value = 1, 
                        step = 0.1),
            sliderInput("cor", 
                        "Correlation between X and Y:", 
                        min = -1, 
                        max = 1, 
                        value = 0, 
                        step = 0.01), 
            sliderInput("nobs", 
                        "Number of Observations:", 
                        min = 10, 
                        max = 1000, 
                        value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
        
        
    ), 
    h1("Central Limit Theorem"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("sampsize", 
                    "Sample Size:", 
                    min = 1, 
                    max = 1000, 
                    step = 10,
                    value = 1), 
        sliderInput("nsamp", 
                    "Number of Samples:", 
                    min = 1, 
                    max = 5000, 
                    step = 10, 
                    value = 1), 
        checkboxInput("checkbox", 
                      label = "Standardized", 
                      value = F)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("cltPlot")
      )
      
      
    ), 
    h1("Hypothesis Testing"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("effsize", 
                    "Sample Mean:", 
                    min = -5, 
                    max = 5, 
                    step = 0.1,
                    value = 0), 
        sliderInput("ssize_ht", 
                    "Sample Size:", 
                    min = 1, 
                    max = 100, 
                    step = 5, 
                    value = 1), 
        sliderInput("pop_sd", 
                    "Population Standard Deviation", 
                    min = 1, 
                    max = 15, 
                    step = 1, 
                    value = 1), 
        radioButtons("ht_type", 
                      label = "Type of Hypothesis Test", 
                      choices = list(
                        "Two-Sided" = 1, 
                        "One-Sided, Greater" = 2, 
                        "One-Sided, Less" = 3
                      ), selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("htPlot")
      )
      
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      covxy <- input$cor*sqrt(input$varx*input$vary)
      Sigma <- matrix(c(input$varx, covxy, covxy, input$vary), nrow = 2)
      sim <- mvrnorm(input$nobs, mu = c(0, 0), Sigma = Sigma)
      correlation_res <- round(cor(sim[1,], sim[2,]), 2)
        # draw the histogram with the specified number of bins
        plot(sim[,1],sim[,2], pch = 16, 
             xlim = c(-6, 6), ylim = c(-6, 6),
             col = 'black', 
             xlab = 'X',ylab = "Y", 
             main = sprintf('Plot of X and Y with Correlation of %1.2f', 
                            input$cor))
    })
    output$cltPlot <- renderPlot({
      samp_means <- replicate(input$nsamp, 
                              mean(sample(pop, size = input$sampsize, replace = T)))
      if(input$checkbox){
        dat <- tibble(samp_means=(samp_means - mean(pop))/(sd(pop)/sqrt(input$sampsize)))
        ggplot(dat, aes(samp_means)) + 
          geom_histogram(aes(y=..density..)) + 
          stat_function(fun = "dnorm", col = "red")+
          labs(x = "Sample Means", y = "Density", 
               title = sprintf("Histogram of %i Sample Means with Samples of Size %i", 
                               input$nsamp, input$sampsize))
      } else{
        dat <- tibble(samp_means = samp_means)
        ggplot(dat, aes(samp_means)) + 
          geom_histogram(aes(y=..density..)) + 
          labs(x = "Sample Means", y = "Density", 
               title = sprintf("Histogram of %i Sample Means with Samples of Size %i", 
                               input$nsamp, input$sampsize))
      }

    })
    output$htPlot <- renderPlot({
      # effsize, ssize_ht, pop_sd, ht_type
      zscore <- input$effsize*sqrt(input$ssize_ht)/input$pop_sd
      ggplot(data.frame(x = c(-3, 3)), aes(x = x)) +
        stat_function(fun = dnorm) + 
        stat_function(fun=dnorm_limit, geom ="area", 
                      fill = "black", alpha = 0.1, 
                      args = list(htype = input$ht_type, 
                                  zscore = zscore))+
        geom_vline(xintercept = zscore, lty = "dashed", color = "blue") + 
        annotate(geom = "text", x = 3, y = 0.5, 
                 label = sprintf("P-Value: %1.3f", pval_calc(input$ht_type, 
                                                          zscore))) +
        labs(x = "X", y = "Density", 
             title = sprintf("Hypothesis Test with Sample Mean of %1.1f", input$effsize))
        
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
