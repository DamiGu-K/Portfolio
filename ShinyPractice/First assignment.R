library(shiny)
library(tidyverse)
library(ggplot2)

#####Import Data

dat<-read_csv("cces_sample_coursera.csv")
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)

ui <- fluidPage(
  titlePanel("Political Ideology Survey"),
  
    fluidRow(
     column(12,
      sliderInput("ideology",
                  "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                  min = 1,
                  max = 5,
                  value = 3,
                  step = 1)
     )
    ),
    
    fluidRow(
      column(12,
      plotOutput("partyPlot", height = "400px")
    )
  )
)

  

server<-function(input,output){
  
  output$partyPlot <- renderPlot({
    # Filter data based on selected ideology
    dat %>%
      filter(ideo5 == input$ideology) %>%
      ggplot(aes(x = factor(pid7))) +
      geom_bar(fill = "darkgray") +
      theme_minimal() +
      labs(x = "7 Point Party ID, 1=Very D, 7=Very R",
           y = "Count") +
      # Fixed y-axis scale (adjust 125 to your preferred maximum)
      scale_y_continuous(limits = c(0, 125), expand = c(0, 0)) +
      theme(panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray90"))
  })
  
}

shinyApp(ui,server)
