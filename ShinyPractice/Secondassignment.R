library(shiny)
library(tidyverse)
library(plotly)
library(DT)

#####Import Data

dat<-read_csv("cces_sample_coursera2.csv")
dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)

#####Make your app

ui <- navbarPage("My Application",
                 
  # Page 1
  tabPanel("Page 1",
           sidebarLayout(
             sidebarPanel(
                    sliderInput("ideology", 
                                "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                                min = 1, max = 5, value = 3, step = 1)
             ),
             mainPanel(
                    tabsetPanel(
                      tabPanel("Tab1", plotOutput("plot1")),
                      tabPanel("Tab2", plotOutput("plot2"))
                    )
             )
           )
  ),
  
  # Page 2
  tabPanel("Page 2",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("gender", "Select Gender",
                                  choices = list("1" = 1, "2" = 2),
                                  selected = 1)
             ),
             mainPanel(
               plotOutput("scatterPlot", height = "400px")
             )
           )
  ),

# Page 3
tabPanel("Page 3",
         sidebarLayout(
           sidebarPanel(
             selectInput("region", "Select Region",
                         choices = 1:4)
           ),
           mainPanel(
             dataTableOutput("table", height = 500)  # Specified height as per hint
           )
         )
)
)

server <- function(input, output) {
  # Page 1 outputs
  output$plot1 <- renderPlot({
    dat %>%
      filter(ideo5 == input$ideology) %>%
      ggplot(aes(x = factor(pid7))) +
      geom_bar(fill = "darkgray") +
      theme_minimal() +
      labs(x = "7 Point Party ID, 1=Very D, 7=Very R",
           y = "Count") +
      scale_y_continuous(limits = c(0, 125), expand = c(0, 0))
  })
  
  output$plot2 <- renderPlot({
    dat %>%
      filter(ideo5 == input$ideology) %>%
      ggplot(aes(x = factor(CC18_308a))) +
      geom_bar(fill = "darkgray") +
      theme_minimal() +
      labs(x = "Trump Support",
           y = "Count") +
      scale_y_continuous(limits = c(0, 125), expand = c(0, 0))
  })
 
  # Page 2 output
  output$scatterPlot <- renderPlot({
    dat %>%
      filter(gender %in% input$gender) %>%
      ggplot(aes(x = educ, y = pid7)) +
      geom_jitter(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE) +
      theme_minimal() + 
      scale_y_continuous(limits = c(1, 7)) +
      scale_x_continuous(limits = c(1, 6))
  })

  # Page 3 output
  output$table <- renderDataTable({
    dat %>%
      filter(region == input$region) %>%
      select(pid7, ideo5, newsint, gender, educ, CC18_308a, region)
  })
}

  
shinyApp(ui,server)