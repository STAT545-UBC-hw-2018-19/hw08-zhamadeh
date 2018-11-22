library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
head(cancer_set)
str(cancer_set)


cancer_set <- read.csv("GSE58606_data.csv") %>%
  select("X42490...hsa.miR.505.5p","X30787...hsa.miR.125b.5p", "X147506...hsa.miR.21.5p", "X13147...hsa.miR.96.5p") %>%
  setnames(c("miRNA_505_5p","miRNA_125b_5p","miRNA_21_5p","miRNA_96_5p")) %>%
  gather("miRNA", "value")

ggplot(cancer_set) + geom_density(aes(value))



ui <- fluidPage(
  titlePanel("MicroRNA expression levels in breast cancer tissue samples"),
  sidebarLayout(
    sidebarPanel(
       checkboxGroupInput("typeInput", "miRNA",
                  choices = c("miRNA_505_5p","miRNA_125b_5p","miRNA_21_5p","miRNA_96_5p"),
                  selected = "miRNA_505_5p"),
       img(src = "image.png",width = 260) 
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {

  filtered <- reactive({
    cancer_set %>%
      filter(cancer_set$miRNA == input$typeInput
      )
  })
  
  output$coolplot <- renderPlot({
    ggplot(filtered(), aes(value)) +
      geom_density(aes(line=miRNA, colour=miRNA)) +
      labs(title = "miRNA Expression Level Distribution",y="Density", x="miRNA expression level") +
      theme_bw()
  })
  output$results <- renderTable({
    filtered() %>%
      group_by_at(vars(-value)) %>%  # group by everything other than the value column. 
      mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
      spread(key=miRNA, value=value) %>%    # spread
      select(-row_id)
  })
}

shinyApp(ui = ui, server = server)
