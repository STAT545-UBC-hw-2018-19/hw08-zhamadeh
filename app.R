library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(rsconnect)

#tidy up dataset and filter for 4 miRNAs 
cancer_set <- read.csv("GSE58606_data.csv") %>%
  select("X42490...hsa.miR.505.5p","X30787...hsa.miR.125b.5p", "X147506...hsa.miR.21.5p", "X13147...hsa.miR.96.5p") %>%
  setnames(c("miRNA_505_5p","miRNA_125b_5p","miRNA_21_5p","miRNA_96_5p")) %>%
  gather("miRNA", "value")

#User interface
ui <- fluidPage(
  titlePanel("MicroRNA expression levels in breast cancer tissue samples"),
  sidebarLayout(
    sidebarPanel(
       checkboxGroupInput("typeInput", "miRNA",
                  choices = c("miRNA_505_5p","miRNA_125b_5p","miRNA_21_5p","miRNA_96_5p"),
                  selected = "miRNA_505_5p"),
       tags$br(),
       img(src = "image.png",width = 260),
       tags$br(),
       tags$b("The role of microRNAs in regulating cellular gene expression"),
       tags$br(),
       tags$br(),
       "These 4 microRNAs have been implicated in breast cancer and proposed as possible diagnostic biomarkers"
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput("coolplot")),
        tabPanel("Table",tableOutput("results")))
    )
  )
)

#server function
server <- function(input, output) {

  #filter dataset reactively based on user input
  filtered <- reactive({
    cancer_set %>%
      filter(cancer_set$miRNA == input$typeInput
      )
  })
  #plot out filtered datset in kernal density, add some labels
  output$coolplot <- renderPlot({
    ggplot(filtered(), aes(value)) +
      geom_density(aes(line=miRNA, colour=miRNA)) +
      labs(title = "miRNA Expression Level Distribution",y="Density", x="miRNA expression level") +
      theme_classic()
  })
  #this method of grouping by and indexing in order to re-spread the data out to be printed in a table 
  #was extrapolated from a tidyr issue on Github found here: https://github.com/tidyverse/tidyr/issues/426
  output$results <- renderTable({
    filtered() %>%
      group_by_at(vars(-value)) %>%  # group by everything other than the value column. 
      mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
      spread(key=miRNA, value=value) %>%    # spread
      select(-row_id)
  })
}

shinyApp(ui = ui, server = server)
