library(BiocManager)
library(breastCancerNKI)
library(shiny)
library(Biobase)

source("http://www.bioconductor.org/biocLite.R")
biocLite("breastCancerNKI", suppressUpdates = TRUE)

cancer_set$miRNA_505_5p <- as.factor(cancer_set$miRNA_505_5p)

cancer_set <- read.csv("GSE58606_data.csv") %>%
  select("X42490...hsa.miR.505.5p","X30787...hsa.miR.125b.5p", "X147506...hsa.miR.21.5p", "X13147...hsa.miR.96.5p") %>%
  setnames(c("miRNA_505_5p","miRNA_125b_5p","miRNA_21_5p","miRNA_96_5p"))

ggplot(cancer_set) +
  geom_density(aes(miRNA_505_5p))+
  geom_density(aes(miRNA_125b_5p)) +
  geom_density(aes(miRNA_21_5p)) +
  geom_density(aes(miRNA_96_5p))

ui <- fluidPage(
  
  # Application title
  titlePanel("Interrogating the NKI breast cancer dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("thegene","Gene to Analyse",
                  choices=c("ESR1","ERBB2","PTEN"),
                  selected  = "ESR1")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("boxplot")
    )
  )
)


server <- function(input, output) {
  
  # If your data are stored in a csv or txt file, you could add the read.csv, read.delim commands here instead
  
  library(breastCancerNKI)
  
  data(nki)
  expression.values <- exprs(nki)
  features <- fData(nki)
  er.status <- pData(nki)$er
  
  output$boxplot <- renderPlot({
    
    gene <- input$thegene
    probe.id <- as.character(features$probe[match(gene, features$HUGO.gene.symbol)])
    
    values <- expression.values[probe.id,]
    boxplot(values ~ er.status)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
nki






