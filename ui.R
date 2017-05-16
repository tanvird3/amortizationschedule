library(shiny)
shinyUI(pageWithSidebar (
  headerPanel( "AMORTIZATION_V2.01"),
  sidebarPanel(width=3,
    numericInput("interest", label = "Enter Interest Rate (in % i.e. 10 if it is 10%)", value=as.numeric(10), min=0),
    numericInput("loan_amount", label = "Enter Total Amount of Loan Taken", value=as.numeric(500000), min=0),
    numericInput("freq", label = "Enter the Frequency of Payment", value=as.numeric(12), min=0),
    numericInput("grace", label = "Enter the No. of Grace Period", value=as.numeric(0), min=0, max=12),
    numericInput("number", label = "Total Length of Period (including Grace Period)", value=as.numeric(60), min=0),
    numericInput("type", label = "1=Beginning, 0=End", value=as.numeric(0), min=0, max=1),
    selectInput("compounding", label = "Frequency of Compounding in a Year", choices=c(1,2,3,4,6,12),selected=4),
    submitButton(text="Apply"),
    
    
    downloadButton('downloadData', 'Download The Amortization Schedule')
  ),
  mainPanel (
    h1("Amortization Schedule"),
    tabsetPanel(
      tabPanel("Amortization Table", tableOutput("A"), DT::dataTableOutput("P")),
      tabPanel("Plots",plotlyOutput("M"), plotlyOutput("NM"))
    ))
))