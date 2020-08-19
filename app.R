# import libraries
library(shiny)
library(tidyverse)
library(Cairo) # better antialiasing on text
options(shiny.usecairo=T)
##################
# User Interface #
##################

#generic line initiating the UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel(HTML("Russian wheat aphid GS30 threshold calculator")),
  
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(
      sliderInput("controlcost","Cost of control ($/ha)",10, 40, 20),
      sliderInput("cerealvalue","Cereal market price ($/t)",200, 500, 300),
      sliderInput("yieldpot","Yield potential (t/ha)",1, 10, 3, step = 0.1),
      sliderInput("controldelay","Approximate time until GS50 (days)",10, 50, 30),
      sliderInput("RWAobs","Observed RWA tillers (%)",1, 100, 10),
      ),
    
    #beginning of main section
    mainPanel(
      tableOutput("table"),
      plotOutput("plot"))
  )
  
  #Close the UI definition
))

##########
# SERVER #
##########

#generic line initiating the SERVER 

server <- shinyServer(function(input, output) {
  a = 0.28 # proportion decrease in yield per proportion RWA tiller
  b = 0.021 # intrinsic growth rate of RWA tillers (1/d)
  
  vals <- reactiveValues(df = tibble(` `=c(
    "Calculated GS30 intervention threshold (percent tillers with RWA):",
    "Calculated loss without intervention ($/ha)",
    "Estimated cost of control ($/ha)",
    "Calculated benefit-cost ratio"), `  `=rep(0,4)))
  
  output$plot = renderPlot(width = 500,height = 500,{
    # make plot 
    nn=100
    d = tibble(Prop_tillers_w_RWA = seq(0, 0.4, length=nn)) %>%
      mutate(RWAyieldloss = a*Prop_tillers_w_RWA*input$yieldpot*input$cerealvalue) 
    
    # economic threshold
    ET = input$controlcost/input$cerealvalue/(a*input$yieldpot)
    IT = ET/exp(b*input$controldelay)
    
    # table output
    vals$df[1,2] = 100*IT                    # intervention threshold
    vals$df[2,2] = (input$RWAobs/100*exp(b*input$controldelay))*
      a*input$yieldpot*input$cerealvalue     # calc losses ($/ha)
    vals$df[3,2] = input$controlcost         # control cost ($/ha)
    vals$df[4,2] = vals$df[2,2]/vals$df[3,2] # benefit-cost
  
    # plot
    pal = viridisLite::inferno(5, end = 0.8)
    labelsize =6
    ggplot(d) + 
      # RWA damage
      geom_line(aes(100*Prop_tillers_w_RWA, RWAyieldloss), color = pal[4]) + 
      annotate("text", 100*d$Prop_tillers_w_RWA[nn]-6, d$RWAyieldloss[nn],
                    label = "RWA impact", color = pal[4], size=labelsize) +
  
      geom_vline(xintercept = 100*IT, alpha=0.5, color = pal[2]) +
      annotate("text", x=100*IT-1, y=mean(d$RWAyieldloss[(nn/2):nn]), label = "intervention threshold", 
                color = pal[2], angle = 90, size=labelsize) +
      # geom_vline(xintercept = 100*ET, alpha=0.5, color = pal[4]) +
      # geom_text(aes(100*ET-1, 10, label = "economic threshold"), color = pal[4], angle = 90) +
      # control cost
      geom_hline(yintercept = input$controlcost, alpha=0.5, color=pal[5]) +
      annotate("text", x=30, y=input$controlcost+2, label = "control cost", size=labelsize, color = pal[5]) +
      theme_bw() +
      xlab("Tillers with Russian wheat aphid (%)") +
      ylab("Economic impact ($/ha)") + 
      theme(plot.title = element_text(face = "bold", size=20), 
            axis.title = element_text(size=16),
            axis.text = element_text(size=12))
      
  })
  
  output$table <- renderTable(vals$df)
  
})

##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)
