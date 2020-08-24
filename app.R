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
  titlePanel(h3("Russian wheat aphid GS30 action threshold calculator"),
             windowTitle = "Russian wheat aphid threshold calculator"),
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(
      HTML("<b>Calculator inputs:</b>"), HTML("<br><br>"),
      sliderInput("controlcost","Cost of control ($/ha)",10, 40, 20),
      sliderInput("cerealvalue","Cereal market price ($/t)",200, 500, 300),
      sliderInput("yieldpot","Yield potential (t/ha)",1, 10, 3, step = 0.1),
      sliderInput("controldelay","Approximate time until GS50 (days)",10, 50, 30),
      sliderInput("RWAobs","Observed RWA tillers (%)",0, 100, 0),
      ),
    
    #beginning of main section
    mainPanel(
      tableOutput("table"),
      plotOutput("plot")
    )
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
  
  vals <- reactiveValues(df = tibble(`Calculator output values:`=c(
    "Action threshold (percent tillers with RWA):",
    "Yield loss without action based on current RWA (kg/ha):",
    "Cost of no action based on current RWA ($/ha):",
    "Benefit-cost ratio of action:"), `  `=rep(0,4)))
  
  output$plot = renderPlot(width = 400,height = 400,{
    # make plot 
    nn=100
    d = tibble(Prop_tillers_w_RWA = seq(0, 0.4, length=nn)) %>%
      mutate(RWAyieldloss = a*Prop_tillers_w_RWA*input$yieldpot*input$cerealvalue) 
    
    # economic threshold
    ET = input$controlcost/input$cerealvalue/(a*input$yieldpot)
    IT = ET/exp(b*input$controldelay)
    
    # function to calc losses from proportion TwRWA 
    calc_loss = function(TwRWA) {
      TwRWA*a*input$yieldpot*input$cerealvalue
    }
    
    # table output
    vals$df[1,2] = 100*IT                    # intervention threshold
    vals$df[2,2] = 1000*a*input$RWAobs/100*input$yieldpot  # calc losses (kg/ha)
    vals$df[3,2] = calc_loss(input$RWAobs*exp(b*input$controldelay)/100) # calc losses ($/ha)
    vals$df[4,2] = vals$df[3,2]/input$controlcost # benefit-cost
  
    # observed and predicted RWA tillers
    RWApred = input$RWAobs*exp(b*input$controldelay)
    RWAobspred = tibble(
      x = c(input$RWAobs, RWApred), 
      y = c(calc_loss(input$RWAobs/100), calc_loss(RWApred/100)), 
      key = c("observed RWA tillers (%)", "predicted RWA tillers at GS50 (%)"))
    
    # plot
    pal = viridisLite::inferno(5, end = 0.8)
    labelsize =6
    ggplot(d) + 
      # RWA damage
      geom_line(aes(100*Prop_tillers_w_RWA, RWAyieldloss), color = pal[4]) + 
      annotate("text", 100*d$Prop_tillers_w_RWA[nn]-6, d$RWAyieldloss[nn],
                    label = "RWA impact", color = pal[4], size=labelsize) +
  
      geom_vline(xintercept = 100*IT, alpha=0.5, color = pal[2]) +
      annotate("text", x=100*IT-1, y=mean(d$RWAyieldloss[(nn/2):nn]), label = "action threshold", 
                color = pal[2], angle = 90, size=labelsize) +
      # geom_vline(xintercept = 100*ET, alpha=0.5, color = pal[4]) +
      # geom_text(aes(100*ET-1, 10, label = "economic threshold"), color = pal[4], angle = 90) +
      # control cost
      geom_hline(yintercept = input$controlcost, alpha=0.5, color=pal[5]) +
      annotate("text", x=30, y=input$controlcost+3, label = "control cost ($/ha)", 
               size=labelsize, color = pal[5]) +
      geom_point(data=RWAobspred, aes(x, y, shape=key), size = 4) + 
      geom_line( data=RWAobspred, aes(x, y), linetype=2, size = 1.5) + 
      theme_bw() +
      scale_x_continuous(breaks = seq(0,100,by=5)) +
      xlab("Tillers with Russian wheat aphid (%)") +
      ylab("Economic impact ($/ha)") + 
      theme(plot.title = element_text(face = "bold", size=20), 
            axis.title = element_text(size=16),
            axis.text = element_text(size=12), 
            legend.position = "bottom",  legend.direction="vertical")+
      scale_shape_manual(values = c(1, 2)) +
      guides(shape = guide_legend(""))
      
  })
  
  output$table <- renderTable(vals$df)
  
})

##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)
