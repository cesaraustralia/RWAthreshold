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
  titlePanel(h3(""),
             windowTitle = "Russian wheat aphid threshold calculator"),
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(
      HTML("<b>Calculator inputs:</b>"), HTML("<br><br>"),
      sliderInput("controlcost","Cost of control ($/ha)",10, 60, 20),
      sliderInput("cerealvalue","Cereal market price ($/t)",50, 500, 300),
      sliderInput("yieldpot","Yield potential (t/ha)",1, 11, 3, step = 0.1),
      sliderInput("controldelay","Approximate time until GS50 (days)",10, 70, 30),
      sliderInput("RWAobs","Observed tillers with RWA (%)",0, 50, 0),
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
    
    # economic threshold
    ET = input$controlcost/input$cerealvalue/(a*input$yieldpot)
    # action threshold
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
      x = c(input$RWAobs, RWApred, IT*100, ET*100), 
      y = calc_loss(x/100),
      key = c("Observed RWA", "Predicted RWA at GS50 without action", 
              "Action threshold", "Economic injury level")) %>% 
      filter(y > 0) %>% 
      arrange(key)
    
    # economic injury region
    EIL = tibble(
      xmin = c(0, 0),
      xmax = c(max(40, RWAobspred$x), ET*100),
      ymin = c(0, 0),
      ymax = calc_loss(xmax/100),
      EIL = c("impact > control cost", "impact < control cost")
      ) 
    
    # plot
    pal = viridisLite::inferno(5, end = 0.8)
    labelsize = 6
    ggplot() + 
      geom_rect(data=EIL, aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax, fill=EIL)) +
      # RWA damage
      geom_abline(slope= a*input$yieldpot*input$cerealvalue/100, intercept=0, linetype=2) +
      annotate("text", 1.5*RWAobspred$x[2]+2, 1.5*RWAobspred$y[2],hjust=0,
                    label = "RWA impact line", size=labelsize) +
      geom_point(data=RWAobspred, aes(x, y, shape=key), size = 4) + 
      theme_bw() +
      scale_fill_manual(values = c("#c2ffa1", "#ff6047")) +
      scale_x_continuous(expand=c(0,0), breaks = seq(0,100,by=5)) +
      scale_y_continuous(expand=c(0,0)) +
      xlab("Tillers with Russian wheat aphid (%)") +
      ylab("Economic impact ($/ha)") + 
      theme(plot.title = element_text(face = "bold", size=20), 
            axis.title = element_text(size=16),
            axis.text = element_text(size=12), plot.margin = margin(1, 2, 1, 1, "cm"),
            legend.position = "bottom",  legend.direction="vertical")+
      scale_shape_manual(values = c(1, 2, 3, 0)) +
      guides(shape = guide_legend(""), fill = guide_legend(""))
      
  })
  
  output$table <- renderTable(vals$df)
  
})

##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)
