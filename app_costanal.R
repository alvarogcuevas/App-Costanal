library(shiny)
library(ggplot2)
library(plotly)

# The code has to have three main parts: 
#   -UI (how the user sees the app)
#   -server(the logic statements R needs to produce the output)
#   -call to the function shinyApp() (to launch the app)

############################## UI 



ui <- fluidPage(
  
  # h*() makes titles, hr() is a horizontal row
  
  h1("Cost analysis for course development"),
  
  hr(),
  
  # The selected layout is fluidRow. 
  # We declare three different columns using the column() function.
  # The first argument of column() is the width of the column. All the widths add up to 12.
  # FIRST COLUMN: *Input() are the different widgets for the user to enter infromation 
  # SECOND COLUMN: *Output() is to show the final output, in this case a plotly object is shown using plotlyOutput()
  
  fluidRow(
    column(3,
           h3("Input"),
           sliderInput("S", 
                       label = "Number of students per class [S]:",
                       min = 0, max = 100, value = c(10,60)),
           br(),
           numericInput("FF",
                        label = "Fixed costs for development of a course [F]:",
                        value=113569),
           br(),
           numericInput("VV",
                        label = "Variable cost per class [V]:",
                        value=1505),
           br(),
           numericInput("N",
                        label = "Number of classes per course [N]:",
                        value=12),
           br(),
           numericInput("C",
                        label="Number of courses [C]:",
                        value=5),
           br(),
           numericInput("profit",
                        label = "Profit:",
                        value=0)
           ),
    
    column(6,
           h3("Cost analysis results"),
           hr(),
           h4("Price per class per student [SPCL]"),
           plotlyOutput("heat_map_pqs_profit"),
           hr(),
           h4("Price per course per student [SPC]"),
           plotlyOutput("heat_map_pqsp_profit")
           ),
    
    column(3,
           h3("Single case scenario"),
           numericInput("NN",
                        label="Number of classes per course [N]:",
                        value=12),
           br(),
           numericInput("CC",
                        label="Number of courses [C]:",
                        value=5),
           br(),
           numericInput("SS",
                        label="Number of students per class [S]:",
                        value=20),
           br(),
           span(textOutput("single_case"),style="color:red")
           )
  )
)


#################################### SERVER 

# server is always a function of two list-type arguments: input and output

server <- function(input, output) {
  
  # Three things will be produced (two plotly objects and some text), and for that we use render*({R CODE})
  
  output$heat_map_pqs_profit<-renderPlotly({
    
    # Q is the range of total number of classes per program (ranges from N (1 course) to N*C (C courses))
    # S is the number of of students per class

    Q<-input$N:(input$N*input$C)
    S<-input$S[1]:input$S[2]
    grid<-expand.grid(Q,S)
    
    # PQ is the fixed cost per class 

    PQ<-(input$FF+input$VV*grid[,1])/grid[,1]
    
    # SPCL is the price per class per student
    # df includes every (Q,S) from the grid created above, while df_single includes only the specific case entered in the rightmost pannel

    df <- data.frame(
      Q = grid[, 1],
      S = grid[, 2],
      PQ = PQ,
      SPCL = PQ / grid[, 2] + input$profit
    )

    df_single <- data.frame(
      Q = input$NN * input$CC,
      S = input$SS,
      SPCL = ((input$FF + input$VV * input$NN * input$CC) /
                (input$NN * input$CC)) / input$SS
    )
    
    k<-ggplot(df)+
      geom_raster(aes(x=Q,y=S,fill=SPCL))+ #heat map
      scale_fill_gradientn(colours = topo.colors(10))+ #change the heat map's default color palette
      xlab("Number of classes per program [N*C]")+
      ylab("Number of students per class [S]")+
      geom_point(data=df_single,aes(x=Q,y=S,z=SPCL),colour="red") #ingle red point entered in the rightmost pannel

    kk<-ggplotly(k)
    kk
  })

  output$heat_map_pqsp_profit<-renderPlotly({

    Q<-input$N:(input$N*input$C)
    S<-input$S[1]:input$S[2]
    grid<-expand.grid(Q,S)

    PQ<-(input$FF+input$VV*grid[,1])/grid[,1]

    # PQSP (same thing as SPC) is the price per course per student
    
    df <- data.frame(
      Q = grid[, 1],
      S = grid[, 2],
      PQ = PQ,
      SPC = (PQ / grid[, 2]) * input$N + input$profit
    )
    
    df_single <-
      data.frame(
        Q = input$NN * input$CC,
        S = input$SS,
        SPC = (input$FF + input$VV * input$NN * input$CC) / (input$SS * input$CC)
      )
    
    k <- ggplot(df) + 
      geom_raster(aes(x = Q, y = S, fill = SPC)) + 
      scale_fill_gradientn(colours = topo.colors(10)) +
      xlab("Number of classes per program [N*C]") + 
      ylab("Number of students per class [S]") +
      geom_point(data = df_single, aes(x = Q, y = S, z = SPC), colour = "red")
    
    kk<-ggplotly(k)
    kk
  })
  
  #Generating the red text displayed in the rightmost pannel, which represents a single case scenario
  
  output$single_case<-renderText({
    paste("The red dot represents SPCL=",round((input$FF+input$VV*input$NN*input$CC)/(input$NN*input$CC*input$SS),digits=2),"and SPC=",round((input$FF+input$VV*input$NN*input$CC)/(input$SS*input$CC),digits=2))
  })
  
}



############################## CALL SHINYAPP TO RENDER APP ############################################

shinyApp(ui = ui, server = server)

############################# PUBLISH APP IN THE SHINY APP ############################################

# library(rsconnect)
# rsconnect::deployApp('C:\\Users\\Stats\\Desktop\\Alvaro\\App-Costanal')
