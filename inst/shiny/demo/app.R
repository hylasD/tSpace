#initialize
require(shiny)
require(purrr)
require(dplyr)
require(plotly)
require(data.table)

#example data
data(tcell)#fread('data/manuscript_T1_shiny.csv', sep = ',', stringsAsFactors = T, data.table = F)#data(iris)

#make some factors
#easier to let ggplot2 control plotting (color, fill) based on type
# data(mtcars)
# uvals<-sapply(mtcars,function(x){length(unique(x))})
# mtcars<-map_if(mtcars,uvals<4,as.factor) %>%
#   as.data.frame()


#plotting theme for ggplot2
# .theme<- theme(
#   axis.line = element_line(colour = 'gray', size = .75),
#   panel.background = element_blank(),
#   plot.background = element_blank()
# )


# UI for app
ui<-(pageWithSidebar(


  # title
  headerPanel(title=div(img(src = 'logo.tif', width = 75, height=75), "tSpace Explorer")),
  #headerPanel(img(src = 'logo.tif', width = 75, height=75), h1("tSpace Explorer")),

  #input
  sidebarPanel
  (
    # Input: Select a file ----
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Input: Checkbox if file has header ----
    # checkboxInput("header", "Header", TRUE),

    # Input: Select separator ----
    # radioButtons("sep", "Separator",
    #              choices = c(Semicolon = ";",
    #                          Comma = ",",
    #                          Tab = "\t"),
    #              selected = ","),
    # Horizontal line ----
    tags$hr(),


    # Input: Select what to display
    selectInput("dataset","Data:",
                choices =list(tcell = "tcell", #mtcars = "mtcars",
                              uploaded_file = "inFile"), selected=NULL),
    selectInput("plot.type","Plot Type:",
                list(box = "box", scatter2d = "scatter2d", scatter3d = "scatter3d")#, density = "density", bar = "bar")
    ),
    helpText("For box plots choose",
             "one variable and group."),

    selectInput("variable1","Variable1:", choices = NULL),
    selectInput("group","Group:", choices = NULL),
    helpText("Grouping variable is used for coloring"),
    selectInput("color1", "Colors", choices = list("Rainbow", "Violet_orange", "Categorical", "Matlab")),
    checkboxInput("show.points", "Show individual points", FALSE),
    tags$hr(),
    helpText("For scatter plots you need additonal variables",
             "you can choose variable 2 for y-axis and variable 3",
             "for color (2D) or z-axis (3D). In 3D Group is used",
             "for color."),
    selectInput("variable2","Variable2:", choices = NULL),
    selectInput("variable3","Variable3:", choices = NULL),
    helpText("Size and Transparency are used only for 2D & 3D."),
    sliderInput("size", "Size", 1, 10, value = 2),
    sliderInput("alpha", "Transparency", 0.1, 1, value = 0.7),
    tags$hr(),
    actionButton("exit", "Exit")
  ),

  # output
  mainPanel(
    h3(textOutput("caption")),
    #h3(htmlOutput("caption")),
    #plotlyOutput("plot")
    uiOutput("plot") # depends on input
  )
))


# shiny server side code for each call
server<-(function(input, output, session){
  options(shiny.maxRequestSize=1200*1024^2)
  #update group and
  #variables based on the data
  observe({
    #browser()
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "variable1", choices = var.opts)
    updateSelectInput(session, "variable2", choices = var.opts)
    updateSelectInput(session, "variable3", choices = var.opts)
    updateSelectInput(session, "group", choices = var.opts)
  })

  output$caption<-renderText({
    switch(input$plot.type,
           "box" = "Box plot",
           "scatter2d" 	= 	"Scatter",
           "scatter3d" =	"3D scatter")#,
    # "density" 	=	"Density plot",
    # "bar" 		=	"Bar graph")
  })

  # temp.plot <- reactive({
  #   updateTextInput(session, "width", value = input$width)
  #   updateTextInput(session, "height", value = input$height)
  # })

  output$plot <- renderUI({
    plotly::plotlyOutput("p", width = "1000", height = "700")#("p")
  })

  #get data object
  get_data<-reactive({

    if(!exists(input$dataset)) return() # if no upload

    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()

    obj<-list(data=get(input$dataset),
              variable1=input$variable1,
              variable2=input$variable2,
              variable3=input$variable3,
              group=input$group
    )

    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$variable1, obj$variable2, obj$variable3, obj$group) %in% colnames(obj$data))
    }

    if(check(obj)) return()


    obj

  })

  color.palette <- list("Categorical" = c("gray80", "#f97075", "#88fcd1", "#fdcc00", "#ea8200", "#a60033", "#a6ffff", "#bf74ff", "#663e90",
                                          "#36c7ff", "#9c9c9c", "#ff78f9", "#ffaffa", "#ee00a4", "#d79eff", "#ffaad7"),
                        "Matlab" = as.character(colorRamps::matlab.like2(20)),
                        "Violet_orange" = c('#c02e9c','#47acb1', '#ffcd34', 'orange'),
                        "Rainbow" = c("#750787", "#004dff", "#00bd38", "#ffed00", "#ff8c00", "#f40303"))

  color.pal <- reactive({
    switch (input$color1,
            "Categorical" = color.palette[[1]],
            "Matlab" = color.palette[[2]],
            "Violet_orange" = color.palette[[3]],
            "Rainbow" = color.palette[[4]]
    )
  })


  #plotting function using ggplot2
  output$p <- plotly::renderPlotly({

    plot.obj<-get_data()

    #conditions for plotting
    if(is.null(plot.obj)) return()

    #make sure variable and group have loaded
    if(plot.obj$variable1 == "" | plot.obj$variable2 == "" | plot.obj$variable3 == "" | plot.obj$group =="") return()

    #plot types
    plot.type<-switch(input$plot.type,
                      "box" = "Box plot",
                      "scatter2d" 	= "scatter2d",
                      "scatter3d" =	"scatter3d"
    )


    if(input$plot.type=="scatter2d")	{		#control for 1D or 2D graphs
      # p<- plot_ly(plot.obj$data,
      #             x = plot.obj$data[,plot.obj$variable1],
      #             y = plot.obj$data[,plot.obj$variable2],
      #             color = plot.obj$data[,plot.obj$group],
      #             colors = color.pal())

      p <- subplot(
        plot_ly(plot.obj$data, x = plot.obj$data[,plot.obj$variable1], type = "histogram"),
        plotly_empty(),
        plot_ly(plot.obj$data,
                x = plot.obj$data[,plot.obj$variable1],
                y = plot.obj$data[,plot.obj$variable2],
                color = plot.obj$data[,plot.obj$variable3],
                colors = color.pal(),
                size=I(input$size),
                alpha = I(input$alpha)),
        plot_ly(plot.obj$data, y = plot.obj$data[,plot.obj$variable2], type = "histogram"),
        nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
        shareX = TRUE, shareY = TRUE, titleX = T, titleY = T
      )
      #p <- layout(p, showlegend = FALSE)



    }

    if(input$plot.type=="box")	{		#control for 1D or 2D graphs
      p<- plot_ly(plot.obj$data,
                  x = plot.obj$data[,plot.obj$group],
                  y = plot.obj$data[,plot.obj$variable1],
                  color = plot.obj$data[,plot.obj$group],
                  colors = color.pal(),
                  type = 'box') #%>%
        # add_trace(showlegend = F,
        #           #split = plot.obj$data[,plot.obj$group],
        #           box = list(
        #             visible = T
        #           ),
        #           meanline = list(
        #             visible = T
        #           )
        # )

      if(input$show.points==TRUE){
        p <- add_trace(p,
                       y = plot.obj$data[,plot.obj$variable1],
                       # split = plot.obj$data[,plot.obj$group],
                       jitter = 0.3, pointpos = -1.8, #boxpoints = 'all',
                       boxpoints = "all", #jitter = 0.3,
                       #pointpos = -1.8,
                       showlegend = F,#,
                       #jitter = 0.75,
                       box = list(
                         visible = FALSE
                       )
        )
      }
    }

    if(input$plot.type=="scatter3d")	{		#control for 1D or 2D graphs
      scene = list(aspectmode = 'manual', aspectratio = list(x=1, y=1, z=1), camera = list(eye = list(x = 1.05, y = -1.7, z = 0.8)))
      p<- plot_ly(plot.obj$data,
                  x = plot.obj$data[,plot.obj$variable1],
                  y = plot.obj$data[,plot.obj$variable2],
                  z = plot.obj$data[,plot.obj$variable3],
                  color = plot.obj$data[,plot.obj$group],
                  colors = color.pal(),
                  size=I(input$size),
                  alpha = I(input$alpha),
                  type = "scatter3d"
      ) %>% layout(paper_bgcolor = 'transparent', scene = scene)
    }
    p
  })

  # set uploaded file
  upload_data<-reactive({

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    #could also store in a reactiveValues
    fread(inFile$datapath,
          header = T,
          sep = ",", stringsAsFactors = T, data.table = F)
  })

  observeEvent(input$file1,{
    inFile<<- upload_data()
  })

  observe({
    if(input$exit > 0){
      stopApp("Thank you for using tSpace")
    }
  })


})


# Create Shiny app ----
shinyApp(ui, server)
