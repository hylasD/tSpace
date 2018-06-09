#initialize
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)

#intestine <- read.csv("")
#tcells <- read.csv("")
#bcells <- read.csv("")


#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)


# UI for app
ui<-(pageWithSidebar(
  # title
  headerPanel("Select Options"),

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
    checkboxInput("header", "Header", TRUE),

    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Semicolon = ";",
                             Comma = ",",
                             Tab = "\t"),
                 selected = ","),
    # Horizontal line ----
    tags$hr(),


    # Input: Select what to display
    selectInput("dataset","Data:",
                choices =list(uploaded_file = "inFile"#,
                              #intestine = "intestine",
                              #tcells = "tcells",
                              #bcell = "bcells"
                              ), selected=NULL),
    selectInput("variable","Variable:", choices = NULL),
    selectInput("variable1","Variable1:", choices = NULL),
    selectInput("variable2","Variable2:", choices = NULL),
    selectInput("group","Group:", choices = NULL),
    selectInput("plot.type","Plot Type:",
                list(plotly3D = 'plotly3D',boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
    ),
    checkboxInput("show.points", "show points", TRUE)
  ),

  # output
  mainPanel(
    h3(textOutput("caption")),
    #h3(htmlOutput("caption")),
    #uiOutput("plot"), # depends on input
    conditionalPanel("output.plot",
                     plotOutput("plot")
    ),
    conditionalPanel("output.plotly",
                     plotlyOutput("plotly")
    )
  )
))


# shiny server side code for each call
server<-(function(input, output, session){

  #update group and
  #variables based on the data
  observe({
    #browser()
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "variable", choices = var.opts)
    updateSelectInput(session, "variable1", choices = var.opts)
    updateSelectInput(session, "variable2", choices = var.opts)
    updateSelectInput(session, "group", choices = var.opts)
  })

  output$caption<-renderText({
    switch(input$plot.type,
           "plotly3D" = "3D scatter",
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })




  #get data object
  get_data<-reactive({

    if(!exists(input$dataset)) return() # if no upload

    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()

    obj<-list(data=get(input$dataset),
              variable=input$variable,
              group=input$group
    )

    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$variable, obj$variable1, obj$variable3, obj$group) %in% colnames(obj$data))
    }

    if(check(obj)) return()


    obj

  })


  output$plot <- renderUI({
    plotOutput("plot")
  })

  output$plotly <- renderUI({
    plotOutput("plotly")
  })

    output$plot <- renderPlot({

    plot.obj<-get_data()

    #conditions for plotting
    if(is.null(plot.obj)) return()

    #make sure variable and group have loaded
    if(plot.obj$variable == "" | plot.obj$group =="") return()

    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge"),
                      req(FALSE)

    )


    if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$group,
                  y 		= plot.obj$variable,
                  fill 	= plot.obj$group # let type determine plotting
                )
      ) + plot.type

      if(input$show.points==TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }

    } else {

      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable,
                  fill 	= plot.obj$group,
                  group 	= plot.obj$group
                  #color 	= as.factor(plot.obj$group)
                )
      ) + plot.type
    }

    p<-p+labs(
      fill 	= input$group,
      x 		= "",
      y 		= input$variable
    )  +
      .theme
    print(p)

  })


  output$plotly <- renderPlotly({
    plot.obj<-get_data()

    #conditions for plotting
    if(is.null(plot.obj)) return()

    #make sure variable and group have loaded
    if(plot.obj$variable == "" | plot.obj$variable1 == "" | plot.obj$variable2 == "" | plot.obj$group =="") return()


    plot.type <- switch(input$plot_type,
                        "violin" = "violin", #ua_views_by_area(input$dates[1],input$dates[2]),
                        "plotly3D" = "scatter3d", #ua_age_access(input$dates[1],input$dates[2]),
                        req(FALSE)
    )

    if(input$plot.type=="scatter3d")	{		#control for 1D or 2D graphs

      p <- plot_ly(plot.obj$data, x = plot.obj$variable, y = plot.obj$variable1, z = plot.obj$variable2, color = plot.obj$group,
                   #colors = colorS,
                   size=I(3),
                   type = plot.type) %>% #, text = ~paste("Pop: ", bm$Cell, "<br>Sam: ", bm$PC25, "<br>Index: ", bm$PC10) ) %>%
        # bm[,input$dot], sizes = c(45,5)
        #as.character(c('gray35', '#4776ff')),
        #add_markers() %>%
        #1.5, 4 size span
        layout(paper_bgcolor='transparent') #, scene = scene)

      # if(input$show.points==TRUE)
      # {
      #   p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      # }

    }

    if(input$plot.type=="violin")	{		#control for 1D or 2D graphs
      p<- plot_ly(plot.obj$data,
        x = as.factor(plot.obj$variable),
        y = plot.obj$variable1,
        #split = ~day,
        type = plot.type,
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) #%>%
        # layout(
        #   xaxis = list(
        #     title = "Day"
        #   ),
        #   yaxis = list(
        #     title = "Total Bill",
        #     zeroline = F
        #   )
        # )
        #

    }

    print(p)
  })
  #plotting function using ggplot2
  # output$p <- renderPlot({
  #
  #   plot.obj<-get_data()
  #
  #   #conditions for plotting
  #   if(is.null(plot.obj)) return()
  #
  #   #make sure variable and group have loaded
  #   if(plot.obj$variable == "" | plot.obj$variable1 == "" | plot.obj$variable2 == "" | plot.obj$group =="") return()
  #
  #   #plot types
  #   plot.type<-switch(input$plot.type,
  #                     "plotly3D" = 'scatter3d',
  #                     "boxplot" 	= geom_boxplot(),
  #                     "histogram" =	geom_histogram(alpha=0.5,position="identity"),
  #                     "density" 	=	geom_density(alpha=.75),
  #                     "bar" 		=	geom_bar(position="dodge")
  #
  #   )
  #
  #
  #   if(input$plot.type=="plotly3D")	{		#control for 1D or 2D graphs
  #     p<-ggplot(plot.obj$data,
  #               aes_string(
  #                 x 		= plot.obj$group,
  #                 y 		= plot.obj$variable,
  #                 fill 	= plot.obj$group # let type determine plotting
  #               )
  #     ) + plot.type
  #
  #     if(input$show.points==TRUE)
  #     {
  #       p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
  #     }
  #
  #   }
  #
  #   if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs
  #     p<-ggplot(plot.obj$data,
  #               aes_string(
  #                 x 		= plot.obj$group,
  #                 y 		= plot.obj$variable,
  #                 fill 	= plot.obj$group # let type determine plotting
  #               )
  #     ) + plot.type
  #
  #     if(input$show.points==TRUE)
  #     {
  #       p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
  #     }
  #
  #   } else {
  #
  #     p<-ggplot(plot.obj$data,
  #               aes_string(
  #                 x 		= plot.obj$variable,
  #                 fill 	= plot.obj$group,
  #                 group 	= plot.obj$group
  #                 #color 	= as.factor(plot.obj$group)
  #               )
  #     ) + plot.type
  #   }
  #
  #   p<-p+labs(
  #     fill 	= input$group,
  #     x 		= "",
  #     y 		= input$variable
  #   )  +
  #     .theme
  #   print(p)
 # })

  # set uploaded file
  upload_data<-reactive({

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    #could also store in a reactiveValues
    read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
  })

  observeEvent(input$file1,{
    inFile<<-upload_data()
  })


})


# Create Shiny app ----
shinyApp(ui, server)
