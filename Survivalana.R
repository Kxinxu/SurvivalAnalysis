library(shiny)
library(shinydashboard)
library(DT)
library(purrr)
library(dplyr)
library(tidyr)
library(readxl)
library(haven)
library(survival)
library(ggplot2)
library(ggsurvfit)
library(gridExtra)
library(knitr)
library(extrafont)
library(forestploter)
library(forestplot)
library(survminer)
library(coxphf)
library(officer)
library(rvg)
library(sortable)
library(svglite)
library(ggpp)
library(grid)
library(gridExtra)
library(patchwork)
library(gtable)
library(reactable)

ui <- dashboardPage(
  
  dashboardHeader(title = "Survival Analysis"), 
  dashboardSidebar(id="",
                   collapsed = T,
                   sidebarMenu(
                     menuItem("KM-PLOT", tabName = "KM"),
                     menuItem("FOREST-PLOT", tabName = "FR")
                   )),
  dashboardBody(
    tabItems(
      #KM Plot
      tabItem(tabName = "KM",
              fluidRow(
                #Plot Preview
                box(
                  title = "KM Plot", width = 7, solidHeader = T, status = "success",
                  fluidRow(
                    column(3,
                           numericInput("pwd","Width (inch)",value = 6,width = "80%")),
                    column(3,
                           numericInput("pht","Height (inch)",value = 4,width = "80%")),
                    column(3,
                           selectInput("dltype","Select file type",
                                       choices = c("WMF"="wmf",
                                                   "PNG"="png",
                                                   #"Powerpoint" = "pptx",
                                                   "PDF"="pdf"))),
                    column(3,
                           br(),
                           downloadButton("kmd","Download")),
                    column(12,
                           uiOutput("plotui"),
                           
                    )
                  )
                  
                ),
                #Plot Customization
                tabBox(width = 5,
                       tabPanel("Basic",
                                fluidRow(
                                  column(6,
                                         selectInput("filetype","Select the type of the file",choices = c("Excel file" = 1, "SAS file" = 2),selected = 2)
                                  ),
                                  column(6,
                                         fileInput("file1", label = "Please upload the Dataset")
                                         
                                  ),
                                  column(8,
                                         uiOutput("sas"),
                                         
                                         uiOutput("excel"),
                                         uiOutput("excelcensored")
                                  ),

                                  column(4,
                                         uiOutput("sbg"),
                                         uiOutput("sgvar"),
                                         uiOutput("sgval")
                                  ),
                                  column(12,
                                         uiOutput("order")
                                  ),
                                  column(8,
                                         uiOutput("level")
                                  ),
                                  column(12,
                                         br(),
                                         uiOutput("color"),
                                         actionButton("labn","Refresh")),

                                )
                       ),
                       tabPanel("Axis",
                                fluidRow(
                                  
                                  column(6,
                                         br(),
                                         textInput("y","Y-axis (Add a '\\n' where you need a line break)",value = "Survival Probability (%)", width = "100%"),
                                         textInput("x","X-axis (Add a '\\n' where you need a line break)",value = "Time", width = "100%"),
                                         actionButton("text",tags$b("Submit"))
                                  ),
                                  column(6,
                                         h5("Font size of the text on y-axis & x-axis"),
                                         actionButton("sizey_up",icon("plus"),width = "18%"),
                                         actionButton("sizey_down",icon("minus"),width = "18%"),
                                         numericInput("sizey",NULL,value = 12,width = "37%"),
                                         h5("Font size of the number on y-axis & x-axis"),
                                         actionButton("sizen_up",icon("plus"),width = "18%"),
                                         actionButton("sizen_down",icon("minus"),width = "18%"),
                                         numericInput("sizen",NULL,value = 9,width = "37%")),
                                  column(6,
                                         h5("Change the length of ticks on axises"),
                                         actionButton("tdn",icon("minus"),width = "18%"),
                                         actionButton("tup",icon("plus"),width = "18%")),
                                  column(12,
                                         br(),
                                         numericInput("interval","The range of interval a tick marks to placed",value = 3),
                                  )
                                )),
                       tabPanel("Legend",
                                fluidRow(
                                  column(12,
                                         p(
                                           checkboxInput("cls",tags$b("Uncheck to omit the legend"),value = T),
                                           radioButtons("lp","Adjust the position of the legend:",
                                                        choices = list("Place at right top" = 1,
                                                                       "Place at left bottom" = 2)),
                                           actionButton("left_btn",icon("arrow-left")),
                                           actionButton("up_btn",icon("arrow-up")),
                                           actionButton("down_btn",icon("arrow-down")),
                                           actionButton("right_btn",icon("arrow-right")),
                                           br(),
                                           br())
                                  ),
                                  column(6,
                                         h5(tags$b("Font size of the text in legend")),
                                         actionButton("size_up",icon("plus"),width = "18%"),
                                         actionButton("size_down",icon("minus"),width = "18%"),
                                         numericInput("fontsize",NULL,value = 8.5,width = "37%")
                                  )
                                  
                                )),
                       tabPanel("Summary Table",
                                fluidRow(
                                  column(6,
                                         br(),
                                         downloadButton("template",label = "Summary table template")
                                  ),
                                  column(12,
                                         br(),
                                         fileInput("file2", label = "Upload the summary table file")
                                  ),
                                  column(6,
                                         radioButtons("tp","Adjust the position of the table:",
                                                      choices = list("Place at right top" = 1,
                                                                     "Place at left bottom" = 2),
                                                      selected = 2),
                                         
                                         actionButton("left_s",icon("arrow-left")),
                                         actionButton("up_s",icon("arrow-up")),
                                         actionButton("down_s",icon("arrow-down")),
                                         actionButton("right_s",icon("arrow-right")),
                                         br(),
                                         br(),
                                         h5(tags$b("Font size of the text in summary table")),
                                         actionButton("sizes_up",icon("plus"),width = "18%"),
                                         actionButton("sizes_down",icon("minus"),width = "18%"),
                                         numericInput("sizes",NULL,value = 6,width = "37%")),
                                )
                       ),
                       tabPanel("Survival Rates",
                                fluidRow(
                                  column(12,
                                         numericInput("numq", "How many time points you would like to add?",value = NULL)
                                  ),
                                  column(6,
                                         uiOutput("quantile")
                                  ),
                                  column(6,
                                         uiOutput("deci")
                                  ),
                                  column(6,
                                         actionButton("q_a_a","Plot Quantile Annotation")
                                  )
                                )),
                       tabPanel("Additional",
                                fluidRow(
                                  column(6,
                                         checkboxInput("rtb",tags$b("Check for displaying risktable"),value = T),
                                         textInput("risklab","The label on the risktable",value = "Number at Risk")),
                                  column(6,
                                         h5("Font size of the number in risktable"),
                                         actionButton("sizer_up",icon("plus"),width = "18%"),
                                         actionButton("sizer_down",icon("minus"),width = "18%"),
                                         numericInput("sizer",NULL,value = 9.5,width = "37%")),
                                  column(12,
                                         h5("The line space of the risktable"),
                                         actionButton("sizert_up",icon("plus"),width = "18%"),
                                         actionButton("sizert_down",icon("minus"),width = "18%")),
                                  column(4,
                                         br(),
                                         h5("Size of top margin"),
                                         actionButton("sizet_down",icon("minus"),width = "28%"),
                                         actionButton("sizet_up",icon("plus"),width = "28%"),
                                         h5("Size of right margin"),
                                         actionButton("sizerm_down",icon("minus"),width = "28%"),
                                         actionButton("sizerm_up",icon("plus"),width = "28%"),
                                  ),
                                  
                                  column(4,
                                         br(),
                                         h5("Length of censor mark"),
                                         actionButton("cmdn",icon("minus"),width = "28%"),
                                         actionButton("cmup",icon("plus"),width = "28%"),
                                         h5("Thickness of censor mark"),
                                         actionButton("ctdn",icon("minus"),width = "28%"),
                                         actionButton("ctup",icon("plus"),width = "28%")
                                  ),
                                  column(4,
                                         br(),
                                         h5("Thickness of axis line"),
                                         actionButton("aldn",icon("minus"),width = "28%"),
                                         actionButton("alup",icon("plus"),width = "28%"),
                                         h5("Thickness of curve line"),
                                         actionButton("cldn",icon("minus"),width = "28%"),
                                         actionButton("clup",icon("plus"),width = "28%")
                                  ),
                                  column(12,
                                         br(),
                                         checkboxInput("cpb",tags$b("Check for plot border"),value = T),
                                         numericInput("brerd","The maximum break of x-axis",value = NULL)),

                                )
                       )
                )
                
              )
      ),
      #Forest Plot
      tabItem(tabName = "FR",
              tabBox(title = "",
                     width = 5,
                     id = "tabset1",
                     #Plot Customization
                     tabPanel("Basic",
                              fluidRow(
                                column(6,
                                       fileInput("FRfile0", "Upload the adtte file"),
                                       checkboxInput("adsl",tags$b("Information from adsl dataset needed")),
                                       uiOutput("adsl")
                                ),
                                column(6,
                                       selectInput("frpara","Select the parameter to analyze",choices = NULL,width = "100%"),
                                       checkboxInput("dtm1","Transform from day to month",value = F),
                                       checkboxInput("etr","Check for filtration variable 1", value = F),
                                       uiOutput("extra"),
                                       uiOutput("extra1"),
                                       checkboxInput("tgp","Check for filtration variable 2",value = F),
                                       uiOutput("tgpvar"),
                                       uiOutput("tgpval")
                                ),
                                
                                column(6,
                                       selectInput("trgr","Select the Treatment Variable",choices = NULL),
                                       uiOutput("refgroup")
                                ),
                                
                                column(6,
                                       selectInput("tie","Select handling method for tied events", choices = c("efron" = "efron",   "exact('discret' in SAS)" = "exact", "breslow" = "breslow"), selected = "efron")),
                                column(12,
                                       checkboxInput("icomb",tags$b("Internal combination: Need combination within a variable"),width = "100%"),
                                       uiOutput("icomb"),
                                       uiOutput("icombinfo"),
                                ),
                                
                              )
                     ),
                     tabPanel("Selection",
                              fluidRow(
                                column(6,
                                       checkboxInput("saf",tags$b("Applying stratified analysis on Forest Plot")),
                                       uiOutput("savar")),
                                column(12,
                                       tags$h5(style = "font-weight : bold;","Select the subgroup to analyze")),
                                column(6,
                                       div(style = "max-height : 300px;
                                                    overflow-y : auto;",
                                           uiOutput("pop")),
                                       div(style = "max-height : 300px;
                                                    overflow-y : auto;",
                                           rank_list(
                                             text = "to here",
                                             labels = list(),
                                             input_id = "selected",
                                             options = sortable_options(group = "Subgroup")
                                           ))),
                                
                                column(6,
                                       uiOutput("e")),
                              )
                     ),
                     tabPanel("Label",
                              fluidRow(
                                column(12,
                                       uiOutput("ch")
                                ),
                                column(6,uiOutput("chname")
                                ),
                                column(6,uiOutput("chlevel")),
                              )
                     ),
                     tabPanel("Addition",
                              fluidRow(
                                column(6,
                                       textInput("lal","Treatment Group label",value = "Treatment Group")
                                ),
                                column(6,
                                       textInput("ral","Reference Group label",value = "Reference Group")),
                                column(6,
                                       checkboxInput("hr",tags$b("Show Hazard Ratio(95% CI)"),value = T),
                                       checkboxInput("med",tags$b("Show median information"),value = T),
                                       checkboxInput("pool",tags$b("Need pooled patient's information"),value = T),
                                       checkboxInput("ctr",tags$b("Show p-value for treatment"),value = T),
                                       checkboxInput("cin",tags$b("Show p-value for interaction"),value = T),
                                       uiOutput("frcol")
                                ),
                                column(6,
                                       numericInput("nbk","Number of indents for subgroup categories",value = 4),
                                       selectInput("shape","Select shape of the point",choices = setNames(c(15,16,17,18),c("square","circle","triangle","diamond")),selected = 18),
                                       numericInput("frf","Fontsize of the text",value = 12)
                                ),
                                column(6,
                                       numericInput("leed","Left end of coordinate",value = NULL)),
                                column(6,
                                       numericInput("ried","Right end of coordinate",value = NULL)),
                                
                              )),
                     tabPanel("Survival Rates",
                              fluidRow(
                                column(12,
                                       numericInput("nump", "How many time points you would like to add?",value = NULL)
                                ),
                                column(6,
                                       uiOutput("times")
                                ),
                                
                              )),
              ),
              #Table Preview
              tabBox(
                title = "", width = 7,  
                id = "tabset2",
                tabPanel("Forest Plot Table",
                         fluidRow(
                           column(12,
                                  tableOutput("NoP"),
                                  downloadButton("FRtable","Download Table"),
                                  downloadButton("cfile","Download Configuration File")
                           )
                         )
                         
                ),
                #Plot Preview
                tabPanel("Forest Plot",
                         fluidRow(
                           tags$style("
                                  .plot-output {
                                  width: 100%;
                                  height: 100%;
                                  max-width: 50cm;
                                  max-height: 35cm;
                                  }
                                  "),
                           column(6,
                                  fileInput("FRtable","Upload table which is ready to plot"),
                           ),
                           column(6,
                                  fileInput("cinfile","Upload configuration file to plot")),
                           column(3,
                                  selectInput("frdltype","Select file type",
                                              choices = c("WMF"="wmf",
                                                          "PNG"="png",
                                                          "SVG"="svg",
                                                          "Powerpoint" = "pptx",
                                                          "PDF"="pdf"))),
                           column(3,
                                  br(),
                                  downloadButton("frd","Download")),

                           column(12,
                                  plotOutput("FRPlot",height = "600px")
                           ))
                ),
                tabPanel("Summary Table",
                         fluidRow(
                           column(12,
                                  downloadButton("dftable","Download Excel"),
                                  uiOutput("savart"),
                                  reactableOutput("dft")     ))
                )
                
              )
              
      )
    )
    
  ) 
)

server <- function(input,output){
  options(shiny.maxRequestSize = 30*1024^2)
  
  #===================KMPLOT===================================  
  censorSize <- reactiveVal(0.02)
  observeEvent(input$cmup,{censorSize(censorSize()+0.005)})
  observeEvent(input$cmdn,{censorSize(censorSize()-0.005)})
  
  censort <- reactiveVal(0.2)
  observeEvent(input$ctup,{censort(censort()+0.05)})
  observeEvent(input$ctdn,{censort(censort()-0.05)})
  
  tickLen <- reactiveVal(0.2)
  observeEvent(input$tup,{tickLen(tickLen()+0.05)})
  observeEvent(input$tdn,{tickLen(tickLen()-0.05)})
  
  axisT <- reactiveVal(0.25)
  observeEvent(input$alup,{axisT(axisT()+0.25)})
  observeEvent(input$aldn,{axisT(axisT()-0.25)})
  
  curveT <- reactiveVal(0.5)
  observeEvent(input$clup,{curveT(curveT()+0.25)})
  observeEvent(input$cldn,{curveT(curveT()-0.25)})
  
  ylab <- reactiveVal("Survival Probability (%)")
  xlab <- reactiveVal("Time")
  title <- reactiveVal()
  observeEvent(input$text,{
    ylab(strsplit(isolate(input$y),"\\\\n")[[1]])
    xlab(strsplit(isolate(input$x),"\\\\n")[[1]])
  })
  
  sizet <- reactiveVal(10)
  sizerm <- reactiveVal(10)
  sizert <- reactiveVal(0.15)
  
  observeEvent(input$sizet_up,
               sizet(sizet()+1))
  observeEvent(input$sizerm_up,
               sizerm(sizerm()+5))
  observeEvent(input$sizert_up,
               sizert(sizert()+0.05))
  observeEvent(input$sizey_up,
               updateNumericInput(getDefaultReactiveDomain(),"sizey",value = input$sizey +1))
  observeEvent(input$sizen_up,
               updateNumericInput(getDefaultReactiveDomain(),"sizen",value = input$sizen +1))
  observeEvent(input$sizer_up,
               updateNumericInput(getDefaultReactiveDomain(),"sizer",value = input$sizer +1))
  observeEvent(input$sizes_up,
               updateNumericInput(getDefaultReactiveDomain(),"sizes",value = input$sizes +1))
  observeEvent(input$size_up,
               updateNumericInput(getDefaultReactiveDomain(),"fontsize",value = input$fontsize +1))
  
  observeEvent(input$sizet_down,
               sizet(sizet()-1))
  observeEvent(input$sizerm_down,
               sizerm(sizerm()-5))
  observeEvent(input$sizert_down,
               sizert(sizert()-0.05))
  observeEvent(input$sizey_down,
               updateNumericInput(getDefaultReactiveDomain(),"sizey",value = input$sizey -1))
  observeEvent(input$sizen_down,
               updateNumericInput(getDefaultReactiveDomain(),"sizen",value = input$sizen -1))
  observeEvent(input$sizer_down,
               updateNumericInput(getDefaultReactiveDomain(),"sizer",value = input$sizer -1))
  observeEvent(input$sizes_down,
               updateNumericInput(getDefaultReactiveDomain(),"sizes",value = input$sizes -1))
  observeEvent(input$size_down,
               updateNumericInput(getDefaultReactiveDomain(),"fontsize",value = input$fontsize -1))
  
  legendx <- reactiveVal()
  legendy <- reactiveVal()
  
  legendx1 <- reactive({
    if(input$lp == 1){
      return(8)
    }else{
      return(input$brerd-1)
    }
  })
  
  legendy1 <- reactive({
    if(input$lp == 1){
      return(0.9)
    }else{
      return(0.2)
    }
  })
  
  observe({legendx(legendx1())})
  observe({legendy(legendy1())})
  
  observeEvent(input$left_btn,{
    legendx(legendx()+1)
  })
  observeEvent(input$right_btn,{
    legendx(legendx()-1)
  })
  observeEvent(input$up_btn,{
    legendy(legendy()+0.05)
  })
  observeEvent(input$down_btn,{
    legendy(legendy()-0.05)
  })
  
  sx <- reactiveVal()
  sy <- reactiveVal()
  sxi <- reactiveVal()
  syi <- reactiveVal()
  
  sx1 <- reactive({
    if(input$tp == 1){
      return(input$brerd)
    }else{
      return(input$brerd*0.5)
    }
  })
  
  sx0 <- reactive({
    if(input$tp == 1){
      return(input$brerd*0.5)
    }else{
      return(0)
    }
  })
  
  sy1 <- reactive({
    if(input$tp == 1){
      return(0.7)
    }else{
      return(0)
    }
  })
  
  sy0 <- reactive({
    if(input$tp == 1){
      return(0.9)
    }else{
      return(0.3)
    }
  })
  
  observe({sx(sx1())})
  observe({sy(sy1())})
  observe({sxi(sx0())})
  observe({syi(sy0())})
  
  observeEvent(input$left_s,{
    sx(sx()-1)
  })
  observeEvent(input$right_s,{
    sx(sx()+1)
  })
  observeEvent(input$up_s,{
    sy(sy()+0.05)
  })
  observeEvent(input$down_s,{
    sy(sy()-0.05)
    
  })
  
  laname <- reactiveVal()
  observeEvent(data_final(),{data <- data_final()
  name <- levels(data$TRTP)
  laname(name)})
  observeEvent(input$labn,
               {
                 data <- data_final()
                 name <- sapply(levels(data$TRTP),function(x) input[[paste("labname_",x)]])
                 laname(name)
                 
               })
  
  #-------------KMPLOT_UI------------
  
  output$sas <- renderUI({
    req(input$file1)
    if(input$filetype == 2){
      if(!grepl("\\.(sas7bdat)$",input$file1$name)){
        showNotification("Please upload an SAS file.",type = "error")
        tags$b(style = "color:red","Please upload an SAS file.")
      }
      else{
        file <- input$file1
        data <- read_sas(file$datapath)
        label <- data %>% map(attr_getter("label"))
        idx1 = grep("Treatment|treatment|Arm|arm|治疗组",label)
        if(length(idx1) != 0){
          col1 = colnames(data)[idx1]
          lab1 = label[idx1]
        }else{
          col1 = colnames(data)
          lab1 = label
        }
        idx2 = grep("Censor|censor|删失",label)
        if(length(idx2) != 0){
          col2 = colnames(data)[idx2]
          lab2 = label[idx2]
        }else{
          col2 = colnames(data)
          lab2 = label
        }
        
        inputList <- list(
          column(6,
                 selectInput("param","Select the parameter",choices = setNames(unique(data$PARAM),unique(data$PARAM))),
                 checkboxInput("transf",  "Change the unit of survival time"),
                 uiOutput("dtm"),
                 uiOutput("mtd"),
          ),
          column(6,
                 selectInput("groupsas","Select the treatment variable",choices = setNames(c(1,col1),c(" ",lab1)),selected = NULL),
                 selectInput("censas","Select the variable of censor",choices = setNames(c(1,col2),c(" ",lab2)),selected = NULL),
                 uiOutput("sascensored")
          )
          
        )
      }
    }
    
    else{
      return(NULL)
    }
    
  })
  
  
  output$excel <- renderUI({
    req(input$file1)
    if(input$filetype == 1){
      if(!grepl("\\.(xlsx|xls)$",input$file1$name)){
        showNotification("Please upload an Excel file.",type = "error")
        tags$b(style = "color:red","Please upload an Excel file.")
      }
      else{
        file <- input$file1
        data <- read_excel(file$datapath)
        idx1 = grep("Treatment|treatment|Arm|arm",colnames(data))
        idx2 = grep("Censor|censor",colnames(data))
        if(length(idx1) != 0){
          col1 = colnames(data)[idx1]
        }else{
          col1 = colnames(data)
        }
        if(length(idx2) != 0){
          col2 = colnames(data)[idx2]
        }else{
          col2 = colnames(data)
        }
        inputList <- list(
          selectInput("para","Select a parameter to analyze",choices = setNames(c(1,colnames(data)),c(" ",colnames(data))),selected = NULL),
          checkboxInput("transf",  "Change unit of survival time in x-axis"),
          uiOutput("dtm"),
          uiOutput("mtd"),
          selectInput("censor","Select the censor variable",choices = setNames(c(1,col2),c(" ",col2)),selected = NULL),
          selectInput("group","Select the treatment variable",choices = setNames(c(1,col1),c(" ",col1)),selected = NULL)
          
        )
        do.call(tagList,inputList)
      }
    }
    
    
    else{
      return(NULL)
    }
    
  })
  
  output$excelcensored <- renderUI({
    if(!is.null(input$censor)){
      if(input$censor != 1){
        file <- input$file1
        data <- read_excel(file$datapath)
        checkboxGroupInput("censored","Select the value of censored", choices = setNames(unlist(unique(data[[input$censor]])),unlist(unique(data[[input$censor]]))))
      }
      else{return(NULL)}
    }
    else{return(NULL)}
  })
  
  output$sascensored <- renderUI({
    if(!is.null(input$censas)){
      if(input$censas != 1){
        file <- input$file1
        data <- read_sas(file$datapath)
        checkboxGroupInput("censorsas", "Select the value of censored", choices = setNames(unique(data[[input$censas]]),unique(data[[input$censas]])))
      }
      else{return(NULL)}
    }
    else{return(NULL)}
  })
  
  output$sbg <- renderUI({
    if(is.null(input$file1) == F){
      checkboxInput("subgroup",tags$b("Explore a subgroup"),value = F)
    }
    else{
      NULL
    }
  })
  
  output$sgvar <- renderUI({
    if(input$subgroup == F || is.null(input$subgroup)){
      return(NULL)
    }
    else{      
      file <- input$file1
      if(input$filetype == 1){
        data <- read_excel(file$datapath)
        selectInput("sbvar","Select the subgroup variable",choices = setNames(c(1,colnames(data)),c(" ",colnames(data))),selected = 1)
        
      }
      else{
        data <- read_sas(file$datapath)
        label <- data %>% map(attr_getter("label"))
        for (i in names(label)) {
          if(is.null(label[[i]])){
            label[[i]] <- i
          }
        }
        selectInput("sbvar","Select the subgroup variable",choices = setNames(c(1,colnames(data)),c(" ",label)),selected = 1)
        
      }
    }  
  })
  
  output$sgval <- renderUI({
    if(is.null(input$subgroup) || input$subgroup == F){
      return(NULL)
    }
    else{
      file <- input$file1
      if(input$filetype == 1){
        data <- read_excel(file$datapath)
      }
      else{
        data <- read_sas(file$datapath)
      }
      val = unique(data[[input$sbvar]])
      selectizeInput("sbval","Select subgroup value",choices = setNames(val,val),multiple = T,selected = val)
    }
  })
  
  output$order <- renderUI({
    req(input$file1)
    checkboxInput("order", tags$b("Need order adjustment for the label display"))
  })
  
  
  output$level <- renderUI({
    req(input$file1)
    req(input$order)
    if(input$order == F)     
      return(NULL)
    file <- input$file1
    val = unlist(levels(data()$TRTP))
    rank_list(
      labels = val,
      input_id = "rank_list")
  })
  
  #Adjust label,color,linetype
  output$color <- renderUI({
    if(is.null(input$file1))     
      return(NULL)
    data <- data_final()
    default_cols = c("blue","darkgreen","red","orange","purple","pink")
    blue <- c("blue","darkblue","skyblue","lightblue","royalblue","slateblue")
    green <- c("green","darkgreen","seagreen","lightgreen","limegreen","springgreen")
    red <- c("red","darkred","violetred","pink","indianred","deeppink")
    orange <- c("darkorange","orange","orangered")
    purple <- c("mediumpurple","purple")
    yellow <- c("yellow","lightyellow")
    
    val = levels(data$TRTP)
    map(val,function(level){
      level_index <- which(val == level)
      default_col = default_cols[level_index]
      tagList(
        column(6,textInput(paste("labname_",level),label = paste0("Label for ",level),value = level)),
        column(3,selectInput(paste0("color_",level),label = "Color",
                             choices = list("Black" = "black",
                                            "Blue" = blue,
                                            "Green" = green,
                                            "Yellow" = yellow,
                                            "Red" = red,
                                            "Purple" = purple,
                                            "Orange" = orange)
                             ,selected = default_col)),
        column(3,selectInput(paste0("linetype_",level),label = "Linetype",
                             choices = list("solid" = "solid",
                                            "longdash" = "42",
                                            "dashed" = "dashed",
                                            "dotted" = "dotted",
                                            "dotdash" = "dotdash"
                             )
                             ,selected = "solid"))
      )
      
      
    })
  })
  
  output$template <- downloadHandler(
    filename = "template.xlsx",
    content = function(file){
      file.copy("template.xlsx",file)
    }
  )
  
  output$quantile <- renderUI({
    if(!is.na(input$numq)&&input$numq>0){     
      data <- data_final()
      val = unlist(unique(data$TRTP))
      map(1:input$numq,~numericInput(paste("q_",.x),paste("Enter the time point ",.x),value = 0))
    }else{
      return(NULL)
    }
    
  })
  
  output$deci <- renderUI({
    if(!is.na(input$numq)&&input$numq>0){
      numericInput("deci","The number of decimal places:",value = 1)
    }else{
      return(NULL)
    }
    
  })
  
  output$dtm <- renderUI({
    if(input$transf == T){
      checkboxInput("dtm","Transform from day to month")
    }
  })
  
  output$mtd <- renderUI({
    if(input$transf == T){
      checkboxInput("mtd","Transform from month to day")
    }
  })
  
  #------Read_Dataset---------------------  
  data <- reactive({
    
    if(is.null(input$file1)==F){
      file <- input$file1
      if(input$filetype == 2 && grepl("\\.(sas7bdat)$",input$file1$name)){
        
        data <- read_sas(file$datapath)
        req(input$censorsas)
        data = subset(data,PARAM == input$param)
        data["TRTP"] = data[input$groupsas]
        if(!is.null(input$censorsas)){
          data["status"] = ifelse(data$CNSR %in% as.vector(input$censorsas),0,1)
        }
        else{return(NULL)}
        
      }
      else if(input$filetype == 1 && grepl("\\.(xlsx|xls)$",input$file1$name)){
        data <- read_excel(file$datapath)
        req(input$censored)
        data["AVAL"] = data[[input$para]]
        data["TRTP"] = data[[input$group]]
        if(!is.null(input$censored)){
          data["status"] = ifelse(data[[input$censor]] %in% as.vector(input$censored),0,1)
        }
        else{return(NULL)}
      }else{
        return(NULL)
      }
      
      
      if(input$subgroup == T){
        if(length(input$sbvar) != 0){
          if(input$sbvar !=1){
            data["subgroup"] = data[[input$sbvar]]
            data <- subset(data,subgroup %in% as.vector(input$sbval))
            data$TRTP = paste0(data$TRTP,"_",data$subgroup)
            
            data
          }
          data
        }
      }
      else{
        data
      }
      
      data$TRTP <- factor(data$TRTP)
      
      
      if(input$transf == T){
        data
        if(length(input$dtm) != 0 && input$dtm == T){
          data$AVAL = data$AVAL/(365.25/12)
          data
        }else{data}
        if(length(input$mtd) != 0 && input$mtd == T){
          data$AVAL = round(data$AVAL*(365.25/12),0)
          data
        }else{data}
      }else{data}
      
      
    }
    else{return(NULL)}
  })
  
  #----Adjust the order------------
  data_final <- reactive({
    req(data())
    if(input$order == T){
      data <- data()
      data$TRTP = factor(data$TRTP,levels = input$rank_list)
      updateNumericInput(session = getDefaultReactiveDomain(),"brerd",value = round(max(data$AVAL),0)+1)
      data
    }
    else{
      data <- data()
      print(data)
      updateNumericInput(session = getDefaultReactiveDomain(),"brerd",value = round(max(data$AVAL),0)+1)
      data
    }
    
  })
  
  label <- reactive({
    data <- data_final()
    label = data %>% map_chr(attr_getter("label"))
  })
  
  #----model----  
  survfit <- reactive({
    data = data_final()
    fit = survfit2(Surv(AVAL, status) ~ TRTP, data = data)  
    fit
  })
  
  
  #----store the color value
  valcolors <- reactive({
    data <- data_final()
    val = unlist(unique(data$TRTP))
    
    color <- sapply(levels(data$TRTP),function(x) input[[paste0("color_",x)]])
    color <- unlist(color)
    color
  })
  
  #-----store the linetype value
  vallinetype <- reactive({
    data <- data_final()
    val = unlist(unique(data$TRTP))
    
    linetype <- sapply(levels(data$TRTP),function(x) input[[paste0("linetype_",x)]])
    linetype <- unlist(linetype)
    linetype
  })
  
  
  output$plot <- renderPlot({kmplot()},res = 120)
  output$plotui <- renderUI({plotOutput("plot",width = input$pwd*120,height = input$pht*120)})
  
  
  #----Draw the KM plot---------  
  kmplot <- reactive({
    req(input$file1)
    if(length(valcolors()) == 0)     
      return(NULL)
    data <- data_final()
    fit = survfit()
    s=summary(fit)
    res = data.frame(cbind(fit$time,fit$surv,fit$n.censor))
    
    dif=diff(res$X2)
    start=which(dif>0)+1
    start=c(1,start)
    
    
    
    par(mar = c(0,0,0,0))
    p <- ggsurvfit(survfit(),type = "survival",linewidth = curveT(),linetype_aes = T,show.legend = F) +
      scale_y_continuous(
        breaks = seq(0,1,0.1),
        labels = function(x) x*100,
        expand = c(0,0),
        limits = c(0,1.05)
      ) +
      scale_x_continuous(
        breaks = seq(0,input$brerd,input$interval),
        expand = c(0,0)
      ) +
      scale_linetype_manual(values = vallinetype(),labels = laname()) +
      scale_color_manual(values = valcolors(),labels = laname()) +
      theme(
        plot.margin = margin(t=sizet(),r=sizerm()),
        title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = input$sizey,family="serif"),
        axis.title.y = element_blank(),
        axis.ticks.length = unit(tickLen(),"cm"),
        axis.ticks = element_line(linewidth = axisT()),
        axis.text.x = element_text(size = input$sizen,family="serif"),
        axis.text.y = element_text(size = input$sizen,family="serif")) +
      guides(fill = guide_legend(byrow = T)) +
      labs(
        x = paste(xlab(),collapse = "\n")) +
      annotation_custom(grob=textGrob(label = paste(ylab(),collapse = "\n"),
                                      rot = 90,
                                      gp = gpar(fontsize = input$sizey, fontfamily = "serif"),
                                      x=-35,default.units = "pt")) +
      coord_cartesian(xlim = c(0,input$brerd),clip = "off") 
    
    ##-----draw censor mark    
    groupsize=diff(c(start,length(fit$surv)+1))
    
    df_line = data.frame(x=fit$time,y=fit$surv,censor=fit$n.censor,
                         color=rep(valcolors(),groupsize))
    
    p <- p+geom_linerange(data = df_line,aes(x=x, ymin=y, ymax=y+censorSize()*(censor!=0)),
                          colour=df_line$color,linewidth=censort())
    
    ##------plot border
    if(input$cpb == T){
      p <- p+theme(panel.border = element_rect(color = "black",fill = NA))
    }
    else{p <- p+theme(panel.border = element_blank(),
                      axis.line = element_line(colour = "black",linewidth = axisT()))}
    
    
    
    ##-----draw survival rates
    if(input$q_a_a >0 && !is.na(input$numq) && input$numq>0 ){
      for(i in 1:input$numq){
        if(is.na(input[[paste("q_",i)]]) == F){
          if(input[[paste("q_",i)]] <= max(data$AVAL)){
            p <- p + geom_vline(xintercept = input[[paste("q_",i)]],linetype = "dashed")
            m = 1
            n = 0
            surv_summary = summary(survfit(),times = input[[paste("q_",i)]])
            if(round(max(surv_summary$surv),2)+0.1 > 1){
              n = 0.25
            }else{
              n = 1
            }
            for (j in levels(data$TRTP)) {
              dot = paste(round(surv_summary$surv[m],input$deci+2)*100,"%")
              p <- p + annotate("text",x = input[[paste("q_",i)]]+0.4,y=n,label=dot,size = 9/.pt,color=valcolors()[j],hjust=0,vjust=1)
              m = m+1
              n = n-0.05
            }
          }
          else{
            p <- p+annotate("text", x = 0, y = 0.5, label = paste0("Error: Please enter a number between 1 and ",round(max(data$AVAL),0)),color="red",hjust=0,size=9)
          }
        }
        else{p}
      }
    }
    else{p}
    
    ##-----draw summary table
    if(is.null(input$file2) == F){
      file <- input$file2
      options(scipen = 999)
      info <- read_excel(file$datapath,na = "" ,trim_ws = F,col_names = F)
      n = sum(info[,1] == "Title")
      info <- select(info,-1)
      info[is.na(info)] <- ""
      
      g=tableGrob(info,rows = NULL,theme = ttheme_minimal(base_size = input$sizes,padding = unit(c(2,1.5),"mm")),cols = NULL)
      g <- gtable_add_grob(g,
                           grobs = segmentsGrob( # line across the bottom
                             x0 = unit(0,"npc"),
                             y0 = unit(0,"npc"),
                             x1 = unit(1,"npc"),
                             y1 = unit(0,"npc"),
                             gp = gpar(lwd = 1.0)),
                           t = n, b = 1, l = 1, r = ncol(g))
      p <- p + annotation_custom(g, xmin=sxi(), xmax=sx(), ymin=syi(), ymax=sy())
      
      
    }
    else{
      p
    }
    
    ##-----draw legend--------
    if(input$cls == T){
      m = input$brerd-legendx()
      n = legendy()
      for(i in 1:length(laname())){
        p <- p + annotation_custom(linesGrob(gp=gpar(col=valcolors()[i],lty = vallinetype()[i])),
                                   xmin = m, xmax = m+1.5, ymin = n, ymax = n)
        p <- p + annotation_custom(textGrob(label = laname()[i],
                                            gp = gpar(fontsize = input$fontsize, fontfamily = "serif",col = valcolors()[i]),
                                            vjust = 0.5,hjust=0),
                                   xmin = m+2,xmax = m+2,ymin = n,ymax=n
        )
        n = n - 0.06
      }
      p <- p + annotation_custom(linesGrob(),
                                 xmin = m+0.75, xmax = m+0.75, ymin = n-0.02, ymax = n+0.02)
      p <- p + annotation_custom(textGrob(label = "Censored",
                                          gp = gpar(fontsize = input$fontsize, fontfamily = "serif"),
                                          vjust = 0.5,hjust=0),
                                 xmin = m+2,xmax = m+2,ymin = n,ymax=n)
      
    }else{
      p
    }
    
    ##------draw risktable
    if(input$rtb == T){
      RiskSetCount <- function(timeindex, survivaltime) {
        atrisk <- NULL
        for (t in timeindex)
          atrisk <- c(atrisk, sum(survivaltime >= t))
        return(atrisk)
      }
      grid=seq(0,input$brerd,input$interval)
      level = levels(data$TRTP)
      atrisk <- c()
      trt <- c()
      x <- c()
      n=length(level)*0.1
      for(i in 1:length(level)){
        atrisk <- c(atrisk,RiskSetCount(grid,data$AVAL[data$TRTP==level[i]]))
        trt <- c(trt,rep(laname()[i],length(grid)))
        x = c(x,grid)
        
      }
      risktable <- data.frame(x=x,trt=trt,atrisk=atrisk)
      risktable$trt = factor(risktable$trt,levels = rev(laname()))
      r <- ggplot(data = risktable,aes(x=x,y=trt))+
        geom_text(label=risktable$atrisk,size = input$sizer/.pt,family = "serif") +
        coord_cartesian(xlim = c(0,input$brerd),clip = "off") +
        scale_x_continuous(breaks = 0:10,expand = c(0, 0)) +
        theme_minimal() + 
        labs(
          title = input$risklab
        ) +
        theme(
          axis.title = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(family = "serif",
                                     colour = rev(valcolors()),
                                     hjust = 0,
                                     size = input$sizer,
                                     margin = margin(r=5)),
          title = element_text(family = "serif",
                               size = input$sizer),
          plot.title.position = "plot"
        )
      
      p <- wrap_plots(p,r,ncol = 1,heights = c(1-sizert(),sizert()))
      
      
    }else{p}
    
    p
  })
  
  output$kmd <- downloadHandler(
    filename = function(){
      paste("kmplot",input$dltype,sep = ".")
    },
    content = function(file){
      if(input$dltype == "pptx"){
          doc = read_pptx()
          doc <- add_slide(doc,"Blank", "Office Theme")
          p <- kmplot()
          p <- ggsurvfit_build(p,combine_plots = T)
          p <- dml(ggobj = p)
          doc <- ph_with(doc,value = p,location = ph_location(width = input$pwd,height = input$pht))
          print(doc,target = file)
      }else{
        p <- kmplot()
        p <- ggsurvfit_build(p,combine_plots = T)
        ggsave(file,plot = print(p),device = input$dltype,width = input$pwd,height=input$pht,units = "in")
      }

    }
  )
  

  
  
  ##==============Forest plot=========================  
  
  ##----read datasets-----
  FRdfsl <- reactive({
    file <- input$FRfile
    data <- read_sas(file$datapath)
    data
  })
  
  FRdftte <- reactive({
    file <- input$FRfile0
    data <- read_sas(file$datapath)
    data
  })
  
  ##updated UI
  output$adsl <- renderUI({
    if(input$adsl == F)
      return(NULL)
    fileInput("FRfile","Upload the patient level file")
  })
  
  
  output$refgroup <- renderUI({
    req(input$trgr)
    data <- all_info()
    trgr <- input$trgr
    if(trgr %in% names(data)){
      radioButtons("ref","Select the Reference Group",choices = setNames(unique(data[[trgr]]),unique(data[[trgr]])))
    }
    else{
      return(NULL)
    }
  })
  
  output$ccupload <- renderUI({
    if(input$ccfile == F)
      return(NULL)
    fileInput("cinfile","Upload the configuration file" )
  })  
  
  
  
  output$icomb <- renderUI({
    req(input$FRfile0)
    if(length(input$icomb) == 0)
      return(NULL)
    else if(input$icomb == F)
      return(NULL)
    inputList <- list(
      column(4,
             selectizeInput("icomv","Select a variable",choices = setNames(names(all_info()),labelsl1())),
             numericInput("inumg","Number of groups",value = NULL)),
      column(4,
             textInput("icomname","Name for the new variable")
      ),
      column(4,
             textInput("icomlabel", "Label for the new variable")
      )
      
    )
    do.call(tagList,inputList)
    
  })
  
  output$icombinfo <- renderUI({
    if(length(input$inumg) == 0)
      return(NULL)
    else if(is.na(input$inumg))
      return(NULL)
    data <- all_info()
    map(1:input$inumg,~selectizeInput(paste("g_",.x),paste("Select the value in group ",.x),choices = setNames(unique(data[input$icomv]),unique(data[input$icomv])),multiple = T,width = "50%"))
  })
  
  
  all_info_update <- reactive({
    data <- all_info()
    data <- subset(data,PARAM == input$frpara)
    if(input$dtm1 == T){
      data$AVAL = data$AVAL/(365.25/12)
    }
    if(input$etr == T){
      data["Extraparam"] = data[input$extrainfo]
      data <- subset(data,Extraparam %in% as.vector(input$extrainfo1))
    }
    if(input$tgp == T){
      data["ScreenedG"] = data[input$tgpvar]
      data <- subset(data,ScreenedG %in% as.vector(input$tgpval))
      print(unique(data["ScreenedG"]))
    }
    
    if(!is.null(input$icomname) && !is.null(input$icomlabel)){
      if(input$icomname != ""){
        data[[input$icomname]] = data[[input$icomv]]
        if(!is.na(input$inumg)){
          for(i in 1:input$inumg){
            data[data[[input$icomname]] %in% input[[paste("g_",i)]],input$icomname] = paste(input[[paste("g_",i)]],collapse = ",")
          }
        }
        if(input$icomlabel != ""){
          attr(data[[input$icomname]],"label") <- input$icomlabel
          data
        }
        else{
          attr(data[[input$icomname]],"label") <- "Unknown"
          data
        }
        updateSelectizeInput(session = getDefaultReactiveDomain(),"pop",choices = setNames(names(data),data %>% map(attr_getter("label"))))
        data
      }
      else{data}
    }
    else{data}
    
  })
  
  output$pop <- renderUI({
    if(is.null(input$FRfile0)){
      label <- NULL
    }else{
      data <- all_info_update()
      label <- data %>% map(attr_getter("label"))
      for (i in names(label)) {
        if(is.null(label[[i]])){
          label[[i]] <- i
        }
      }
      
      label <- label[!(names(label) %in% input$selected)]
    }
    
    rank_list(
      text = "Drag from here",
      labels = label,
      input_id = "rank_list_1",
      options = sortable_options(group = "Subgroup")
    )
  })
  
  output$savar <- renderUI({
    if(input$saf == T){
      data <- all_info_update()
      label <- data %>% map(attr_getter("label"))
      selectizeInput("savar","Select the stratified variable",choices = setNames(colnames(data),label),multiple = T)
    }else{
      return(NULL)
    }
  })
  
  output$savart <- renderUI({
    req(input$FRfile0)
    data <- all_info_update()
    label <- data %>% map(attr_getter("label"))
    for (i in names(label)) {
      if(is.null(label[[i]])){
        label[[i]] <- i
      }
    }
    selectizeInput("savart","Select the stratified variable",choices = setNames(colnames(data),label),multiple = T)
  })
  
  all_info <- reactive({
    if(input$adsl == T){
      if(is.null(input$FRfile) == F){
        data <- left_join(FRdftte(),FRdfsl(),by="USUBJID")
      }
      else{
        data <- FRdftte()
      }
    }
    else{
      data <- FRdftte()
    }
    labelsl = data %>% map(attr_getter("label"))
    updateSelectInput(session = getDefaultReactiveDomain(),"frpara",choices = setNames(unique(data$PARAM),unique(data$PARAM)))
    idx1 = grep("Treatment|treatment|Arm|arm|治疗组",labelsl)
    if(length(idx1) != 0){
      col1 = colnames(data)[idx1]
      lab1 = labelsl[idx1]
    }else{
      col1 = colnames(data)
      lab1 = labelsl
    }
    
    
    updateSelectInput(session = getDefaultReactiveDomain(),"trgr",choices = setNames(col1,lab1))
    updateSelectizeInput(session = getDefaultReactiveDomain(),"pop",choices = setNames(names(data),labelsl))
    data
  })
  
  output$tgpvar <- renderUI({
    if(input$tgp == T){
      data <- all_info()
      labelsl = data %>% map(attr_getter("label"))
      selectInput("tgpvar","Select the filtration variable",choices = setNames(names(data),labelsl))
    }else{
      return(NULL)
    }
  })
  
  output$tgpval <- renderUI({
    if(input$tgp == T){
      data <- all_info()
      labelsl = data %>% map(attr_getter("label"))
      var = unlist(unique(data[[input$tgpvar]]))
      checkboxGroupInput("tgpval",label = NULL, choices = setNames(var,var))
    }else{
      return(NULL)
    }
  })
  
  
  output$extra <- renderUI({
    if(input$etr == T){
      data <- all_info()
      labelsl = data %>% map(attr_getter("label"))
      selectInput("extrainfo", NULL, choices = setNames(names(data),labelsl))
    }else{
      return(NULL)
    }
  })
  
  output$extra1 <- renderUI({
    if(input$etr == T){
      data <- all_info()
      labelsl = data %>% map(attr_getter("label"))
      var = unlist(unique(data[input$extrainfo]))
      checkboxGroupInput("extrainfo1",label = NULL, choices = setNames(var,var))
    }else{
      return(NULL)
    }
  })
  
  labelsl1 <- reactive({
    data <- all_info()
    labelsl = data %>% map(attr_getter("label"))
    labelsl
  })
  
  labelsl <- reactive({
    data <- all_info_update()
    labelsl = data %>% map(attr_getter("label"))
    labelsl
  })
  
  observeEvent(input$FRfile0,{
    updateSelectizeInput(session = getDefaultReactiveDomain(),"pop",choices = setNames(names(all_info_update()),labelsl()))
  })
  
  
  output$e <- renderUI({
    if(is.null(input$FRfile0))
      return(NULL)
    data <- all_info_update()
    map(input$selected,~selectizeInput(paste("cta_",.),label = paste0("Select which ",.),choices = setNames(unique(data[.]),unique(data[.])),multiple = T,width = "70%"))
  })
  
  fit <- reactive({
    data <- all_info()
    data$status <- -data$CNSR+1
    fit <- coxph(Surv(AVAL,status) ~ TRT01P, data = data[data[i,] == k])
  })
  
  ##============Summary Table=================  
  output$times <- renderUI({
    if(!is.na(input$nump)&&input$nump>0){     
      map(1:input$nump,~numericInput(paste("p_",.x),paste("Enter the time point ",.x),value = NULL))
    }else{
      return(NULL)
    }
  })
  
  dfs <- reactive({
    req(input$FRfile0)
    info <- all_info_update()
    var <- names(info)
    lab <- info %>% map(attr_getter("label"))
    
    if(input$trgr %in% names(info)){
      info$TRTP <- info[[input$trgr]]
      info$TRTP <- factor(info$TRTP)
      if(length(levels(info$TRTP))>1){
        if(length(input$ref) > 0){
          info$TRTP <- relevel(info$TRTP,input$ref)
        }
      }else
        return(NULL)
    }
    else
      return(NULL)
    
    info$event <- ifelse(info$CNSR == 0,1,0)
    #number of patients
    n1 = c(nrow(info[info$TRTP == levels(info$TRTP)[2],]))
    n2 = c(nrow(info[info$TRTP == levels(info$TRTP)[1],]))
    n = n1+n2
    
    #Status    
    factor <- c("Status,n(%)")
    T1 <- c(" ")
    T2 <- c(" ")
    TT <- c(" ")
    order <- c(1)
    #-event
    factor <- append(factor,"  Events observed")
    T1 <-  append(T1,nrow(info[info$event == 1 & info$TRTP == levels(info$TRTP)[2],]))
    T2 <- append(T2,nrow(info[info$event == 1 & info$TRTP == levels(info$TRTP)[1],]))
    TT <- append(TT,nrow(info[info$event == 1,]))
    order <- append(order,2)
    for (i in unique(info[info$event == 1,]$EVNTDESC)) {
      factor <- append(factor,paste("    ",i,sep = " "))
      T1 <- append(T1,nrow(info[info$TRTP == levels(info$TRTP)[2] & info$EVNTDESC == i,]))
      T2 <- append(T2,nrow(info[info$TRTP == levels(info$TRTP)[1] & info$EVNTDESC == i,]))
      TT <- append(TT,nrow(info[info$EVNTDESC == i,]))
      order <- append(order,3)
    }
    #-censored
    factor <- append(factor,"  Censored")
    T1 <-  append(T1,nrow(info[info$CNSR == 1 & info$TRTP == levels(info$TRTP)[2],]))
    T2 <- append(T2,nrow(info[info$CNSR == 1 & info$TRTP == levels(info$TRTP)[1],]))
    TT <- append(TT,nrow(info[info$CNSR == 1,]))
    order <- append(order,2)
    for (i in unique(info[info$CNSR == 1,]$EVNTDESC)) {
      factor <- append(factor,paste("    ",i,sep = " "))
      T1 <- append(T1,nrow(info[info$TRTP == levels(info$TRTP)[2] & info$EVNTDESC == i,]))
      T2 <- append(T2,nrow(info[info$TRTP == levels(info$TRTP)[1] & info$EVNTDESC == i,]))
      TT <- append(TT,nrow(info[info$EVNTDESC == i,]))
      order <- append(order,3)
    }
    
    surv_fit <- survfit2(Surv(AVAL,event) ~ TRTP, data = info)
    fit <- survfit2(Surv(AVAL,event) ~ 1, data = info)
    #quantile
    q_tr <- quantile(surv_fit)
    q <- quantile(fit)
    #25%
    factor <- append(factor,"25% quantile")
    T1 <- append(T1,ifelse(is.na(q_tr$quantile[2,1]),"NE",round(q_tr$quantile[2,1],1)))
    T2 <- append(T2,ifelse(is.na(q_tr$quantile[1,1]),"NE",round(q_tr$quantile[1,1],1)))
    TT <- append(TT,ifelse(is.na(q$quantile[[1]]),"NE",round(q$quantile[[1]],1)))
    order <- append(order,1)
    mtlci <- q_tr$lower[2,1]
    mtuci <- q_tr$upper[2,1]
    mrlci <- q_tr$lower[1,1]
    mruci <- q_tr$upper[1,1]
    ml <- q$lower[[1]]
    mu <- q$upper[[1]]
    factor <- append(factor,"(95% CI)")
    T1 <-  append(T1,sprintf("(%.1f, %.1f)",
                             mtlci,mtuci))
    T2 <- append(T2,sprintf("(%.1f, %.1f)",
                            mrlci,mruci))
    TT <- append(TT,sprintf("(%.1f, %.1f)",
                            ml,mu))
    order <- append(order,2)
    #median
    factor <- append(factor,"Estimate of median (months)")
    T1 <- append(T1,ifelse(is.na(q_tr$quantile[2,2]),"NE",round(q_tr$quantile[2,2],1)))
    T2 <- append(T2,ifelse(is.na(q_tr$quantile[1,2]),"NE",round(q_tr$quantile[1,2],1)))
    TT <- append(TT,ifelse(is.na(q$quantile[[2]]),"NE",round(q$quantile[[2]],1)))
    order <- append(order,1)
    mtlci <- q_tr$lower[2,2]
    mtuci <- q_tr$upper[2,2]
    mrlci <- q_tr$lower[1,2]
    mruci <- q_tr$upper[1,2]
    ml <- q$lower[[2]]
    mu <- q$upper[[2]]
    factor <- append(factor,"(95% CI)")
    T1 <-  append(T1,sprintf("(%.1f, %.1f)",
                             mtlci,mtuci))
    T2 <- append(T2,sprintf("(%.1f, %.1f)",
                            mrlci,mruci))
    TT <- append(TT,sprintf("(%.1f, %.1f)",
                            ml,mu))
    order <- append(order,2)
    #75%
    factor <- append(factor,"75% quantile")
    T1 <- append(T1,ifelse(is.na(q_tr$quantile[2,3]),"NE",round(q_tr$quantile[2,3],1)))
    T2 <- append(T2,ifelse(is.na(q_tr$quantile[1,3]),"NE",round(q_tr$quantile[1,3],1)))
    TT <- append(TT,ifelse(is.na(q$quantile[[3]]),"NE",round(q$quantile[[3]],1)))
    order <- append(order,1)
    
    mtlci <- q_tr$lower[2,3]
    mtuci <- q_tr$upper[2,3]
    mrlci <- q_tr$lower[1,3]
    mruci <- q_tr$upper[1,3]
    ml <- q$lower[[3]]
    mu <- q$upper[[3]]
    factor <- append(factor,"(95% CI)")
    T1 <-  append(T1,sprintf("(%.1f, %.1f)",
                             mtlci,mtuci))
    T2 <- append(T2,sprintf("(%.1f, %.1f)",
                            mrlci,mruci))
    TT <- append(TT,sprintf("(%.1f, %.1f)",
                            ml,mu))
    order <- append(order,2)
    
    #min-max
    factor <- append(factor,"Minimum, Maximum")
    mint <- round(min(info[info$TRTP == levels(info$TRTP)[2],]$AVAL),3)
    maxt <- round(max(info[info$TRTP == levels(info$TRTP)[2],]$AVAL),3)
    minr <- round(min(info[info$TRTP == levels(info$TRTP)[1],]$AVAL),3)
    maxr <- round(max(info[info$TRTP == levels(info$TRTP)[1],]$AVAL),3)
    min <- round(min(info$AVAL),3)
    max <- round(max(info$AVAL),3)
    T1 <-  append(T1,sprintf("%.3f, %.3f",
                             mint,maxt))
    T2 <- append(T2,sprintf("%.3f, %.3f",
                            minr,maxr))
    TT <- append(TT,sprintf("%.3f, %.3f",
                            min,max))
    order <- append(order,1)
    
    #Stratified analysis
    if(!is.null(input$savart)){
      idx = which(colnames(info) %in% input$savart)
      factor <- append(factor,"Stratified analysis")
      T1 <-  append(T1," ")
      T2 <- append(T2," ")
      TT <- append(TT," ")
      order <- append(order,1)
      factor <- append(factor,"  Log-rank p-value (two-sided)")
      T1 <-  append(T1,ifelse(survdiff(Surv(AVAL,event) ~ TRTP + strata(info[,idx]), data = info)$pvalue < 0.0001,"<0.0001",format(round(survdiff(Surv(AVAL,event) ~ TRTP + strata(info[,idx]), data = info)$pvalue,4),nsmall = 4)))
      T2 <- append(T2," ")
      TT <- append(TT," ")
      order <- append(order,2)
      factor <- append(factor,"  Estimate of hazard ratio")
      T1 <-  append(T1,round(exp(coef(coxph(Surv(AVAL,event) ~ TRTP + strata(info[,idx]),data = info,ties = input$tie))),2))
      T2 <- append(T2," ")
      TT <- append(TT," ")
      order <- append(order,2)
      factor <- append(factor,"  (95% CI)")
      ciu <- c(round(exp(confint(coxph(Surv(AVAL,event) ~ TRTP + strata(info[,idx]),data = info,ties = input$tie)))[,2],3))
      cil <- c(round(exp(confint(coxph(Surv(AVAL,event) ~ TRTP + strata(info[,idx]),data = info,ties = input$tie)))[,1],3))
      T1 <-  append(T1,sprintf("(%.3f, %.3f)",
                               cil,ciu))
      T2 <- append(T2," ")
      TT <- append(TT," ")
      order <- append(order,2)
    }
    
    
    #Unstratified analysis
    factor <- append(factor,"Unstratified analysis")
    T1 <-  append(T1," ")
    T2 <- append(T2," ")
    TT <- append(TT," ")
    order <- append(order,1)
    factor <- append(factor,"  Log-rank p-value")
    T1 <-  append(T1,ifelse(survdiff(Surv(AVAL,event) ~ TRTP, data = info)$pvalue < 0.0001,"<0.0001",format(round(survdiff(Surv(AVAL,event) ~ TRTP, data = info)$pvalue,4),nsmall = 4)))
    T2 <- append(T2," ")
    TT <- append(TT," ")
    order <- append(order,2)
    factor <- append(factor,"  Estimate of hazard ratio")
    T1 <-  append(T1,round(exp(coef(coxph(Surv(AVAL,event) ~ TRTP,data = info,ties = input$tie))),2))
    T2 <- append(T2," ")
    TT <- append(TT," ")
    order <- append(order,2)
    factor <- append(factor,"  (95% CI)")
    ciu <- c(round(exp(confint(coxph(Surv(AVAL,event) ~ TRTP,data = info,ties = input$tie)))[,2],3))
    cil <- c(round(exp(confint(coxph(Surv(AVAL,event) ~ TRTP,data = info,ties = input$tie)))[,1],3))
    T1 <-  append(T1,sprintf("(%.3f, %.3f)",
                             cil,ciu))
    T2 <- append(T2," ")
    TT <- append(TT," ")
    order <- append(order,2)
    
    #survival rates
    if(is.na(input$nump) == F && input$nump >0){
      for(i in 1:input$nump){
        if(!is.null(input[[paste("p_",i)]]) && !is.na(input[[paste("p_",i)]])){
          surv_summary = summary(surv_fit,times = input[[paste("p_",i)]])
          fit_summary = summary(fit,times = input[[paste("p_",i)]])
          factor <- append(factor,paste(input[[paste("p_",i)]],"Month Survival Rate"))
          T1 <- append(T1,paste(round(surv_summary$surv[2],3)*100,"%"))
          T2 <- append(T2,paste(round(surv_summary$surv[1],3)*100,"%"))
          TT <- append(TT,paste(round(fit_summary$surv[1],3)*100,"%"))
          order <- append(order,1)
          factor <- append(factor,"  (95% CI)")
          uppert <- surv_summary$upper[2]*100
          upperr <- surv_summary$upper[1]*100
          lowert <- surv_summary$lower[2]*100
          lowerr <- surv_summary$lower[1]*100
          upper <- fit_summary$upper[1]*100
          lower <- fit_summary$lower[2]*100
          T1 <- append(T1,sprintf("(%.1f%%, %.1f%%)",
                                  lowert,uppert))
          T2 <- append(T2,sprintf("(%.1f%%, %.1f%%)",
                                  lowerr,upperr))
          TT <- append(TT,sprintf("(%.1f%%, %.1f%%)",
                                  lower,upper))
          order <- append(order,2)
        }
      }
    }
    
    T1 <- gsub("NA|NA%","NE",T1)
    T2 <- gsub("NA|NA%","NE",T2)
    TT <- gsub("NA|NA%","NE",TT)
    data <- cbind(factor,T1,T2,TT,order)
    colnames(data) <- c("Factor",paste(input$lal," (N=",n1,")",sep = ""),paste(input$ral," (N=",n2,")",sep = ""),paste("Total (N=",n,")",sep = ""),"order")
    rownames(data) <- NULL
    print(data)
    
    data <- as.data.frame(data)
    data
  })
  
  output$dft <-  renderReactable({
    req(!is.null(dfs()))
    dfs <- dfs()
    table <- reactable(dfs,columns = list(
      Factor = colDef(
        style = function(value,index){
          if(dfs$order[index] == "1"){
            fontWeight <- "bold"
            paddingLeft <- "1px"
          }else if(dfs$order[index] == "2"){
            fontWeight <- "plain"
            paddingLeft <- "10px"
          }else{
            fontWeight <- "plain"
            paddingLeft <- "30px"
          }
          list(paddingLeft = paddingLeft,fontWeight = fontWeight)
        }
      ),
      order = colDef(show = F)
    ),pagination = F,height = 600) 
    
    
  })
  
  output$dftable <- downloadHandler(
    filename = "table.xlsx",
    content = function(file){
      t <- dfs() %>%
        select(-order)
      write.xlsx(t,file,firstRow = T,colWidths = "auto")
    }
  )
  ##============Forest Plot================== 
  df <- reactive({
    options(scipen = 999)
    req(input$FRfile0)
    info <- all_info_update()
    var <- names(info)
    lab <- info %>% map(attr_getter("label"))
    
    info$status <- -info$CNSR + 1
    if(input$trgr %in% names(info)){
      info$TRTP <- info[[input$trgr]]
      info$TRTP <- factor(info$TRTP)
      if(length(levels(info$TRTP))>1){
        if(length(input$ref) > 0){
          info$TRTP <- relevel(info$TRTP,input$ref)
        }
      }
      else
        return(NULL)
      
      
    }
    else
      return(NULL)
    
    
    
    
    
    a <- c("Overall")
    b <- c(nrow(info))
    n1t <- c(nrow(info[info$TRTP == levels(info$TRTP)[2],]))
    n2t <- c(nrow(info[info$TRTP == levels(info$TRTP)[1],]))
    b1t <- c(nrow(info[info$status == 1 & info$TRTP == levels(info$TRTP)[2],]))
    b2t <- c(nrow(info[info$status == 1 & info$TRTP == levels(info$TRTP)[1],]))
    c <- c(paste(rep(" ",40),collapse = " "))
    surv_fit <- survfit2(Surv(AVAL,status) ~ TRTP, data = info)
    med_surv <- surv_median(surv_fit)
    mtg <- c(med_surv$median[2])
    mrg <- c(med_surv$median[1])
    mtlci <- c(med_surv$lower[2])
    mtuci <- c(med_surv$upper[2])
    mrlci <- c(med_surv$lower[1])
    mruci <- c(med_surv$upper[1])
    if(input$saf == T){
      req(input$savar)
      savar = which(colnames(info) %in% input$savar)
      hr <- c(round(exp(coef(coxph(Surv(AVAL,status) ~ TRTP + strata(info[,savar]),data = info,ties = input$tie))),2))
      ciu <- c(round(exp(confint(coxph(Surv(AVAL,status) ~ TRTP + strata(info[,savar]),data = info,ties = input$tie)))[,2],3))
      cil <- c(round(exp(confint(coxph(Surv(AVAL,status) ~ TRTP + strata(info[,savar]),data = info,ties = input$tie)))[,1],3))
      p_v <- c(ifelse(survdiff(Surv(AVAL,status) ~ TRTP + strata(info[,savar]), data = info)$pvalue < 0.0001,"<0.0001",format(round(survdiff(Surv(AVAL,status) ~ TRTP + strata(info[,savar]), data = info)$pvalue,4),nsmall = 4)))
    }else{
      hr <- c(round(exp(coef(coxph(Surv(AVAL,status) ~ TRTP,data = info,ties = input$tie))),2))
      ciu <- c(round(exp(confint(coxph(Surv(AVAL,status) ~ TRTP,data = info,ties = input$tie)))[,2],3))
      cil <- c(round(exp(confint(coxph(Surv(AVAL,status) ~ TRTP,data = info,ties = input$tie)))[,1],3))
      p_v <- c(ifelse(survdiff(Surv(AVAL,status) ~ TRTP, data = info)$pvalue < 0.0001,"<0.0001",format(round(survdiff(Surv(AVAL,status) ~ TRTP, data = info)$pvalue,4),nsmall = 4)))
      
    }
    p_v_i <- c("")
    a1 <- c("Overall")
    a2 <- c("overall")
    if(!is.null(input$cinfile)){
      file <- input$cinfile
      var <-  read.csv(file$datapath,sep = ",",header = TRUE)
      var$level <- ifelse(is.na(var$level),"NA",var$level)
      for(i in unlist(unique(var$subgroup))){
        a <- append(a,var[var$subgroup == i,]$subgroup_edit[1])
        b <- append(b,"")
        n1t <- append(n1t,"")
        n2t <- append(n2t,"")
        b1t <- append(b1t,"")
        b2t <- append(b2t,"")
        c <- append(c,paste(rep(" ",40),collapse = " "))
        mtg <- append(mtg,"")
        mrg <- append(mrg,"")
        mtlci <- append(mtlci,"")
        mtuci <- append(mtuci,"")
        mrlci <- append(mrlci,"")
        mruci <- append(mruci,"")
        hr <- append(hr,"")
        ciu <- append(ciu,"")
        cil <- append(cil,"")
        p_v <- append(p_v,"")
        info1 <- info[info[[i]] %in% as.vector(var[var$subgroup == i,]$level),]
        p_v_i <- append(p_v_i,ifelse(summary(coxph(Surv(AVAL,status) ~ TRTP + info1[[i]] + TRTP*info1[[i]],data = info1))$coefficients[3,5] < 0.0001,"<0.0001",format(round(summary(coxph(Surv(AVAL,status) ~ TRTP + info1[[i]] + TRTP*info1[[i]],data = info1))$coefficients[3,5],4),nsmall = 4)))
        a1 <- append(a1,var[var$subgroup == i,]$subgroup_edit[1])
        a2 <- append(a2,"subgroup")
        for(k in var[var$subgroup == i,]$level){
          if(is.na(k)){
            
            k = "NA"
          }
          a <- append(a,var[var$level == k & var$subgroup == i,]$level_edit)
          b <- append(b,sum(info[i] == k))
          n1t <- append(n1t,nrow(info[info$TRTP == levels(info$TRTP)[2] & info[[i]] == k,]))
          n2t <- append(n2t,nrow(info[info$TRTP == levels(info$TRTP)[1] & info[[i]] == k,]))
          b1t <- append(b1t,nrow(info[info$status == 1 & info$TRTP == levels(info$TRTP)[2] & info[[i]] == k,]))
          b2t <- append(b2t,nrow(info[info$status == 1 & info$TRTP == levels(info$TRTP)[1] & info[[i]] == k,]))
          c <- append(c,paste(rep(" ",40),collapse = " "))
          surv_fit <- survfit2(Surv(AVAL,status) ~ TRTP, data = info[info[[i]] == k,])
          med_surv <- surv_median(surv_fit)
          mtg <- append(mtg,med_surv$median[2])
          mrg <- append(mrg,med_surv$median[1])
          mtlci <- append(mtlci,med_surv$lower[2])
          mtuci <- append(mtuci,med_surv$upper[2])
          mrlci <- append(mrlci,med_surv$lower[1])
          mruci <- append(mruci,med_surv$upper[1])
          hr <- append(hr,round(exp(coef((coxph(Surv(AVAL,status)~TRTP,data = info[info[[i]] == k,],ties = input$tie)))),3))
          ciu <- append(ciu,round(exp(confint((coxph(Surv(AVAL,status) ~ TRTP,data = info[info[[i]] == k,],ties = input$tie)))[,2]),3))
          cil <- append(cil,round(exp(confint((coxph(Surv(AVAL,status) ~ TRTP,data = info[info[[i]] == k,],ties = input$tie)))[,1]),3))
          p_v <- append(p_v,ifelse(survdiff(Surv(AVAL,status) ~ TRTP, data = info[info[[i]] == k,])$pvalue < 0.0001,"<0.0001",format(round(survdiff(Surv(AVAL,status) ~ TRTP, data = info[info[[i]] == k,])$pvalue,4),nsmall = 4)))
          p_v_i <- append(p_v_i,"")
          a1 <- append(a1,paste(var[var$subgroup == i,]$subgroup_edit[1],"_",var[var$level == k & var$subgroup == i,]$level_edit))
          a2 <- append(a2,"level")
          
        }
      }
    }
    else{
      for(i in input$selected){
        idx = which(var == i)
        a <- append(a,lab[[idx]])
        
        b <- append(b,"")
        n1t <- append(n1t,"")
        n2t <- append(n2t,"")
        b1t <- append(b1t,"")
        b2t <- append(b2t,"")
        c <- append(c,paste(rep(" ",40),collapse = " "))
        mtg <- append(mtg,"")
        mrg <- append(mrg,"")
        mtlci <- append(mtlci,"")
        mtuci <- append(mtuci,"")
        mrlci <- append(mrlci,"")
        mruci <- append(mruci,"")
        hr <- append(hr,"")
        ciu <- append(ciu,"")
        cil <- append(cil,"")
        p_v <- append(p_v,"")
        req(input[[paste("cta_",i)]])
        if(length(input[[paste("cta_",i)]])>1){
          info1 <- info[info[[i]] %in% as.vector(input[[paste("cta_",i)]]),]
        }else{
          info1 <- info
        }
        p_v_i <- append(p_v_i,ifelse(summary(coxph(Surv(AVAL,status) ~ TRTP + info1[[i]] + TRTP*info1[[i]],data = info1))$coefficients[3,5] < 0.0001,"<0.0001",format(round(summary(coxph(Surv(AVAL,status) ~ TRTP + info1[[i]] + TRTP*info1[[i]],data = info1))$coefficients[3,5],4),nsmall = 4)))
        a1 <- append(a1,lab[[idx]])
        a2 <- append(a2,"subgroup")
        for(k in input[[paste("cta_",i)]]){
          a <- append(a,k)
          b <- append(b,sum(info[i] == k))
          n1t <- append(n1t,nrow(info[info$TRTP == levels(info$TRTP)[2] & info[[i]] == k,]))
          n2t <- append(n2t,nrow(info[info$TRTP == levels(info$TRTP)[1] & info[[i]] == k,]))
          b1t <- append(b1t,nrow(info[info$status == 1 & info$TRTP == levels(info$TRTP)[2] & info[[i]] == k,]))
          b2t <- append(b2t,nrow(info[info$status == 1 & info$TRTP == levels(info$TRTP)[1] & info[[i]] == k,]))
          c <- append(c,paste(rep(" ",40),collapse = " "))
          surv_fit <- survfit2(Surv(AVAL,status) ~ TRTP, data = info[info[[i]] == k,])
          med_surv <- surv_median(surv_fit)
          mtg <- append(mtg,med_surv$median[2])
          mrg <- append(mrg,med_surv$median[1])
          mtlci <- append(mtlci,med_surv$lower[2])
          mtuci <- append(mtuci,med_surv$upper[2])
          mrlci <- append(mrlci,med_surv$lower[1])
          mruci <- append(mruci,med_surv$upper[1])
          hr <- append(hr,round(exp(coef(coxph(Surv(AVAL,status)~TRTP,data = info[info[[i]] == k,],ties = input$tie))),4))
          ciu <- append(ciu,round(exp(confint(coxph(Surv(AVAL,status) ~ TRTP,data = info[info[[i]] == k,],ties = input$tie))[,2]),3))
          cil <- append(cil,round(exp(confint(coxph(Surv(AVAL,status) ~ TRTP,data = info[info[[i]] == k,],ties = input$tie))[,1]),3))
          p_v <- append(p_v,ifelse(survdiff(Surv(AVAL,status) ~ TRTP, data = info[info[[i]] == k,])$pvalue < 0.0001,"<0.0001",format(round(survdiff(Surv(AVAL,status) ~ TRTP, data = info[info[[i]] == k,])$pvalue,4),nsmall = 4)))
          p_v_i <- append(p_v_i,"")
          a1 <- append(a1,lab[[idx]])
          a2 <- append(a2,"level")
        }
      }
    }
    
    
    data <- data.frame(Subgroup = a, "No. of Patient" = b, "Treatment_Event" = as.numeric(b1t), "Reference_Event" = as.numeric(b2t), "Treat_N" = as.numeric(n1t),
                       "Refer_N" = as.numeric(n2t)," " = c, mtg = round(as.numeric(mtg),1), mrg = round(as.numeric(mrg),1),
                       mtlci = round(as.numeric(mtlci),1), mtuci = round(as.numeric(mtuci),1), mrlci = round(as.numeric(mrlci),1), 
                       mruci = round(as.numeric(mruci),1),
                       HR = as.numeric(hr), 
                       lower = round(as.numeric(cil),3), upper = round(as.numeric(ciu),3), 
                       "P Value" = p_v, "P Interaction" = p_v_i,sub1 = a1)
    data$"Hazard Ratio (95% CI)" = ifelse(is.na(data$HR),"",sprintf("%.2f (%.3f, %.3f)",
                                                                    data$HR,data$lower,data$upper)) ##nchar()=19
    data$"TreatmentGroup Median" = ifelse(is.na(data$HR),"",sprintf("%.1f (%.1f, %.1f)",
                                                                    data$mtg,data$mtlci,data$mtuci))
    data$"TreatmentGroup Median" = gsub("NA","NE",data$"TreatmentGroup Median")
    data$"ReferenceGroup Median" = ifelse(is.na(data$HR),"",sprintf("%.1f (%.1f, %.1f)",
                                                                    data$mrg,data$mrlci,data$mruci))
    data$"ReferenceGroup Median" = gsub("NA","NE",data$"ReferenceGroup Median")
    data$"Type" = a2
    data$"Treatment Event/N" = ifelse(is.na(data$HR),"",paste(data$"Treatment_Event","/",data$"Treat_N"))
    data$"Reference Event/N" = ifelse(is.na(data$HR),"",paste(data$"Reference_Event","/",data$"Refer_N"))
    
    
    
    if(min(data$lower,na.rm = T) <= 0.25){
      le <- 0.125
    }
    else if(0.25 < min(data$lower,na.rm = T) && min(data$lower,na.rm = T) <= 0.5){
      le <- 0.25
    }
    else if(min(data$lower,na.rm = T) < 0.125){
      le <- 0.0625
    }
    else{
      le <- 0.5
    }
    
    if(4 < max(data$upper,na.rm = T) && max(data$upper,na.rm = T) <= 8){
      re <- 8
    }
    else if(2 < max(data$upper,na.rm = T) && max(data$upper,na.rm = T) <= 4){
      re <- 4
    }
    else{
      re <- 2
    }
    updateNumericInput(session = getDefaultReactiveDomain(),"leed",value = le)
    updateNumericInput(session = getDefaultReactiveDomain(),"ried",value = re)
    colnames(data)<-c("Subgroup","No. of Patient","Treatment_Event","Reference_Event",
                      "Treat_N","Refer_N"," ","Treatment Median","Reference Median","Treatment Lower",
                      "Treatment Upper","Reference Lower","Reference Upper","HR","lower","upper",
                      "P Value","P Interaction","sub1","HR (95% CI)",input$lal,input$ral,"Type",
                      paste(input$lal,"E/N"),paste(input$ral,"E/N"))
    data
    #  data$"Treatment Group Median"
  })
  
  cha_df <- reactiveVal()
  observeEvent(df(),{
    req(!is.null(df()))
    data <- df()
    cha_df(data)
    
  })
  
  observeEvent(input$chvar,{
    if(!is.null(input$FRfile0)){
      df <- df()
      output$chname <- renderUI({
        tagList(
          textInput("name",label = paste0("Label for ",input$chvar),value = input$chvar,width="70%"),
          checkboxInput("keep",label = paste("Omit the row of ",input$chvar),value = F),
          map(df[df$sub1 == input$chvar & df$Type == "level",]$Subgroup,~textInput(paste("name_",input$chvar,.),label = paste0("Label for ",.),value = .)),
          actionButton("up_sg","Update")
        )
      })
      
    }
  })
  
  observeEvent(input$up_sg,{
    req(!is.null(cha_df()))
    updated_df = cha_df()
    updated_df$Subgroup[updated_df$sub1 == input$chvar & updated_df$Type == "subgroup"] <- input$name
    if(input$keep == T){
      updated_df <- subset(updated_df,Subgroup != input$chvar)
    }
    for(k in updated_df$Subgroup[updated_df$sub1 == input$chvar & updated_df$Type == "level"]){
      if(length(input[[paste("name_",input$chvar,k)]]) != 0){
        updated_df$Subgroup[updated_df$sub1 == input$chvar & updated_df$Subgroup == k] <- input[[paste("name_",input$chvar,k)]]
      }
    }
    cha_df(updated_df)
  })
  
  cdata <-  reactive({
    
    info <- cha_df()
    sub <- info$Subgroup[info$Type == "subgroup"]
    lel <- info$Subgroup[info$Type == "level"]
    print(sub)
    subgroup <- c()
    level <- c()
    subgroup_edit <- c()
    level_edit <- c()
    n=1
    m=1
    for(i in input$selected){
      for(k in input[[paste("cta_",i)]]){
        subgroup <- append(subgroup,i)
        level <- append(level,k)
        subgroup_edit <- append(subgroup_edit,sub[n])
        level_edit <- append(level_edit,lel[m])
        m = m+1
      }
      n = n+1
    }
    
    cdata <- data.frame(subgroup,subgroup_edit,level,level_edit)
    cdata
  })
  
  output$cfile <- downloadHandler(
    filename = function() {
      paste("Configuration File", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(cdata(),file,row.names = F)
    }
  )
  
  output$ch <- renderUI({
    if(is.null(input$FRfile0))
      return(NULL)
    df <- df()
    selectizeInput("chvar",label = "Select the subgroup that need adjustment:",choices= unique(df$sub1))
  })
  
  
  
  
  output$NoP <-  renderTable({
    if(is.null(input$FRfile0))
      return(NULL)
    data <- cha_df()
    if(input$pool == T){
      if(input$cin == T){
        col = c(1,2,7,21,22,20,17,18)
      }
      else{
        col = c(1,2,7,21,22,20,17)
      }
    }
    else{
      if(input$cin == T){
        col = c(1,24,25,7,21,22,20,17,18)
      }
      else{
        col = c(1,24,25,7,21,22,20,17)
      }
    }
    data <- data[,col]
    data
  })
  
  output$FRtable <- downloadHandler(
    filename = function() {
      paste("ForestPlot_Table", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data <- cha_df()[,c(1:6,8:18,23)]
      data$Parameter <- input$frpara
      write.csv(data,file,row.names = F)
    }
  )
  
  output$tick <- renderUI({
    if(is.na(input$numt)){     
      return(NULL)
    }
    map(1:input$numt,~numericInput(paste("t_",.x),paste("Enter the value of tick ",.x),value = NULL))
  })
  
  format_num <- function(x){
    if(is.na(x)){
      return(" ")
    }
    else if(substr(x,1,1) == "<"){
      return(x)
    }  
    else{
      return(sprintf("%.4f",as.numeric(x)))
    }
  }
  
  
  
  get_scale <- function(plot,width_wanted,height_wanted,unit = "in"){
    h <- convertHeight(sum(plot$heights),unit,TRUE)
    w <- convertWidth(sum(plot$widths),unit,TRUE)
    max(c(w/width_wanted,h/height_wanted))
  }
  
  frtable <- reactive({
    req(input$FRtable)
    file <- input$FRtable
    df <- read.csv(file$datapath,sep = ",",header = TRUE,check.names = F,stringsAsFactors = F)
    df[,1] <- ifelse(df[,ncol(df)]=="level",paste(paste(rep(" ",input$nbk),collapse = ""),df[,1]),df[,1])
    df[,2] <- ifelse(is.na(df[,2]),"",df[,2])
    df[,16] <- ifelse(is.na(df[,16]),"",df[,16])
    df[,17] <- ifelse(is.na(df[,17]),"",df[,17])
    df$"HR (95% CI)" = ifelse(is.na(df$HR),"",sprintf("%.2f (%.3f, %.3f)",
                                                      df$HR,df$lower,df$upper))
    df$"TreatmentGroup Median" = ifelse(is.na(df$HR),"",sprintf("%.1f (%.1f, %.1f)",
                                                                df$"Treatment Median",df$"Treatment Lower",df$"Treatment Upper"))
    df$"TreatmentGroup Median" = gsub("NA","NE",df$"TreatmentGroup Median")
    df$"ReferenceGroup Median" = ifelse(is.na(df$HR),"",sprintf("%.1f (%.1f, %.1f)",
                                                                df$"Reference Median",df$"Reference Lower",df$"Reference Upper"))
    df$"ReferenceGroup Median" = gsub("NA","NE",df$"ReferenceGroup Median")
    df$"Treatment Event/N" = ifelse(is.na(df$HR),"",paste(df$"Treatment_Event","/",df$"Treat_N"))
    df$"Reference Event/N" = ifelse(is.na(df$HR),"",paste(df$"Reference_Event","/",df$"Refer_N"))
    df[,25] <- paste(rep(" ",35),collapse = " ")
    
    colnames(df)<-c("Subgroup","No. of Patient","Treatment_Event","Reference_Event",
                    "Treat_N","Refer_N","Treatment Median","Reference Median","Treatment Lower",
                    "Treatment Upper","Reference Lower","Reference Upper","HR","lower","upper",
                    "P Value","P Interaction","Type","Parameter","Hazard Ratio(95% CI)",input$lal,input$ral,
                    paste(input$lal,"E/N"),paste(input$ral,"E/N")," ")
    df
    
  })
  
  output$frcol <- renderUI({
    if(!is.null(input$FRfile0)){
      if(!is.null(cha_df())){
        df <- cha_df()
        col <- c(1,7)
        if(input$pool == T){
          col <- append(col,2)
        }else{
          col <- append(col,c(24,25))
        }
        if(input$med == T){
          col <- append(col,c(21,22))
        }
        if(input$hr == T){
          col <- append(col,20)
        }
        if(input$ctr == T){
          col <- append(col,17)
        }
        if(input$cin == T){
          col <- append(col,18)
        }
        val = colnames(df[,col])
        print(val)
        rank_list(
          labels = val,
          input_id = "fr_col")
      }
    }else if(!is.null(input$FRtable)){
      df <- frtable()
      col <- c(1,25)
      if(input$pool == T){
        col <- append(col,2)
      }else{
        col <- append(col,c(23,24))
      }
      if(input$med == T){
        col <- append(col,c(21,22))
      }
      if(input$hr == T){
        col <- append(col,20)
      }
      if(input$ctr == T){
        col <- append(col,16)
      }
      if(input$cin == T){
        col <- append(col,17)
      }
      val = colnames(df[,col])
      print(val)
      rank_list(
        labels = val,
        input_id = "fr_col")
    }
  })
##-----------draw forest polt----------------  
  frp <- reactive({
    tm <- forest_theme(base_size = input$frf,
                       base_family = "serif",
                       ci_pch = as.numeric(input$shape),
                       ci_Theight = 0.2,
                       ci_lwd = 1.5,
                       refline_lwd = 1.5,
                       refline_lty = 3,
                       arrow_lwd = 1.5,
                       xaxis_lwd = 1.5,
                       core=list(fg_params = list(vjust = 0.5),
                                 bg_params = list(fill = c("lightgrey","white"))))
    
    
    if(!is.null(input$FRfile0)){
      if(!is.null(cha_df())){
        df <- cha_df()
        cicol = 3
        col = c(1,2,7,21,22,20,17,18)
        if(input$pool == T){
          mtcol = 4
          mrcol = 5
        }else{
          mtcol = 5
          mrcol = 6
        }
        
        ntcol = 2
        nrcol = 3


        df$Subgroup[df$Type == "level"] = paste(paste(rep(" ",input$nbk),collapse = ""),df$Subgroup[df$Type == "level"])
        req(input$leed)
        req(input$ried)
        if(!is.null(input$fr_col)){
          val = as.vector(input$fr_col)
          req(val %in% colnames(df))
          b = which(val == "")
          val[b] <- " "
          col = c()
          for (i in val) {
            col = append(col,which(colnames(df) == i))
          }
          cicol = which(val == " ")
          mtcol = which(val == input$lal)
          mrcol = which(val == input$ral)
          ntcol = which(val == paste(input$lal,"E/N"))
          nrcol = which(val == paste(input$ral,"E/N"))
          
        }
        g <- forest(df[,col],
                    est = df$HR,
                    lower = df$lower,
                    upper = df$upper,
                    sizes = 0.5,
                    ci_column = cicol,
                    ref_line = 1,
                    arrow_lab = c(paste(input$lal,"Better"),paste(input$ral,"Better")),
                    xlim = c(input$leed,input$ried),
                    x_trans = "log",
                    ticks_at = c(0.0625,0.125,0.25,0.5,1,2,4,8),
                    theme = tm
        )
        g <- edit_plot(g,col = c(2:length(col)),which = "text",hjust=unit(0.5,"npc"),x=unit(0.5,"npc"))
        

        if(input$med == T){
          req(length(mtcol) != 0)
          req(length(mrcol) != 0)
          g <- insert_text(g,text = "Median (95% CI) (Months)",col = mtcol:mrcol,part = "header",just="center",gp=gpar(fontface = "bold",fontsize = input$frf,fontfamily = "serif"))
        }
        
        if(input$pool == F){
          req(length(ntcol) != 0)
          req(length(nrcol) != 0)
          if(input$med == T){
            g <- add_text(g,text = "Number of events/N",row = 1,col = ntcol:nrcol,part = "header",just="center",gp=gpar(fontface = "bold",fontsize = input$frf,fontfamily = "serif"))
          }else{
            g <- insert_text(g,text = "Number of events/N",col = ntcol:nrcol,part = "header",just="center",gp=gpar(fontface = "bold",fontsize = input$frf,fontfamily = "serif"))
          }
        }
        
        g <- insert_text(g,
                         text = "",
                         row = nrow(df)+1,
                         part = "body",
                         gp = gpar(fontsize = 2))
        
        g
      }
      else{
        return(NULL)
      }
    }
    else if(!is.null(input$FRtable)){
      df <- frtable()
      if(min(df$lower,na.rm = T) <= 0.25){
        le <- 0.125
      }
      else if(0.25 < min(df$lower,na.rm = T) && min(df$lower,na.rm = T) <= 0.5){
        le <- 0.25
      }
      
      else{
        le <- 0.5
      }
      
      if(4 < max(df$upper,na.rm = T)){
        re <- 8
      }
      else if(2 < max(df$upper,na.rm = T) && max(df$upper,na.rm = T) <= 4){
        re <- 4
      }
      else{
        re <- 2
      }
      if(!is.na(input$leed)){
        print(input$leed)
        le <- input$leed
      }
      if(!is.na(input$ried)){
        re <- input$ried
      }
      cicol = 3
      col = c(1,2,25,21,22,20,16,17)
      if(input$pool == T){
        mtcol = 4
        mrcol = 5
      }else{
        mtcol = 5
        mrcol = 6
      }
      
      ntcol = 2
      nrcol = 3
      
      
      if(!is.null(input$fr_col)){
        val = as.vector(input$fr_col)
        print(val)
        b = which(val == "")
        val[b] <- " "
        col = c()
        for (i in val) {
          col = append(col,which(colnames(df) == i))
        }
        cicol = which(val == " ")
        mtcol = which(val == input$lal)
        mrcol = which(val == input$ral)
        ntcol = which(val == paste(input$lal,"E/N"))
        nrcol = which(val == paste(input$ral,"E/N"))
        
      }
      g <- forest(df[,col],
                  est = df$HR,
                  lower = df$lower,
                  upper = df$upper,
                  ci_column = cicol,
                  ref_line = 1,
                  arrow_lab = c(paste(input$lal,"Better"),paste(input$ral,"Better")),
                  xlim = c(le,re),
                  x_trans = "log",
                  ticks_at = c(0.0625,0.125,0.25,0.5,1,2,4,8),
                  theme = tm)
      g <- edit_plot(g,col = c(2:length(col)),which = "text",hjust=unit(0.5,"npc"),x=unit(0.5,"npc"))
      
      

      if(input$med == T){
        req(length(mtcol) != 0)
        req(length(mrcol) != 0)
        g <- insert_text(g,text = "Median (95% CI) (Months)",col = mtcol:mrcol,part = "header",just="center",gp=gpar(fontface = "bold",fontsize = input$frf,fontfamily = "serif"))
      }

      if(input$pool == F){
        req(length(ntcol) != 0)
        req(length(nrcol) != 0)
        if(input$med == T){
          g <- add_text(g,text = "Number of events/N",row = 1,col = ntcol:nrcol,part = "header",just="center",gp=gpar(fontface = "bold",fontsize = input$frf,fontfamily = "serif"))
        }else{
          g <- insert_text(g,text = "Number of events/N",col = ntcol:nrcol,part = "header",just="center",gp=gpar(fontface = "bold",fontsize = input$frf,fontfamily = "serif"))
        }
      }
      print(unique(df$Parameter))
      if(length(unique(df$Parameter))>1){
        n = 1
        s = table(df$Parameter)
        print(s)
        for(i in unique(df$Parameter)){
          g <- insert_text(g,
                           text = "",
                           row = n,
                           part = "body",
                           gp = gpar(fontsize = 2))
          n = n+1
          g <- insert_text(g,
                           text = i,
                           row = n,
                           part = "body",
                           just = "left",
                           gp = gpar(fontface = "bold",fontsize = input$frf,fontfamily = "serif"))
          n = n+s[i]+1
        }
        g <- insert_text(g,
                         text = "",
                         row = nrow(df)+2*length(unique(df$Parameter))+1,
                         part = "body",
                         gp = gpar(fontsize = 2))

      }else{
        g <- insert_text(g,
                         text = "",
                         row = nrow(df)+1,
                         part = "body",
                         gp = gpar(fontsize = 2))        
      }

      g
    }
    else{
      return(NULL)
    }
  })
  
  
  output$FRPlot <- renderPlot({
    if(is.null(frp()))
      return(NULL)
    frp()
  },res = 80)
  
  output$frd <- downloadHandler(
    filename = function(){
      paste("forestplot",input$frdltype,sep = ".")
    },
    content = function(file){
      if(input$frdltype == "pptx"){
        doc = read_pptx()
        doc <- add_slide(doc,"Blank", "Office Theme")
        p <- frp()
        p <- dml(ggobj = p)
        wh <- get_wh(frp(),unit = "in")
        doc <- ph_with(doc,value = p,location = ph_location(width = wh[1],height= wh[2]))
        print(doc,target = file)
      }else{
        p_sc <- get_scale(plot = frp(),width_wanted = 13,height_wanted = 8,unit = "in")
        wh <- get_wh(frp(),unit = "in")
        ggsave(file,plot = frp(),device = input$frdltype,width = wh[1],height= wh[2],units = "in")

      }
      
    }
  )
  
 
  

  
}

shinyApp(ui,server)