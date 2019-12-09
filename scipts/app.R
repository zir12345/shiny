source("libraries.R")

options(shiny.host = '127.0.0.1')
options(shiny.port = 3838)

url_5000 <- "http://localhost:5000"
url_8080 <- "http://localhost:8080"

######################################################################### SERVER
server <- function(input, output, session) {
  
  ##### --- Use Case Library
  #py_run_file()
  output$menuLame1 <- renderMenu({
    sidebarMenu(
      menuItem("Prototype 5000", icon = icon("fas fa-robot"), 
               href = url_5000)
    )
  })
  
  output$menuLame2 <- renderMenu({
    sidebarMenu(
      menuItem("Prototype 8080", icon = icon("fas fa-robot"), 
               href = url_8080)
    )
  })
  
  output$colorPlotGer18 <- renderPlot({
    #data <- dataBTO %>% group_by(COLOR_GROUP)
    ggplot(grouped_data_Ger_18, aes(fill=grouped_data_Ger_18[,1], y=VEHICLE_ID_17, x=grouped_data_Ger_18[,3])) + geom_bar(position="fill", stat="identity") + scale_fill_manual(values=c("black", "blue","brown","grey","orange","red","pink","white"))+ labs(x = "Month", y = "% Share", fill = "Legend") + ggtitle("2018")
  })
  
  output$colorPlotGer19 <- renderPlot({
    #data <- dataBTO %>% group_by(COLOR_GROUP)
    ggplot(grouped_data_Ger_19, aes(fill=grouped_data_Ger_19[,1], y=VEHICLE_ID_17, x=grouped_data_Ger_19[,3])) + geom_bar(position="fill", stat="identity") + scale_fill_manual(values=c("black", "blue","brown","grey","orange","red","pink","white"))+ labs(x = "Month", y = "% Share", fill = "Legend") + ggtitle("2019")
  })
  
  output$colorGER <- DT::renderDataTable(colorGER)
  
  observeEvent(input$columns,{
    cols <- as.numeric(input$columns)
    if(length(input$columns) == 1){
      df <- data.frame(colorGER[,cols])
      names(df) <- names(colorGER)[cols]
      output$colorGER = renderDataTable(df)
      
    }else{
      output$colorGER = renderDataTable(colorGER[,cols])
      
    }
    
  })
  
  output$colorPlotMea18 <- renderPlot({
    #data <- dataBTO %>% group_by(COLOR_GROUP)
    ggplot(grouped_data_Mea_18, aes(fill=grouped_data_Mea_18[,1], y=VEHICLE_ID_17, x=grouped_data_Mea_18[,3])) + geom_bar(position="fill", stat="identity") + scale_fill_manual(values=c("black", "blue","brown","grey","orange","red","pink","white"))+ labs(x = "Month", y = "% Share", fill = "Legend") + ggtitle("2018")
  })
  
  output$colorPlotMea19 <- renderPlot({
    #data <- dataBTO %>% group_by(COLOR_GROUP)
    ggplot(grouped_data_Mea_19, aes(fill=grouped_data_Mea_19[,1], y=VEHICLE_ID_17, x=grouped_data_Mea_19[,3])) + geom_bar(position="fill", stat="identity") + scale_fill_manual(values=c("black", "blue","brown","grey","orange","red","pink","white"))+ labs(x = "Month", y = "% Share", fill = "Legend") + ggtitle("2019")
  })
  
  output$colorMEA <- DT::renderDataTable(colorMEA)
  
  observeEvent(input$columns,{
    cols <- as.numeric(input$columns)
    if(length(input$columns) == 1){
      df <- data.frame(colorMEA[,cols])
      names(df) <- names(colorMEA)[cols]
      output$colorMEA = renderDataTable(df)
      
    }else{
      output$colorMEA = renderDataTable(colorMEA[,cols])
      
    }
    
  })
  
  output$effectivity <- DT::renderDataTable(h)
  
  ##### --- Predictive Analytics - Model Library
  output$asso <- DT::renderDataTable(e)
  
  ##### --- Instagram
  
  output$insta <- DT::renderDataTable(c)
  
  observeEvent(input$columns,{
    cols <- as.numeric(input$columns)
    if(length(input$columns) == 1){
      df <- data.frame(insta[,cols])
      names(df) <- names(insta)[cols]
      output$insta = renderDataTable(df)
      
    }else{
      output$c = renderDataTable(insta[,cols])
      
    }
    
  })

  output$instaColor <- DT::renderDataTable(instaColor)
  output$downloadinstaColor <- downloadHandler(
    filename = function() { 
      paste("instaColor-dataset-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(instaColor, file)
    })
  
  output$irrWords <- DT::renderDataTable(d)
  
  output$colorPlotInsta18 <- renderPlot({
    #data <- dataBTO %>% group_by(COLOR_GROUP)
    ggplot(grouped_data_Insta_18, aes(fill=grouped_data_Insta_18[,1], y=like_count, x=grouped_data_Insta_18[,3])) + geom_bar(position="fill", stat="identity") + scale_fill_manual(values=c("black", "blue","brown","grey","orange","red","pink","white"))+ labs(x = "Month", y = "% Share", fill = "Legend") + ggtitle("2018")
  })

  ##### --- LAVAP ADMINISTRATION AND MONITORING
  output$loggedU <- renderPlotly({y <- i
  
  y <- y %>% filter(year == input$loggedUYear & month == input$loggedUMonth)
  p <- plot_ly(y, x = ~time, y = ~max_user, name = 'max_user', type = 'scatter', mode = 'lines', fillcolor='rgba(103,133,193,0.2)',line = list(color = 'rgb(103,133,193)', width = 0.5),fill='tozeroy') %>%
    add_trace(y = ~min_user, name = 'min_user', mode = 'lines',fillcolor='rgba(0,128,177,0.5)',line = list(color = 'rgb(103,133,193)', width = 0.01)) %>%
    layout(title = "",
           paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
           xaxis = list(title = "months",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE),
           yaxis = list(title = "logged users",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE))
  p
  })
  
  output$memo <- renderPlotly({y <- i
  
  y <- y %>% filter(year == input$loggedUYear & month == input$loggedUMonth)
  p <- plot_ly(y, x = ~time, y = ~max_used_memory, name = 'max % used memory', type = 'scatter', mode = 'lines', fillcolor='rgba(230,182,0,0.2)',line = list(color = 'rgb(230,182,0)', width = 0.5),fill='tozeroy') %>%
    add_trace(y = ~min_used_memory, name = 'min % used memory', mode = 'lines',fillcolor='rgba(188,67,40,0.5)',line = list(color = 'rgb(188,67,40)', width = 0.01)) %>%
    layout(title = "",
           paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
           xaxis = list(title = "months",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE),
           yaxis = list(title = "% used memory",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE))
  p
  })
  
  output$disk <- renderPlotly({y <- i
  
  y <- y %>% filter(year == input$loggedUYear & month == input$loggedUMonth)
  p <- plot_ly(y, x = ~time, y = ~max_used_diskspace, name = 'max % disk space', type = 'scatter', mode = 'lines', fillcolor='rgba(146,208,80,0.2)',line = list(color = 'rgb(146,208,80)', width = 0.5),fill='tozeroy') %>%
    add_trace(y = ~min_used_diskspace, name = 'min % disk space', mode = 'lines',fillcolor='rgba(0,176,80,0.5)',line = list(color = 'rgb(0,176,80)', width = 0.01)) %>%
    layout(title = "",
           paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
           xaxis = list(title = "months",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE),
           yaxis = list(title = "% used diskspace",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE))
  p
  })
  
  output$data2 <- DT::renderDataTable(i) 
  
  #output$UID <- renderText({
  #  req(input$go)
  #  isolate(input$userid)
  #})
  
  #output$PW <- renderText({
  #  req(input$go)
  #  isolate(input$password)
  #})
  
  output$DBList <- DT::renderDataTable(j) 
} # closing server

######################################################################### UI
ui <- fluidPage(
    list(tags$head(HTML('<link rel="icon", href="MyIcon.png", type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="My Window Title"
        )
    ),
navbarPage(title=div(img(src="NTTDLogo.jpg", width = "15%"), "A.I. Booklet", p("Powered by CC Business Analytics", style = "font-size:50%")), theme = shinytheme("paper"),
                ##### --- Use Case Library
                tabPanel("Use Case Library", 
                         navbarPage(title = "",
                         tabPanel("DIS.A.STEER - LA.M.E",
                                  dashboardPage(skin = "black",
                                  dashboardHeader(title = "LA.M.E."),
                                    dashboardSidebar(
                                      sidebarMenu(id = 'sidebarmenuLame',
                                      menuItem("Prototype", tabName ="ProtoLame", icon = icon("fas fa-robot"),
#output$menuLame1
#output$menuLame2
                                      sidebarMenuOutput("menuLame1"),
                                      sidebarMenuOutput("menuLame2")),
                                      
                                      menuItem("Explorative Data Analysis", tabName = "EDA", icon = icon("fas fa-search"),
                                        menuItem('IVS-R', tabName = 'IVSR', icon = icon('line-chart')),
                                        menuItem('Other', tabName = 'otherLame', icon = icon('line-chart'))
                                      )
                                      )
                                    ),

                                    dashboardBody(
                                      tabItems(
                                        tabItem(tabName = "IVSR",
                                        navbarPage(title = "",
                                          tabPanel("Feature Exploration",
                                                   fluidPage(title = "",
                                                             fluidRow(h4("Feature Sum Distribution"), hr(), h5("Germany"),
                                                                      column(3,
                                                                             img(src='HIST_2_GER_2018.png', width="95%", align = "center")),
                                                                      column(3,
                                                                             img(src='HIST_3_GER_2018.png', width="95%", align = "center")),
                                                                      column(3,
                                                                             img(src='HIST_6_GER_2018.png', width="95%", align = "center")),
                                                                      column(3,
                                                                             img(src='HIST_7_GER_2018.png', width="95%", align = "center"))
                                                             ),
                                                             fluidRow(h5("Middle East"),
                                                                      column(3,
                                                                             img(src='HIST_2_MEA_2018.png', width="95%", align = "center")),
                                                                      column(3,
                                                                             img(src='HIST_3_MEA_2018.png', width="95%", align = "center")),
                                                                      column(3,
                                                                             img(src='HIST_6_MEA_2018.png', width="95%", align = "center")),
                                                                      column(3,
                                                                             img(src='HIST_7_MEA_2018.png', width="95%", align = "center"))
                                                             ),
                                                             fluidRow(h4("Mandatory Parts"), hr(),
                                                                      column(6,
                                                                             DT::dataTableOutput("effectivity"))
                                                             )
                                                   )
                                                   
                                                   ),
                                          tabPanel("Color_MEA",
                                            fluidRow(
                                              box(
#output$colorPlotMea18
                                                    plotOutput("colorPlotMea18")),
                                              box(
#output$colorPlotMea19 
                                                    plotOutput("colorPlotMea19")
                                                  )
                                              ),
                                              fluidRow(
                                                     checkboxGroupInput("columns","Select Columns",choices=choicesColorMea,inline = T),
                                                     #output$colorMEA
                                                     DT::dataTableOutput("colorMEA")
                                                   )
                                                   ),
                                          tabPanel("Color_GER",
                                            fluidRow(
                                              box(
#output$colorPlotGer18
                                                   plotOutput("colorPlotGer18")),
                                              box(
#output$colorPlotGer19 
                                                   plotOutput("colorPlotGer19")
                                              )
                                                   ),
                                            fluidRow(
                                              checkboxGroupInput("columns","Select Columns",choices=choicesColorGer,inline = T),
#output$colorGER
                                              DT::dataTableOutput("colorGER")
                                            )
                                        )
                                        )
                                        ),
                                        tabItem(tabName = "otherLame", h2("Widgets tab content")
                                        )
                                      )  
                                    )
                          )
                         ),
                         
                         tabPanel("DIS.A.STEER - FA.M.E",""),
                         
                         tabPanel("C.A.E.",
                                  img(src='CAE_1.png', width="80%", align = "center"),
                                  img(src='CAE_2.png', width="80%", align = "center"),
                                  img(src='CAE_3.png', width="80%", align = "center"),
                                  img(src='CAE_4.png', width="80%", align = "center"),
                                  img(src='CAE_5.png', width="80%", align = "center"),
                                  img(src='CAE_6.png', width="80%", align = "center"),
                                  img(src='CAE_7.png', width="80%", align = "center"),
                                  img(src='CAE_8.png', width="80%", align = "center"),
                                  img(src='CAE_9.png', width="80%", align = "center"),
                                  img(src='CAE_10.png', width="80%", align = "center"),
                                  img(src='CAE_11.png', width="80%", align = "center")
                         ),
                         
                         tabPanel("I.REN.E.",
                                  img(src='IR_1.png', width="80%", align = "center"),
                                  img(src='IR_2.png', width="80%", align = "center"),
                                  img(src='IR_3.png', width="80%", align = "center"),
                                  img(src='IR_4.png', width="80%", align = "center"),
                                  img(src='IR_5.png', width="80%", align = "center"),
                                  img(src='IR_6.png', width="80%", align = "center"),
                                  img(src='IR_7.png', width="80%", align = "center"),
                                  img(src='IR_8.png', width="80%", align = "center"),
                                  img(src='IR_9.png', width="80%", align = "center")
                          ),
                        
                        tabPanel("MO.ANA.",
                                 ""
                                 )
                  )
                ),
                
                ##### --- Predictive Analytics - Library
                tabPanel("Predictive Analytics - Model Library", 
                          navbarPage(title = "",
                                     tabPanel("Demand Prediction",
                                              "average <- mean(dataSet18$freq)"), 
                                     tabPanel("Association Analysis",
                                              p("The association analysis embraces a search for strong rules. 
                                                         The resulting association rules are describing correlations in between
                                                         two objects which appear together. If A -> B."),
                                              h5("LAME_TR_MEDIUM_RULES"),
#output$asso
                                              DT::dataTableOutput("asso")
                                     ),
                                     tabPanel("Cluster Analysis",
                                              img(src='CLUS_1.png', width="80%", align = "center"),
                                              img(src='CLUS_2.png', width="80%", align = "center"),
                                              img(src='CLUS_3.png', width="80%", align = "center"),
                                              img(src='CLUS_4.png', width="80%", align = "center"),
                                              img(src='CLUS_5.png', width="80%", align = "center"),
                                              img(src='CLUS_6.png', width="80%", align = "center"),
                                              img(src='CLUS_7.png', width="80%", align = "center"),
                                              img(src='CLUS_8.png', width="80%", align = "center"),
                                              img(src='CLUS_9.png', width="80%", align = "center"),
                                              img(src='CLUS_10.png', width="80%", align = "center"),
                                              img(src='CLUS_11.png', width="80%", align = "center"),
                                              img(src='CLUS_12.png', width="80%", align = "center"),
                                              img(src='CLUS_13.png', width="80%", align = "center"),
                                              img(src='CLUS_14.png', width="80%", align = "center"),
                                              img(src='CLUS_15.png', width="80%", align = "center"),
                                              img(src='CLUS_16.png', width="80%", align = "center"),
                                              img(src='CLUS_17.png', width="80%", align = "center"),
                                              img(src='CLUS_18.png', width="80%", align = "center"),
                                              img(src='CLUS_19.png', width="80%", align = "center")),
                                     tabPanel("Social Media",""),
                                     tabPanel("Prescriptive Analytics",""))
                 ),
                 
                ##### --- Prescriptive Analytics - Library
                tabPanel("Prescriptive Analytics - Model Library", 
                          navbarPage(title = "",
                                     tabPanel("Operations Research",
                                              img(src='OR1.png', width="80%", align = "center"),
                                              img(src='OR2.png', width="80%", align = "center"),
                                              img(src='OR3.png', width="80%", align = "center")), 
                                     tabPanel("Simulation",""
                                     ))
                 ),
                 
                 ##### --- Instagram                
                 tabPanel("Social Media Analytics",
                          navbarPage(title = "",
                                     tabPanel("Data Introduction",
                                              img(src='Insta.png', width="80%", align = "center"),
                                              img(src='Insta_2.png', width="80%", align = "center")),
                                     
                                     tabPanel("Social Media Mood",
                                              fluidPage(
                                                fluidRow(
                                                p("After having executed all cleansing and data preparation tasks for the text analysis all words within the text chunk 'media_text' are compared with regard
                                                    to mood expressions using the NRC Emotion Lexicon. The NRC Emotion Lexicon is a list of English words and their associations with eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive). The annotations were manually done by crowdsourcing. The results of this
                                                    comparison are displayed on the right hand side of the table below with binary indication for each mood.")
                                                  
                                                ),
                                                fluidRow(
                                                        column(6, img(src='instaMood.png', width="50%", align = "center")),
                                                        column(6, img(src='instaMood2.png', width="50%", align = "center"))
                                                ),
                                                fluidRow(
#output$insta                                                  
                                                  column(6, 
                                                         checkboxGroupInput("columns","Select Columns",choices=choicesC,inline = T),
                                                         DT::dataTableOutput("insta")),
                                                  column(6, ""))
                                                
                                                )),
                                     
                                     tabPanel("Social Media Pictures",
#output$instaColor
                                              fluidPage(
                                                fluidRow(h4(""),
                                                         sidebarPanel(
                                                           img(src='instaIrene.gif', width="70%", align = "center")
                                                           #,helptext("INSTA_TR_BMW_v4 and INSTA_TR_BMW_COLOR. See media_id = 2026450395808428844 as reference output.")
                                                         ),
                                                         mainPanel(column(6, img(src='insta1.png', width="100%", align = "center")),
                                                         column(6, img(src='insta2.png', width="100%", align = "center"))
                                                         )
                                                ),
                                                fluidRow(downloadButton("downloadinstaColor", "Download Data")),
                                                hr(),
                                                DT::dataTableOutput("instaColor")
                                              )),
                                     
                                     tabPanel("Irrelevant Words",
                                              sidebarLayout(
                                                
                                                sidebarPanel(
                                                  p("On the right a table with all words considered as irrelevant
                                                  words are enlisted. These are excluded from the text during data cleansing
                                                  and preparation for text analysis. This page shall be further extended to
                                                  maintain the list (update and delete funtions).")
                                                ),
                                                
                                                mainPanel(
#output$irrWords
                                                  DT::dataTableOutput("irrWords"))
                                              )),
                                     tabPanel("Color Coding Method and Trend Derivation",
                                              "https://www.kba.de/DE/ZentraleRegister/ZFZR/Info_behoerden/Regelungen_ZulBescheinigungen/anlage_1_Farben_Codierung_pdf.pdf?__blob=publicationFile&v=4",
                                              img(src='Bild1.png', width="70%", align = "center"))
                                     )),
                 
                 tabPanel("Data Production Monitoring",
                          fluidPage(h4("Hi"),
                                    hr(),
                                    fluidRow(
                                      column(2,
                                             img(src='RARE.png', width="90%", align = "center")),
                                      column(2,
                                             img(src='RARE.png', width="90%", align = "center")),
                                      column(2,
                                             img(src='RARE.png', width="90%", align = "center")),
                                      column(2,
                                             img(src='RARE.png', width="90%", align = "center")),
                                      column(2,
                                             img(src='RARE.png', width="90%", align = "center"))
                                    ))), 
                 
                 
                 ##### --- LAVAP ADMINISTRATION AND MONITORING                 
                 tabPanel("LAVAP Administration and Monitoring",
                          navbarPage(title = "",
                                     tabPanel("Introduction",
                                              img(src='LAVAP_1.png', width="80%", align = "center"),
                                              img(src='LAVAP_2.png', width="80%", align = "center")),
                                     tabPanel("Usage Monitoring",
                                              fluidPage(h5(""),
                                                sidebarPanel(h5(""),
                                                  fluidRow(
                                                    h5("Drilling parameters:"),
                                                      selectInput("loggedUYear", "Year:", choices=years),
                                                      sliderInput("loggedUMonth", "Month:", min = 1, max = 12, value = months),
                                                    helpText("Tablename: SYSTEM_SERVER_USAGE")
                                                  )
                                                ,width = 2),
                                                
                                                mainPanel(h5(""),
#output$loggedU
                                                fluidRow("Logged users:",
                                                         plotlyOutput("loggedU", width = 1700)
                                                ),
#output$memo
                                                fluidRow("% used memory:",
                                                         plotlyOutput("memo", width = 1800)
                                                ),
#output$disk
                                                fluidRow("% used diskspace:",
                                                         plotlyOutput("disk", width = 1800)
                                                ),
#output$data2
                                                fluidRow(DT::dataTableOutput("data2"))
                                                )
                                              )),
                                     
                                     tabPanel("CRUD",
                                              fluidPage(
                                                fluidRow(
                                                  column(4, 
                                                         h5("Welcome to the CRUD aka. OKURRRRRR Interface of LAVAP."),
                                                         img(src='CardiB.gif', width="30%", align = "center"),
                                                         img(src='CardiB.gif', width="30%", align = "center")),
                                                  column(4,""
#input$UID                            
                                                         #passwordInput("userid", "User ID:"),
                                                         #actionButton("go", "Go"),
                                                         #verbatimTextOutput("value"),
#input$PW                                                         
                                                         #passwordInput("password", "Password:"),
                                                         #actionButton("go", "Go"),
                                                         #verbatimTextOutput("value")
                                                  ),
                                                  column(4,""      
                                                )),
                                                fluidRow(
                                                  hr(),
#output$DBList
                                                  DT::dataTableOutput("DBList")
                                                )
                                              )), # closing tabPanel CRUD
                                      tabPanel("Dockerization Monitoring",
                                               fluidPage(
                                                 fluidRow(
                                                   column(4, img(src='Willy.gif', width="30%", align = "center"))
                                                 ),
                                                 fluidRow(
                                                   hr(),
                                                   infoBox("I.REN.E", "Last Update:", icon = icon("camera"), fill = TRUE, color = "light-blue"),
                                                   infoBox("C.A.E", "Last Update:", icon = icon("car-crash"), fill = TRUE, color = "light-blue"),
                                                   infoBox("SEID", "Last Update:", icon = icon("fas fa-baby-carriage"), fill = TRUE, color = "light-blue")
                                                 ),
                                                 fluidRow(
                                                   infoBox("LA.M.E", "Last Update:", icon = icon("cart-arrow-down"), fill = TRUE, color = "light-blue"),
                                                   infoBox("IoT&I.REN.E", "Last Update:", icon = icon("cart-arrow-down"), fill = TRUE, color = "light-blue"),
                                                   infoBox("J.A.R.V.I.S", "Last Update:", icon = icon("fas fa-carrot"), fill = TRUE, color = "light-blue")
                                                 )
                                               )
                                      ) # closing tabPanel Docker
                          ))# closer tabPanel LAVAP ADMINISTRATION AND MONITORING
                 
) # closing UI
) # closing fluidPage
shinyApp(ui, server)
