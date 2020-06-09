#=)
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(magrittr)
library(rpart)
library(dplyr)
library(rpart.plot)
library(RWeka)
library(RWekajars)
library(RColorBrewer)
library(rattle)
library(MASS)
library(readr)
library(ggplot2)
library(scales)

jscode <- "shinyjs.refresh = function() { history.go(0); }"

dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)

# Define UI for application that draws a histogram
ui <- dashboardPage( 
  
    skin = "red",
    dashboardHeader(
        title = span(img(src="a.png", width=50),"DSS", Style = "font-size: 13px; ;text-align: center; font-family: sans-serif; line-height: 100%;color: #0000; font-style: normal;"), titleWidth = 130),
        #Menu lateral
        dashboardSidebar(
            disable = FALSE,
            collapsed = TRUE,
            width = 200,
            useShinyjs(),
            extendShinyjs(text = jscode),
            #Menu
            sidebarMenu( menuItem(span("Calculate cost", Style = "font-size: 16px; ;text-align: center; font-family: sans-serif; line-height: 100%;color: #0000; font-style: normal;"), tabName = "principal", icon = icon("bar-chart-o")))
        ),
    
        #cuerpo 
        dashboardBody(
            useShinyjs(),
            tags$head(tags$style(HTML(".content-wrapper {background-color: white !important;} "))),
            tags$head(tags$style("#nResult2{color:black; font-size:12px; overflow-y:scroll; max-height: 200px; background: ghostwhite;}")),
            tags$head(tags$style(HTML(' .main-header .logo { font-weight: bold; font-size: 20px; color: #0000; } '))),
        
        #Menu lateral - inside
        tabItems(
            #primer menu
            tabItem(tabName = "principal",
               div(img(src="b.jpg", height="88px", width="1730px"), style = 'position: relative; left: -20px;top: -20px; opacity: 0.5;'),
               fluidRow(
                  tabBox(width=12,
                     tabPanel(span(strong("Shipping Cost"),Style = "font-size: 16px"), 
                     fluidRow(
                        column(width=6,
                           box(title = "Transportation companies:",
                              solidHeader=TRUE,
                              width = 12,
                              status= "success",
                               collapsible = TRUE,
                               collapsed = FALSE,
                               verbatimTextOutput("nResult2"),
                               hr(),
                               valueBoxOutput("approvalBox", width = 40))
                           ),
                        column(width=6,
                            box(title = "Cost graph:",
                               solidHeader=TRUE,
                               width = 12,
                               status= "success",
                               collapsible = TRUE,
                               collapsed = TRUE,
                               plotOutput("plot", height = 350)))
                      )
                   ),
                   tabPanel(span(strong("Annual shipping cost"),Style = "font-size: 16px"), 
                      fluidRow(
                         column(width=6,
                            sliderInput("slider", "Enter quantity:", 1, 100, 50),
                            hr(),
                            strong("How many times for years?"),
                            verbatimTextOutput("abc"),
                            hr(),
                            verbatimTextOutput("abc2"),
                            valueBoxOutput("approvalBox2", width = 40)),
                          column(width=6,span(strong("Cost per year:"),Style = "font-size: 18px"),plotOutput("plot2", height = 350))))
                       )
               ),
               fluidRow(
                   column(width=4,#hr(),
                      selectInput(inputId = "d4", label = span(strong("Destination"), style="font-size: 16px"),c(Choose='',"Destination 1" = "101", "Destination 2" = "102","Destination 3" = "103","Destination 4" = "104", "Destination 5" = "105","Destination 6" = "106","Destination 7" = "107", "Destination 8" = "108","Destination 9" = "109","Destination 10" = "110")),
                      verbatimTextOutput("d4", placeholder = TRUE)),
                   column(width=2,
                      br(),
                      actionButton("otimizacao", strong("Optimal values"), icon("th"),style="color: #fff; background-color: #2676B6; border-color: #2676B6"),
                      hr(),
                      actionButton("refresh", strong("Update"), icon("refresh"),style="color: #fff; background-color: #ce4040; border-color: #ce4040",width = 120)),
                   column(width = 2, br(),downloadButton("downloadData", strong("Download costs"), style="color: #fff; background-color: #C4AE35; border-color: #C4AE35") )
               ),
               fluidRow(
                   br(),
                   column(width=12,span(strong("Quantities shipped per Production Line (PL):"), style="font-size: 16px")),
                   br(),br(),
                   column(width=4,textInput(inputId="Q0a", label="Others", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q1a", label="PL 1", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q2a", label="PL 2", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q3a", label="PL 3", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q4a", label="PL 4", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q5a", label="PL 5", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q6a", label="PL 6", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q7a", label="PL 7", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q8a", label="PL 8", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q9a", label="PL 9", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q10a", label="PL 10", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q11a", label="PL 11", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q12a", label="PL 12", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q13a", label="PL 13", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q14a", label="PL 14", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q15a", label="PL 15", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q16a", label="PL 16", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q17a", label="PL 17", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q18a", label="PL 18", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q19a", label="PL 19", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q20a", label="PL 20", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q21a", label="PL 21", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q22a", label="PL 22", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q23a", label="PL 23", value = 0, width = NULL, placeholder = NULL)),
                   column(width=4,textInput(inputId="Q24a", label="PL 24", value = 0, width = NULL, placeholder = NULL))
               )
             )
         ) 
    )
)

# Define server logic required to draw a histogram
#
server <- function(input, output, session) {
    
    output$d4<-renderText({input$d4})
    observeEvent(input$Q0a,{ if(input$Q0a==""){ observe({ updateTextInput(session, "Q0a", value = 0) }) } })
    observeEvent(input$Q1a,{ if(input$Q1a==""){ observe({ updateTextInput(session, "Q1a", value = 0) }) } })
    observeEvent(input$Q2a,{ if(input$Q2a==""){ observe({ updateTextInput(session, "Q2a", value = 0) }) } })
    observeEvent(input$Q3a,{ if(input$Q3a==""){ observe({ updateTextInput(session, "Q3a", value = 0) }) } })
    observeEvent(input$Q4a,{ if(input$Q4a==""){ observe({ updateTextInput(session, "Q4a", value = 0) }) } })
    observeEvent(input$Q5a,{ if(input$Q5a==""){ observe({ updateTextInput(session, "Q5a", value = 0) }) } })
    observeEvent(input$Q6a,{ if(input$Q6a==""){ observe({ updateTextInput(session, "Q6a", value = 0) }) } })
    observeEvent(input$Q7a,{ if(input$Q7a==""){ observe({ updateTextInput(session, "Q7a", value = 0) }) } })
    observeEvent(input$Q8a,{ if(input$Q8a==""){ observe({ updateTextInput(session, "Q8a", value = 0) }) } })
    observeEvent(input$Q9a,{ if(input$Q9a==""){ observe({ updateTextInput(session, "Q9a", value = 0) }) } })
    observeEvent(input$Q10a,{ if(input$Q10a==""){ observe({ updateTextInput(session, "Q10a", value = 0) }) } })
    observeEvent(input$Q11a,{ if(input$Q11a==""){ observe({ updateTextInput(session, "Q11a", value = 0) }) } })
    observeEvent(input$Q12a,{ if(input$Q12a==""){ observe({ updateTextInput(session, "Q12a", value = 0) }) } })
    observeEvent(input$Q13a,{ if(input$Q13a==""){ observe({ updateTextInput(session, "Q13a", value = 0) }) } })
    observeEvent(input$Q14a,{ if(input$Q14a==""){ observe({ updateTextInput(session, "Q14a", value = 0) }) } })
    observeEvent(input$Q15a,{ if(input$Q15a==""){ observe({ updateTextInput(session, "Q15a", value = 0) }) } })
    observeEvent(input$Q16a,{ if(input$Q16a==""){ observe({ updateTextInput(session, "Q16a", value = 0) }) } })
    observeEvent(input$Q17a,{ if(input$Q17a==""){ observe({ updateTextInput(session, "Q17a", value = 0) }) } })
    observeEvent(input$Q18a,{ if(input$Q18a==""){ observe({ updateTextInput(session, "Q18a", value = 0) }) } })
    observeEvent(input$Q19a,{ if(input$Q19a==""){ observe({ updateTextInput(session, "Q19a", value = 0) }) } })
    observeEvent(input$Q20a,{ if(input$Q20a==""){ observe({ updateTextInput(session, "Q20a", value = 0) }) } })
    observeEvent(input$Q21a,{ if(input$Q21a==""){ observe({ updateTextInput(session, "Q21a", value = 0) }) } })
    observeEvent(input$Q22a,{ if(input$Q22a==""){ observe({ updateTextInput(session, "Q22a", value = 0) }) } })
    observeEvent(input$Q23a,{ if(input$Q23a==""){ observe({ updateTextInput(session, "Q23a", value = 0) }) } })
    observeEvent(input$Q24a,{ if(input$Q24a==""){ observe({ updateTextInput(session, "Q24a", value = 0) }) } })
  
    #call the function for prediction
    nResult2 <- eventReactive(input$otimizacao, {oti(input) })

    #prediction function 
    load("TModel.rda")
    oti<- function(input) {
        d4=as.numeric(input$d4)
        Q0=as.double(input$Q0a)
        Q1=as.double(input$Q1a)
        Q2=as.double(input$Q2a)
        Q3=as.double(input$Q3a)
        Q4=as.double(input$Q4a)
        Q5=as.double(input$Q5a)
        Q6=as.double(input$Q6a)
        Q7=as.double(input$Q7a)
        Q8=as.double(input$Q8a)
        Q9=as.double(input$Q9a)
        Q10=as.double(input$Q10a)
        Q11=as.double(input$Q11a)
        Q12=as.double(input$Q12a)
        Q13=as.double(input$Q13a)
        Q14=as.double(input$Q14a)
        Q15=as.double(input$Q15a)
        Q16=as.double(input$Q16a)
        Q17=as.double(input$Q17a)
        Q18=as.double(input$Q18a)
        Q19=as.double(input$Q19a)
        Q20=as.double(input$Q20a)
        Q21=as.double(input$Q21a)
        Q22=as.double(input$Q22a)
        Q23=as.double(input$Q23a)
        Q24=as.double(input$Q24a)
        Env<-Q0+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12+Q13+Q14+Q15+Q16+Q17+Q18+Q19+Q20+Q21+Q22+Q23+Q24
        a<-c(10, 20, 30, 60)
        b<-c(10, 20, 60)
        c<-c(10, 20, 30, 60)
        d<-c(10, 20, 30, 60)
        e<-c(10, 20)
        f<-c(10, 20, 60)
        g<-c(10, 20, 40)
        h<-c(10, 20, 60)
        i<-c(20, 50)
        j<-c(10, 20, 60)
        Tr <- switch(input$d4, "101"=a, "102"=b, "103"=c, "104"=d, "105"=e, "106"=f, "107"=g, "108"=h, "109"=i, "110"=j)
         
        CLC <- vector("double", length(Tr))
        POS <- vector("character", length(Tr))
        if(Env >= 1){
            #Ciclo for -
            for (t3 in Tr) {
                dt_Pr1 <- data.frame(t3, d4, Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20, Q21, Q22, Q23, Q24)
                POS[t3] = c(t3)
                #ML model prediction
                CLC[t3] = predict(TModel, dt_Pr1)
                print( HTML(paste("Transportation company", as.character(t3), ":", dollar_format()(round(as.double(CLC[t3]),2)))))
            }#Fin - for
            VT <- c(as.numeric(CLC[!is.na(CLC)]))
            val1<- as.numeric(VT[VT>0])
            val<- round(as.numeric(VT[VT>0]),2)
            P_VT <- c(as.character(POS[!is.na(POS)]))
            P_val<- as.character(P_VT[P_VT>0])
            df=data.frame(P_val,val)  
            #Min
            output$approvalBox <- renderValueBox({ valueBox( dollar_format()(min(val)), paste("Best cost: Transportation company", P_val[c(which.min(val))]), icon = icon("thumbs-up", lib = "glyphicon"),color = "orange")})
            #Plot-Cost
            output$plot<- renderPlot({
                ggplot(data=df, aes(x=P_val, y=val, fill= val)) + geom_bar(width=0.5, stat = "identity")+ #,fill = rgb(0.2, 0.2, 1, 0.3), color = "blue"
                theme_bw() + xlab("Transportation company") + ylab("Cost") + geom_text(aes(label = val),  vjust = -0.4 , size = 4)+ scale_fill_gradient(low = "#FFDB6D", high = "#00AFBB")
            })
            #ANUAL
            output$abc<- renderPrint({
                Val_An<-as.numeric(input$slider)
                print( HTML(paste(Val_An, "times per year.")))
                AnV<-round(as.numeric(val1*Val_An),2)
                P_val2<-P_val
                dfAnual=data.frame(P_val2,AnV)
                output$plot2<- renderPlot({
                    ggplot(data=dfAnual, aes(x=P_val2, y=AnV, fill= AnV)) + geom_line(aes(group=1),colour="#A52A2A")+
                    theme_bw()+ xlab("Transportation company") + ylab("Cost") +
                    geom_text(aes(label = AnV),  vjust = -0.4 , size = 4)+
                    geom_point(size=4, shape=20, fill="red", colour="#A52A2A") 
                })
                output$approvalBox2 <- renderValueBox({ valueBox( dollar_format()(min(AnV)), paste("Best cost: Transportation company", P_val2[c(which.min(AnV))]), icon = icon("thumbs-up", lib = "glyphicon"),color = "aqua")})
            })
            #downloadData Predictions
            Dw <- data.frame("Transportation company" = c(P_val), "Shipping Cost" = c(val), "Times per year" = c(as.numeric(input$slider)), "Annual shipping cost" = c(val*as.numeric(input$slider)), "Best cost: Transportation company" = c(P_val[c(which.min(val))]), "Best cost" = c(min(val)), "Best cost per year" = c(min(val*as.numeric(input$slider))))
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("COST_per_Shipping", Sys.Date(), ".csv", sep="")
              },
              content = function(file) {
                write.csv(Dw, file)
            })
            
        }else{ print( HTML(paste("Missing input values"))) }
    #write.csv(dfAnual,"MyData.csv", row.names = FALSE)

    }#Fin funcion
  
    #Atualizar site
    observeEvent(input$refresh, {
        js$refresh();
    })
  
    output$nResult2 <- renderPrint({nResult2()}) # para otimizacion
}
 
# Run the application 
shinyApp(ui = ui, server = server)

