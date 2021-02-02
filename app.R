if (!require(shinyjs)) install.packages('shinyjs')

library(shiny)
library(shinyjs)

ui<-navbarPage(title="Quiz-Time",
               useShinyjs(),
               tabPanel("Spielinfos",
                        tags$h1("Spielregeln"),
                        tags$p(tags$strong("Achtung!"),"Vor dem Beginn muss die Gruppengröße gewählt werden."),
                        tags$p("Hier stehen die Regeln"),
                        numericInput("groups","Anzahl der Gruppen wählen und bestätigen",value=4,min =2, max=6),
                        actionButton("start","Bestätigen")),
               tabPanel("Questionboard",
                        conditionalPanel(condition = "input.start =='0'",
                                         tags$p(tags$strong("Achtung!"),"Vor dem Beginn muss unter Spielregeln die Gruppengröße gewählt werden.")),
                        fluidRow(column(2,align="center",tags$p(tags$strong("Versuchsgrundlagen"),style = "color: black; background-color: #FF0018")),column(2,align="center",tags$p(tags$strong("Testtheorie"),style = "color: black; background-color: #FFA52C")),column(2,align="center",tags$p(tags$strong("statistische Grundlagen"),style = "color: black; background-color: #FFFF41")),column(2,align="center",tags$p(tags$strong("statistische Verfahren"),style = "color: black; background-color: #008018")),column(2,align="center",tags$p(tags$strong("R-Grundlagen"),style = "color: black; background-color: #0000F9")),column(2,align="center",tags$p(tags$strong("R-Befehle"),style = "color: black; background-color: #86007D"))),
                        fluidRow(column(2,actionButton(inputId = "R1_S1",label="1",style = "color: black; background-color: #FF0018",width = "184")),column(2,actionButton(inputId = "R1_S2",label="1",style = "color: black; background-color: #FFA52C",width = "184")),column(2,actionButton(inputId = "R1_S3",label="1",style = "color: black; background-color: #FFFF41",width = "184")),column(2,actionButton(inputId = "R1_S4",label="1",style = "color: black; background-color: #008018",width = "184")),column(2,actionButton(inputId = "R1_S5",label="1",style = "color: black; background-color: #0000F9",width = "184")),column(2,actionButton(inputId = "R1_S6",label="1",style = "color: black; background-color: #86007D",width = "184"))),
                        fluidRow(column(2,actionButton(inputId = "R2_S1",label="2",style = "color: black; background-color: #FF0018",width = "184")),column(2,actionButton(inputId = "R2_S2",label="2",style = "color: black; background-color: #FFA52C",width = "184")),column(2,actionButton(inputId = "R2_S3",label="2",style = "color: black; background-color: #FFFF41",width = "184")),column(2,actionButton(inputId = "R2_S4",label="2",style = "color: black; background-color: #008018",width = "184")),column(2,actionButton(inputId = "R2_S5",label="2",style = "color: black; background-color: #0000F9",width = "184")),column(2,actionButton(inputId = "R2_S6",label="2",style = "color: black; background-color: #86007D",width = "184"))),
                        fluidRow(column(2,actionButton(inputId = "R3_S1",label="3",style = "color: black; background-color: #FF0018",width = "184")),column(2,actionButton(inputId = "R3_S2",label="3",style = "color: black; background-color: #FFA52C",width = "184")),column(2,actionButton(inputId = "R3_S3",label="3",style = "color: black; background-color: #FFFF41",width = "184")),column(2,actionButton(inputId = "R3_S4",label="3",style = "color: black; background-color: #008018",width = "184")),column(2,actionButton(inputId = "R3_S5",label="3",style = "color: black; background-color: #0000F9",width = "184")),column(2,actionButton(inputId = "R3_S6",label="3",style = "color: black; background-color: #86007D",width = "184"))),
                        fluidRow(column(2,actionButton(inputId = "R4_S1",label="4",style = "color: black; background-color: #FF0018",width = "184")),column(2,actionButton(inputId = "R4_S2",label="4",style = "color: black; background-color: #FFA52C",width = "184")),column(2,actionButton(inputId = "R4_S3",label="4",style = "color: black; background-color: #FFFF41",width = "184")),column(2,actionButton(inputId = "R4_S4",label="4",style = "color: black; background-color: #008018",width = "184")),column(2,actionButton(inputId = "R4_S5",label="4",style = "color: black; background-color: #0000F9",width = "184")),column(2,actionButton(inputId = "R4_S6",label="4",style = "color: black; background-color: #86007D",width = "184"))),
                        fluidRow(column(2,actionButton(inputId = "R5_S1",label="5",style = "color: black; background-color: #FF0018",width = "184")),column(2,actionButton(inputId = "R5_S2",label="5",style = "color: black; background-color: #FFA52C",width = "184")),column(2,actionButton(inputId = "R5_S3",label="5",style = "color: black; background-color: #FFFF41",width = "184")),column(2,actionButton(inputId = "R5_S4",label="5",style = "color: black; background-color: #008018",width = "184")),column(2,actionButton(inputId = "R5_S5",label="5",style = "color: black; background-color: #0000F9",width = "184")),column(2,actionButton(inputId = "R5_S6",label="5",style = "color: black; background-color: #86007D",width = "184"))),
                        absolutePanel(tags$p(""),
                                      span(tags$strong(textOutput("question")),style="font-size:20px"),
                                      fluidRow(column(6,textInput("answer","Beantworten Sie die Frage","")),column(6,numericInput("groupnumber","Welche Gruppe gibt diese Antwort?",value = "",min=0))),
                                      actionButton(inputId = "submit","Antwort absenden"),
                                      textOutput("control"),
                                      textOutput("Info"),
                                      actionButton("close","Frage schließen"))
                        ),
               tabPanel("Ergebnisse",
                        conditionalPanel(condition = "input.start =='0'",
                                         tags$p(tags$strong("Achtung!"),"Vor dem Beginn muss unter Spielregeln die Gruppengröße gewählt werden.")),
                        plotOutput("result")
               )
)


server <- function(input,output,session){
  
###Landing Page  
  #dummyevent
  start<-1 
  
  # event will be called when start changes, which only happens once, when it is initially calculated
  observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = start, { 
    showModal(modalDialog(
      h1('Willkommen zum Grundlagenquiz!'),
      p('Das Quiz wurde im Rahmen der Veranstaltung "Experimentelles-Praktikum" an der Goethe-Uni in Frankfurt entwickelt und soll die Stoffe der Methodik aus den ersten Bachelor-Semestern spielerisch auffrischen. Das Spiel ist für mehrere Rategruppen konzipiert, braucht aber einen Moderator der die Oberfläche bedient und eine Liste mit der genauen Schreibweise der richtigen Antworten hat.'), 
      footer = modalButton("Start!")
    ))})
  
  #Define Questions, Answers and Information
  question <- matrix(c("Frage",1:4,"Test2",5:28),5,6)
  corrects<-matrix(c("antwort",1:4,"Test2", 5:28),5,6)
  infos<-matrix(c("Info",1:4,"Info2",5:28),5,6)
  
  
  #Reactiv Values for Outputs and score-keeping
  texts<-reactiveValues(quest = "",correct="",info="")
  score <- reactiveValues(names= 0, gruppen = 0, points = 0, groupnum = 0)
 
  
  #Hide Questionrelated Widgets
  hide("close")
  hide("groupnumber")
  hide("answer")
  hide("submit")
  hide("Info")
  
  observeEvent(input$start,{
    hide("start")
    hide("groups")
    num<-input$groups
    score$gruppen <- matrix(0,1,num+1)
    score$names<-c(0:num)
  })
  
  ###ActionButtons
  observe({
    qus <- as.matrix(expand.grid(1:5, 1:6))
    qus <- split(qus, 1:nrow(qus))
    qus <- lapply(qus, unlist)
    lapply(qus, function(x) {
      tmp_btn <- paste0('R', x[1], '_S', x[2])
      observeEvent(input[[tmp_btn]], {
        
        # Disable button
        shinyjs::disable(tmp_btn)
        
        # Update texts/Points
        texts$question <- question[x[1], x[2]]
        texts$correct <- corrects[x[1], x[2]]
        texts$info <- infos[x[1], x[2]]
        score$points <- x[1]
        
        # Render question and infos with texts
        output$question <- renderText(texts$question)
        output$Info<-renderText(texts$info)
        
        # Display input fields
        toggle('answer')
        toggle('groupnumber')
        toggle('submit')
        
      })
    })
  })
  ###Check input Answer
  observeEvent(input$submit,{
    answer<-input$answer
    if (answer == texts$correct){
      
      #Send Feedback and Show Information
      output$control <- renderText("Super! Korrekt!")
      toggle("Info")
      
      #Add Points to the group and rewrite Plot
      score$groupnum <- input$groupnumber
      gruppenzahl<-score$groupnum+1
      score$gruppen[gruppenzahl]<-score$gruppen[gruppenzahl]+score$points
      output$result <-renderPlot(barplot(score$gruppen,names.arg = score$names))
      
      #Hide Submit Button and show close Button
      toggle("submit")
      toggle("close")
    }
    else {
      #Send Feedback
      output$control <- renderText("Leider falsch")
    }
  })
  ###close all question related widgets
  observeEvent(input$close,{
    
    #Reset to default 
    reset("answer")
    reset("groupnumber")
    
    #Hide input fields
    toggle("answer")
    toggle("groupnumber")
    toggle("close")
    toggle("Info")
    
    #Empty output fields
    texts$question<- ""
    output$quest<-renderText("texts$quest")
    output$control <- renderText("")
  })
}

shinyApp(ui=ui,server=server)
