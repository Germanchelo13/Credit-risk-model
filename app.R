library(dplyr) # manejo de funciones
library(shiny) # aplicacion
library(plotly) # graficos 
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)# to add a loader while graph is populating

score_global<-read.csv("coef_score.csv")[,c(2,6)]
score_datos<-read.csv("score_poblacional.csv")[,2]
score_datos<-sort(score_datos,T)
emp_temp<-c("10+ years", "1 year"  ,  "2 years",   "4 years" ,  "3 years",   "7 years" ,  "5 years"   ,"6 years"  ,
            "9 years"  , "8 years" ,  "< 1 year" 
            )

url_github<-"https://github.com/Germanchelo13/Credit-risk-model"
usuario<- fluidPage(
  dashboardPage(
    # header princial
    dashboardHeader(title="Riesgo de \n crédito",
                    # titleWidth = 400, 
                    tags$li(class="dropdown",
                            tags$a(href=url_github, 
                                   icon("github"), 
                                   "Source Code", 
                                   target="_blank"))),
    # left 
    dashboardSidebar(
      sidebarMenu(id="sidebar",
                  fluidRow(style='height:5vh'),
                  menuItem("Introduction", tabName="intro", icon=icon("users")),
                  menuItem("Modelo riesgo de crédito", tabName="data", icon=icon("database"))
      )  ),
    # itmes
    dashboardBody(tabItems (
      
      # item descripcion 
      tabItem(tabName = 'intro',fluidRow(style='height:5vh'),
              tabBox(id='t3',width=12,tabPanel(HTML('<i class="fa-solid fa-book"></i> Contexto'), 
                                               fluidPage(
                                                 fluidRow(uiOutput('intro_'))
                                               )) )) ,
      tabItem(tabName="data",
              tabBox(id="t3",width= 12,
                     tabPanel(title="Prediccion score",icon=icon('table'),
                              fluidPage(
                                fluidRow( column(6,selectInput(inputId="pub_rec",
                                                      label="Seleccione 1 si tiene derogatory public records,\n  0 sino",
                                                      choices=c(0,1),multiple=F)),
                                          column(6,sliderInput(inputId="open_acc",
                                                               label="Selecione El número de líneas de crédito abiertas.",
                                                               min = 0,
                                                               max=84,
                                                               value=0,
                                                               step=1,
                                                               width = "100%")) ),
                                fluidRow( column(6,selectInput(inputId ="home_ownership", label="Seleccione el estado \n de su vivienda",
                                                      choices =c("OWN","RENT","MORTGAGE","NONE","OTHER") ) ),
                                          column(6,selectInput(inputId = "emp_length",
                                                               label="Selecione cuantos años de trabajo lleva",
                                                               choices = emp_temp) )),
                                fluidRow(sliderInput("loan_amnt",
                                                     label = "Seleccione el monto del prestamo que va a solicitar",
                                                     min = 1000,
                                                     max = 35000,
                                                     value = 1000,
                                                     step = 250,
                                                     width = "100%") ),
                                fluidRow(sliderInput("int_rate",
                                                     label = "Seleccione la tasa de interes del prestamo",
                                                     min = 6,
                                                     max = 26,
                                                     value = 10,
                                                     step = 0.1,
                                                     width = "100%") ),
                                fluidRow( uiOutput("Predicion"),
                                          plotlyOutput('score_poblacional'))
                              ))))
    )
    
    )
  )
)



servidor<-function(input, output) {
  
output$intro_<- renderUI({
  members_<-tags$div(
    tags$b('Authors:'),tags$br(),
    HTML(paste('&#9658' ,tags$a(href="https://www.linkedin.com/in/germ%C3%A1n-alonso-pati%C3%B1o-hurtado-828783241/", 
                                icon("linkedin"), "Germán Patiño", target="_blank"),
               'Estudiante de Estadística en la Universidad Nacional de Colombia.' ) ),
    tags$br(),
    HTML('&#9658 David Andres Cano Gonzalez Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia.' ),
    tags$br(),
    HTML('&#9658 David Garcia Blandon Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia.' ),
    tags$br(),
    HTML(paste('&#9658',tags$a(href='https://www.linkedin.com/in/juan-pablo-buitrago-diaz-5b960922b/',icon("linkedin"), 'Juan Pablo Buitrago Diaz', target="_blank"), 'Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia.') ),
  )
  HTML("<h2 style='text-align:center' > Introducción <h2/>
  <h5>Les brindamos esta aplicacion que 
  permite de forma agil calcular el porccentaje de riesgo y el
  score crediticio a los usuarios del publico con respecto a información de crédito otorgado 
  por <a href='https://www.lendingclub.com/'> lendingclub  </a>. común<h5/>
  <h2 style='text-align:center' > Objetivo <h2/>
    <h5>Que el usuario consulte su score a futuro al cabo de 12 meses desde que 
    solicita un crédito y según algunas caracteristicas pueda compararse frente a una población en
    especifico.<h5/>
  <h2 style='text-align:center'> ¿A quién va dirigido? <h2/>
<h5>  A usuarios que esten interesados en su historial crediticio.<h5>
<h2 style='text-align:center'> Video promocional <h2/>
<iframe width='560' height='315'
src='https://www.youtube.com/embed/IKFASg3vT-c'
title='YouTube video player' frameborder='0' 
allow='accelerometer; autoplay; clipboard-write; encrypted-media;
gyroscope; picture-in-picture' allowfullscreen></iframe>
        <br/>   
           <b >Miembros:</b>
   <h5> &#9658 <a href='https://www.linkedin.com/in/germ%C3%A1n-alonso-pati%C3%B1o-hurtado-828783241/' target='_blank'>
  <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
  Germán Patiño
</a> Estudiante de Estadística en la Universidad Nacional de Colombia.<h5/>
  <h5> &#9658 David Andres Cano Gonzalez Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia. <h5/>

  <h5> &#9658 David Garcia Blandon Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia. <h5/>
  <h5> &#9658 <a href='https://www.linkedin.com/in/juan-pablo-buitrago-diaz-5b960922b/' target='_blank'> 
  <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
  Juan Pablo Buitrago Diaz
</a> Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia. <h5/>  
             " )
  
  
})
list.files()


output$Predicion<-renderUI({
  home_temp= grepl(input$home_ownership, score_global$variables_modelo )
  if (sum(home_temp)==0 ){
    home_temp=0
  }
  else{
    home_temp=score_global[home_temp,2]  
  }
  emp_<-grepl(input$emp_length, score_global$variables_modelo )
  if (sum(emp_)==0 ){
    emp_=0
  }
  else{
    emp_=score_global[emp_,2]  
  }
  loan_temp<-0
  if ( (input$loan_amnt>9500) & (input$loan_amnt<=18000) ){
    loan_temp=-17
  } 
  else{ if((input$loan_amnt<=26500) & (input$loan_amnt>18000)){
    loan_temp=-19
  }else{if((input$loan_amnt>26500) & (input$loan_amnt<=35000) ){loan_temp=-10}} }
  int_temp<-0
  if ( (input$int_rate>11.015) & (input$int_rate<=16.03) ){
    int_temp=-91
  } 
  else{ if((input$int_rate<=21.045) & (input$int_rate>16.03)){
    int_temp=-151
  }else{if((input$int_rate>21.045) & (input$int_rate<=26.06) ){int_temp=-190}} }  
  open_temp<-0
  if (input$open_acc>42){
    open_temp<-49
  }
  score_=(score_global[1,2]+
            score_global[2,2]+
            score_global[3,2]*as.numeric(input$pub_rec)+
            home_temp+
            emp_+
            loan_temp+
            int_temp+
            open_temp
  )
  percentil_temp<-round(sum(score_datos<=score_)/length(score_datos)*100)
  HTML(paste("<h2> El usuario que cumple con estas caracteristicas tiene un score de ",
             as.character(score_),
             "Y se ubica en el ", percentil_temp,
             " % de los mejores, esto aplica para los 12 primeros meses </h2>." 
             ,sep=" " ) )
})
  
output$score_poblacional<- renderPlotly({
  home_temp= grepl(input$home_ownership, score_global$variables_modelo )
  if (sum(home_temp)==0 ){
    home_temp=0
  }
  else{
    home_temp=score_global[home_temp,2]  
  }
  emp_<-grepl(input$emp_length, score_global$variables_modelo )
  if (sum(emp_)==0 ){
    emp_=0
  }
  else{
    emp_=score_global[emp_,2]  
  }
  loan_temp<-0
  if ( (input$loan_amnt>9500) & (input$loan_amnt<=18000) ){
    loan_temp=-17
  } 
  else{ if((input$loan_amnt<=26500) & (input$loan_amnt>18000)){
    loan_temp=-19
  }else{if((input$loan_amnt>26500) & (input$loan_amnt<=35000) ){loan_temp=-10}} }
  int_temp<-0
  if ( (input$int_rate>11.015) & (input$int_rate<=16.03) ){
    int_temp=-91
  } 
  else{ if((input$int_rate<=21.045) & (input$int_rate>16.03)){
    int_temp=-151
  }else{if((input$int_rate>21.045) & (input$int_rate<=26.06) ){int_temp=-190}} }  
  open_temp<-0
  if (input$open_acc>42){
    open_temp<-49
  }
  score_=(score_global[1,2]+
            score_global[2,2]+
            score_global[3,2]*as.numeric(input$pub_rec)+
            home_temp+
            emp_+
            loan_temp+
            int_temp+
            open_temp
  )
  #print(score_)
#  score_temps<-data.frame(Score=c(score_datos,100), Tipo=c(rep("Poblacion",length(score_datos)),"Usuario")  )
  fig <- plot_ly(alpha = 0.6,xbins = list(size = 20))
  fig <- fig %>% add_histogram(x=~score_datos)
  fig <-fig %>%  add_lines(
    y = c(0,50000),
    x = score_,
    line = list(
      color = "grey"
    ),
    inherit = FALSE,
    showlegend = FALSE
  )

  fig
} )

}

shinyApp(
  ui = usuario,
  
  server = servidor
)
