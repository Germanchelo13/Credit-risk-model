library(DT) # print table beutefull
library(sf) # manupulación geolocalizaciones
library(tmap)  # graficos interactivos de mapas
library(dplyr) # manejo de funciones
library(shiny) # aplicacion
library(plotly) # graficos 
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)# to add a loader while graph is populating
df=read.csv("datos_modelo.csv")
modelo_logistico <- glm(good_status ~., data = df[,-1], family = "binomial")


url_github<-"https://github.com/Germanchelo13/Credit-risk-model"
usuario<- fluidPage(
  dashboardPage(
    # header princial
    dashboardHeader(title="modelo riesgo de credito",
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
                  menuItem("Modelo riesgo de credito", tabName="data", icon=icon("database"))
      )  ),
    # itmes
    dashboardBody(tabItems (
      
      # item descripcion 
      tabItem(tabName = 'intro',fluidRow(style='height:5vh'),
              tabBox(id='t3',width=12,tabPanel(HTML('<i class="fa-solid fa-book"></i> Contexto'), 
                                               fluidPage(
                                                 fluidRow(uiOutput('intro_'))
                                               )),
                     tabPanel(HTML('<i class="fa-solid fa-graduation-cap"></i> Caracterización'), fluidPage(fluidRow( 
                       uiOutput('caracterizacion') ) ) ) )) ,
      tabItem(tabName="data",
              tabBox(id="t3",width= 12,
                     tabPanel(title="Prediccion score",icon=icon('table'),
                              fluidPage(
                                fluidRow( column(6,selectInput(inputId="pub_rec",
                                                      label="Seleccione 1 si tiene derogatory public records,\n  0 sino",
                                                      choices=c(0,1),multiple=F)),
                                          column(6,selectInput(inputId="open_acc", label="Selecione 1 si tiene mas \n de una linea de crédito, 0 sino",
                                                      choices=c(0,1) )) ),
                                fluidRow( column(6,selectInput(inputId ="home_ownership", label="Seleccione el estado \n de su vivienda",
                                                      choices =unique(df$home_ownership)) ),
                                          column(6,selectInput(inputId = "emp_length",
                                                               label="Selecione cuantos años de trabajo lleva",
                                                               choices = unique(df$emp_length)) )),
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
                                                     step = 1,
                                                     width = "100%") )
                              ))))
    )
    
    )
  )
)
summary(df$int_rate)



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
  print(members_)
  HTML("<h2 style='text-align:center' > Introducción <h2/>
  <h5>Les brindamos esta aplicacion que 
  permite de forma agil calcular el porccentaje de riesgo y el
  score crediticio a los usuarios del publico común<h5/>
  <h2 style='text-align:center'> Objetivo <h2/>
  <h5>Que el usuario tenga otra alternativa para encontrar una universidad que cumpla con sus espectativas y que se 
  ubique en un lugar que sea acorde a sus necesidades.<h5/>
  <h2 style='text-align:center'> ¿A quién va dirigido? <h2/>
<h5>  A personas interedas en buscar universidades que cumplan con sus expectativas, pueden observar
  la descripción de los 4 grupos e identificar cuales son de su interés, luego pueden ver la 
  ubicación geográfica de las universidades según los grupos y el estado donde más se sientan 
  comodos, ya sea por que busquen una universidad que cumpla con sus espectativas pero no a 
  una distancia tan lejana de donde residen, el usuario puede dar click en un punto del mapa y 
  obtener la URL de la página principal de dicha universidad.<h5>
<h2 style='text-align:center'> Video promocional <h2/>
 <iframe width='560' height='315' style='text-align:center' 
 src='https://www.youtube.com/embed/CqstGgo_E4c'
 title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>
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
  
  
}

shinyApp(
  ui = usuario,
  
  server = servidor
)
