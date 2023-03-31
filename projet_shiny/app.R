#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
######################
#    projet_shiny    #
######################
# une application 
# - point
#   +automatique
#   +au moins d'une grapique





###nettoyer des bdds

##1: telecharger des library
#install.packages("pacman") 
library(tidyverse)
library(questionr)
library(shiny)


##2: telecharger une donnée
mariage<-read.csv("FD_MAR_2018.csv", sep = ";", fileEncoding ="utf8" )
str(mariage)

##3: recodage de sexe

fct_freq_sexe <- function(x){
  recod <- fct_collapse(factor(x),
                        "Homme"="M",
                        "Femme"="F")
  return(recod)
}

mariage$SEXE1 <- fct_freq_sexe(mariage$SEXE1)
mariage$SEXE2 <- fct_freq_sexe(mariage$SEXE2)

round(table(mariage$SEXE1, useNA = "ifany") %>% 
        prop.table()*100,1)#<<
round(table(mariage$SEXE2, useNA = "ifany") %>% 
        prop.table()*100,1)#<<


##4: recodage de l'etat matrimonial antérieur 

fct_freq_etat_matriimonial <- function(x){
  recod <- fct_collapse(factor(x),
                        "Célibataire"="1",
                        "Veuf"="3",
                        "Divorcé"="4")
  return(recod)
}

mariage$ETAMAT1 <- fct_freq_etat_matriimonial(mariage$ETAMAT1 )
mariage$ETAMAT2 <- fct_freq_etat_matriimonial(mariage$ETAMAT2 )

#Recodage de la nationalité

fct_freq_natio <- function(x){
  recod <- fct_collapse(factor(x),
                        "Française"="1",
                        "Etrangere"="2")
  return(recod)
}

mariage$INDNAT1 <- fct_freq_natio(mariage$INDNAT1)
mariage$INDNAT2 <- fct_freq_natio(mariage$INDNAT2)

#Recodage de l'age
mariage$AGE1 <- 2018 - mariage$ANAIS1
mariage$AGE2 <- 2018 - mariage$ANAIS2


#Recodage de Département

fct_freq_dep <- function(x){
  recod <- fct_collapse(factor(x),
                        "01 : Ain"="01",
                        "02 : Aisne"="02",
                        "03 : Allier"="03",
                        "04 : Alpes-de-Haute-Provence"="04",
                        "05 : Hautes-Alpes"="05",
                        "06 : Alpes-Maritimes"="06",
                        "07 : Ardèche"="07",
                        "08 : Ardennes"="08",
                        "09 : Ariège"="09",
                        "10 : Aube"="10",
                        "11 : Aude"="11",
                        "12 : Aveyron"="12",
                        "13 : Bouches-du-Rhône"="13",
                        "14 : Calvados"="14",
                        "15 : Cantal"="15",
                        "16 : Charente"="16",
                        "17 : Charente-Maritime"="17",
                        "18 : Cher"="18",
                        "19 : Corrèze"="19",
                        "2A : Corse-du-Sud"="2A",
                        "2B : Haute-Corse"="2B",
                        "21 : Côte-d'Or"="21",
                        "22 : Côtes-d'Armor"="22",
                        "23 : Creuse"="23",
                        "24 : Dordogne"="24",
                        "25 : Doubs"="25",
                        "26 : Drôme"="26",
                        "27 : Eure"="27",
                        "28 : Eure-et-Loir"="28",
                        "29 : Finistère"="29",
                        "30 : Gard"="30",
                        "31 : Haute-Garonne"="31",
                        "32 : Gers"="32",
                        "33 : Gironde"="33",
                        "34 : Hérault"="34",
                        "35 : Ille-et-Vilaine"="35",
                        "36 : Indre"="36",
                        "37 : Indre-et-Loire"="37",
                        "38 : Isère"="38",
                        "39 : Jura"="39",
                        "40 : Landes"="40",
                        "41 : Loir-et-Cher"="41",
                        "42 : Loire"="42",
                        "43 : Haute-Loire"="43",
                        "44 : Loire-Atlantique"="44",
                        "45 : Loiret"="45",
                        "46 : Lot"="46",
                        "47 : Lot-et-Garonne"="47",
                        "48 : Lozère"="48",
                        "49 : Maine-et-Loire"="49",
                        "50 : Manche"="50",
                        "51 : Marne"="51",
                        "52 : Haute-Marne"="52",
                        "53 : Mayenne"="53",
                        "54 : Meurthe-et-Moselle"="54",
                        "55 : Meuse"="55",
                        "56 : Morbihan"="56",
                        "57 : Moselle"="57",
                        "58 : Nièvre"="58",
                        "59 : Nord"="59",
                        "60 : Oise"="60",
                        "61 : Orne"="61",
                        "62 : Pas-de-Calais"="62",
                        "63 : Puy-de-Dôme"="63",
                        "64 : Pyrénées-Atlantiques"="64",
                        "65 : Hautes-Pyrénées"="65",
                        "66 : Pyrénées-Orientales"="66",
                        "67 : Bas-Rhin"="67",
                        "68 : Haut-Rhin"="68",
                        "69 : Rhône"="69",
                        "70 : Haute-Saône"="70",
                        "71 : Saône-et-Loire"="71",
                        "72 : Sarthe"="72",
                        "73 : Savoie"="73",
                        "74 : Haute-Savoie"="74",
                        "75 : Paris"="75",
                        "76 : Seine-Maritime"="76",
                        "77 : Seine-et-Marne"="77",
                        "78 : Yvelines"="78",
                        "79 : Deux-Sèvres"="79",
                        "80 : Somme"="80",
                        "81 : Tarn"="81",
                        "82 : Tarn-et-Garonne"="82",
                        "83 : Var"="83",
                        "84 : Vaucluse"="84",
                        "85 : Vendée"="85",
                        "86 : Vienne"="86",
                        "87 : Haute-Vienne"="87",
                        "88 : Vosges"="88",
                        "89 : Yonne"="89",
                        "90 : Territoire de Belfort"="90",
                        "91 : Essonne"="91",
                        "92 : Hauts-de-Seine"="92",
                        "93 : Seine-Saint-Denis"="93",
                        "94 : Val-de-Marne"="94",
                        "95 : Val-d Oise"="95",
                        "971 : Guadeloupe"="971",
                        "972 : Martinique"="972",
                        "973 : Guyane"="973",
                        "974 : La Réunion"="974",
                        "975 : Saint-Pierre-et-Miquelon"="975",
                        "976 : Mayotte * (DOM depuis avril 2011)"="976",
                        "977 : Saint-Barthélemy"="977",
                        "978 : Saint-Martin"="978",
                        "984 : Terres australes et antarctiques françaises"="984",
                        "986 : Wallis et Futuna"="986",
                        "987 : Polynésie française"="987",
                        "988 : Nouvelle-Calédonie"="988",
                        "99 : Étranger"="99"
  )
  return(recod)
}

mariage$DEPDOM <- fct_freq_dep(mariage$DEPDOM)
mariage$DEPMAR <- fct_freq_dep(mariage$DEPMAR)
mariage$DEPNAIS1  <- fct_freq_dep(mariage$DEPNAIS1)
mariage$DEPNAIS2  <- fct_freq_dep(mariage$DEPNAIS2)




head(mariage)

mariage<-mariage %>%
  select(SEXE1,SEXE2,AGE1,AGE2,INDNAT1,INDNAT2, ETAMAT1, ETAMAT2,DEPMAR,DEPNAIS1,DEPNAIS2,DEPNAIS1,DEPNAIS2, NBENFCOM )

head(mariage)

write.csv(mariage,"mariage2018.csv", sep = ";", fileEncoding ="utf8")

#########dataframe#####
setwd("C:/Users/white/OneDrive/Bureau/rh")
#install.packages("Require")
library(Require)
library(lubridate)

#install.packages("maps")
library(maps)


# lire de data
mariage_data<-read.csv("mariage2018.csv", sep = ",", fileEncoding ="utf8" )


#########Application###########
###telecharger des library
library("shiny")
library("datasets")
library("pacman")
library("DT")
library("dplyr")
library("ggplot2")
library("leaflet")

#preparation de carte
france<- map_data("france")
View(france)


#app

ui <- fluidPage(
  #titre de l'app
  titlePanel("L’étude sur les mariages dans en France en 2018"),
  hr(),
  p("Cette application est basée sur les données de l'état civil en 2018 en France par INSEE"),
  br(),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Data Table",
                         mainPanel(
                           #il faut modifier le nom de colonne
                           dataTableOutput("dataTable")
                         )),
                tabPanel("Statistique", 
                         h1("Statistique value"),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("col_select", "var_select", names(mariage_data))
                           ),
                           mainPanel(
                             verbatimTextOutput("total")
                           )
                         )
                ),
                #Hist(numeric)     
                tabPanel("Histogram", h1("Histogram"),
                         plotOutput("hist"), 
                         conditionalPanel(
                           condition = "input['hist.x'] !== undefined && $.isNumeric(input['hist.x'])",
                           fluidRow(
                             column(3, htmlOutput("hist.Bins")),
                             column(4, htmlOutput("hist.X")),
                             column(4, htmlOutput("hist.Fill"))
                           )
                         )
                ),
                
                #Bar
                tabPanel("bar", h1("Bar"),
                         plotOutput("bar"), 
                         fluidRow(
                           column(4, htmlOutput("bar.X")),
                           column(4, htmlOutput("bar.Fil"))
                         )
                ),
                tabPanel("Carte",
                         sidebarLayout(
                           sidebarPanel(
                             h3("Filtering data"),
                             selectInput("dataset", "Choose a dataset (or a subset) :", 
                                         choices = sort(c("all biggest cities", "Ile-de-France", "Provence-Alpes-Cote d'Azur", "Rhone-Alpes", 
                                                          "Midi-Pyrenees", "Pays de la Loire", "Alsace", "Languedoc-Roussillon", "Aquitaine", "Nord-Pas-de-Calais")))
                           ), 
                           
                           # MainPanel divided into 2 tabPanels
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Plot", h1("Carte"), leafletOutput("leafletMap", width = "100%", height="700")),
                               tabPanel("Table", h1("Data"), tableOutput("table"))
                             ) 
                           )
                         ))
    ))
)




server <- function(input, output) {
  # Define reactive data
  data <- reactive({
    mariage_data
  })
  
  # Render data table
  output$dataTable <- renderDataTable({
    data()
  })
  
  #Statistique
  output$total <- renderPrint({
    col_name <- input$col_select
    col_data <- mariage_data[[col_name]]
    
    if (is.numeric(col_data)) {
      cat("Moyenne: ", mean(col_data), "\n")
      cat("Minimum: ", min(col_data), "\n")
      cat("Maximum: ", max(col_data), "\n")
    } else {
      cat("Resultat: \n")
      table(col_data)
    }
  })
  
  # Histogram
  output$hist.X <- renderUI({
    selectInput('hist.x', 'x', names(data())[sapply(data(), is.numeric)])
  })
  
  output$hist.Fill <- renderUI({
    selectInput('hist.fill', 'fill', c(None='None', names(data())), selected = NULL)
  })
  
  output$hist <- renderPlot({
    req(input$hist.x)
    
    if (is.numeric(data()[[input$hist.x]])) {
      g <- ggplot(data(), aes_string(x = input$hist.x)) + geom_histogram(bins = input$hist.bins)
      if (input$hist.fill != 'None') {
        req(input$hist.fill)
        g <- g + aes_string(fill = input$hist.fill)
      }
      print(g)
    } else {
      plot(NULL, main = "Please select a numeric variable for histogram")
    }
  })
  
  
  
  #bar
  output$bar.X <- renderUI({
    selectInput('bar.x', 'x', names(data()))
  })
  
  output$bar.Fil <- renderUI({
    selectInput('bar.fil', 'fill', c(None='None', names(data())))
  })
  
  output$bar <- renderPlot({
    g <- ggplot(data(), aes_string(x = input$bar.x)) + geom_bar()
    if (input$bar.fil != 'None') {
      g <- g + aes_string(fill = input$bar.fil)
    }
    print(g)
  })
  
  
  
  
  
}

# Run the app
shinyApp(ui, server)

