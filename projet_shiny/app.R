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
                        "Ain"="01",
                        "Aisne"="02",
                        "Allier"="03",
                        "Alpes-de-Haute-Provence"="04",
                        "Hautes-Alpes"="05",
                        "Alpes-Maritimes"="06",
                        "Ardeche"="07",
                        "Ardennes"="08",
                        "Ariege"="09",
                        "Aube"="10",
                        "Aude"="11",
                        "Aveyron"="12",
                        "Bouches-du-Rhône"="13",
                        "Calvados"="14",
                        "Cantal"="15",
                        "Charente"="16",
                        "Charente-Maritime"="17",
                        "Cher"="18",
                        "Correze"="19",
                        "Corse"="20",
                        "Corse du Sud"="2A",
                        "Haute-Corse"="2B",
                        "Cote-Dor"="21",
                        "Cotes-Darmor"="22",
                        "Creuse"="23",
                        "Dordogne"="24",
                        "Doubs"="25",
                        "Drome"="26",
                        "Eure"="27",
                        "Eure-et-Loir"="28",
                        "Finistere"="29",
                        "Gard"="30",
                        "Haute-Garonne"="31",
                        "Gers"="32",
                        "Gironde"="33",
                        "Herault"="34",
                        "Ille-et-Vilaine"="35",
                        "Indre"="36",
                        "Indre-et-Loire"="37",
                        "Isere"="38",
                        "Jura"="39",
                        "Landes"="40",
                        "Loir-et-Cher"="41",
                        "Loire"="42",
                        "Haute-Loire"="43",
                        "Loire-Atlantique"="44",
                        "Loiret"="45",
                        "Lot"="46",
                        "Lot-et-Garonne"="47",
                        "Lozere"="48",
                        "Maine-et-Loire"="49",
                        "Manche"="50",
                        "Marne"="51",
                        "Haute-Marne"="52",
                        "Mayenne"="53",
                        "Meurthe-et-Moselle"="54",
                        "Meuse"="55",
                        "Morbihan"="56",
                        "Moselle"="57",
                        "Nievre"="58",
                        "Nord"="59",
                        "Oise"="60",
                        "Orne"="61",
                        "Pas-de-Calais"="62",
                        "Puy-de-Dome"="63",
                        "Pyrenees-Atlantiques"="64",
                        "Hautes-Pyrenees"="65",
                        "Pyrenees-Orientales"="66",
                        "Bas-Rhin"="67",
                        "Haut-Rhin"="68",
                        "Rhone"="69",
                        "Haute-Saone"="70",
                        "Saone-et-Loire"="71",
                        "Sarthe"="72",
                        "Savoie"="73",
                        "Haute-Savoie"="74",
                        "Paris"="75",
                        "Seine-Maritime"="76",
                        "Seine-et-Marne"="77",
                        "Yvelines"="78",
                        "Deux-Sevres"="79",
                        "Somme"="80",
                        "Tarn"="81",
                        "Tarn-et-Garonne"="82",
                        "Var"="83",
                        "Vaucluse"="84",
                        "Vendee"="85",
                        "Vienne"="86",
                        "Haute-Vienne"="87",
                        "Vosges"="88",
                        "Yonne"="89",
                        "Territoire de Belfort"="90",
                        "Essonne"="91",
                        "Hauts-de-Seine"="92",
                        "Seine-Saint-Denis"="93",
                        "Val-de-Marne"="94",
                        "Val-Doise"="95",
                        "DOM-TOM"=c("971","972","973","974","975","976","977","978","984","985","986","987","988"),
                        "Etranger"="99"
  )
  return(recod)
}

mariage$DEPDOM <- fct_freq_dep(mariage$DEPDOM)
mariage$DEPMAR <- fct_freq_dep(mariage$DEPMAR)
mariage$DEPNAIS1  <- fct_freq_dep(mariage$DEPNAIS1)
mariage$DEPNAIS2  <- fct_freq_dep(mariage$DEPNAIS2)


#mariage<-mariage %>%
#  select(SEXE1,SEXE2,AGE1,AGE2,INDNAT1,INDNAT2, ETAMAT1, ETAMAT2,dep_mariage_count,dep_domicile_count,dep_nais1_count,dep_nais2_count,NBENFCOM,AMAR,MMAR )
#-> error: Adding missing grouping variables: `DEPNAIS2`

mariage <- mariage %>%
  select(SEXE1,SEXE2,AGE1,AGE2,INDNAT1,INDNAT2, ETAMAT1, ETAMAT2, NBENFCOM, AMAR, MMAR, DEPDOM,DEPMAR,DEPNAIS1,DEPNAIS2) %>%
  ungroup()


#regoupe de region et creer un nouveau df


library(dplyr)
library(forcats)

pop_DEPMAR <- mariage %>%
  count(DEPMAR, name = "pop_DEPMAR") %>%
  mutate(DEP = fct_infreq(as.factor(DEPMAR)))

pop_DEPDOM <- mariage %>%
  count(DEPDOM, name = "pop_DEPDOM") %>%
  mutate(DEP = fct_infreq(as.factor(DEPDOM)))

pop_DEPNAIS1 <- mariage %>%
  count(DEPNAIS1, name = "pop_DEPNAIS1") %>%
  mutate(DEP = fct_infreq(as.factor(DEPNAIS1)))

pop_DEPNAIS2 <- mariage %>%
  count(DEPNAIS2, name = "pop_DEPNAIS2") %>%
  mutate(DEP = fct_infreq(as.factor(DEPNAIS2)))

pop_fr <- full_join(pop_DEPMAR %>% select(DEP, pop_fr = pop_DEPMAR),
                    pop_DEPDOM %>% select(DEP, pop_fr = pop_DEPDOM),
                    by = "DEP") %>%
          full_join(pop_DEPNAIS1 %>% select(DEP, pop_fr = pop_DEPNAIS1),
                    by = "DEP") %>%
          full_join(pop_DEPNAIS2 %>% select(DEP, pop_fr = pop_DEPNAIS2),
                    by = "DEP") %>%
          mutate(pop_fr = rowSums(select(., starts_with("pop_fr")), na.rm = TRUE))

head(pop_fr)# il faut renomer chaque colonne 

pop_fr <- pop_fr %>%
rename(pop_marage = pop_fr.x, pop_domicile = pop_fr.y, pop_nais1=pop_fr.x.x, pop_nais2=pop_fr.y.y,departement = DEP)



#mariage<-mariage %>%
#  select(SEXE1,SEXE2,AGE1,AGE2,INDNAT1,INDNAT2, ETAMAT1, ETAMAT2,dep_mariage_count,dep_domicile_count,dep_nais1_count,dep_nais2_count,NBENFCOM,AMAR,MMAR )
#-> error: Adding missing grouping variables: `DEPNAIS2`

mariage <- mariage %>%
  select(SEXE1,SEXE2,AGE1,AGE2,INDNAT1,INDNAT2, ETAMAT1, ETAMAT2, NBENFCOM, AMAR, MMAR, DEPDOM,DEPMAR,DEPNAIS1,DEPNAIS2) %>%
  ungroup()


write.csv(mariage,"mariage2018.csv", fileEncoding ="utf8")
write.csv(mariage,"pop_fr.csv", fileEncoding ="utf8")

#########dataframe#####
#setwd("C:/Users/white/OneDrive/Bureau/rh")
#install.packages("Require")
library(Require)
library(lubridate)

#Preparation de la Carte en France
#install.packages("maps")
library(maps)


# lire de data
mariage_data<-read.csv("mariage2018.csv", sep = ",", fileEncoding ="utf8" )
pop_fr<--read.csv("pop_fr.csv", sep = ",", fileEncoding ="utf8" )

#########Application###########
###telecharger des library
library("shiny")
library("datasets")
library("pacman")
library("DT")
library("dplyr")
library("ggplot2")
library("leaflet")
library(magrittr)

#preparation de carte
france<- map_data("france")
View(france)

table(france$region)


#app

ui <- fluidPage(
  #titre de l'app
  titlePanel("L’étude sur les mariages dans en France en 2018"),
  hr(),
  p("Cette application est basée sur les données de l'état civil en 2018 en France par INSEE"),
  br(),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("But", h1(kkkkkk),
                         p(fffff)),
                
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
                             h3("Carte de la France et la pop "),
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
  
  pop <- reactive({
    pop_fr
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
  

  #Carte
  # Formatting PopUps
  pop = within(pop, {
    PopUp = paste(PopUp, pop$, df$Population, sep="<br>")
  })
  
  ## Define a new icon 
  cityLeafIcon <- makeIcon(
    iconUrl = "www/city.png",
    iconAnchorX = 10, iconAnchorY = 10,
    shadowUrl = "www/city.png"
  )
  
  # Define server logic 
  shinyServer(function(input, output) {
    datasetInput <- reactive({
      switch(input$dataset,
             "all biggest cities" = df,
             "Ile-de-France" = subset(df, df$Region == "Ile-de-France"),
             "Provence-Alpes-Cote d'Azur" = subset(df, df$Region == "Provence-Alpes-Cote d'Azur"),
             "Rhone-Alpes"  = subset(df, df$Region == "Rhone-Alpes"),
             "Midi-Pyrenees" = subset(df, df$Region == "Midi-Pyrenees"),
             "Pays de la Loire" = subset(df, df$Region == "Pays de la Loire"),
             "Alsace" = subset(df, df$Region == "Alsace"),
             "Languedoc-Roussillon" = subset(df, df$Region == "Languedoc-Roussillon"),
             "Aquitaine" = subset(df, df$Region == "Aquitaine"),
             "Nord-Pas-de-Calais" = subset(df, df$Region == "Nord-Pas-de-Calais"))
    })
    
    ## Show the entire table of our dataset
    output$table <- renderTable({
      datasetInput()[1:5]
    })
    
    ## Create and show our Leaflet Map
    output$leafletMap <- renderLeaflet({
      df <- datasetInput()
      map <- leaflet() %>% 
        addTiles() %>% 
        setView(1.846033, 46.97068, zoom = 6) %>% 
        addMarkers(data = df, lng = ~ Longitude, lat = ~ Latitude, popup = df$PopUp, icon = cityLeafIcon)
      map
    })
    
  })
  
  
  
  
  
}

# Run the app
shinyApp(ui, server)

