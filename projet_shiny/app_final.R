######################
#    projet_shiny    #
######################
# une application 
# - point
#   +automatique
#   +au moins d'une grapique

###telecharger des library
library("shiny")
library("tidyverse")
library("questionr")
library("dplyr")
library("forcats")
library("datasets")
#library("pacman")
library("DT")
library("ggplot2")
library("leaflet")
#library("magrittr")
library("geojsonio")
#library("Require")
#library("lubridate")
library("rgdal")
library("sf")

###nettoyer des bdds

##1: telecharger une donnée
mariage<-read.csv("FD_MAR_2018.csv", sep = ";", fileEncoding ="utf8" )
str(mariage)

##2: recodage de sexe

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


##3: recodage de l'etat matrimonial antérieur 

fct_freq_etat_matriimonial <- function(x){
  recod <- fct_collapse(factor(x),
                        "Célibataire"="1",
                        "Veuf"="3",
                        "Divorcé"="4")
  return(recod)
}

mariage$ETAMAT1 <- fct_freq_etat_matriimonial(mariage$ETAMAT1 )
mariage$ETAMAT2 <- fct_freq_etat_matriimonial(mariage$ETAMAT2 )

##4Recodage de la nationalité

fct_freq_natio <- function(x){
  recod <- fct_collapse(factor(x),
                        "Française"="1",
                        "Etrangere"="2")
  return(recod)
}

mariage$INDNAT1 <- fct_freq_natio(mariage$INDNAT1)
mariage$INDNAT2 <- fct_freq_natio(mariage$INDNAT2)

##5Recodage de l'age
mariage$AGE1 <- 2018 - mariage$ANAIS1
mariage$AGE2 <- 2018 - mariage$ANAIS2


##6Recodage de Département

fct_freq_dep <- function(x){
  recod <- fct_collapse(factor(x),
                        "Ain"="01",
                        "Aisne"="02",
                        "Allier"="03",
                        "Alpes-de-Haute-Provence"="04",
                        "Hautes-Alpes"="05",
                        "Alpes-Maritimes"="06",
                        "Ardèche"="07",
                        "Ardennes"="08",
                        "Ariège"="09",
                        "Aube"="10",
                        "Aude"="11",
                        "Aveyron"="12",
                        "Bouches-du-Rhône"="13",
                        "Calvados"="14",
                        "Cantal"="15",
                        "Charente"="16",
                        "Charente-Maritime"="17",
                        "Cher"="18",
                        "Corrèze"="19",
                        "Corse"="20",
                        "Corse-du-Sud"="2A",
                        "Haute-Corse"="2B",
                        "Côte-d'Or"="21",
                        "Côtes-d'Armor"="22",
                        "Creuse"="23",
                        "Dordogne"="24",
                        "Doubs"="25",
                        "Drôme"="26",
                        "Eure"="27",
                        "Eure-et-Loir"="28",
                        "Finistère"="29",
                        "Gard"="30",
                        "Haute-Garonne"="31",
                        "Gers"="32",
                        "Gironde"="33",
                        "Hérault"="34",
                        "Ille-et-Vilaine"="35",
                        "Indre"="36",
                        "Indre-et-Loire"="37",
                        "Isère"="38",
                        "Jura"="39",
                        "Landes"="40",
                        "Loir-et-Cher"="41",
                        "Loire"="42",
                        "Haute-Loire"="43",
                        "Loire-Atlantique"="44",
                        "Loiret"="45",
                        "Lot"="46",
                        "Lot-et-Garonne"="47",
                        "Lozère"="48",
                        "Maine-et-Loire"="49",
                        "Manche"="50",
                        "Marne"="51",
                        "Haute-Marne"="52",
                        "Mayenne"="53",
                        "Meurthe-et-Moselle"="54",
                        "Meuse"="55",
                        "Morbihan"="56",
                        "Moselle"="57",
                        "Nièvre"="58",
                        "Nord"="59",
                        "Oise"="60",
                        "Orne"="61",
                        "Pas-de-Calais"="62",
                        "Puy-de-Dôme"="63",
                        "Pyrénées-Atlantiques"="64",
                        "Hautes-Pyrénées"="65",
                        "Pyrénées-Orientales"="66",
                        "Bas-Rhin"="67",
                        "Haut-Rhin"="68",
                        "Rhône"="69",
                        "Haute-Saône"="70",
                        "Saône-et-Loire"="71",
                        "Sarthe"="72",
                        "Savoie"="73",
                        "Haute-Savoie"="74",
                        "Paris"="75",
                        "Seine-Maritime"="76",
                        "Seine-et-Marne"="77",
                        "Yvelines"="78",
                        "Deux-Sèvres"="79",
                        "Somme"="80",
                        "Tarn"="81",
                        "Tarn-et-Garonne"="82",
                        "Var"="83",
                        "Vaucluse"="84",
                        "Vendée"="85",
                        "Vienne"="86",
                        "Haute-Vienne"="87",
                        "Vosges"="88",
                        "Yonne"="89",
                        "Territoire de Belfort"="90",
                        "Essonne"="91",
                        "Hauts-de-Seine"="92",
                        "Seine-Saint-Denis"="93",
                        "Val-de-Marne"="94",
                        "Val-d Oise"="95",
                        "Guadeloupe"="971",
                        "Martinique"="972",
                        "Guyane"="973",
                        "La Réunion"="974",
                        "Saint-Pierre-et-Miquelon"="975",
                        "Mayotte"=c("976","985"),
                        "Saint-Barthélemy"="977",
                        "Saint-Martin"="978",
                        "Terres australes et antarctiques françaises"="984",
                        "Wallis et Futuna"="986",
                        "Polynésie française"="987",
                        "Nouvelle-Calédonie"="988",
                        "Étranger"="99"
  )
  return(recod)
}

mariage$DEPDOM <- fct_freq_dep(mariage$DEPDOM)
mariage$DEPMAR <- fct_freq_dep(mariage$DEPMAR)
mariage$DEPNAIS1  <- fct_freq_dep(mariage$DEPNAIS1)
mariage$DEPNAIS2  <- fct_freq_dep(mariage$DEPNAIS2)


mariage <- mariage %>%
  select(SEXE1,SEXE2,AGE1,AGE2,INDNAT1,INDNAT2, ETAMAT1, ETAMAT2, NBENFCOM, AMAR, MMAR, DEPDOM,DEPMAR,DEPNAIS1,DEPNAIS2) %>%
  ungroup()


##7regoupe de region et creer un nouveau df

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


mariage <- mariage %>%
  select(SEXE1,SEXE2,AGE1,AGE2,INDNAT1,INDNAT2, ETAMAT1, ETAMAT2) %>%
  ungroup()


write.csv(mariage,"mariage2018.csv", fileEncoding ="utf8")
write.csv(pop_fr,"pop_fr.csv", fileEncoding ="utf8")



#########dataframe#####
# lire de data
mariage_data<-read.csv("mariage2018.csv", sep = ",", fileEncoding ="utf8" )
pop_fr<-read.csv("pop_fr.csv", sep = ",", fileEncoding ="utf8" )
france <- st_read(dsn = "departements-20180101.shp")

pop_dep <- unique(pop_fr$departement)
france_dep <- unique(france$nom)

common_dep <- intersect(pop_dep, france_dep)
code_insee <- france$code_insee[match(common_dep, france_dep)]

pop_fr <- pop_fr %>%
  mutate(code_insee = code_insee[match(departement, common_dep)])


#app

ui <- fluidPage(
  #titre de l'app
  titlePanel("L’étude sur les mariages dans en France en 2018"),
  hr(),
  p("Cette application permet d'analyser l'état du mariage en France par Les mariages en 2018"),
  br(),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("But", h1("Présentation de base donnée"),
                         p("Nous avons décidé de faire une analyse quantitative sur les mariages célébrés."),
                         p("On va regarder les informations générales sur le jeu de données de l’état civil en 2018
                         concernant les mariages fourni par l’INSEE."),
                         P("url:https://www.insee.fr/fr/statistiques/4273672?sommaire=4273674"),
                         hr(),
                         h2("Utilisation"),
                         p("La rubrique Data Table permet d'obtenir une vue détaillée des données utilisées."),
                         p("Dans la rubrique statistiques, si vous sélectionnez une variable dans la boîte de sélection, vous pouvez voir la moyenne, les valeurs maximales et minimales pour les variables de type numérique. Dans le cas des variables de type texte, vous pouvez voir combien de groupes existent pour chaque variable et compter le nombre de chacun d'entre eux.
                         La rubrique Bar permet d'afficher un diagramme en barres."),
                         p("Dans x, vous sélectionnez la variable qui sera l'axe des abscisses, et bar.Fil est le filtre."),
                         p("Enfin, La rubrique Carte affiche la population mariée par département."),
                         hr(),
                         h2("Donnée et variables"),
                         p("Voici la liste des variables dans ce jeu des données:"),
                         p("1.AMAR Année de mariage"),
                         p("2.ANAIS1 Année de naissance du conjoint 1"),
                         p("3.ANAIS2 Année de naissance du conjoint 2"),
                         p("3.DEPDOM Département de domicile après le mariage"),
                         p("4.DEPMAR Département de mariage"),
                         p("5.DEPNAIS1 Département de naissance du conjoint 1"),
                         p("6.DEPNAIS2 Département de naissance du conjoint 2"),
                         p("7.ETAMAT1 État matrimonial antérieur du conjoint 1"),
                         p("8.ETAMAT2 État matrimonial antérieur du conjoint 2"),
                         p("9.INDNAT1 Indicateur de nationalité du conjoint 1"),
                         p("10.INDNAT2 Indicateur de nationalité du conjoint 2"),
                         p("11.JSEMAINE Jour de mariage dans la semaine"),
                         p("12.MMAR Mois de mariage : 01-12"),
                         p("13.NBENFCOM Enfants en commun avant le mariage"),
                         p("14.SEXE1 Sexe du conjoint 1"),
                         p("15.SEXE2 Sexe du conjoint 2")),
                
                
                tabPanel("Data Table",
                         mainPanel(
                           #il faut modifier le nom de colonne
                           dataTableOutput("dataTable")
                         )),
                tabPanel("Statistique", 
                         p("Statistique value :Sélectionner une variable."),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("col_select", "choix de variable", names(mariage_data))),
                           mainPanel(
                             verbatimTextOutput("total"))
                         )),
                
                #Bar
                tabPanel("Bar", p("Un graphique à barres s'affiche ; sélectionnez une variable sur l'axe des x et filtrez les variables qui vous intéressent."),
                         plotOutput("bar"), 
                         fluidRow(
                           column(4, htmlOutput("bar.X")),
                           column(4, htmlOutput("bar.Fil"))
                         )),
                #Carte
                tabPanel("Carte",h1("Carte"),
                         leafletOutput(outputId = "map")
                )
    )
  )
)


server <- function(input, output) {
  # Define reactive data
  data <- reactive({
    mariage_data
  })
  
  pop <- reactive({
    pop_fr
  })
  
  map <- reactive({
    france
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
  
  

  # Carte
  # montre map france
  output$map <- renderLeaflet({
    leaflet(data = map()) %>%
      addTiles() %>%
      addPolygons(fillColor = "white", color = "#444444", weight = 1, fillOpacity = 0.5) %>%
      addPolygons(data = pop(), fillColor = "red", color = "#444444", weight = 1, fillOpacity = 0.5, 
                  popup = paste("Code INSEE:", pop()$code_insee, "<br>", 
                                "Département:", pop()$departement, "<br>",
                                "Population de mariage:", pop()$pop_mariage))
  })
  
}




# Run the app
shinyApp(ui, server)

