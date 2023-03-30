
#install.packages("Require")
library(Require)
library(lubridate)

# lire de data
mariage_data <- rio::import(here::here("mariage2018.csv")) %>% 
  as_tibble()


#########Application###########
###telecharger des library

library("shiny")
library("datasets")
library("pacman")
library("DT")
library("dplyr")

# Import data
mariage_data <- rio::import(here::here("mariage2018.csv"))%>% 
  
  head(mariage_data)

# Statistiques
summary(mariage_data$AGE1)  
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  17.00   30.00   35.00   38.57   45.00   98.00 
summary(mariage_data$AGE2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.0    28.0    33.0    36.5    43.0    92.0 

ana_stat <-function(mariage_data) {
  classes <- mariage_data %>% summarise_all(class) %>% t()
  means <- mariage_data %>% select(!where(is.character)) %>% summarise_all(mean, na.rm=TRUE) %>% t()
  vars <- mariage_data %>% select(!where(is.character)) %>% summarise_all(var, na.rm=TRUE) %>% t()
  sds <- mariage_data %>% select(!where(is.character)) %>% summarise_all(sd, na.rm=TRUE) %>% t()
  qs <- mariage_data %>% select(!where(is.character)) %>% summarise_all(quantile, na.rm=TRUE) %>% t()
  colnames(qs) <- c("min", "q14", "median", "q34", "max")
  is.nas <- df %>% is.na %>% as.data.frame %>% summarise_all(sum) %>% t()
  
  stat.1 <- data.frame(
    rownames=rownames(classes),
    classes=classes,
    is.nas=is.nas,
    is.not.nas=nrow(mariage_data)-is.nas
  )
  
  stat.2 <- data.frame(
    rownames=rownames(means),
    means=means,
    vars=vars,
    sds=sds,
    qs
  )
  
  stat <- full_join(stat.1, stat.2, by="rownames")
  
  return(stat)
}

ana_cor <- function(mariage_data) {
  r <- mariage_data %>% select(!where(is.character)) %>% cor
  return(r)
}



###App

# Define UI
ui <- fluidPage(
  #titre de l'app
  titlePanel("L’étude sur les mariages dans le Nord en 2018"),
  hr(),
  h4("Cette application est basée sur les données de mariage en 2018 en France par INSEE"),
  br(),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Datatable",
        mainPanel(
          #il faut modifier le nom de colonne
          dataTableOutput("dataTable")
        )
      ),
      #Stat
      tabPanel("Statistique", tableOutput("outstat")),
      tabPanel("Correlation", tableOutput("outCor")),
      
      #Graphique
      tabPanel(
        "Histogram",plotOutput("hist"), 
        sidebarLayout(
          sidebarPanel(fluidRow(
            column(3, htmlOutput("Hist.Bins")),
            column(4, htmlOutput("Hist.X")),
            column(4, htmlOutput("Hist.Fill"))
          )
          ))),
      
      tabPanel(
        "Bar", plotOutput("bar"), 
        fluidRow(
          column(4, htmlOutput("Bar.X")),
          column(4, htmlOutput("Bar.Fil"))
        ),
        
        
        tabPanel(
          "Carte",
          hr(),
        )
      )
    )
  )
)


# Define server

server <- function(input, output) {
  # Define reactive data
  data <- reactive({
    mariage_data
  })
  
  #logique de stat
  stat <- reactive({
    tmp <- mariage_data
    if (is.null(tmp)) {
      return(NULL)
    } else {
      res <- ana_stat(tmp)
      return(res)
    }
  })
  
  r <- reactive({
    tmp <- mariage_data
    if (is.null(tmp)) {
      return(NULL)
    } else {
      res <- ana_cor(tmp)
      return(res)
    }
  })
  
  
  # Render data table
  output$dataTable <- renderDataTable({
    data()
  })
  
  #Statistique
  output$outStat <- DT::renderDataTable({
    data.frame(stat())
  }, extensions = c('Buttons'), 
  options = list(dom = 'Blfrtip',
                 buttons = c('csv', 'excel', 'pdf'))
  )
  
  output$outCor <- DT::renderDataTable({
    data.frame(r())
  }, extensions = c('Buttons'), 
  options = list(dom = 'Blfrtip',
                 buttons = c('csv', 'excel', 'pdf'))
  )
  
  #Histogram
  output$Hist.Bins <- renderUI({
    sliderInput('hist.bins', 'bins', min=1, max=(round(nrow(data())/10)), value=(round(nrow(data())/20)))
  })
  
  output$Hist.X <- renderUI({
    selectInput('hist.x', 'x', names(data()))
  })
  
  output$Hist.Fill <- renderUI({
    selectInput('hist.fill', 'fill', c(None='None', names(data())))
  })
  
  output$hist <- renderPlot({
    g <- ggplot(data(), aes_string(x = input$hist.x)) + geom_histogram(bins = input$hist.bins)
    if (input$hist.fill != 'None') {
      g <- g + aes_string(fill = input$hist.fill)
    }
    print(g)
  })
  
  #bar
  output$Bar.X <- renderUI({
    selectInput('bar.x', 'x', names(data()))
  })
  
  output$Bar.Fil <- renderUI({
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

