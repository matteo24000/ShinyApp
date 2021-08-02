# @author : LEBRIAT Mattéo 
# @date : 27/07/2021
# @title : Open Data Sciences Politiques Avignon
# @descriptions : This is the user-interface definition of a Shiny web application. 
#                 You can run the application by clicking 'Run App' above.
#                 Find out more about building applications with Shiny here:
#
#                 http://shiny.rstudio.com/
#

#Installation of packages

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")
#install.packages("shinyWidgets")
#install.packages("magrittr")
#install.packages("sf")
#install.packages("plotly)
#install.packages("leaflet")
#install.packages("vioplot")

#Loading libraries
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(magrittr)
library(sf)
library(plotly)
library(leaflet)
library(vioplot)

# Define UI for application
ui <-  dashboardPage(
    dashboardHeader(title = "Sciences Politiques Avignon",
                    titleWidth = 400
                    ),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Accueil", tabName = "home", icon = icon("home")),
            menuItem("Télécharger les données", tabName = "download", icon = icon("download")),
            menuItem("Ouverture", tabName = "dashboard", icon = icon("folder-open")),
            menuItem("Visualisation graphique", tabName = "visualisation", icon = icon("chart-bar")),
            menuItem("Carte intéractive", tabName = "mapVisualisation", icon = icon("globe"))
        )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
        tabItems(
          # Second tab content
          tabItem(tabName = "home",
                  box(
                    title = "Bienvenue sur l'outil R Shiny des Sciences Politiques d'Avignon", width = 8, solidHeader = TRUE, status = "primary",
                    "Cette application a été développé par ",strong("LEBRIAT Mattéo"), ", un étudiant de Licence Porfessionnelle Développeur Big Data de l'IUT de Périgueux.",br(),
                    "Grâce à cet outil, vous pouvez charger des données de votes issues de data.gouv.fr et voir leur interprétation.",
                  ),
                  box(
                    title = "Le contexte de cette application Shiny", width = 8, solidHeader = TRUE, status = "primary",
                    "Dans le cadre de la Licence Professionnelle Développeur Big Data de l'IUT de Périgueux, le projet tutoré est une partie importante de la formation.", br(),br(),
                    "En d'autre terme, le projet tutoré est un travail réalisé par un ou plusieurs étudiants dans le cadre d'une formation à l'université. Leur objectif est de mettre 
                     en pratique des savoirs acquis pendant le cours à partir d'une problématique technique conrète proposée par un professeur ou une entreprise.",br(),br(),
                    "Dans le cas de Mattéo, le projet lui a été proposé par son professeur", strong("BOUSQUET Julien"),", qui avait une demande précise de l'université des Sciences Politiques d'Avignon.",
                    "Le projet était de constuire une application en RShiny permettant de visualiser graphiquement les données de votes.",br(),br(), 
                    "La solution qu'il a pu développer durant cette année de formation correspondrait aux attentes fixées. En effet, on peut grâce à son application RShiny télécharger les données isssues du site
                    ", a("https://www.data.gouv.fr", href="https://data.gouv.fr"), ", les ouvrir pour voir ce qu'elles contiennent, puis les visualiser graphiquement selon différents critères.", style = "text-align: justify;"
                  ),          ),
          tabItem(tabName = "download",
                  box(
                    title = "Télécharger les données de votes issues du site data.gouv.fr", width = 10, solidHeader = TRUE, status = "primary",
                    "Sur cette page, vous allez pouvoir télécharger les jeux de données sur les votes issues du site data.gouv.fr.",br()
                  ),
                  box(
                    title = "Fichiers CSV", width = 10, solidHeader = TRUE, status = "primary",
                      selectInput('website', 'Choisissez un lien CSV'
                                  , read.csv("C:/Users/lebriat/Desktop/Projet Sciences Po Avignon/app/import_data/liens_csv_page_web")
                      )
                      , htmlOutput("mySite")
                  ),
                  box(
                    title = "Fichiers XLS et XLSX", width = 10, solidHeader = TRUE, status = "primary",
                    selectInput('link', 'Choisissez un lien XLS ou XLSX'
                                , read.csv("C:/Users/lebriat/Desktop/Projet Sciences Po Avignon/app/import_data/liens_xls_page_web")
                    )
                    , htmlOutput("link_xls")
                  ),
          ),
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(
                            title = "Ouverture des données ", width = 8, solidHeader = TRUE, status = "primary",
                            "Sur cette page, vous pourrez ajouter et visualiser les données.",br(),
                        ),
                        box(
                            title = "Selectionnez des données", width = 8, solidHeader = TRUE, status = "primary",
                            # Input: Select a file ----
                            fileInput("file1", "Choisissez un fichier",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            # Horizontal line ----
                            tags$hr(),
                            
                            p(strong("Filtre pour l'affichage")),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Checkbox if file has header ----
                            checkboxInput("header", "Afficher le titre des colonnes", TRUE),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select separator ----
                            radioButtons("sep", "Séparateur",
                                         choices = c(Virgule = ",",
                                                     "Point-virgule" = ";",
                                                     Tabulation = "\t"),
                                         selected = ","),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select number of rows to display ----
                            radioButtons("disp", "Affichage",
                                         choices = c("Afficher quelques lignes" = "head",
                                                     "Afficher toutes les lignes" = "all"),
                                         selected = "head"),
                        ),
                        # Main panel for displaying outputs ----
                          box( title = "Lecture des données", status = "primary", width = 8 ,solidHeader = T, 
                                      DT::dataTableOutput("contents", width=500),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                        ),
                    ),
                    
            ),
            # Second tab content
            tabItem(tabName = "visualisation",
                    box(
                      title = "Visualisation graphique des jeux de données", width = 10, solidHeader = TRUE, status = "primary",
                      "Dans cette onglet, vous pourrez visualiser les données de façon graphique en fonction de différents critères."
                    ),
                    # Sidebar with controls
                      sidebarLayout(
                        sidebarPanel(
                          h3("Filtrer les données CSV :"),
                          selectInput("Xvar", "Choisir la variable X", 
                                      choices = c("Votants", "Exprimés","Inscrits")),
                          selectInput("Yvar", "Choisir la variable Y", 
                                      choices = c("Votants", "Exprimés", "Inscrits"), selected = "Inscrits"),
                          h3("K-Means"),
                          numericInput("clusters", "Cluster count", 3, min = 1, max = 9),
                        ),
                        # MainPanel divided into many tabPanel
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Affichage générale", h1("Scatterplot", style ="font-weight:bold;"), plotOutput("simplePlot"), h1("Boxplot", style ="font-weight:bold;"), plotOutput("boxPlot")),
                            tabPanel("Description statistiques", h1("Description statistiques", style ="font-weight:bold;"),verbatimTextOutput("summary")),
                            tabPanel("Taux de participation", h1("Taux de participation", style ="font-weight:bold;"), plotlyOutput("tauxParticipation"), heigth=100),
                            tabPanel("Taux d'abstention", h1("Taux d'abstention", style ="font-weight:bold;"), plotlyOutput("tauxAbstention"), heigth=100),
                            tabPanel("Regroupement Partis Politiques", h1("K-Means", style ="font-weight:bold;"), textOutput("NbClust"), plotOutput("kmeansPlot"), textOutput("KMlegendes"))
                          ) 
                        )
                      )
                    ),
          tabItem(tabName = "mapVisualisation",
                  box(
                    title = "Carte intéractive des jeux de données", width = 10, solidHeader = TRUE, status = "primary",
                    "Dans cette onglet, vous pourrez visualiser les données sur une carte intéractive."
                  ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Taux de participation par département", h1("Taux de particpation par département", style ="font-weight:bold;"), plotOutput("geoPlot")),
                        tabPanel("Taux d'abstention par département", h1("Taux d'abstention par département", style ="font-weight:bold;"),plotOutput("geoPlotAbs"))
                      ) 
                    )
                  )
                )
          )
    )


          
                    
        
    


