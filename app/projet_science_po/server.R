## @author : LEBRIAT Mattéo 
# @date : 27/07/2021
# @title : Open Data Sciences Politiques Avignon
# @descriptions : This is the server logic of a Shiny web application. 
#                 You can run theapplication by clicking 'Run App' above.
#                 Find out more about building applications with Shiny here:
#
#                 http://shiny.rstudio.com/
#

#Installation of packages

#install.packages("shiny")
#install.packages("openxlsx") 
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("gapminder")
#install.packages("raster")

#Loading librairies
library(shiny)
library(openxlsx)
library(readxl)
library(ggplot2)
library(plotly)
library(gapminder)
library(raster)
library(sp)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$contents <- DT::renderDataTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
              if (tools::file_ext(input$file1$datapath) == "csv")
                {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               fileEncoding = "UTF-8")
              }
              else if (tools::file_ext(input$file1$datapath) == "txt"){
                df <- read.table(input$file1$datapath,header=TRUE, sep="")
              }
              else if (tools::file_ext(input$file1$datapath) == "xlsx"){
                df <- read.xlsx(input$file1$datapath)
              }
              else if (tools::file_ext(input$file1$datapath) == "xls"){
                df <- read_xls(input$file1$datapath)
              }
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
      DT::datatable(df, options = list(paging = FALSE))
      ## Formatting PopUps
    })
    
    output$simplePlot <- renderPlot({
      req(input$file1)
      
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "UTF-8")
      }
        switch(input$Xvar,
               "Votants" = df$Votants,
               "Exprimés" = df$Exprimés, 
               "Inscrits" = df$Inscrits)
      
        switch(input$Yvar,
               
               "Votants" = df$Votants,
               "Exprimés" = df$Exprimés, 
               "Inscrits" = df$Inscrits)
      
      
      plot(df[,c(input$Xvar,input$Yvar)], xlab= input$Xvar, ylab= input$Yvar,
           main="Répartition des votes")
      
    })
    
    output$summary <- renderPrint({
      req(input$file1)
      
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "UTF-8")
      }
      summary(df)
    })
    
    output$boxPlot <- renderPlot({
      req(input$file1)
      
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "UTF-8")
      }
      
        boxplot(df[,c(input$Xvar,input$Yvar)] , input$Xvar~input$Yvar, 
                main="Répartition des votes")
    })
    
    # K-Means Plot
    output$NbClust <- renderText({ 
      paste("K-means clustering performed with ", input$clusters," clusters.")
    })
    
    output$kmeansPlot <- renderPlot({
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "iso-8859-1")
      }
      df<- data.matrix(na.omit(df))
      
      
      acp <- prcomp(df[,1:ncol(df)])
      names(acp)
      head(acp$x)
      
      #On visualise les données graphiquement
      plot(acp$x[,1],acp$x[,2], pch=16)
      text(acp$x[,1],acp$x[,2], rownames(df), cex=0.8)
      KM <- kmeans(df[,1:ncol(df)], center=input$clusters)
      
      #On fait un plot pour visualiser les données du Kmeans 
      plot(acp$x[,1], acp$x[,2] ,pch=KM$cluster, col=KM$centers, cex=1.5)
      points(KM$centers,col=KM$centers, pch=10, cex=0.9)
      legend("bottomright", legend=colnames(df[,1:ncol(df)]),
             col=KM$centers, pch = KM$cluster, cex=0.9)
      
    })
    
    
    output$tauxParticipation <- renderPlotly ({
      req(input$file1)
      
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "UTF-8")
      }
      taux <- (df$Votants/df$Inscrits)*100
      ggp <- ggplot(df, aes(département, taux, color=département,
                            text = paste("Taux de participation: ", taux,
                                         "<br>Département : $", département))) +
        geom_point()+
        ggtitle("Taux de participation par département")
      
      fig <- ggplotly(ggp)
      fig
      
      
    })
    
    output$tauxAbstention <- renderPlotly ({
      req(input$file1)
      
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "UTF-8")
      }
      taux <- trunc(100-(df$Votants/df$Inscrits)*100)
      ggp <- ggplot(df, aes(département, taux, color=département,
                            text = paste("Taux d'abstention: ", taux,"%",
                                         "<br>Département : $", département))) +
        geom_point()+
        ggtitle("Taux d'abstention par département")
      
      fig <- ggplotly(ggp)
      fig
      
      
    })
    output$geoPlot <- renderPlot({
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "UTF-8")
      }
      
      taux <- trunc((df$Votants/df$Inscrits)*100)
      adm_fr <- getData('GADM', country='FRA', level=2)
      
      z_niveau_foliaire = sort(abs(taux))
      #Créer un gradient de couleurs triées en fonction du vecteur z
      colfunc<-colorRampPalette(taux)
      colors <- (colfunc(100))
      colors <- colors[rank(z_niveau_foliaire)]
      par(fig=c(0.2,1,0,0.9), mar = c(6,4,0.5,2), new=TRUE)
      

      plot(adm_fr,col=taux)
      # Graphique 2 : fenêtre de la légende
      par(fig=c(0,0.2,0,0.9), mar = c(2,0,0.5,2), new=TRUE)
      # Préparer une image raster : gradient de couleur de la légende
      legend_image <- as.raster(matrix(rev(colfunc(21)), ncol=1.5))
      # Tracer un graphique blanc à la place de la légende
      
      plot(c(0,5),c(0,2),type = 'n', axes = F, xlab = '', ylab = '', main = 'Taux de participation par département',cex.main=0.5)
      # Tracer les graduations de la légende, ici le paramétrage en est strictement manuel 5 paliers entre 0 et 1
      text(x=2, y = seq(0,1,l=5), labels = seq(0,1,l=5))
      # Ajouter l'image de gradient
      rasterImage(legend_image, 0, 0, 1,1)
    })
    
    output$geoPlotAbs <- renderPlot({
      if (tools::file_ext(input$file1$datapath) == "csv")
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       fileEncoding = "UTF-8")
      }
      adm_fr <- getData('GADM', country='FRA', level=2)
      
      # Coloriser proportionnellement aux nombres d'admis.
      n= 10 # nombre de niveaux souhait?s
      ## Niveaux de couleurs : j'utilise la fonction cut
      tauxAbstention <- trunc(100-(df$Votants/df$Inscrits)*100)
      tauxAbstention
      
      z_niveau_foliaire = sort(abs(tauxAbstention))
      #Créer un gradient de couleurs triées en fonction du vecteur z
      colfunc<-colorRampPalette(tauxAbstention)
      colors <- (colfunc(100))
      colors <- colors[rank(z_niveau_foliaire)]
      par(fig=c(0.2,1,0,0.9), mar = c(6,4,0.5,2), new=TRUE)
      
      
      plot(adm_fr,col=tauxAbstention)
      # Graphique 2 : fenêtre de la légende
      par(fig=c(0,0.2,0,0.9), mar = c(2,0,0.5,2), new=TRUE)
      # Préparer une image raster : gradient de couleur de la légende
      legend_image <- as.raster(matrix(rev(colfunc(21)), ncol=1.5))
      # Tracer un graphique blanc à la place de la légende
      
      plot(c(0,5),c(0,2),type = 'n', axes = F, xlab = '', ylab = '', main = 'Taux de participation par département',cex.main=0.5)
      # Tracer les graduations de la légende, ici le paramétrage en est strictement manuel 5 paliers entre 0 et 1
      text(x=2, y = seq(0,1,l=5), labels = seq(0,1,l=5))
      # Ajouter l'image de gradient
      rasterImage(legend_image, 0, 0, 1,1)
    })
    

    output$mySite <- renderUI({
      tags$a(href = input$website, input$website)
    })
    output$link_xls <- renderUI({
      tags$a(href = input$link, input$link)
    })
})
