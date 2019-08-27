#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(shiny)
library(scales)
library(DT)

library(sp)
library(rgdal)
library(ggmap)
library(tmap)
library(adehabitatHR)
library(raster)
library(spatstat)
library(spatialkernel)
library(lubridate)


setwd("/GitDev/Footyapp/data/") # use here function instead

FootyGames <- read.csv("FootyGamesUs.csv", stringsAsFactors = FALSE)
footy_match_play <- read.csv("FootyMatchPlayUs.csv", stringsAsFactors = FALSE)
FootyGames$orderDate <- ymd(substr(FootyGames$time, 1, 10))
#write.csv(FootyGames, "FootyGamesUs2.csv", row.names = FALSE)

FootyGames$widthMetres = 75 - FootyGames$widthMetres 
FootyGames$widthFraction = 1 - FootyGames$widthFraction 

footy_pass <- FootyGames %>% filter(playByUs == "false" & #sequenceNumber == 1 &
        adjEventName %in% c("first touch", "kickoff",
                            "throw in", "goal kick", "keeper punt", 
                            "offside restart", "direct restart", "indirect restart"))
# points from scratch
#coords = cbind(footy_pass[footy_pass$playNumber > 0, ]$lengthMetres, 
#               footy_pass[footy_pass$playNumber > 0, ]$widthMetres)
#sp = SpatialPoints(coords)
## make spatial data frame
#spdf = SpatialPointsDataFrame(coords, footy_pass[footy_pass$playNumber > 0, ])
#kde.output <- kernelUD(spdf, h="href", grid = 1000)


comp <- "2019 NSFA 1 Boys Under 13"
team <- "NFC Red"

footy_match_play$teamName = ifelse(footy_match_play$playByUs == "true", footy_match_play$ourName, footy_match_play$theirName)
footy_match_play$orderCol <- case_when(footy_match_play$competition == comp & footy_match_play$teamName == team ~ 0,
                                       footy_match_play$competition %in% c("2019 NSFA 1 Boys Under 13",
                                                                           "2019 NSFA 1 Girls Under 16", "2019 Boys Under 14") ~ 1,
                                       footy_match_play$competition=="2019 International Friendly Mens Open" ~ 2,
                                       footy_match_play$competition=="2019 Icc Football Mens Open" ~ 3,
                                       TRUE ~ 4)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
            menuItem("Team Statistics", tabName = "teamstats", icon = icon("th")),
            menuItem("Game Statistics", tabName = "gamestats", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                
            # Boxes need to be put in a row (or column)
            fluidPage(
            
            # Application title
            titlePanel("Football Statistics Northbridge Under 16"),
            
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    selectInput("FirstEvent",
                                "Phase Play:",
                                choices = c("All" = "all",
                                            "Turnover" = "first touch",
                                            "Throw In" = "throw in", 
                                            "Goal Kick" = "goal kick", 
                                            "Corner Kick" = "corner kick", 
                                            "Kick Off" = "kick off",    
                                            "Keeper Punt" = "keeper punt",
                                            "Foul Restart" = "foul"),
                                selected = "all" ),
                    checkboxInput("Passes", "Include no passes",
                                  value = FALSE),
                    selectInput("Opponent",
                                "Opponent:",
                                choices = c("All", unique(footy_match_play$theirName)),
                                selected = "All" ),
                    checkboxInput("theirStats", "Show Opponent Against us",
                                  value = FALSE),
                    width = 3
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                    #   plotOutput("BallPlot"),
                    plotOutput("possPlot")
                )
            )
        )
    ),
    
    # Second tab content
    tabItem(tabName = "heatmap",
            fluidPage(
              tabsetPanel(
                tabPanel("Heat", plotOutput("heatPlot"), plotOutput("heatPlot1")),
                tabPanel("Scat", plotOutput("scatPlot")),
                tabPanel("Table", DT::dataTableOutput("heatTable"))
              )
            ,
            hr(),
            fluidRow(
              selectInput("hmFirstEvent",
                            "Phase Play:",
                            choices = c("All" = "all",
                                        "Turnover" = "first touch",
                                        "Throw In" = "throw in", 
                                        "Goal Kick" = "goal kick", 
                                        "Corner Kick" = "corner kick", 
                                        "Kick Off" = "kick off",    
                                        "Keeper Punt" = "keeper punt",
                                        "Foul Restart" = "foul"),
                            selected = "all" ),
                checkboxInput("hmPasses", "Include no passes",
                              value = FALSE),
                selectInput("hmOpponent",
                            "Opponent:",
                            choices = c("All", unique(FootyGames$theirName)),
                            selected = "All" ),
                checkboxInput("hmtheirStats", "Show Opponent Against us",
                              value = FALSE),
              
              selectInput("hmResult",
                          "Play Result:",
                          choices = c("All" = "all",
                                      "Attacking 3rd" = "AttackIncursion",
                                      "Goal" = "goal"),
                          selected = "all" ),
              checkboxInput("hmJustFirst", "Show just 1st contact",
                            value = FALSE),
              selectInput("hmMatch",
                          "Match:",
                          choices = c("All", rev(unique(FootyGames$matchName))),
                          selected = "All"
              )
              )
      ) # fluidpage tabitem 2
    ), # tab item 2
    
    # Third tab content
    tabItem(tabName = "teamstats",
            fluidPage(
              
              
              # Application title
              titlePanel("Football Statistics Northbridge Under 13"),
              
              # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  selectInput("tsFirstEvent",
                              "Phase Play:",
                              choices = c("All" = "all",
                                          "Turnover" = "first touch",
                                          "Throw In" = "throw in", 
                                          "Goal Kick" = "goal kick", 
                                          "Corner Kick" = "corner kick", 
                                          "Kick Off" = "kick off",    
                                          "Keeper Punt" = "keeper punt",
                                          "Foul Restart" = "foul"),
                              selected = "all" ),
                  checkboxInput("tsPasses", "Include no passes",
                                value = TRUE),
                  width = 3
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("teamPlot")#,
               #   plotOutput("teamPlotThem")
                )
              )
            ) # fluidpage tabitem 3
    ), # tab item 3
    
    # Third tab content
    tabItem(tabName = "gamestats",
            fluidPage(
              
              # Application title
              titlePanel("Football Statistics Northbridge Under 13"),
              
              fluidRow(
                  selectInput("gsMatch",
                              "Match:",
                              choices = c(rev(unique(FootyGames$matchName)))
                              ),
                  selectInput("gsType",
                              "Possession:",
                              choices = c("All" = "all",
                                          "Defensive Possession" = "defPossession",
                                          "Attacking Possession" = "attPossession"),
                              selected = "all"
                  )
              ),
                # Show a plot of the generated distribution
hr(),
              plotOutput("gamePlot")#,
            ) # fluidpage tabitem 4
    ) # tab item 4
        ) # tabitems
    
) # dashboardBody
) # dashboard page


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$BallPlot <- renderPlot({
        ggplot(footy_match_play %>% 
            ungroup %>% group_by(teamName, orderCol) %>%
            summarise(ppm = sum(totalPasses)/ (sum(possessionDuration/60))) ,
        aes(x=reorder(teamName, orderCol, FUN=max), y= ppm, fill=teamName) ) +
        geom_bar( stat = "identity", position = "dodge") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              plot.title = element_text(hjust=0.5)) + 
        labs(x = "Team", y = "Passes", 
             title = "Passes Per Minute")
    })
    
    output$possPlot <- renderPlot({
        print(input$FirstEvent)
        data <- footy_match_play
        if (input$FirstEvent != "all") {
            if (input$FirstEvent == "foul") {
                eventsList <- c("offside restart", "direct restart", "indirect restart")
            } else {
                eventsList <- input$FirstEvent
            }
            data <- data %>% filter(firstEvent %in% eventsList)
        }
        if (!input$Passes) {
            data <- data %>% filter(totalPasses > 0)
        }
        if (input$Opponent != "All") {
            data <- data %>% filter(theirName == input$Opponent)
        }
        ggplot(data %>% filter(playByUs == ifelse(input$theirStats, "false", "true")) %>% 
                   ungroup() %>% #group_by(ourName) %>%
        mutate(tp = n() ) %>%
        group_by(totalPasses) %>%
        summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
    aes(x=totalPasses, y= perc) ) +
        geom_bar(stat = "identity", fill = "light blue") +
        geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 0.5)) +
        theme(plot.title = element_text(hjust=0.5)) + 
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(x = "Number of Passes", y = "Percentage", 
             title = "Passes per Play - Us") 
    })
    
    my_heat <- reactive({
      print(input$hmResult)
      data <- FootyGames
      if (input$hmFirstEvent != "all") {
        if (input$hmFirstEvent == "foul") {
          eventsList <- c("offside restart", "direct restart", "indirect restart")
        } else {
          eventsList <- input$hmFirstEvent
        }
        data <- data %>% filter(firstEvent %in% eventsList)
      }
      if (!input$hmPasses) {
        data <- data %>% filter(totPass > 0)
      }
      if (input$hmResult != "All") {
        if (input$hmResult == "goal") {
          data <- data %>% filter(isGoal == 1) 
        } else if (input$hmResult == "AttackIncursion") {
        data <- data %>% filter(isAttackIncursion == 1)
        }
      }
      if (input$hmJustFirst) {
        data <- data %>% filter(sequenceNumber == 1)
      }
      if (input$hmOpponent != "All") {
        data <- data %>% filter(theirName == input$hmOpponent)
      }
      if (input$hmMatch != "All") {
        data <- data %>% filter(matchName == input$hmMatch)
      }
      data <- data %>% filter(playByUs == ifelse(input$hmtheirStats, "false", "true"))
      
      data
    })
    
    output$heatPlot <- renderPlot({
      print(input$hmFirstEvent)

      pppxy <- ppp(my_heat()$lengthMetres, my_heat()$widthMetres,
                   c(0,110), c(0,75), checkdup=FALSE)
      plot(density(pppxy, diggle=TRUE), multiplot=FALSE)
      
    })
    
    output$heatPlot1 <- renderPlot({
      print(input$hmFirstEvent)
      ggplot(my_heat(), aes(x = lengthMetres, y = widthMetres)) + 
        stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
        geom_point(size = 1, alpha = 0.5) + 
        coord_fixed(xlim=c(0, 110), ylim=c(0, 75)) +
        scale_fill_gradient2(low = "white", high = "red")
      
      
    })
    
    output$heatTable <- DT::renderDataTable({
      # Show the gapminder object in the table
      my_heat() %>% select(lengthMetres, widthMetres, teamName)
    })
    output$scatPlot <- renderPlot({

      ggplot(my_heat(),
             aes(x=lengthMetres, y= widthMetres) ) +
        geom_point() +
      coord_fixed(xlim=c(0, 110), ylim=c(0, 75))
    })
    
    my_team <- reactive({
      data <- footy_match_play
      if (input$tsFirstEvent != "all") {
        if (input$tsFirstEvent == "foul") {
          eventsList <- c("offside restart", "direct restart", "indirect restart")
        } else {
          eventsList <- input$tsFirstEvent
        }
        data <- data %>% filter(firstEvent %in% eventsList)
      }
      if (!input$tsPasses) {
        data <- data %>% filter(totalPasses > 0)
      }
      data
    })

     output$teamPlot <- renderPlot({

      ggplot(my_team() %>% #filter(playByUs == "true") %>% 
               ungroup() %>% #group_by(ourName) %>%
               group_by(matchName, gameDate, playByUs) %>%
               summarise(totP = n(),
                         totZero = sum(ifelse(totalPasses == 0, 1, 0)),
                         tot1_3 = sum(ifelse(totalPasses > 0 & totalPasses < 4, 1, 0)),
                         tot4_6 = sum(ifelse(totalPasses > 3 & totalPasses < 7, 1, 0)),
                         tot7Plus = sum(ifelse(totalPasses > 6, 1, 0))
                  ) %>% gather(key = "PassRange", value = "Measurement",
                               totZero, tot1_3, tot4_6, tot7Plus),
              aes(x=reorder(matchName, gameDate, FUN=max), y= Measurement, 
                  fill = factor(PassRange,
                          levels = c("tot7Plus", "tot4_6", "tot1_3", "totZero"),
                          labels = c("7 Plus", "4 - 6", "1 - 3", 'Zero')) ) ) +
        geom_bar(position = "fill",stat = "identity") +
        scale_fill_discrete(name="Passes",
                             labels=c("7 Plus", "4 - 6", "1 - 3", 'Zero')) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust=0.5)) + 
        labs(x = "Team", y = "Pass Sequence Percentage", 
             title = "Play Sequence by Passes (Us)") +
        facet_grid(rows = vars(factor(playByUs,
                               labels = c("Opponents", "Us")) ) )
     })
     
   output$teamPlotThem <- renderPlot({ 
     
      ggplot(my_team() %>% filter(playByUs == "false") %>% 
               ungroup() %>% #group_by(ourName) %>%
               group_by(matchName, gameDate) %>%
               summarise(totP = n(),
                         totZero = sum(ifelse(totalPasses == 0, 1, 0)),
                         tot1_3 = sum(ifelse(totalPasses > 0 & totalPasses < 4, 1, 0)),
                         tot4_6 = sum(ifelse(totalPasses > 3 & totalPasses < 7, 1, 0)),
                         tot7Plus = sum(ifelse(totalPasses > 6, 1, 0))
               ) %>% gather(key = "PassRange", value = "Measurement",
                            totZero, tot1_3, tot4_6, tot7Plus),
             aes(x=reorder(matchName, gameDate, FUN=max), y= Measurement, 
                 fill = factor(PassRange,
                               levels = c("tot7Plus", "tot4_6", "tot1_3", "totZero"),
                               labels = c("7 Plus", "4 - 6", "1 - 3", 'Zero')) ) ) +
        geom_bar(position = "fill",stat = "identity") +
        scale_fill_discrete(name="Passes") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust=0.5)) + 
        labs(x = "Team", y = "Pass Sequence Percentage", 
             title = "Play Sequence by Passes (Oppositions)")
    })
   
   output$gamePlot <- renderPlot({
     print(input$gsType)
     data <- FootyGames %>% filter(matchName == input$gsMatch)
     if (input$gsType != "all") {
       if (input$gsType == "defPossession") {
         print("Defense")
         data <- data %>%
           gather(key = "TeamPossession", value = "TimePossesion",
                  ourDefensivePossession, theirDefensivePossession)
       } else {
         print("Attack")
         data <- data  %>%
           gather(key = "TeamPossession", value = "TimePossesion",
                  ourAttackingPossession, theirAttackingPossession)
       }
       } else {
         data <- data %>%
           gather(key = "TeamPossession", value = "TimePossesion",
                  ourPossession, theirPossession)
         
       }
     
     ggplot(data ,
            aes(x=epochTime, y= TimePossesion, colour=TeamPossession)) +
       geom_line() 
   })
   

    
}


# Run the application 
shinyApp(ui = ui, server = server)
