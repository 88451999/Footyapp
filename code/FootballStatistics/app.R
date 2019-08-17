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

library(sp)
library(rgdal)
library(ggmap)
library(tmap)
library(adehabitatHR)
library(raster)


setwd("/GitDev/Footyapp/data/") # use here function instead

FootyGames <- read.csv("FootyGamesUs.csv", stringsAsFactors = FALSE)
footy_match_play <- FootyGames %>% #filter(playNumber == 1) %>%
    group_by(gameID, competition, ourName, theirName, playNumber) %>%
    arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
    summarise(playByUs = first(byUs),
              usPhase = first(usPhase),
              oppositionPhase = first(oppositionPhase),
              firstEvent = first(adjEventName),
              lastEvent = last(adjEventName),
              Area = first(case_when(lengthFraction <= 0.333333 ~ "Defence",
                                     lengthFraction >= 0.666667 ~ "Attack",
                                     TRUE ~ "Midfield") ),
              firstEventArea = paste(firstEvent, "-", Area),
              firstPenalyIncursion = first(penaltyIncursion),
              numSequences = max(sequenceNumber),
              numPlayers = max(player),
              isAttackIncursion = max(attackIncursion),
              isPenaltyIncursion = max(penaltyIncursion),
              playPhase = first(ifelse(playByUs == "true", usPhase, oppositionPhase)),
              totalPasses = sum(adjPass),
              possessionDuration = sum(ifelse(pseudoEvent == FALSE &
                                                  byUs == playByUs &
                                                  eventName != "keeper", duration, 0)),
              playDuration = sum(duration),
              goals = sum(ifelse(adjEventName == "goal", 1, 0)),
              shots = sum(ifelse(adjEventName == "shot", 1, 0)) )


FootyGames <- FootyGames %>% group_by(gameID, byUs, playNumber) %>%
    mutate(maxSeq = max(sequenceNumber), 
           playByUs = first(byUs))
footy_pass <- FootyGames %>% filter(playByUs == "false" & #sequenceNumber == 1 &
        adjEventName %in% c("first touch", "kickoff",
                            "throw in", "goal kick", "keeper punt", 
                            "offside restart", "direct restart", "indirect restart"))
# points from scratch
coords = cbind(footy_pass[footy_pass$playNumber > 0, ]$lengthMetres, 
               footy_pass[footy_pass$playNumber > 0, ]$widthMetres)
sp = SpatialPoints(coords)
# make spatial data frame
spdf = SpatialPointsDataFrame(coords, footy_pass[footy_pass$playNumber > 0, ])
kde.output <- kernelUD(spdf, h="href", grid = 1000)


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
            menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                
            # Boxes need to be put in a row (or column)
            fluidPage(
            
            # Application title
            titlePanel("Football Statistics Northbridge Under 13"),
            
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
    tabItem(tabName = "widgets",
            fluidPage(
            mainPanel(
                #   plotOutput("BallPlot"),
                plotOutput("heatPlot"),
                plotOutput("trackMap")
            )
            )
    )
    )
  )
    
)


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
    
    output$heatPlot <- renderPlot({
        ggplot(FootyGames %>% filter(byUs == "false") %>%
                   mutate(zone_x = ceiling(zone/3),
                          zone_y = ifelse(zone%%3==0, 3, zone%%3) ) %>%
                   group_by(zone_x, zone_y, byUs) %>%
                   summarise(ctr = n()), 
                  aes(x=zone_x, y=zone_y, fill=ctr)) +
            geom_tile()
    })
    output$trackMap <- renderPlot({       
        plot(kde.output)
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
