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
library(ggsoccer)


setwd("/GitDev/Footyapp/data/") # use here function instead

FootyGames <- read.csv("FootyGames113.csv", stringsAsFactors = FALSE)
footy_match_play <- read.csv("FootyMatchPlay13.csv", stringsAsFactors = FALSE)
footy_match_age <- read.csv("FootyMatchAgeFull.csv", stringsAsFactors = FALSE)

FootyGames$orderDate <- ymd(substr(FootyGames$time, 1, 10))
#write.csv(FootyGames, "FootyGamesUs2.csv", row.names = FALSE)

#footy_match_play$teamName = ifelse(footy_match_play$playByUs == "true", footy_match_play$ourName, footy_match_play$theirName)

#FootyGames$widthMetres = 75 - FootyGames$widthMetres 
#FootyGames$widthFraction = 1 - FootyGames$widthFraction 

FootyGames$lengthMetres = ifelse(FootyGames$playByUs == "false", 110 - FootyGames$lengthMetres, FootyGames$lengthMetres)
FootyGames$widthMetres = ifelse(FootyGames$playByUs == "false", 75 - FootyGames$widthMetres, FootyGames$widthMetres)
FootyGames$lengthFraction = ifelse(FootyGames$playByUs == "false", 1 - FootyGames$lengthFraction, FootyGames$lengthFraction)
FootyGames$widthFraction = ifelse(FootyGames$playByUs == "false", 1 - FootyGames$widthFraction, FootyGames$widthFraction)

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

stats_field = list()
stats_field$length = 110
stats_field$width = 75
stats_field$penalty_box_length = 16.5
stats_field$penalty_box_width = 40.32
stats_field$six_yard_box_length = 5.5
stats_field$six_yard_box_width = 18.32
stats_field$penalty_spot_distance = 11
stats_field$goal_width = 7.32
stats_field$origin_x = 0
stats_field$origin_y = 0

# Setup metaData for Season Reporting
listGraphs <- list()
listGraphs[["AttackSides"]] <- list(columns=c("leftAttack", "rightAttack", "middleAttack"),
                                    levels=c("leftAttack", "middleAttack", "rightAttack"),
                                    labels=c("Left", "Middle", "Right"),
                                    title="Channel into Attacking Third",
                                    ylab="Attack Channel",
                                    type="both")
listGraphs[["TurnoverDirection"]] <- list(columns=c("backPassTurnover", "sidePassTurnover", "fwdPassTurnover"),
                                          levels=c("sidePassTurnover", "backPassTurnover", "fwdPassTurnover"),
                                          labels=c("Side", "Back", "Forward"),
                                          title="First Pass Direction After Turnover",
                                          ylab="Pass Direction",
                                          type="both")
listGraphs[["RegionDuration"]] <- list(columns=c("AttackDuration", "MidfieldDuration", "DefenceDuration"),
                                       levels=c("AttackDuration", "MidfieldDuration", "DefenceDuration"),
                                       labels=c("Attack", "Midfield", "Defence"),
                                       title="Time Spent in Each Region",
                                       ylab="Region",
                                       type="both")
listGraphs[["PassDirection"]] <- list(columns=c("totalBackPasses", "totalSidePasses", "totalFwdPasses"),
                                       levels=c("totalBackPasses", "totalSidePasses", "totalFwdPasses"),
                                       labels=c("Back", "Side", "Forward"),
                                       title="Direction of Passes",
                                       ylab="Direction",
                                      type="both")
listGraphs[["PassLength"]] <- list(columns=c("total10mPasses", "total10_20mPasses", "total20mPasses"),
                                      levels=c("total10mPasses", "total10_20mPasses", "total20mPasses"),
                                      labels=c("< 10m", "10 - 20m", "20m +"),
                                      title="Length of Forward Passes",
                                      ylab="Length",
                                   type="both")
listGraphs[["RegionDistance"]] <- list(columns=c("attackDistance", "midfieldDistance", "defenceDistance"),
                                   levels=c("attackDistance", "midfieldDistance", "defenceDistance"),
                                   labels=c("Attack", "Midfield", "Defence"),
                                   title="Distance of Ball in Regions",
                                   ylab="Region",
                                   type="both")
listGraphs[["RegionPass"]] <- list(columns=c("attackPasses", "midfieldPasses", "defencePasses"),
                                       levels=c("attackPasses", "midfieldPasses", "defencePasses"),
                                       labels=c("Attack", "Midfield", "Defence"),
                                       title="Number of Passes in Regions",
                                       ylab="Region",
                                   type="both")
listGraphs[["CrossfieldPlay"]] <- list(columns=c("crossfieldPlay"),
                                   levels=c("crossfieldPlay"),
                                   labels=c("CrossField"),
                                   title="Changes of Attack in Defence",
                                   ylab="Number",
                                   type="count")
listGraphs[["ppm"]] <- list(columns=c("ppm"),
                                       levels=c("ppm"),
                                       labels=c("Passes Per Minute"),
                                       title="Passes Per Minute",
                                       ylab="Passes",
                                       type="count")
listGraphs[["velocity"]] <- list(columns=c("aveVelocity"),
                            levels=c("aveVelocity"),
                            labels=c("Velocity"),
                            title="Velocity of Ball in Possession",
                            ylab="Velocity",
                            type="count")
listGraphs[["backKeeper"]] <- list(columns=c("backKeeperPasses"),
                                 levels=c("backKeeperPasses"),
                                 labels=c("Back to Keeper"),
                                 title="Number of Passes back to Keeper Box",
                                 ylab="Passes",
                                 type="count")
listGraphs[["RegionMoves"]] <- list(columns=c("MAPlay", "AMPlay", "MDPlay", "DMPlay"),
                                   levels=c("MAPlay", "AMPlay", "MDPlay", "DMPlay"),
                                   labels=c("Mid - Att", "Att - Mid", "Mid - Def", "Def - Mid"),
                                   title="Switch Between Regions",
                                   ylab="Plays",
                                   type="Both")
listGraphs[["PassSequence"]] <- list(columns=c("passZero", "pass1", "pass2_3", "pass4_6", "pass7Plus"),
                                    levels=c("pass7Plus", "pass4_6", "pass2_3", "pass1", "passZero"),
                                    labels=c("7+", "4 - 6", "2 - 3", "1", "0"),
                                    title="Number of Passes per Play",
                                    ylab="Possession",
                                    type="Both")
listGraphs[["ppmAttack"]] <- list(columns=c("ppmAttack"),
                            levels=c("ppmAttack"),
                            labels=c("Passes Per Minute"),
                            title="Passes Per Minute in Attack Zone",
                            ylab="Passes",
                            type="count")
listGraphs[["ppmMidfield"]] <- list(columns=c("ppmMidfield"),
                                  levels=c("ppmMidfield"),
                                  labels=c("Passes Per Minute"),
                                  title="Passes Per Minute in Midfield Zone",
                                  ylab="Passes",
                                  type="count")
listGraphs[["ppmDefence"]] <- list(columns=c("ppmDefence"),
                                  levels=c("ppmDefence"),
                                  labels=c("Passes Per Minute"),
                                  title="Passes Per Minute in Defence Zone",
                                  ylab="Passes",
                                  type="count")
listGraphs[["attackVelocity"]] <- list(columns=c("attackVelocity"),
                                 levels=c("attackVelocity"),
                                 labels=c("Velocity"),
                                 title="Velocity of Ball in Attack Zone",
                                 ylab="Velocity",
                                 type="count")
listGraphs[["midfieldVelocity"]] <- list(columns=c("midfieldVelocity"),
                                       levels=c("midfieldVelocity"),
                                       labels=c("Velocity"),
                                       title="Velocity of Ball in Midfield Zone",
                                       ylab="Velocity",
                                       type="count")
listGraphs[["defenceVelocity"]] <- list(columns=c("defenceVelocity"),
                                         levels=c("defenceVelocity"),
                                         labels=c("Velocity"),
                                         title="Velocity of Ball in Defence Zone",
                                         ylab="Velocity",
                                         type="count")
listGraphs[["goalRegion"]] <- list(columns=c("goalAttack", "goalMidfield", "goalDefence"),
                                        levels=c("goalAttack", "goalMidfield", "goalDefence"),
                                        labels=c("Attack", "Midfield", "Defence"),
                                        title="Zone Where Goals Start From",
                                        ylab="Goals",
                                        type="both")
listGraphs[["goalTransition"]] <- list(columns=c("goalTurnover", "goalRestart"),
                                   levels=c("goalTurnover", "goalRestart"),
                                   labels=c("Turnover", "Restart"),
                                   title="Play Where Goals Start From",
                                   ylab="Goals",
                                   type="both")

stat_value = "stack"


comp <- "2019 NSFA 1 Boys Under 13"
team <- "NFC Red"

footy_match_play$orderCol <- case_when(footy_match_play$competition == comp & footy_match_play$teamName == team ~ 0,
                                       footy_match_play$competition %in% c("2019 NSFA 1 Boys Under 13",
                                                                           "2019 NSFA 1 Girls Under 16", "2019 Boys Under 14") ~ 1,
                                       footy_match_play$competition=="2019 International Friendly Mens Open" ~ 2,
                                       footy_match_play$competition=="2019 Icc Football Mens Open" ~ 3,
                                       TRUE ~ 4)

print(unique(FootyGames$matchName))
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
            menuItem("Playmap", tabName = "playmap", icon = icon("th")),
            menuItem("Team Statistics", tabName = "teamstats", icon = icon("th")),
            menuItem("Age Group Statistics", tabName = "agestats", icon = icon("th")),
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
                  #  checkboxInput("theirStats", "Show Opponent Against us",
                  #                value = FALSE),
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
                tabPanel("Heat", plotOutput("heatPlot"), 
                                 plotOutput("heatPlota"), 
                                 plotOutput("heatPlot1")),
             #   tabPanel("Scat", plotOutput("scatPlot")),
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
              #  checkboxInput("hmtheirStats", "Show Opponent Against us",
              #                value = FALSE),
              
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
  
    # Second tab content
    tabItem(tabName = "playmap",
            fluidPage(
              tabsetPanel(
                tabPanel("Play", plotOutput("playPlot"),
                         plotOutput("playPlot1"))
              )
              ,
              hr(),
              fluidRow(
                sliderInput("pmGameTime", "Time:",
                            min = 0, max = 100, value = c(0, 100)
                ),
                selectInput("pmFirstEvent",
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
                selectInput("pmOpponent",
                            "Opponent:",
                            choices = c("All", unique(FootyGames$theirName)),
                            selected = "All" ),
                selectInput("pmResult",
                            "Play Result:",
                            choices = c("All" = "all",
                                        "Attacking 3rd" = "AttackIncursion",
                                        "Goal" = "goal"),
                            selected = "all" ),
                selectInput("pmMatch",
                            "Match:",
                            choices = c(rev(unique(FootyGames$matchName)))
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
                  selectInput("tsAttribute",
                              "Statistic:",
                              choices = c("Turnover Direction" = "TurnoverDirection",
                                          "Attack Channel" = "AttackSides", 
                                          "Pass Direction" = "PassDirection", 
                                          "Pass Length" = "PassLength",
                                          "Region Duration" = "RegionDuration",
                                          "Region Distance" = "RegionDistance", 
                                          "Region Passes" = "RegionPass", 
                                          "Crossfield Plays" = "CrossfieldPlay", 
                                          "Passes Per Minute" = "ppm", 
                                          "Play Speed" = "velocity",
                                          "Back to Keeper" = "backKeeper",
                                          "Region Moves" = "RegionMoves",
                                          "Passing Sequences" = "PassSequence", 
                                          "Passes Per Minute (Attack)" = "ppmAttack", 
                                          "Passes Per Minute (Midfield)" = "ppmMidfield",
                                          "Passes Per Minute (Defence)" = "ppmDefence", 
                                          "Play Speed (Attack)" = "attackVelocity",
                                          "Play Speed (Midfield)" = "midfieldVelocity", 
                                          "Play Speed (Defence)" = "defenceVelocity", 
                                          "Goals (Starting Region)" = "goalRegion", 
                                          "Goals (Starting Event)" = "goalTransition"),
                              selected = "TurnoverDirection" ),
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
                  radioButtons("tsType", "Statistic type:",
                               c("Proportions" = "fill",
                                 "Counts" = "stack"),
                               selected = "fill"),
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
    tabItem(tabName = "agestats",
            fluidPage(
              
              
              # Application title
              titlePanel("Football Statistics Northbridge Under 13"),
              
              # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  selectInput("asAttribute",
                              "Statistic:",
                              choices = c("Turnover Direction" = "TurnoverDirection",
                                          "Attack Channel" = "AttackSides", 
                                          "Pass Direction" = "PassDirection", 
                                          "Pass Length" = "PassLength",
                                          "Region Duration" = "RegionDuration",
                                          "Region Distance" = "RegionDistance", 
                                          "Region Passes" = "RegionPass", 
                                          "Crossfield Plays" = "CrossfieldPlay", 
                                          "Passes Per Minute" = "ppm", 
                                          "Play Speed" = "velocity",
                                          "Back to Keeper" = "backKeeper",
                                          "Region Moves" = "RegionMoves",
                                          "Passing Sequences" = "PassSequence", 
                                          "Passes Per Minute (Attack)" = "ppmAttack", 
                                          "Passes Per Minute (Midfield)" = "ppmMidfield",
                                          "Passes Per Minute (Defence)" = "ppmDefence", 
                                          "Play Speed (Attack)" = "attackVelocity",
                                          "Play Speed (Midfield)" = "midfieldVelocity", 
                                          "Play Speed (Defence)" = "defenceVelocity", 
                                          "Goals (Starting Region)" = "goalRegion", 
                                          "Goals (Starting Event)" = "goalTransition"),
                              selected = "TurnoverDirection" ),
                  selectInput("asFirstEvent",
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
                  radioButtons("asType", "Statistic type:",
                               c("Proportions" = "fill",
                                 "Counts" = "stack"),
                               selected = "fill"),
                  width = 3
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("agePlot")#,
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
        ggplot(data %>% #filter(playByUs == ifelse(input$theirStats, "false", "true")) %>% 
                   ungroup() %>% #group_by(ourName) %>%
                 group_by(playByUs) %>%
        mutate(tp = n() ) %>%
        group_by(totalPasses, playByUs) %>%
        summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
    aes(x=totalPasses, y= perc) ) +
        geom_bar(stat = "identity", fill = "light blue") +
        geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 0.5)) +
        theme(plot.title = element_text(hjust=0.5)) + 
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(x = "Number of Passes", y = "Percentage", 
             title = "Passes per Play - Us") +
          facet_grid(rows = vars(factor(playByUs,
                                        ordered = TRUE,
                                        levels = c("true", "false"),
                                        labels = c("Us", "Opponents"))) )
        
    })
    
    my_heat <- reactive({
      print(input$hmResult)
      print(nrow(FootyGames))
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
        data <- data %>% filter(numberPasses > 0)
      }
      print(nrow(data))
      if (input$hmResult != "All") {
        if (input$hmResult == "goal") {
          data <- data %>% filter(isGoal == 1) 
        } else if (input$hmResult == "AttackIncursion") {
        data <- data %>% filter(isAttackIncursion == 1)
        }
      }
      print(nrow(data))
      if (input$hmJustFirst) {
        data <- data %>% filter(sequenceNumber == 1)
      }
      print(nrow(data))
      if (input$hmOpponent != "All") {
        data <- data %>% filter(theirName == input$hmOpponent)
      }
      if (input$hmMatch != "All") {
        data <- data %>% filter(matchName == input$hmMatch)
      }

      data
    })
    
    output$heatPlot <- renderPlot({
      print(input$hmFirstEvent)
      print(nrow(my_heat()))
      data <- my_heat() %>% filter(playByUs == "true")
      pppxy <- ppp(data$lengthMetres, data$widthMetres,
                   c(0,110), c(0,75), checkdup=FALSE)
      plot(density(pppxy, diggle=TRUE), multiplot=FALSE)
      
    })
    
    output$heatPlota <- renderPlot({
      print(input$hmFirstEvent)
      print(nrow(my_heat()))
      data <- my_heat() %>% filter(playByUs == "false")
      pppxy <- ppp(data$lengthMetres, data$widthMetres,
                   c(0,110), c(0,75), checkdup=FALSE)
      plot(density(pppxy, diggle=TRUE), multiplot=FALSE)
      
    })
    
    output$heatPlot1 <- renderPlot({
      print(input$hmFirstEvent)
      ggplot(my_heat() %>% filter(playByUs == "true"), aes(x = lengthMetres, y = widthMetres)) + 
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

      ggplot(my_heat() %>% group_by(gameID, playNumber) %>%
               arrange(gameID, playNumber, sequenceNumber) %>%
               mutate(x1 = lead(lengthMetres),
                      y1 = lead(widthMetres)) %>%
               ungroup(), #%>%
              # filter(sequenceNumber != maxSequenceNumber), 
             aes(x = lengthMetres, y = widthMetres, 
                 xend = x1, yend = y1)) +
        annotate_pitch(dimensions = stats_field, fill = "light green") +
        geom_segment(data = . %>% filter(sequenceNumber != maxSequenceNumber)) +
        geom_point(aes(color=factor(ifelse(sequenceNumber == maxSequenceNumber &
                                             byUs != playByUs, 1, 0)))) +
        theme_pitch() +
        theme(legend.position = "none")
        
      
    })

    my_play <- reactive({
      print(nrow(FootyGames))
      data <- FootyGames
      if (input$pmFirstEvent != "all") {
        if (input$pmFirstEvent == "foul") {
          eventsList <- c("offside restart", "direct restart", "indirect restart")
        } else {
          eventsList <- input$pmFirstEvent
        }
        data <- data %>% filter(firstEvent %in% eventsList)
      }
      if (input$pmResult != "All") {
        if (input$pmResult == "goal") {
          data <- data %>% filter(isGoal == 1) 
        } else if (input$pmResult == "AttackIncursion") {
          data <- data %>% filter(isAttackIncursion == 1)
        }
      }
      data <- data %>% filter(matchName == input$pmMatch)
      print(nrow(data))
      start_time = min(data$epochTime)
      end_time = max(data$epochTime)
      
      data <- data %>% filter(epochTime >= start_time + (end_time - start_time) * input$pmGameTime[1]/100 &
                                epochTime <= start_time + (end_time - start_time) * input$pmGameTime[2]/100)
      
      print(nrow(data))
      data
    })
    
    
    output$playPlot <- renderPlot({
      
      #  ggplot(my_heat(),
      #         aes(x=lengthMetres, y= widthMetres) ) +
      #    geom_point() +
      #  coord_fixed(xlim=c(0, 110), ylim=c(0, 75))
      
      data <- my_play() %>% filter(playByUs == "true")
      ggplot(data %>% group_by(gameID, playNumber) %>%
               arrange(gameID, playNumber, sequenceNumber) %>%
               mutate(x1 = lead(lengthMetres),
                      y1 = lead(widthMetres)) %>%
               ungroup(), #%>%
             # filter(sequenceNumber != maxSequenceNumber), 
             aes(x = lengthMetres, y = widthMetres, 
                 xend = x1, yend = y1)) +
        annotate_pitch(dimensions = stats_field, fill = "light green") +
        geom_segment(data = . %>% filter(sequenceNumber != maxSequenceNumber)) +
        geom_point(aes(color=factor(ifelse(sequenceNumber == maxSequenceNumber &
                                             byUs != playByUs, 1, 0)))) +
        theme_pitch() +
        theme(legend.position = "none")
      
      
    })
    
    output$playPlot1 <- renderPlot({
      
      #  ggplot(my_heat(),
      #         aes(x=lengthMetres, y= widthMetres) ) +
      #    geom_point() +
      #  coord_fixed(xlim=c(0, 110), ylim=c(0, 75))
      
      data <- my_play() #%>% filter(playByUs == "false")
      ggplot(data %>% group_by(gameID, playNumber) %>%
               arrange(gameID, playNumber, sequenceNumber) %>%
               mutate(x1 = lead(lengthMetres),
                      y1 = lead(widthMetres)) %>%
               ungroup(), #%>%
             # filter(sequenceNumber != maxSequenceNumber), 
             aes(x = lengthMetres, y = widthMetres, 
                 xend = x1, yend = y1)) +
        annotate_pitch(dimensions = stats_field, fill = "light green") +
        geom_segment(data = . %>% filter(sequenceNumber != maxSequenceNumber),
                     mapping = aes(color = playByUs)) +
        geom_point(aes(color=factor(ifelse(sequenceNumber == maxSequenceNumber &
                                             byUs != playByUs, 1, 0)))) +
        theme_pitch() +
        theme(legend.position = "none")
      
      
    })
    
        
    my_team <- reactive({
      data <- footy_match_play %>%
        mutate(goalAttack = ifelse(isGoal == 1 & Area == "Attack", 1, 0),
               goalMidfield = ifelse(isGoal == 1 & Area == "Midfield", 1, 0),
               goalDefence = ifelse(isGoal == 1 & Area == "Defence", 1, 0),
               goalTurnover = ifelse(isGoal == 1 & firstEvent == "first touch", 1, 0),
               goalRestart = ifelse(isGoal == 1 & firstEvent != "first touch", 1, 0)
               )
      
      if (input$tsFirstEvent != "all") {
        if (input$tsFirstEvent == "foul") {
          eventsList <- c("offside restart", "direct restart", "indirect restart")
        } else {
          eventsList <- input$tsFirstEvent
        }
        data <- data %>% filter(firstEvent %in% eventsList)
      }
      data
    })

     output$teamPlot <- renderPlot({

     game_level <- my_team() %>% filter(playNumber > 0) %>%
         mutate(gameDate = substr(gameDate, 1, 10)) %>%
         group_by(gameID, matchName, playByUs, gameDate) %>%
         summarise_if(is.numeric, list(sum), na.rm = TRUE) %>%
         mutate(ppm = totalPasses/(possessionDuration/60),
                ppmAttack = attackPasses/(AttackDuration/60),
                ppmMidfield = midfieldPasses/(MidfieldDuration/60),
                ppmDefence = defencePasses/(DefenceDuration/60),
                aveVelocity = distance/(possessionDuration),
                attackVelocity = attackDistance/(AttackDuration),
                midfieldVelocity = midfieldDistance/(MidfieldDuration),
                defenceVelocity = defenceDistance/(DefenceDuration)
                )
       
       HomeWins <- game_level  %>% 
         group_by(matchName, gameDate)  %>%
         summarise(wld = case_when(sum(ifelse(playByUs=="true", isGoal, 0))==sum(ifelse(playByUs=="false", isGoal, 0)) ~ 2,
                                       sum(ifelse(playByUs=="true", isGoal, 0)) < sum(ifelse(playByUs=="false", isGoal, 0)) ~ 1,
                                           TRUE ~ 3)) 
       myPalette <- c("red", "black", "blue")
       
      if (listGraphs[[input$tsAttribute]]$type != "count") {
      ggplot(game_level %>% 
               gather(key = "Key", value = "Measurement",
                      listGraphs[[input$tsAttribute]]$columns),
             aes(x=reorder(matchName, gameDate, FUN=max), y= Measurement, 
                 fill = factor(Key, ordered = TRUE,
                               levels = listGraphs[[input$tsAttribute]]$levels,
                               labels = listGraphs[[input$tsAttribute]]$labels) 
             ) ) + 
        geom_bar(position = input$tsType, stat = "identity") +
        scale_fill_discrete(name="") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                         color=myPalette[unlist(HomeWins[order(HomeWins$gameDate), 3])]),
              plot.title = element_text(hjust=0.5)) + 
        labs(x = "Team", y = listGraphs[[input$tsAttribute]]$ylab, 
             title = listGraphs[[input$tsAttribute]]$title,
             legend = "") +
        facet_grid(rows = vars(factor(playByUs,
                                      ordered = TRUE,
                               levels = c("true", "false"),
                               labels = c("Us", "Opponents"))) )
      } else {
      #  print(paste(game_level$ppm, game_level$totalPasses, game_level$possessionDuration))
        ggplot(game_level %>% 
                 gather(key = "Key", value = "Measurement",
                        listGraphs[[input$tsAttribute]]$columns),
               aes(x=reorder(matchName, gameDate, FUN=max), y= Measurement, 
                   fill = factor(Key, ordered = TRUE,
                                 levels = listGraphs[[input$tsAttribute]]$levels,
                                 labels = listGraphs[[input$tsAttribute]]$labels) 
               ) ) + 
          geom_bar(position = "stack", stat = "identity") +
          scale_fill_discrete(name="") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                           color=myPalette[unlist(HomeWins[order(HomeWins$gameDate), 3])]),
                plot.title = element_text(hjust=0.5)) + 
          labs(x = "Team", y = listGraphs[[input$tsAttribute]]$ylab, 
               title = listGraphs[[input$tsAttribute]]$title,
               legend = "") +
          facet_grid(rows = vars(factor(playByUs,
                                        ordered = TRUE,
                                        levels = c("true", "false"),
                                        labels = c("Us", "Opponents"))) )
        
        
        
        
      }
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
         data <- data  %>%
           gather(key = "TeamPossession", value = "TimePossesion",
                  ourPossession, theirPossession)
         
       }
     
     ggplot(data ,
            aes(x=epochTime, y= TimePossesion, colour=TeamPossession)) +
       geom_line() 
   })
   
   
   my_age <- reactive({
     data <- footy_match_age 
     
     if (input$asFirstEvent != "all") {
       if (input$asFirstEvent == "foul") {
         eventsList <- c("offside restart", "direct restart", "indirect restart")
       } else {
         eventsList <- input$asFirstEvent
       }
       data <- data %>% filter(firstEvent %in% eventsList)
     }
     data
   })
   
   output$agePlot <- renderPlot({
     print(paste(input$asAttribute, listGraphs[[input$asAttribute]]$type))
     game_level <- my_age() #%>% filter(playNumber > 0) %>%
#       group_by(age_group, playByUs) %>%
#       summarise_if(is.numeric, list(sum), na.rm = TRUE) %>%
#       mutate(ppm = totalPasses/(possessionDuration/60),
#              ppmAttack = attackPasses/(AttackDuration/60),
#              ppmMidfield = midfieldPasses/(MidfieldDuration/60),
#              ppmDefence = defencePasses/(DefenceDuration/60),
#              aveVelocity = distance/(possessionDuration),
#              attackVelocity = attackDistance/(AttackDuration),
#              midfieldVelocity = midfieldDistance/(MidfieldDuration),
#              defenceVelocity = defenceDistance/(DefenceDuration)
#       )
     if (listGraphs[[input$asAttribute]]$type != "count") {
       ggplot(game_level %>% 
                gather(key = "Key", value = "Measurement",
                       listGraphs[[input$asAttribute]]$columns),
              aes(x=age_group, y= Measurement, 
                  fill = factor(Key, ordered = TRUE,
                                levels = listGraphs[[input$asAttribute]]$levels,
                                labels = listGraphs[[input$asAttribute]]$labels) 
              ) ) + 
         geom_bar(position = input$asType, stat = "identity") +
         scale_fill_discrete(name="") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               plot.title = element_text(hjust=0.5)) + 
         labs(x = "Team", y = listGraphs[[input$asAttribute]]$ylab, 
              title = listGraphs[[input$asAttribute]]$title,
              legend = "") +
         facet_grid(rows = vars(factor(playByUs,
                                       ordered = TRUE,
                                       levels = c("true", "false"),
                                       labels = c("Us", "Opponents"))) )
     } else {
       #  print(paste(game_level$ppm, game_level$totalPasses, game_level$possessionDuration))
       ggplot(game_level %>% 
                gather(key = "Key", value = "Measurement",
                       listGraphs[[input$asAttribute]]$columns),
              aes(x=age_group, y= Measurement, 
                  fill = factor(Key, ordered = TRUE,
                                levels = listGraphs[[input$asAttribute]]$levels,
                                labels = listGraphs[[input$asAttribute]]$labels) 
              ) ) + 
         geom_bar(position = "stack", stat = "identity") +
         scale_fill_discrete(name="") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               plot.title = element_text(hjust=0.5)) + 
         labs(x = "Team", y = listGraphs[[input$asAttribute]]$ylab, 
              title = listGraphs[[input$asAttribute]]$title,
              legend = "") +
         facet_grid(rows = vars(factor(playByUs,
                                       ordered = TRUE,
                                       levels = c("true", "false"),
                                       labels = c("Us", "Opponents"))) )
       
       
       
       
     }
   })
   
    
}


# Run the application 
shinyApp(ui = ui, server = server)
