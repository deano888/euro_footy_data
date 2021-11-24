library(shiny)
library(data.table)
library(tidyverse)
library(DT)
library(leaflet)
library(geosphere)
library(ggrepel)
library(kableExtra)
library(bigrquery)
library(dplyr)
library(brio)
library(testthat)
library(ggdark)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "fdproject-331917",
  dataset = "fd_sched_data"
  #billing = "my_project_id"
)

#dbListTables(con)

dt <- tbl(con, "data_sched")

dt <- dt %>% 
  collect()

dt <- data.table(dt)
# change names
setnames(dt, c("season", "League",	"Date",	"Month",	"Home",	"Away","HomeGoals",	"AwayGoals",	"TotalGoals",	"odds1",	"oddsX",	"odds2",	"oddsOver2.5",	"oddsUnder2.5", "hs", "as", "hst", "ast"))


#### normally use this
#dt <- fread("data22.csv")
#print(dt)
# remove dups
dt <- dt[!duplicated(dt), ]

#dt[, Month:=NULL]
season.order <- sort(unique(dt$season), decreasing=TRUE)
this.season <- season.order[1]
last.season <- season.order[2]

coords <- fread('https://raw.githubusercontent.com/deano888/euro_footy_stats_data/master/coords_data.csv')
#unique(coords$League)
# write.csv(coords, 'co_ords_orig.csv')

dt <- dt %>% mutate(League = case_when(League=="D1" ~ "Germany1", 
                                       League=="E0" ~ "England Prem",  
                                       League=="F1" ~ "France1", 
                                       League=="SP1" ~ "Spain1", 
                                       League=="I1" ~ "Italy1"))

last_game <- max(dt$Date)


shinyApp(
  ui = fluidPage(
    
    sidebarPanel(
      
      selectInput(inputId = 'league_selected',
                  label = 'League',
                  choices = sort(unique(dt$League))
      ),
      selectInput(inputId = 'home_team',
                  label = 'Home',
                  choices = NULL
                  
      ), 
      selectInput(inputId = 'away_team',
                  label = 'Away',
                  choices = NULL
                  
      ), 
      
      width = 2
    ), 
    
    
    mainPanel(
      tabsetPanel(
        ## 
        tabPanel("Ave.Goals.By.Country", 
                 htmlOutput("last_update"),
                 br(),
                 DTOutput("country_stats_THIS")
        ),
        tabPanel("Home.Goals", 
                 br(), 
                 br(),
                 plotOutput("home_graph")
                 
        ),
        tabPanel("Away.Goals", 
                 br(),
                 br(),
                 plotOutput("away_graph")
                 
        ),
        tabPanel("Results.Home",
                 br(),
                 tableOutput("results_home_kable")
        ),
        tabPanel("Results.Away",
                 br(),
                 tableOutput("results_away_kable")
        ),
        tabPanel("H2H", 
                 br(),
                 htmlOutput("h2h_text"),
                 br(),
                 tableOutput("head2head")
        ),
        tabPanel("Shots", 
                 br(),
                 ##########htmlOutput("h2h_text"),
                 br(),
                 plotOutput("shots_graph")
        ),
        tabPanel("map", 
                 br(),
                 htmlOutput("map_text"),
                 br(),
                 htmlOutput("distance_clubs"),
                 br(),
                 leafletOutput("mymap")
        )
        
      ) # end tabsetPanel
      
    )
  ),
  
  
  ######################################################################### SERVER ########################
  
  server = function(session, input, output) {
    # observe any change in input$league_selected
    observe({
      unique_home_team_names <- dt %>%
        filter(League == input$league_selected) %>% 
        distinct(Home) %>%
        select(Home) %>%
        arrange(Home)
      
      updateSelectInput(session, 'home_team', choices = unique_home_team_names)
      
    })
    
    # observe any change in input$league_selected
    observe({
      unique_away_team_names <- dt %>%
        filter(League == input$league_selected) %>%
        distinct(Away) %>%
        select(Away) %>%
        arrange(Away)
      
      updateSelectInput(session, 'away_team', choices = unique_away_team_names)
      
    })
    
    #### TAB #### Home.Goals
    
    home_graphR <- reactive({
      # filter by Home team to use this for the result dt and ave dt can thhen rbind them
      hgR <- dt %>%
        filter(League == input$league_selected & season==this.season) %>% 
        group_by(Home) %>% 
        summarise(hg = round(mean(HomeGoals), 2), ag = round(mean(AwayGoals), 2)  ) %>% 
        mutate(Home = paste0(Home, " (", hg, ",", ag, ")"))
      
    })
    
    
    
    output$home_graph <- renderPlot({
      # 
      home_graphR()  %>% 
        ggplot(aes(ag, hg, label=Home)) +
        geom_point() +
        geom_text_repel(aes(label=Home),hjust=0, vjust=0) +
        labs(
          title = "Average goals Scored, Conceded by teams at Home this season", 
          y = "Average Goals Scored", 
          x = "Average Goals Conceded"
        ) +
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    
    away_graphR <- reactive({
      # filter by Home team to use this for the result dt and ave dt can thhen rbind them
      agR <- dt %>%
        filter(League == input$league_selected & season==this.season) %>% 
        group_by(Away) %>% 
        summarise(ag = round(mean(AwayGoals), 2), hg = round(mean(HomeGoals), 2)  ) %>% 
        mutate(Away = paste0(Away, " (", ag, ",", hg, ")"))
      
      print(agR)
    })
    
    output$away_graph <- renderPlot({
      # 
      away_graphR()  %>% 
        ggplot(aes(hg, ag, label=Away)) +
        geom_point() +
        geom_text_repel(aes(label=Away),hjust=0, vjust=0) +
        labs(
          title = "Average goals Scored, Conceded by teams Away this season", 
          y = "Average Goals Scored", 
          x = "Average Goals Conceded"
        ) +
        theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    ## last date updated text
    
    output$last_update <- renderText({
      output <- paste("Last game added - ", last_game)
    })
    
    ## h2h text 
    
    output$h2h_text <- renderText({
      output <- paste("Choose 2 different clubs to show results between them this season", "")
    })
    
    output$head2head <- renderTable({
      # get home and away past matches
      h2h1 <- dt %>%
        # take only league selected + group by home for all teams
        filter(Home %in% input$home_team & Away %in% input$away_team) 
      
      h2h1$Date <- as.Date(h2h1$Date, "%Y-%m-%d")
      print(h2h1)
      print(str(h2h1))
      # get away and home past matches
      h2h2 <- dt %>%
        filter(Home %in% input$away_team & Away %in% input$home_team)
      
      #h2h <- rbind(h2h1, h2h2)
      return(h2h1)
      # selct exact cols
      #h2h %>%
      #  arrange(Date) %>%
      #  select(Date, Home, Away, one, X, two, over2, under, HTFT, HG, AG, TG, FT, HS, AS, HST, AST, HC, AC, TC, HY, AY, TY)
      
    })
    
    
    ### output map
    output$mymap <- renderLeaflet({
      
      league_wanted <- coords %>%
        na.omit() %>%
        filter(League == input$league_selected)  %>%
        mutate(map_colour = ifelse(FDCOUK == input$home_team | FDCOUK == input$away_team, 'red', 'blue')) %>%
        distinct()
      
      
      leaflet()  %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        # addMarkers(lng=league_wanted$Longitude , lat=league_wanted$Latitude, popup = league_wanted$Capacity)
        addCircleMarkers(lng = league_wanted$Longitude, lat = league_wanted$Latitude,
                         radius = 4, color = league_wanted$map_colour, popup = league_wanted$details)
      
      
    })
    
    ## map text 
    
    output$map_text <- renderText({
      output <- paste("Only available for clubs in England, France, Germany and Spain. Choose 2 different teams to see distance between them", "")
    })
    
    ## distance text output
    
    output$distance_clubs <- renderText({
      
      dist_clubs <- coords %>%
        na.omit() %>%
        filter(League == input$league_selected) 
      
      dist_clubs <- dist_clubs %>%
        filter(FDCOUK == input$home_team | FDCOUK == input$away_team)
      
      if (nrow(dt == 2)) {
        result <- distHaversine(c(dist_clubs$Longitude[1], dist_clubs$Latitude[1]),
                                c(dist_clubs$Longitude[2], dist_clubs$Latitude[2])) * 0.0006213712
        result <- paste0('Miles between clubs = ', round(result, 0))
        
      } else {
        
        result <- "Dam we don't have that data!"
      }
      text <- paste("", result)
      
    })
    
    ########### shots tab
    
    # reactive domeResults
    shotsR <- reactive({
      # code for appropriate colors
      shots <- dt %>% 
        filter(League == input$league_selected & season==this.season) %>% 
        mutate(hs_colour = case_when(Home == input$home_team ~ "yellow",
                                     Away == input$away_team ~ "green",
                                     Home != input$home_team &  Away != input$away_team ~ "grey"))
      print("------ home -------")
      print(shots%>% 
              filter(Home == input$home_team))
      print("------ away -------")
      print(shots%>% 
              filter(Away == input$away_team))
      print(shots)
      
    })
    
    output$shots_graph <- renderPlot({
      # 
      shotsR()  %>% 
        ggplot(aes(as, hs, colour = factor(hs_colour, levels = c("yellow", "green", "grey")))) +
        geom_point() +
        dark_theme_gray() +
        ###  use these two lines to only have 1 legend!
        scale_colour_manual(values = c("yellow", "green", "grey"))
      #labs(
      #  title = "Average goals Scored, Conceded by teams Away this season", 
      #  y = "Average Goals Scored", 
      #  x = "Average Goals Conceded"
      #) +
      #theme(plot.title = element_text(hjust = 0.5))
      
    })
    
    #### TAB #### Country.Stats Last
    
    output$country_stats_THIS <- renderDT({
      # 
      cs <- dt %>%
        # take only league selected + group by home for all teams
        
        group_by(League) %>%
        filter(season==this.season) %>% 
        summarize(GamesSoFar = n(), AveTotalGoalsPerMatch = round(mean(TotalGoals), 2)) %>% 
        arrange(desc(AveTotalGoalsPerMatch))
      
      datatable(cs, options = list(scrollX = TRUE, pageLength = 20))
      
    })
    
    # reactive domeResults
    homeResultsR <- reactive({
      home.results <- dt %>%
        # take only league selected + group by home for all teams
        filter(Home %in% input$home_team) %>% 
        filter(season==this.season) 
      
    })
    
    # display home results
    output$results_home_kable <- function() {
      homeResultsR() %>%
        knitr::kable("html", align = "cccc") %>%
        kable_styling(bootstrap_options = "striped", full_width = F)
    }
    
    # reactive domeResults
    awayResultsR <- reactive({
      away.results <- dt %>%
        # take only league selected + group by home for all teams
        filter(Away %in% input$away_team) %>% 
        filter(season==this.season) 
      
    })
    
    # display away results
    output$results_away_kable <- function() {
      awayResultsR() %>%
        knitr::kable("html", align = "cccc") %>%
        kable_styling(bootstrap_options = "striped", full_width = F)
    }
    
  }
)
