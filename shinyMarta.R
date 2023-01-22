library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinycssloaders)
library(ggalluvial)
library(jsonlite)
library(ggridges)

#Potrzebne, żeby Marcie działały nazwy miesięcy
Sys.setlocale("LC_TIME", "en_US.UTF-8")

#Dane
data <- fromJSON("m_user_data.json")

#Podstawowy theme do wykresów
my_theme <- theme(plot.background = element_rect(fill = "#010101",color = "#010101"),
                  panel.background = element_rect(fill = "#010101"),
                  legend.background = element_rect(fill = "#010101"),
                  legend.key = element_rect(fill = "#010101"),
                  legend.text = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 18, colour = "white", face = "bold"),
                  text = element_text(colour = "white"),
                  axis.line = element_line(colour = "white"),
                  axis.text = element_text(colour = "white", size = 16),
                  axis.title = element_text(size = 18, colour = "white", face = "bold"),
                  panel.border = element_blank(),
                  panel.grid = element_blank())

#dashboardUi
motywUI <- shinyDashboardThemeDIY(
  appFontFamily = "Arial",
  appFontColor = "#010101",
  primaryFontColor = "#010101"
  ,infoFontColor = "#010101"
  ,successFontColor = "#010101"
  ,warningFontColor = "#010101"
  ,dangerFontColor = "#010101"
  ,bodyBackColor = "#010101"
  
  ### header
  ,logoBackColor = "#EE1D52"
  
  ,headerButtonBackColor = "#EE1D52"
  ,headerButtonIconColor = "#010101"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "#EE1D52"
  ,headerBoxShadowColor = "#010101"
  ,headerBoxShadowSize = "1px 1px 1px"
  
  ### sidebar
  ,sidebarBackColor = "#69C9D0"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "1px 1px 1px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "#e4e6eb"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "#e4e6eb"
  
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  
  ,sidebarTabBackColorHover = "#e4e6eb"
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
  
)

#strony ui
activity <- fluidPage(
  plotOutput("plot7"),
  sliderInput("dateRange",
              "Choose date range",
              min = as.POSIXct("2022-06-01", format = "%Y-%m-%d"),
              max = as.POSIXct("2022-12-31", format = "%Y-%m-%d"),
              value = c(as.POSIXct("2022-06-01", format = "%Y-%m-%d"),
                        as.POSIXct("2022-12-31", format = "%Y-%m-%d")),
              timeFormat = "%Y-%m-%d",
              width = '100%'),
  tags$style(HTML('#dateRange-label.control-label, *{
        color: white;
  }')),
  textOutput("plot7Comment")
)

sharing <- fluidPage(
  
)

tiktoks <- fluidPage(
  plotOutput("plot4"),
  fluidRow(
    column(6,
           selectInput("plotType",
                       "Choose type of plot",
                       c("linear","alluvial"),
                       selected = "alluvial")
    ),
    column(6,
           checkboxInput("allTiktoks",
                         label = "Do you want see data for all tiktoks? (Otherwise we will show you only tiktoks liked, shared or added to favourite)",
                         value = FALSE
           )
    )
  ),
  textOutput("plot4Comment"),
  plotOutput("plot6"),
  checkboxGroupInput("watchingTime",
                     "Choose watching time ranges",
                     c("<1 sec","1-6 sec","7-15 sec","16-30 sec","31-60 sec","1-3 min",">3 min"),
                     c("<1 sec","1-6 sec","7-15 sec","16-30 sec","31-60 sec","1-3 min",">3 min"),
                     inline = TRUE),
  tags$style(HTML('#plotType-label.control-label, #watchingTime-label.control-label, *{
        color: white;
                  .selectize-dropdown-content {
                  color: black')),
  textOutput("plot6Comment")
)

shinyUI <- dashboardPage(
  
  dashboardHeader(title = tags$a(tags$img(src='logo.png',width='100%'))), 
  
  dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem("Activity", tabName = 'activity', icon = icon("heart")),
      menuItem("Sharing", tabName = 'sharing', icon = icon("heart")),
      menuItem("TikToks", tabName = 'tikToks', icon = icon("heart")),
      uiOutput('sidebar'),
      
      uiOutput("changeColors")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'activity', activity),
      tabItem(tabName = 'sharing', sharing),
      tabItem(tabName = 'tikToks', tiktoks)
    )),
  
  motywUI

)





server <- function(input, output) {
  
  #wybor uzytkownika
  output$sidebar <- renderUI({
    selectInput("user",
                "Select user data:",
                c("Purple Monster",
                  "Orange Monster",
                  "Red Monster"),
                selected = "Purple Monster")
  })
  
  #zmiana koloru selectInput zależnie od wyboru użytkownika
  bg <- reactive({
    case_when(input$user =="Purple Monster" ~ '.irs--shiny .irs-bar,.irs--shiny .irs-to,.irs--shiny .irs-from, input[type=checkbox], .selectize-dropdown-content, .selectize-input.items.has-options.full.has-items {
                                                background-color: purple;
                                                accent-color: purple;
                                                background: purple;
                                                border-top: purple;
                                                border-bottom: purple;
                                                }',
              input$user =="Orange Monster" ~ '.irs--shiny .irs-bar,.irs--shiny .irs-to,.irs--shiny .irs-from,input[type=checkbox], .selectize-dropdown-content, .selectize-input.items.has-options.full.has-items {
                                                background-color: orange;
                                                accent-color: orange;
                                                background: orange;
                                                border-top: orange;
                                                border-bottom: orange;
                                                }',
              input$user =="Red Monster" ~ '.irs--shiny .irs-bar,.irs--shiny .irs-to,.irs--shiny .irs-from,input[type=checkbox], .selectize-dropdown-content, .selectize-input.items.has-options.full.has-items {
                                                background-color: red;
                                                accent-color: red;
                                                background: red;
                                                border-top: red;
                                                border-bottom: red;
                                                }',
              TRUE  ~ ' {.irs--shiny .irs-bar,.irs--shiny .irs-to,.irs--shiny .irs-from,.selectize-dropdown-content, .selectize-input.items.has-options.full.has-items
                                                background-color: blue;
                                                accent-color: blue;
                                                background: blue;
                                                border-top: blue;
                                                border-bottom: blue;
                                                }')
  })

  output$changeColors <- renderUI({
    tags$style(HTML(bg()))
  })

  #wykres4 (alluvial + wersja liniowa)
  output$plot4 <- renderPlot({
    all <- data[["Activity"]][["Video Browsing History"]][["VideoList"]]
    likes <- data[["Activity"]][["Like List"]][["ItemFavoriteList"]] %>% 
      mutate(Like = "liked")
    shared <- data[["Activity"]][["Share History"]][["ShareHistoryList"]] %>% 
      filter(SharedContent == "video") %>% 
      mutate(Shared = "shared") %>% 
      select(Date, Link, Shared)
    favourite <- data[["Activity"]][["Favorite Videos"]][["FavoriteVideoList"]] %>% 
      mutate(Favourite = "favourite")
    if(input$plotType == "alluvial") {
      if(input$allTiktoks == TRUE) {
        data4 <- full_join(all,likes,by = c("Date","Link"))
      } else {
        data4 <- likes
      }
      data4 <- 
        data4 %>% 
        full_join(shared,by = c("Date","Link")) %>% 
        full_join(favourite, by = c("Date","Link")) %>% 
        mutate(Like = coalesce(Like,"not liked"),
               Shared = coalesce(Shared,"not shared"),
               Favourite = coalesce(Favourite, "not favourite")) %>%
        group_by(Like, Shared, Favourite) %>% 
        summarise(Freq = n())
      ggplot(data4,
             aes(y = Freq, axis1 = Like, axis2 = Shared, axis3 = Favourite)) +
        geom_alluvium(fill = "#ee1d52", width = 1/12) +
        geom_stratum(width = 1/12) +
        geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
        labs(y = "Number of tiktoks") +
        my_theme +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank())
    } else {
      likes2 <- likes %>% 
        mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
               Month = factor(format(Date, format = "%b"),levels = month.abb)) %>% 
        group_by(Month) %>% 
        summarise(Freq = n())
      shared2 <- shared %>% 
        mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
               Month = factor(format(Date, format = "%b"),levels = month.abb)) %>% 
        group_by(Month) %>% 
        summarise(Freq = n())
      favourite2 <- favourite %>% 
        mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
               Month = factor(format(Date, format = "%b"),levels = month.abb)) %>% 
        group_by(Month) %>% 
        summarise(Freq = n())
      
      if(input$allTiktoks == TRUE) {
        all2 <- all %>% 
          mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
                 Month = factor(format(Date, format = "%b"),levels = month.abb)) %>% 
          group_by(Month) %>% 
          summarise(Freq = n())
        
        likes2 %>% 
          ggplot(aes(x = Month, y = Freq)) +
          geom_line(aes(group = 1, color = "like"), size = 1.5) +
          geom_line(data = shared2, aes(group = 1, color = "shared"), size = 1.5) +
          geom_line(data = favourite2, aes(group = 1, color = "favourite"), size = 1.5) +
          geom_line(data = all2, aes(group = 1, color = "all"), size = 1.5) +
          scale_color_manual(values = c("#D3D3D3", "#EE1D52","#69C9D0","#7D1EFF")) +
          labs(color = "Option",
               y = "Number of tiktoks") +
          my_theme +
          theme(panel.grid.major  = element_line(colour = "#787878"))
      } else {
        likes2 %>% 
          ggplot(aes(x = Month, y = Freq)) +
          geom_line(aes(group = 1, color = "like"), size = 1.5) +
          geom_line(data = shared2, aes(group = 1, color = "shared"), size = 1.5) +
          geom_line(data = favourite2, aes(group = 1, color = "favourite"), size = 1.5) +
          scale_color_manual(values = c("#EE1D52","#69C9D0","#7D1EFF")) +
          labs(color = "Option",
               y = "Number of tiktoks") +
          my_theme +
          theme(panel.grid.major  = element_line(colour = "#787878"))
      }
      
    }
  })
  
  output$plot4Comment <- renderText(
    {"The plot above shows number of tiktoks with given characteristics: liked, shared or added to favourite."}
  )
  
  #wykres6 (tiktoki w zależności od czasu oglądania)
  output$plot6 <- renderPlot({
    data6 <- data[["Activity"]][["Video Browsing History"]][["VideoList"]] %>% 
      select(Date) %>% 
      mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
             Month = factor(format(Date, format = "%b"), levels = month.abb)) %>% 
      transform(Time = as.numeric(difftime(Date, c(Date[-1],NA), units = "secs"))) %>% 
      head(-1) %>% 
      filter(Time <= 10*60) %>%
      mutate(Time_range = factor(case_when(Time < 1 ~ "<1 sec",
                                           Time <= 6 ~ "1-6 sec",
                                           Time <= 15 ~ "7-15 sec",
                                           Time <= 30 ~ "16-30 sec",
                                           Time <= 60 ~ "31-60 sec",
                                           Time <= 180 ~ "1-3 min",
                                           TRUE ~ ">3 min"),
                                 levels = c("<1 sec","1-6 sec","7-15 sec","16-30 sec","31-60 sec","1-3 min",">3 min"))) %>% 
      filter(Time_range %in% input$watchingTime) %>% 
      group_by(Time_range,Month) %>% 
      summarise(Freq = n()) %>% 
      ungroup() %>% 
      group_by(Month) %>% 
      summarise(Sum = sum(Freq), Time_range = Time_range, Freq = Freq/Sum)
    colours <- c("#69c9d0", "#00aff8", "#0086ff", "#7d1eff",
                          "#d600bc", "#f1007f", "#ee1d52")
                          ggplot(data6, aes(y = Freq, x = Month, fill =Time_range)) +
                            geom_col(position = "dodge") +
                            scale_y_continuous(labels = scales::percent,expand = c(0,0)) +
                            labs(y = "Percentage of tiktoks",
                                 fill = "Watching time") +
                            scale_fill_manual(values = colours) +
                            my_theme +
                            theme(panel.grid.major.y  = element_line(colour = "#787878"))
  })
  
  output$plot6Comment <- renderText(
    {"The plot above shows for every month percentage of watched tiktoks with selected watching time ranges."}
  )
  
  #wykres7 (ggridges)
  output$plot7 <- renderPlot({
    data7 <- data[["Activity"]][["Login History"]][["LoginHistoryList"]] %>% 
      select(Date, NetworkType) %>% 
      mutate(NetworkType = case_when(NetworkType == "Wi-Fi" ~ "Wi-Fi",
                                     TRUE ~ "Cellular"),
             Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
             Month = factor(format(Date, format = "%b"), levels = month.abb),
             Day = format(Date, format = "%Y-%m-%d")) %>%
      mutate(Day = as.Date(Day)) %>% 
      filter(Date <= input$dateRange[2], Date >= input$dateRange[1])
    ggplot(data7, aes(x = Day, y = Month, fill = NetworkType, color = NetworkType)) + 
      geom_density_ridges(scale = 2, alpha = 0.7) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_fill_manual(values = c("#EE1D52","#69C9D0"), name = "Network Type") +
      scale_color_manual(values = c("#EE1D52","#69C9D0"), name = "Network Type") +
      my_theme +
      theme(panel.grid.major  = element_line(colour = "#787878"))
  })
  output$plot7Comment <- renderText(
    {"The plot above shows how the number of tiktoks watched via cellular network vs Wi-FI changed on the following days of each month."}
  )
  
}


shinyApp(ui = shinyUI, server = server)
