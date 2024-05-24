

#UI side


library(stringr)
library(shiny)
library(dplyr)
library(rvest)
library(readr)
library(shinyWidgets)
library(elo)


#Define the UI for application

ui <- (fluidPage(
  
  # pick a theme
  theme = shinythemes::shinytheme("paper"),
  # application title
  titlePanel("Division III WSOC Ratings"),
  # sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "Year", "Select a year: ", choices = c("2022", "2023"), selected = "2023"),
      radioGroupButtons(
        inputId = "ConfReg", label = "", choices = c("Conference", "Region"), justified = TRUE, selected = "Conference"),
      uiOutput("ConfRegBox")),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Standings", dataTableOutput("standings"),
          downloadButton("downloadstandings", "Download Standings File")),
        tabPanel(
          "Games", dataTableOutput("games"),
          downloadButton("downloadgames", "Download Games File")),
        tabPanel(
          "RPI PPG", dataTableOutput("rpi_ppg"),
          downloadButton("downloadrpi_ppg", "Download RPI_PPG File")),
        tabPanel(
          "RPI WP", dataTableOutput("rpi_wp"),
          downloadButton("downloadrpi_wp", "Download RPI_WP File")),
        tabPanel(
          "Poisson Rating", textOutput("poisson_stats"), dataTableOutput("poisson_table"),
          downloadButton("downloadpoisson", "Download Poisson Ratings")),
        tabPanel(
          "Elo Rating",
          flowLayout(
            
            sliderInput("home_advantage", "Value for home field:",
                        min = 0, max = 100, value = 30, width = "100%"),
            sliderInput("k_value", "Choose value for k:", min = 0, max = 50, value = 20, width = "100%")),
          dataTableOutput("elo_rating"),
          downloadButton("downloadelo", "Download Elo Ratings")),
        tabPanel("About",
                 h6("This R shiny app displays the standings and game schedule based on conference or region for the 2022 and 2023 Division III women's soccer teams. In addition, the app displays RPI rankings, Elo rankings and offensive and defensive rankings based on a Poisson probability model for each of the teams."),
                 
                 
                 h6("This R shiny app was developed by Hope Donoghue for a honors Senior Year Experience in Data Science at St. Lawrence University during the academic school year of 2023-2024. This project was supervised by Dr. Robin Lock."),
                 
                 h6("Data for this app is scraped from masseyratings.com"))
        
      )))))
