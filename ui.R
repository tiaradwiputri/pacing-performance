#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Facebook Campaign Optimization"),
        dashboardSidebar(
            textInput("token", 
                      label = "Facebook token:",
                      value = ""),
            uiOutput("user"),
            sidebarMenu(
                menuItem("Pacing & Performance", 
                         tabName = "pacing",
                         icon = icon("usd")),
                menuItem("Active Campaigns", 
                         tabName = "active",
                         icon = icon("area-chart"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem("pacing", 
                        box(title = "Budget Pacing", width = 12,
                            fluidRow(
                                column(width = 12, uiOutput("active-month"))
                            ),
                            fluidRow(
                                column(width = 12, dataTableOutput("pacing-table"))
                            ),
                            fluidRow(
                                column(width = 6, 
                                       uiOutput("select_account")),
                                column(width = 6, numericInput("budget", 
                                                                label = "Specify Monthly Budget",
                                                                value = 0))
                            ),
                            fluidRow(
                                column(width = 9, plotOutput("pacing-plot")),
                                column(width = 3, uiOutput("pacing-opt"))
                            ))),
                tabItem("active", 
                        fluidRow(
                            box(title = "Active Campaigns", width = 12,
                                dataTableOutput("active-campaigns"))
                        ))
            )
        )
    )
)
