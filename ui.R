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
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Facebook Campaign Optimization"),
        dashboardSidebar(
            textInput("token", 
                      label = "Facebook token:",
                      value = ""),
            uiOutput("user"),
            uiOutput("select_account"),
            sidebarMenu(
                menuItem("Ad Accounts Overview",
                         tabName = "overview",
                         icon = icon("dashboard")),
                menuItem("Pacing & Performance", 
                         tabName = "pacing",
                         icon = icon("usd"))
                # menuItem("Rules and Watchlist", 
                #          tabName = "watchlist",
                #          icon = icon("area-chart"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem("overview",
                        box(title = "Ad Accounts Overview", width = 12,
                            verticalLayout(
                                uiOutput("active-month"),
                                dataTableOutput("account-perf")
                                # // TO DO
                                # uiOutput("select-metrics"),
                                # plotlyOutput("res-spend")
                            )
                        )
                ),
                tabItem("pacing", 
                        tabBox(id = "pacingtab", width = 12,
                            tabPanel(title = "Account Pacing", width = 12,
                                     verticalLayout(
                                         uiOutput("budget_input"),
                                         plotlyOutput("pacing-plot")
                                         )
                                     ),
                            tabPanel(title = "Campaign Drilldown", width = 12,
                                     verticalLayout(
                                         uiOutput("select-account"),
                                         actionButton("campaign-fetch", label = "Fetch"),
                                         dataTableOutput("campaign-table")
                                         # plotlyOutput("campaign-pacing-plot")
                                     )
                            ),
                            tabPanel(title = "Campaign Pacing", width = 12,
                                     verticalLayout(
                                         uiOutput("campaign-budget"),
                                         plotlyOutput("pacing-campaign")
                                     ))
                        )
                )
                # tabItem("watchlist", 
                #         fluidRow(
                #             box(title = "Rules and Watchlist", width = 12
                #                 )
                #         ))
            )
        )
    )
)
