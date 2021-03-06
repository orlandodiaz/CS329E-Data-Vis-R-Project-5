#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstabs, KPIs, Parameters", tabName = "crosstab", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Crosstab tab content.
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         sliderInput("KPI1", "KPI_Low:", 
                                     min = 0, max = .1,  value = .1),
                         sliderInput("KPI2", "KPI_Medium:", 
                                     min = .1, max = .2,  value = .2),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data1")
                ),
                tabPanel("Crosstab", plotOutput("plot1", height=1000))
              )
      )
      # End Crosstab tab content.
    )
  )
)
