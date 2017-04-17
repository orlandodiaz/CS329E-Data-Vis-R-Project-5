# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

shinyServer(function(input, output) { 
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  # Begin Crosstab Tab 1 ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
    if(online1() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-project-5", type="sql",
        query="select `Ship Mode`, g.State, 
        sum(population) as sum_pop, 
        sum(Sales) as sum_sales, 
        sum(Sales) / sum(population) as ratio,
        
        case
        when sum(Sales) / sum(population) < ? then '03 Low'
        when sum(Sales) / sum(population) < ? then '02 Medium'
        else '01 High'
        end AS kpi
        
        from globalshipments g join `census-pop-sex` c on g.`Country` = c.`Country`
        where `Ship Mode` in ('First Class', 'Same Day') 
        group by `Ship Mode`, g.`State`
        order by `Ship Mode`, g.`State`",
        queryParameters = list(KPI_Low(), KPI_Medium())
      ) # %>% View()
    }
    # else {
    #   print("Getting from csv")
    #   file_path = "www/globalshipments.csv"
    #   df <- readr::read_csv(file_path)
    #   df %>% 
    #     dplyr::group_by(`Sub-Category`, Country) %>% 
    #     dplyr::summarize(sum_profit = sum(Profit), sum_sales = sum(Sales),
    #                      ratio = sum(Profit) / sum(Sales),
    #                      kpi = if_else(ratio <= KPI_Low(), '03 Low',
    #                                    if_else(ratio <= KPI_Medium(), '02 Medium', '01 High'))) # %>% View()
    # }
  })
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(df1()) + 
      theme(axis.text.x=element_text(angle=90, size=15, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_text(aes(x=`Ship Mode`, y=State, label=sum_sales), size=5) +
      geom_tile(aes(x=`Ship Mode`, y=State, fill=kpi), alpha=0.50)
      # coord_fixed(ratio = 1)
      # theme(panel.background = element_rect(size = 150))
  })
  # End Crosstab Tab 1 ___________________________________________________________
  
  })
