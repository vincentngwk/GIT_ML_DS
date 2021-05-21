library(shiny)

chart_vol_UI <- function(id){
  dygraphOutput(NS(id, "TA_chart_vol"), height = 100)
}

chart_vol_server <- function(id, ticker, period, start_dt, end_dt, dygrp){
  moduleServer(id, function(input, output, session){
    output$TA_chart_vol <- renderDygraph({
      ticker_selected <- ticker()
      
      ticker_data_daily <- tq_get(ticker_selected,
                                  get = "stock.prices",
                                  from = start_dt(),
                                  to = end_dt()) %>%
        tq_transmute(select     = NULL,
                     mutate_fun = to.period,
                     period     = period())
      
      ticker_data_daily_xts <- tk_xts(
        ticker_data_daily %>%  select(date, volume) )
      
      chart_title <- paste(ticker(), " - Volume")
      
      ticker_data_daily_xts %>%
        dygraph(main = chart_title, group = dygrp) %>%
        dyBarChart() %>%
        dyCrosshair(direction = "vertical")
      
    })
  })
}
