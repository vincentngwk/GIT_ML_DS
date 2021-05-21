

chart_bb_UI <- function(id){
  dygraphOutput(NS(id, "TA_chart_bb"), height = 300)
}

chart_bb_server <- function(id, ticker, period, start_dt, end_dt, dygrp){
  moduleServer(id, function(input, output, session){
    
    output$TA_chart_bb <- renderDygraph({
      
      ticker_selected <- ticker()
      
      ticker_data_daily <- tq_get(ticker_selected,
                                  get = "stock.prices",
                                  from = start_dt(),
                                  to = end_dt()) %>%
        tq_transmute(select     = NULL, 
                     mutate_fun = to.period, 
                     period     = period())
      ticker_data_daily_xts <- tk_xts(
        ticker_data_daily %>%  select(date, open,high,low,close) )
      
      bb20 <- BBands(ticker_data_daily_xts$close, sd=2.0, n=14, maType=EMA)
      
      ticker_data_daily_xts_bb <- data.frame(ticker_data_daily_xts, bb20)
      
      chart_bband <- cbind(ticker_data_daily_xts_bb[,1:4], ticker_data_daily_xts_bb[,5], ticker_data_daily_xts_bb[,6], ticker_data_daily_xts_bb[,7])
      colnames(chart_bband)[5]  <- "Lower Band"
      colnames(chart_bband)[6]  <- "Moving Average"
      colnames(chart_bband)[7]  <- "Upper Band"
      
      chart_title <- paste(ticker(), " - Bollinger Bands (BB)")
      
      chart_bband %>%
        dygraph(main = chart_title, group = dygrp) %>% 
        dyCandlestick() %>% 
        # dyRangeSelector() %>% 
        dyCrosshair(direction = "vertical")
      
    })
  })
}


