

chart_sma_UI <- function(id){
  dygraphOutput(NS(id, "TA_chart_sma"), height = 300)
}

chart_sma_server <- function(id, ticker, period, start_dt, end_dt, dygrp){
  moduleServer(id, function(input, output, session){
    
    output$TA_chart_sma <- renderDygraph({
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
      
      # 20-day SMA
      sma_fast <- SMA(ticker_data_daily_xts$close, n=20)
      # 40-day SMA
      sma_med <- SMA(ticker_data_daily_xts$close, n=40)
      # 100-day SMA
      sma_slow <- SMA(ticker_data_daily_xts$close, n=100)
      
      ticker_data_daily_xts_sma <- data.frame(ticker_data_daily_xts, sma_fast, sma_med, sma_slow)
      
      chart_sma <- cbind(ticker_data_daily_xts_sma[,1:4], ticker_data_daily_xts_sma[,5], ticker_data_daily_xts_sma[,6], ticker_data_daily_xts_sma[,7])
      colnames(chart_sma)[5]  <- "Fast SMA"
      colnames(chart_sma)[6]  <- "Medium SMA"
      colnames(chart_sma)[7]  <- "Slow SMA"
      
      chart_title <- paste(ticker(), " - Simple Moving Averages (SMA)")
      chart_sma %>%
        dygraph(main = chart_title, group = dygrp) %>% 
        dyCandlestick() %>% 
        # dyRangeSelector() %>% 
        dyCrosshair(direction = "vertical")
      
    })
    
  })
}
