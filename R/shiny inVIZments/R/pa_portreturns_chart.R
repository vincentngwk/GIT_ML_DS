
pa_portreturns_chart_UI <- function(id){
  plotlyOutput(NS(id, "pa_portreturns_chart"))
}

pa_portreturns_chart_server <- function(id, start_dt, end_dt,
                                        ticker1, ticker2, ticker3, ticker4, ticker5,
                                        t1_port_wgt, t2_port_wgt, t3_port_wgt, t4_port_wgt, t5_port_wgt,
                                        portfolio_number){
  moduleServer(id, function(input, output, session){
    output$pa_portreturns_chart <- renderPlotly({
      
      portfolio_ticker_selection <- c(ticker1(), ticker2(), ticker3(), ticker4(), ticker5())
      
      stock_returns_monthly <- portfolio_ticker_selection %>%
        tq_get(get  = "stock.prices",
               from = start_dt(),
               to   = end_dt()) %>%
        group_by(symbol) %>%
        tq_transmute(select     = adjusted, 
                     mutate_fun = periodReturn, 
                     period     = "monthly", 
                     col_rename = "Ra")
      
      wts_1 <- c(t1_port_wgt(), t2_port_wgt(), t3_port_wgt(), t4_port_wgt(), t5_port_wgt())
      
      portfolio_returns_monthly_1 <- stock_returns_monthly %>%
        tq_portfolio(assets_col  = symbol, 
                     returns_col = Ra, 
                     weights     = wts_1, 
                     col_rename  = "Ra") %>% 
        mutate(gain_loss = if_else(Ra > 0, "gain", "loss"))
      
      chart_title <- paste0("Portfolio ", portfolio_number, " Monthly Returns")
      
      port_returns_1_plotly <- portfolio_returns_monthly_1 %>%
        ggplot(aes(x = date, y = Ra)) +
        geom_bar(stat = "identity", aes(fill = gain_loss)) +
        scale_fill_manual("legend", values = c("gain" = "green4", "loss" = "red")) +
        labs(title = chart_title,
             x = "", y = "Monthly Returns") +
        geom_smooth(method = "lm") +
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::percent)
      
      port_returns_1_plotly <- ggplotly(port_returns_1_plotly)
      
    })
  })
}

