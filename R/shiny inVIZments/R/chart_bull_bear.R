
chart_bull_bear_UI <- function(id){
  dygraphOutput(NS(id, "TA_chart_bull_bear"), height = 100)
}

chart_bull_bear_server <- function(id, ticker, period, start_dt, end_dt, dygrp){
  moduleServer(id, function(input, output, session){
    output$TA_chart_bull_bear <- renderDygraph({
      ticker_selected <- ticker()
      
      ticker_data_daily <- tq_get(ticker_selected,
                                  get = "stock.prices",
                                  from = start_dt(),
                                  to = end_dt()) %>%
        tq_transmute(select     = NULL,
                     mutate_fun = to.period,
                     period     = period())
      
      price <- tk_xts(ticker_data_daily)
      
      # INDECISIVE candlestick pattern
      price$doji <- doji(price)
      # BULLISH candlestick patterns
      price$dragonfly_doji <- dragonfly.doji(price)
      price$hammer <- hammer(price)
      price$bullish_engulf <- bullish.engulf(price)
      price$bullish_harami <- bullish.harami(price)
      price$kick_up <- kick.up(price)
      price$three_white_soldiers <- three.white.soldiers(price)
      price$morning_star <- morning.star(price)
      price$rising_three <- rising.three(price)
      price$piercing_line <- piercing.line(price)
      price$dark_cloud_cover <- dark.cloud.cover(price)
      # BEARISH candlestick patterns
      price$gravestone_doji <- gravestone.doji(price)
      price$inverted_hammer <- inverted.hammer(price)
      price$bearish_engulf <- bearish.engulf(price)
      price$bearish_harami <- bearish.harami(price)
      price$kick_down <- kick.down(price)
      price$three_black_crows <- three.black.crows(price)
      price$evening_star <- evening.star(price)
      price$falling_three <- falling.three(price)
      
      price$total_bull_score <- rowSums(price[, c("dragonfly_doji", "hammer", "bullish_engulf", "bullish_harami", "kick_up", "three_white_soldiers",
                                                  "morning_star", "morning_star", "piercing_line", "dark_cloud_cover")])
      price$total_bear_score <- rowSums(price[, c("gravestone_doji", "inverted_hammer", "bearish_engulf", "bearish_harami", "kick_down", "three_black_crows",
                                                  "evening_star", "falling_three")]) * -1
      price$total_bull_bear_score <- price$total_bull_score + price$total_bear_score
      price[is.na(price)] <- 0
      # Use recoding to create bull_bear_strength here from -2 (very bearish sentiment) to 2 (very bullish sentiment).
      price$bull_bear_strength[price$doji == 1 | price$total_bull_bear_score == 0] <- 0
      price$bull_bear_strength[price$total_bull_bear_score > 2] <- 2
      price$bull_bear_strength[price$total_bull_bear_score <= 2 & price$total_bull_bear_score > 0] <- 1
      price$bull_bear_strength[price$total_bull_bear_score < -2] <- -2
      price$bull_bear_strength[price$total_bull_bear_score >= -2 & price$total_bull_bear_score < 0] <- -1
      
      price <- data.frame(price) %>% 
        select("bull_bear_strength")
      
      chart_title <- paste(ticker(), " - Bullish/ Bearish Indicator")
      price %>%
        dygraph(main = chart_title, group = dygrp) %>%
        dySeries("bull_bear_strength") %>% 
        dyBarChart() %>%
        dyCrosshair(direction = "vertical")
      
    })
  })
}


