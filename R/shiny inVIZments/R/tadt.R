library(shiny)

# tadt_UI <- function(id){
#   
#   ns <- NS(id)
#   taglist(
#     DTOutput(ns("TA_DT"))
#   )
# }

tadt_UI <- function(id){
  
    DTOutput(NS(id, "TA_DT"))

}

tadt_server <- function(id, ticker, period, start_dt, end_dt){
  moduleServer(id, function(input, output, session){
    output$TA_DT <- renderDT(server = FALSE, {
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
      
      # price <- data.frame(price) %>% 
      #   select("close","adjusted","volume","bull_bear_strength")
      
      # To re-arrange the columns, starting with the most important columns.
      price <- data.frame(price) %>% 
        select("open","high","low","close","adjusted","volume","bull_bear_strength","total_bull_score", "total_bear_score",
               
               "doji", "dragonfly_doji", "hammer", "bullish_engulf",
               "bullish_harami", "kick_up", "three_white_soldiers", "morning_star", "rising_three", "piercing_line", "dark_cloud_cover",
               
               "gravestone_doji", "inverted_hammer", "bearish_engulf", "bearish_harami", "kick_down", "three_black_crows", 
               "evening_star", "falling_three")
      
      price <- datatable(price, filter = 'top', caption = ticker(), extensions = c("Buttons"),
                options = list(scrollX = TRUE, scrollY = "500px",
                               dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = "csv", text = "Download Current Page", filename = "page",
                                      exportOptions = list(
                                        modifier = list(page = "current")
                                      )
                                 ),
                                 list(extend = "csv", text = "Download Full Results", filename = "data",
                                      exportOptions = list(
                                        modifier = list(page = "all")
                                      )
                                 )
                               
                               
                               )
                
                )
                
                )
      
      price
      
      # datatable(PORT_rolling_corr,
      #           options = list(scrollX = TRUE, scrollY = "300px")
      
    })
  })
}