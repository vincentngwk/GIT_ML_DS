
# packages = c('sf','tmap','tidyverse','forecast',
#              'tseries','readxl','tidyquant',
#              'dygraphs','TSstudio','plotly',
#              'tsibble','ggplot2','tidymodels',
#              'modeltime','modeltime.ensemble',
#              'timetk','glmnet','randomForest',
#              'shiny','readr', 'DT', 'shinydashboard',
#              'CandleStickPattern', 'ggpubr', 'dplyr',
#              'cluster', 'formattable', 'reshape2',
#              'zoo', 'tibble', 'ggdendro', 'knitr',
#              'kableExtra', 'dendextend', 'scales', 'data.table')

# Changing the setup code to load libraries the normal way allowed the shiny app to be deployed successfully to shinyapps.io
# Using the loop method kept failing when the website loaded returning an error saying that packages could not be found.
library(sf)
library(tmap)
library(tidyverse)
library(forecast)
library(tseries)
library(readxl)
library(tidyquant)
library(dygraphs)
library(TSstudio)
library(plotly)
library(tsibble)
library(ggplot2)
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(timetk)
library(glmnet)
library(randomForest)
library(shiny)
library(readr)
library(DT)
library(shinydashboard)
library(CandleStickPattern)
library(ggpubr)
library(dplyr)
library(cluster)
library(formattable)
library(reshape2)
library(zoo)
library(tibble)
library(ggdendro)
library(knitr)
library(kableExtra)
library(dendextend)
library(scales)
library(data.table)
library(corrplot)
library(shinycssloaders)
library(shinydisconnect)
library(shinyscreenshot)
library(shinydashboardPlus)

# for (p in packages) {
#     library(p,character.only = T)
# }

# for (p in packages) {
#     if(!require(p,character.only = T)){
#         install.packages(p)
#     }
#     library(p,character.only = T)
# }

# @@@@@@@@@@@@@@@@@@@@@ START OF CLUSTERING PREPROCESSING CODE CHUNK @@@@@@@@@@@@@@@@@@@@@ 

#read and process Bloomberg data
bloomberg_data <- read_csv("./data/nasdaq_ratios.csv") %>%
    mutate(across(where(is.numeric), round, 2))
bloomberg_data$Date <- as.Date(bloomberg_data$Date, format="%d/%m/%y") #convert to date format
bloomberg_data$Year <- format(bloomberg_data$Date, format="%Y") #Make a Year column
bloomberg_data$Year <- as.numeric(bloomberg_data$Year)

#Pull data from tidyquant
Stocks <- 
    tq_get(c("AAPL",  "ADBE",  "ADI"  , "ADP" ,  "ADSK" , "ALGN" , "ALXN",  "AMAT",  "AMD" ,  "AMGN" , "AMZN",  "ANSS" , "ATVI" , "AVGO"  ,"BIDU" , "BIIB",  "BKNG" , "CDNS" , "CDW" ,  "CERN" , "CHKP",  "CHTR",  "CMCSA",
             "COST" , "CPRT" , "CSCO" , "CSX" ,  "CTAS" , "CTSH",  "DLTR", "DXCM" , "EA",    "EBAY" , "EXC" ,  "FAST","FB",  "FISV" , "FOXA",  "GILD",  "GOOGL", "IDXX" , "ILMN",
             "INCY" , "INTC" , "INTU",  "ISRG" , "JD" ,   "KHC" , 
             "KLAC" , "LRCX" , "LULU",  "MAR" ,  "MCHP",  "MDLZ", 
             "MELI" , "MNST"  ,"MRVL"  ,"MSFT" , "MU"  ,  "MXIM" ,
             "NFLX" , "NTES",  "NVDA" , "NXPI" , "ORLY" , "PAYX" ,
             "PCAR" , "PEP" ,  "PYPL"  ,"QCOM",  "REGN" , "ROST" ,
             "SBUX" , "SGEN",  "SIRI" , "SNPS" , "SWKS" , "TCOM", 
             "TEAM" , "TMUS" , "TSLA" , "TXN"  , "VRSK",  "VRSN", 
             "VRTX" , "WBA"  , "XEL" ,  "XLNX" , "ZM" ), 
           get = "stock.prices", from = "2015-01-01") %>%
    group_by(symbol)


returns_yearly <- 
    Stocks %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "yearly") 

returns_yearly$Year <- format(returns_yearly$date, format="%Y") %>%
    as.numeric(returns_yearly$Year)

returns_yearly_final <- returns_yearly  %>% 
    
    select('symbol','yearly.returns','Year') %>% #select columns
    rename('Ticker' = 'symbol',
           '1-Year Return' = 'yearly.returns') %>%
    mutate(across(where(is.numeric), round, 2))

#Join Bloomberg Data with tidyquant data
combine <- dplyr::inner_join(bloomberg_data, returns_yearly_final, by =c("Ticker"="Ticker","Year"="Year"))

# @@@@@@@@@@@@@@@@@@@@@ END OF CLUSTERING PREPROCESSING CODE CHUNK @@@@@@@@@@@@@@@@@@@@@ 


# We set the global options here for shinycssloader, the spinner options.
# spinner.type Option 1 is like powerbi bar loading, option 6 is round comet like. Otherwise consider using gif.
# We can also change spinner color eg. spinner.color="#0dc5c1"
options(spinner.type = 6) 

sidebar <- dashboardSidebar(
    minified = TRUE, collapsed = FALSE, width = 265,
    
    sidebarMenu(
        # style = "position: fixed; overflow: visible;",
                menuItem("Getting Started", tabName = "tab0", icon = icon("info-circle")),
                menuItem("Input Parameters", icon = icon("sliders"),
                         dateInput(
                             inputId = "start_date",
                             label = "Select Start of Period",
                             value = Sys.Date() - 365*5
                         ),
                         dateInput(
                             inputId = "end_date",
                             label = "Select End of Period",
                             value = Sys.Date()
                         ),
                         textInput(
                             inputId = "ticker1",
                             label = "Input Stock Symbol 1 here:",
                             value = "AAPL"
                         ),
                         textInput(
                             inputId = "ticker2",
                             label = "Input Stock Symbol 2 here:",
                             value = "GOOG"
                         ),
                         textInput(
                             inputId = "ticker3",
                             label = "Input Stock Symbol 3 here:",
                             value = "FB"
                         ),
                         textInput(
                             inputId = "ticker4",
                             label = "Input Stock Symbol 4 here:",
                             value = "TSLA"
                         ),
                         textInput(
                             inputId = "ticker5_bm",
                             label = "Input Benchmark Stock Symbol here:",
                             value = "XLK"
                         ),
                         selectInput(
                             inputId = "periodicity",
                             label = "Select Periodicity",
                             choices = c("days","weeks","months","years")
                         )),
                menuItem("Technical Indicators", tabName = "tab1", icon = icon("chart-line")),
                menuItem("Correlation Analysis", tabName = "tab2", icon = icon("pushed")),
                menuItem("Portfolio Analysis", tabName = "tab3", icon = icon("coins")), # or use "balance-scale-left"
                menuItem("Correlation/Clustering Analysis", tabName = "tab4", icon = icon("bezier-curve")),
                menuItem("Forecasting Analysis", tabName = "tab5", icon = icon("battle-net")),
                screenshotButton()
    )
)



body <- dashboardBody(

    disconnectMessage(
        text = "Your session timed out, reload the application.",
        refresh = "Reload now",
        background = "#f89f43",
        colour = "white",
        overlayColour = "grey",
        overlayOpacity = 0.3,
        refreshColour = "brown"
    ),
    tabItems(
        tabItem(tabName = "tab0",
                fluidRow(
                    tabBox(width = 12,
                           tabPanel(title = "All About inVIZments",
                                    fluidRow(
                                        box(width = 4,
                                            uiOutput("active_side"),
                                            flipBox(
                                                id = "flipbox1", width = 12,
                                                front = div(class = "text-center",
                                                            h2("Click me"),
                                                            img(src = 'inVIZments_noR.png', height = "400px", width = "80%")
                                                ),
                                                back = div(
                                                    class = "text-center", height = "400px", width = "100%",
                                                    h2("Click to flip"),
                                                    p("inVIZments was built on R and deployed using shiny."),
                                                    br(),
                                                    p("Multiple R packages were used to build the final product. The key analysis packages used are: Tidyquant, timetk, Modeltime, dygraphs, dendextend."),
                                                    br(),
                                                    p("It is mainly put together based on the shinydashboardPlus package. Various other packages helped to add
                                                      towards improving the responsiveness, aesthetics, and general quality of life benefits to improve user experience.")
                                                )
                                            )),
                                        box(title = "Using inVIZments", width = 8,
                                            div(
                                                class = "text-left",
                                                p("Navigate using tabs on the left sidebar to get started IMMEDIATELY."),
                                                br(),
                                                p("Alternatively, this page serves to provide a new user with some basic familiarity of inVIZments."),
                                                br(),
                                                p("Most retail investors rely on traditional financial data websites such as Yahoo Finance to retrieve relevant financial data to aid in their investing decisions.
                                                However, these sites' offerings tend to be homogeneous and comprise usual information such as price quotes, market commentary and corporate actions.
                                                Investors are left on their own to make sense of these information, without tools to further perform actions such as price forecasting or segmenting stocks into groups."),
                                                br(),
                                                p("Having identified this gap, we developed InVIZMents, an interactive visual analytics dashboard to allow investors to not only retrieve basic price information of stocks,
                                                but also apply models such as ARIMA forecasting and hierarchical clustering.  The application will also allow users to construct bespoke portfolios and see their historical performances.
                                                  On top of this, the dashboard also provides users with a tool that readily aggregates and interprets candlestick patterns for the user.
                                                  All these different tools come together to deliver a product that adds value that far exceeds what traditional finance data websites can provide."),
                                                br(),
                                                p("This application was targeted at investors to provide useful tools to aid them in their investing process that traditional sources do not provide.
                                                  There are many ways to use InVIZments and how each user interacts with InVIZments depends on his or her own objectives, investing timeframes and risk profile."),
                                                br(),
                                                p("While every possible care and effort has been taken to make the application as accurate as possible.
                                                  The application is meant as decision support and users are advised to exercise due diligence on their part when investing.
                                                  InVIZments shall not in any event be liable for any damages or injury arising out of your access to,
                                                  or inability to access the application or from your reliance on any information provided within it.")
                                            ))
                                    )),
                           tabPanel(title = "Sub-Modules",
                                    fluidRow(width = 6,
                                        box(title = "(A) Input Parameters",
                                            div(
                                                class = "text-left",
                                                p("Input Parameters allows users to key in and change various parameters to interaact with the app."),
                                                p("1) Users can select the start and end period."),
                                                p("2) Users can select up to 5 stock symbols."),
                                                p("3) The 1st stock symbol is used by default for sub-modules focusing on single stocks."),
                                                p("4) The Benchmark stock symbol is used to provide a form of comparison to the other stocks."),
                                                p("5) Users can also change the periodicity (eg. days/ weeks/ etc.) for certain sub-modules where relevant.")
                                            )),
                                        box(title = "(D) Portfolio Analysis",
                                            div(
                                                class = "text-left",
                                                p("The Portfolio Analysis tab is split into 3 main logical segments:"),
                                                p("1) Top - Initial Equity amount and individual stock weights for 3 portfolios under comparison."),
                                                p("2) Middle - Charts depicting monthly returns of the 3 constructed portfolios."),
                                                p("3) Bottom - Chart comparing the Portfolio Equity Curves of the 3 constructed portfolios.")
                                            ))
                                        
                                    ),
                                    fluidRow(width = 6,
                                             box(title = "(B) Technical Indicators",
                                                 div(
                                                     class = "text-left",
                                                     p("The Technical Indicators tab is split into 2 main logical segments:"),
                                                     p("1) Top - A tabbed box offering users a choice of 3 types of candlestick charts: Plain, Simple Moving Averages, or with Bollinger Bands."),
                                                     p("2) Bottom - A tabbed box offering users useful data on both Stock Symbol 1 and the Benchmark Stock Symbol. Users are also able to download these data.")
                                                 )),
                                             box(title = "(E) Correlation & Clustering Analysis",
                                                 div(
                                                 class = "text-left",
                                                 p("The Correlation and Clustering Analysis tab is split into 3 main logical segments:"),
                                                 p("1) Top left - Variable selection and other inputs for clustering"),
                                                 p("2) Top right - Data tables containing details on cluster constituent stocks and cluster returns/volatility"),
                                                 p("3) Bottom - Charts such as: Univariate analysis, Correlation plot, Dendrogram, Line graph for cluster variables breakdown")
                                                 )
                                             )
                                             
                                             ),
                                    fluidRow(width = 6,
                                             box(title = "(C) Correlation Analysis",
                                                 div(
                                                     class = "text-left",
                                                     p("The Correlation Analysis tab is split into 4 main logical segments:"),
                                                     p("1) Top left - Time Series Rolling Correlations are calculated between the 4 stocks and the benchmark symbol."),
                                                     p("2) Top right - Data tables containing details on calculated rolling correlations are available for use and download."),
                                                     p("3) Bottom left - Interactive charts containing monthly stock prices for the 4 stocks."),
                                                     p("4) Bottom right - Interactive charts containing Anomaly Diagnostics on certain potential turning points in the price action of the 4 stocks.")
                                                 )),
                                             box(title = "(F) Forecasting Analysis",
                                                 div(
                                                     class = "text-left",
                                                     p("The Time Series Forecasting tab is split into 3 main logical segments:"),
                                                     p("1) Top left - Forecasting parameters that users can select and change"),
                                                     p("2) Top right - Data tables containing accuracy measures of the various forecasting models"),
                                                     p("3) Bottom - Forecast plot showing historical and forecasted prices based on the mean forecast of the top 3 performing models")
                                                 )))
                                    ))
                )
                
                ),
        tabItem(tabName = "tab1",
                # Button to simulate and test for disconnection, and disconnect message
                # actionButton("disconnect", "Disconnect the app"),
                
                fluidRow(
                    box(title = "Technical Analysis Charts", collapsible = TRUE, width = 12,
                        tabBox(width = 12,
                               tabPanel(title = "No Indicators",
                                        withSpinner(chart_normal_UI('chart_normal_ts1')),
                                        withSpinner(chart_bull_bear_UI('chart_bull_bear_norm_ts1')),
                                        withSpinner(chart_vol_UI('chart_vol_norm_ts1')),
                                        withSpinner(chart_normal_UI('chart_normal_bm5'))
                                        # dygraphOutput(outputId = "chart_normal_ts_1", height = 325, width = '100%'),
                                        # dygraphOutput(outputId = "chart_bull_bear_ts1", height = 100, width = '100%'),
                                        # dygraphOutput(outputId = "chart_vol_ts1", height = 100, width = '100%'),
                                        # dygraphOutput(outputId = "chart_normal_bm", height = 325, width = '100%')
                               ),
                               tabPanel(title = "Simple Moving Averages",
                                        withSpinner(chart_sma_UI('chart_sma_ts1')),
                                        withSpinner(chart_bull_bear_UI('chart_bull_bear_sma_ts1')),
                                        withSpinner(chart_vol_UI('chart_vol_sma_ts1')),
                                        withSpinner(chart_sma_UI('chart_sma_bm5'))
                                        # dygraphOutput(outputId = "chart_SMA_ts_1", height = 350, width = '100%'),
                                        # dygraphOutput(outputId = "chart_SMA_bull_bear_ts1", height = 100, width = '100%'),
                                        # dygraphOutput(outputId = "chart_SMA_vol_ts1", height = 100, width = '100%'),
                                        # dygraphOutput(outputId = "chart_SMA_bm", height = 350, width = '100%')
                               ),
                               tabPanel(title = "Bollinger Bands",
                                        withSpinner(chart_bb_UI('chart_bb_ts1')),
                                        withSpinner(chart_bull_bear_UI('chart_bull_bear_bb_ts1')),
                                        withSpinner(chart_vol_UI('chart_vol_bb_ts1')),
                                        withSpinner(chart_bb_UI('chart_bb_bm5'))
                                        # dygraphOutput(outputId = "chart_BB_ts_1", height = 350, width = '100%'),
                                        # dygraphOutput(outputId = "chart_BB_bull_bear_ts1", height = 100, width = '100%'),
                                        # dygraphOutput(outputId = "chart_BB_vol_ts1", height = 100, width = '100%'),
                                        # dygraphOutput(outputId = "chart_BB_bm_1", height = 350, width = '100%')
                               )
                        ) ) ) ,
                fluidRow(
                    box(title = "Data Tables", collapsible = TRUE, width = 12,
                        tabBox(width = 12,
                               tabPanel(title = "Stock Data", withSpinner(tadt_UI('datatable_ts1') )),
                               tabPanel(title = "Benchmark Data", withSpinner(tadt_UI('datatable_bm5')) ))
                    ) )
        ),
        
        tabItem(tabName = "tab2",
                fluidRow(
                    box(height = 430, title = "Rolling Correlations",
                        withSpinner(plotlyOutput("mthly_stock_prices_rolling_corr", width = '100%', height = '410')) ),
                    box(height = 430, title = "Data for Rolling Correlations",
                        withSpinner(DTOutput('datatable_port_rolling_corr', height = '300') )
                    )),
                fluidRow(
                    box(height = 430, title = "Monthly Stock Prices",
                        withSpinner(plotlyOutput("mthly_stock_prices", width = '100%', height = '410') ) ),
                    box(height = 430, title = "Anomaly Diagnostics",
                        withSpinner(plotlyOutput("mthly_stock_prices_anomaly", width = '100%', height = '410') )
                    )
                )
        ),
        tabItem(tabName = "tab3",
                fluidRow(
                    box(title = "Portfolio Weights Allocation", width = 12, collapsible = TRUE,
                        fluidRow(box(title = "Initial Equity", numericInput(inputId="initial_investment_capital", label="Initial Equity ($)", value=10000, min=1000, step=1))),
                        fluidRow(
                            box(title = "Portfolio 1", width = 4, collapsible = TRUE,
                                
                                numericInput(inputId="Port1_wt1", label="Ticker 1 %", value=0.20, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port1_wt2", label="Ticker 2 %", value=0.20, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port1_wt3", label="Ticker 3 %", value=0.20, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port1_wt4", label="Ticker 4 %", value=0.20, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port1_wt5", label="Ticker 5 %", value=0.20, min=0, max=1.00, step=0.01)
                                
                            ),
                            box(title = "Portfolio 2", width = 4, collapsible = TRUE,
                                
                                numericInput(inputId="Port2_wt1", label="Ticker 1 %", value=0.40, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port2_wt2", label="Ticker 2 %", value=0.30, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port2_wt3", label="Ticker 3 %", value=0.20, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port2_wt4", label="Ticker 4 %", value=0.10, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port2_wt5", label="Ticker 5 %", value=0.00, min=0, max=1.00, step=0.01)
                                
                            ),
                            box(title = "Portfolio 3", width = 4, collapsible = TRUE,
                                
                                numericInput(inputId="Port3_wt1", label="Ticker 1 %", value=0.15, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port3_wt2", label="Ticker 2 %", value=0.15, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port3_wt3", label="Ticker 3 %", value=0.15, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port3_wt4", label="Ticker 4 %", value=0.15, min=0, max=1.00, step=0.01),
                                numericInput(inputId="Port3_wt5", label="Ticker 5 %", value=0.40, min=0, max=1.00, step=0.01)
                                
                            ) )
                    ) ),
                fluidRow(
                    box(title = "Portfolio Monthly % Returns", width = 12, collapsible = TRUE,
                        box(width = 4,
                            withSpinner(pa_portreturns_chart_UI('pa_chart_port1'))
                            # plotlyOutput("PA_mthly_returns_1")
                        ), # Port 1 returns
                        box(width = 4,
                            withSpinner(pa_portreturns_chart_UI('pa_chart_port2'))
                            # plotlyOutput("PA_mthly_returns_2")
                        ), # Port 2 returns
                        box(width = 4,
                            withSpinner(pa_portreturns_chart_UI('pa_chart_port3'))
                            # plotlyOutput("PA_mthly_returns_3")
                        ) # Port 3 returns
                    ) ),
                fluidRow(
                    box(title = "Portfolio Equity Curves Comparison", width = 12, collapsible = TRUE,
                        withSpinner(plotlyOutput("PA_mthly_comparison")) ) # Final Equity Curves comparison here
                )
        ),
        tabItem(tabName = "tab4",
                fluidRow(
                    box(title = "Hierarchical Clustering Parameters", height = 350, width = 6,
                        selectInput(inputId = "Year",
                                    label = "Year:",
                                    choices = list("2015" = 2015,
                                                   "2016" = 2016,
                                                   "2017" = 2017,
                                                   "2018" = 2018,
                                                   "2019" = 2019,
                                                   "2020" = 2020),
                                    selected = "2015"),
                        
                        #Second input: Checkbox to select variables
                        checkboxGroupInput(inputId = "variables",
                                           label = "Variables to cluster by:",
                                           choices = c("1-Year Return" = "1-Year Return",
                                                       "1-Year Volatility" = "1-Year Volatility",
                                                       "Dividend Yield" = "Average Dividend Yield",
                                                       "Return on Assets" = "Return on Assets",
                                                       "Total Debt to Total Assets" = "Total Debt to Total Assets"),
                                           #choices: list of choices,
                                           selected = c( "1-Year Return","1-Year Volatility","Average Dividend Yield", 
                                                         "Return on Assets", "Total Debt to Total Assets") 
                        ),
                        
                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                            
                            #Third Input: Dropdown to Select Dist Matrix method
                            selectInput(inputId = "dist_method",
                                        label = "Distance Method",
                                        choices = list("Euclidean" = "euclidean",
                                                       "Maximum" = "maximum",
                                                       "Manhattan" = "manhattan",
                                                       "Canberra" = "canberra",
                                                       "Binary" = "binary",
                                                       "Minkowski" = "minkowski"),
                                        selected = "Euclidean") ),
                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                            
                            #Fourth Input: Dropdown to Select Agglomeration Method
                            selectInput(inputId = "linkage_method",
                                        label = "Linkage Method",
                                        choices = list("Ward D2" = "ward.D2",
                                                       "Ward" = "ward",
                                                       "Single" = "single",
                                                       "Complete" = "complete",
                                                       "Average" = "average",
                                                       "Centroid" = "centroid"),
                                        selected = "Ward D2") ) ,
                        
                        #Fifth Input: Slider to select num of clusters
                        sliderInput(inputId = "clusters",
                                    label = "Number of Clusters",
                                    min = 5,
                                    max = 20,
                                    value = c(10))
                        
                    ),
                    tabBox(height = 350, width = 6,
                           tabPanel(title = "Individual Cluster Constituent stocks",
                                    withSpinner(dataTableOutput(outputId = "clustertable")) ),
                           tabPanel(title = "Clusters' Return and Volatility Profile",
                                    withSpinner(dataTableOutput(outputId = "returnstable")) )
                    )
                ),
                fluidRow(
                    tabBox(width = 12,
                           tabPanel(title = "Distribution of Variables",
                                    withSpinner(plotlyOutput("distriPlot")) ),
                           tabPanel(title = "Correlation Plot",
                                    withSpinner(plotOutput("correlPlot")) ),
                           tabPanel(title = "Cluster Dendrogram",
                                    withSpinner(plotOutput("clusterPlot")) ),
                           tabPanel(title = "Line Graph showing Cluster Variables Breakdown",
                                    withSpinner(plotlyOutput("lineGraph")) )
                    )
                )
                
        ),
        tabItem(tabName = "tab5",
                fluidRow(
                    box(title = "Forecasting Parameters:", width = 4, # Extra Input Controls here
                        dateInput(
                            inputId = "fc_start_date",
                            label = "Select Data Start Date",
                            value = Sys.Date() - months(6),
                            min = Sys.Date() - years(1),
                            max = Sys.Date() - months(4),
                            format = "dd-M-yyyy"
                        ),
                        selectInput(inputId = "measure",
                                    label = "Accuracy Measure:",
                                    choices = c('mae','mape','mase','smape','rmse','rsq')
                        ),
                        sliderInput(inputId = "forecastperiod",
                                    label = "Forecasting Period (Weeks):",
                                    min = 1,
                                    max = 4,
                                    value = 1
                        )
                    ),
                    box(title = "Forecasting Sub-Models Accuracy Measure Results", width = 8,
                        withSpinner(DTOutput("accuracyTab") )
                    )
                ),
                fluidRow(
                    box(width = 12,
                        withSpinner(plotlyOutput("chart_forecast") )
                    ) # Output Forecast Chart here
                )
        )
    )
)



# UI Object
ui <- dashboardPage(
    md = TRUE,
    skin = "green-light",
    options = list(sidebarExpandOnHover = TRUE),
    dashboardHeader(title = tagList(
        span(class = "logo-lg", "inVIZments", width = 265),
        img(src = 'inVIZments_noR.png', width = 30, height = 30) # align = "center", noWS = NULL
    ) ),
    sidebar,
    body,
    controlbar = dashboardControlbar(collapsed = TRUE, skinSelector()),
    footer = dashboardFooter(left = "Made with R Shiny", right = "Singapore, 2021"),
    title = "Skin Selector")

# Server Object
server <- function(input, output, session){
    
    # Having a test button to disconnect the app
    observeEvent(input$disconnect, {
        session$close()
    })

    
    output$active_side <- renderUI({
        side <- if (input$flipbox1) "front" else "back"
        dashboardBadge(side, color = "blue")
    })
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ TECHNICAL INDICATORS TAB START @@@@@@@@@@@@@@@@@@@@@@@@@@@@@    
    
    tadt_server("datatable_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                start_dt = reactive(input$start_date), end_dt = reactive(input$end_date) )
    tadt_server("datatable_bm5", ticker = reactive(input$ticker5_bm), period = reactive(input$periodicity),
                start_dt = reactive(input$start_date), end_dt = reactive(input$end_date) )
    
    chart_vol_server("chart_vol_norm_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                     start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "charting" )
    chart_vol_server("chart_vol_sma_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                     start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_sma" )
    chart_vol_server("chart_vol_bb_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                     start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_bband" )
    
    chart_bull_bear_server("chart_bull_bear_norm_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                           start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "charting" )
    chart_bull_bear_server("chart_bull_bear_sma_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                           start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_sma" )
    chart_bull_bear_server("chart_bull_bear_bb_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                           start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_bband" )
    
    chart_normal_server("chart_normal_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                        start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "charting" )
    chart_normal_server("chart_normal_bm5", ticker = reactive(input$ticker5_bm), period = reactive(input$periodicity),
                        start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "charting" )
    
    chart_sma_server("chart_sma_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                     start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_sma" )
    chart_sma_server("chart_sma_bm5", ticker = reactive(input$ticker5_bm), period = reactive(input$periodicity),
                     start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_sma" )
    
    chart_bb_server("chart_bb_ts1", ticker = reactive(input$ticker1), period = reactive(input$periodicity),
                    start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_bband" )
    chart_bb_server("chart_bb_bm5", ticker = reactive(input$ticker5_bm), period = reactive(input$periodicity),
                    start_dt = reactive(input$start_date), end_dt = reactive(input$end_date), dygrp = "chart_bband" )
    
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ TECHNICAL INDICATORS TAB END @@@@@@@@@@@@@@@@@@@@@@@@@@@@@    
    
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CORRELATION ANALYSIS TAB START @@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
    # renderPlotly()/plotlyOutput()
    
    output$mthly_stock_prices <- renderPlotly({
        
        port_corr_ticker_selection <- c(input$ticker1, input$ticker2, input$ticker3, input$ticker4)
        port_corr_benchmark_ticker <- input$ticker5_bm
        
        PORT_mth <- port_corr_ticker_selection %>%
            tq_get(get  = "stock.prices",
                   from = input$start_date,
                   to   = input$end_date) %>%
            group_by(symbol) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = to.period, 
                         period     = "months")
        
        port_mth_plotly <- PORT_mth %>%
            ggplot(aes(x = date, y = adjusted, color = symbol)) +
            geom_line(size = 1) +
            labs(title = "Monthly Stock Prices",
                 x = "", y = "", color = "") +
            facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
            scale_y_continuous(labels = scales::dollar) +
            theme_tq() + 
            scale_color_tq()
        port_mth_plotly <- ggplotly(port_mth_plotly) %>% 
            layout(yaxis = list(title = list(text = "Adjusted Prices", standoff = 20L)))
        # port_mth_plotly
        
    })
    
    output$mthly_stock_prices_anomaly <- renderPlotly({
        
        port_corr_ticker_selection <- c(input$ticker1, input$ticker2, input$ticker3, input$ticker4)
        port_corr_benchmark_ticker <- input$ticker5_bm
        
        PORT_mth <- port_corr_ticker_selection %>%
            tq_get(get  = "stock.prices",
                   from = input$start_date,
                   to   = input$end_date) %>%
            group_by(symbol) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = to.period, 
                         period     = "months")
        
        PORT_mth %>%
            group_by(symbol) %>%
            plot_anomaly_diagnostics(date, adjusted, .facet_ncol = 2) %>% 
            layout(yaxis = list(title = list(text = "Adjusted Prices", standoff = 20L)))
    })
    
    output$mthly_stock_prices_rolling_corr <- renderPlotly({
        
        port_corr_ticker_selection <- c(input$ticker1, input$ticker2, input$ticker3, input$ticker4)
        port_corr_benchmark_ticker <- input$ticker5_bm
        
        PORT_returns_monthly <- port_corr_ticker_selection %>%
            tq_get(get  = "stock.prices",
                   from = input$start_date,
                   to   = input$end_date) %>%
            group_by(symbol) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = periodReturn, 
                         period     = "monthly")
        
        # Baseline Returns
        baseline_returns_monthly <- port_corr_benchmark_ticker %>%
            tq_get(get  = "stock.prices",
                   from = input$start_date,
                   to   = input$end_date) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = periodReturn,
                         period     = "monthly")
        
        returns_joined <- left_join(PORT_returns_monthly, 
                                    baseline_returns_monthly,
                                    by = "date")
        
        PORT_rolling_corr <- returns_joined %>%
            tq_transmute_xy(x          = monthly.returns.x, 
                            y          = monthly.returns.y,
                            mutate_fun = runCor,
                            n          = 6,
                            col_rename = "rolling.corr.6")
        
        chart_title <- paste0("Portfolio of selected stocks: Six Month Rolling Correlation to: ", input$ticker5_bm)
        
        port_rolling_corr_plotly <- PORT_rolling_corr %>%
            ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
            geom_hline(yintercept = 0, color = palette_light()[[1]]) +
            geom_line(size = 1) +
            labs(title = chart_title,
                 x = "", y = "", color = "") +
            facet_wrap(~ symbol, ncol = 2) +
            theme_tq() + 
            scale_color_tq()
        
        port_rolling_corr_plotly <- ggplotly(port_rolling_corr_plotly) %>% 
            layout(yaxis = list(title = list(text = "Rolling Correlation", standoff = 20L)))
        # port_rolling_corr_plotly
    })
    
    output$datatable_port_rolling_corr <- renderDT({
        
        port_corr_ticker_selection <- c(input$ticker1, input$ticker2, input$ticker3, input$ticker4)
        port_corr_benchmark_ticker <- input$ticker5_bm
        
        PORT_returns_monthly <- port_corr_ticker_selection %>%
            tq_get(get  = "stock.prices",
                   from = input$start_date,
                   to   = input$end_date) %>%
            group_by(symbol) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = periodReturn, 
                         period     = "monthly")
        
        # Baseline Returns
        baseline_returns_monthly <- port_corr_benchmark_ticker %>%
            tq_get(get  = "stock.prices",
                   from = input$start_date,
                   to   = input$end_date) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = periodReturn,
                         period     = "monthly")
        
        returns_joined <- left_join(PORT_returns_monthly, 
                                    baseline_returns_monthly,
                                    by = "date")
        
        PORT_rolling_corr <- returns_joined %>%
            tq_transmute_xy(x          = monthly.returns.x, 
                            y          = monthly.returns.y,
                            mutate_fun = runCor,
                            n          = 6,
                            col_rename = "rolling.corr.6")
        # datatable(PORT_rolling_corr,
        #           options = list(scrollX = TRUE, scrollY = "300px"))
        datatable(PORT_rolling_corr, filter = 'top', caption = paste0("Data"), extensions = c("Buttons"),
                  options = list(scrollX = TRUE, scrollY = "200px",
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
        
    })
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CORRELATION ANALYSIS TAB END @@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ PORTFOLIO ANALYSIS TAB END @@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
    
    pa_portreturns_chart_server("pa_chart_port1", start_dt = reactive(input$start_date),
                                
                                end_dt = reactive(input$end_date), ticker1 = reactive(input$ticker1), ticker2 = reactive(input$ticker2),
                                ticker3 = reactive(input$ticker3), ticker4 = reactive(input$ticker4), ticker5 = reactive(input$ticker5_bm),
                                
                                t1_port_wgt = reactive(input$Port1_wt1), t2_port_wgt = reactive(input$Port1_wt2),
                                t3_port_wgt = reactive(input$Port1_wt3), t4_port_wgt = reactive(input$Port1_wt4),
                                t5_port_wgt = reactive(input$Port1_wt5), portfolio_number = "1" )
    
    pa_portreturns_chart_server("pa_chart_port2", start_dt = reactive(input$start_date),
                                
                                end_dt = reactive(input$end_date), ticker1 = reactive(input$ticker1), ticker2 = reactive(input$ticker2),
                                ticker3 = reactive(input$ticker3), ticker4 = reactive(input$ticker4), ticker5 = reactive(input$ticker5_bm),
                                
                                t1_port_wgt = reactive(input$Port2_wt1), t2_port_wgt = reactive(input$Port2_wt2),
                                t3_port_wgt = reactive(input$Port2_wt3), t4_port_wgt = reactive(input$Port2_wt4),
                                t5_port_wgt = reactive(input$Port2_wt5), portfolio_number = "2" )
    
    pa_portreturns_chart_server("pa_chart_port3", start_dt = reactive(input$start_date),
                                
                                end_dt = reactive(input$end_date), ticker1 = reactive(input$ticker1), ticker2 = reactive(input$ticker2),
                                ticker3 = reactive(input$ticker3), ticker4 = reactive(input$ticker4), ticker5 = reactive(input$ticker5_bm),
                                
                                t1_port_wgt = reactive(input$Port3_wt1), t2_port_wgt = reactive(input$Port3_wt2),
                                t3_port_wgt = reactive(input$Port3_wt3), t4_port_wgt = reactive(input$Port3_wt4),
                                t5_port_wgt = reactive(input$Port3_wt5), portfolio_number = "3" )
    
    
    
    output$PA_mthly_comparison <- renderPlotly({
        
        portfolio_ticker_selection <- c(input$ticker1, input$ticker2, input$ticker3, input$ticker4, input$ticker5_bm)
        
        weights <- c(
            input$Port1_wt1, input$Port1_wt2, input$Port1_wt3, input$Port1_wt4, input$Port1_wt5,
            input$Port2_wt1, input$Port2_wt2, input$Port2_wt3, input$Port2_wt4, input$Port2_wt5, 
            input$Port3_wt1, input$Port3_wt2, input$Port3_wt3, input$Port3_wt4, input$Port3_wt5
        )
        
        weights_table <-  tibble(portfolio_ticker_selection) %>%
            tq_repeat_df(n = 3) %>%
            bind_cols(tibble(weights)) %>%
            group_by(portfolio)
        
        stock_returns_monthly <- portfolio_ticker_selection %>%
            tq_get(get  = "stock.prices",
                   from = input$start_date,
                   to   = input$end_date) %>%
            group_by(symbol) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = periodReturn, 
                         period     = "monthly", 
                         col_rename = "Ra")
        
        stock_returns_monthly_multi <- stock_returns_monthly %>%
            tq_repeat_df(n = 3)
        
        portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
            tq_portfolio(assets_col   = symbol, 
                         returns_col  = Ra, 
                         weights      = weights_table, 
                         col_rename   = "investment.growth",
                         wealth.index = TRUE) %>%
            mutate(investment.growth = investment.growth * input$initial_investment_capital) # Reference user input for initial equity here!
        
        portfolio_plotly <- portfolio_growth_monthly_multi %>%
            ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
            geom_line(size = 1) + geom_point() +
            labs(title = "Comparing Portfolio Growth/ Equity Curve",
                 x = "", y = "Portfolio Value",
                 color = "Portfolio") +
            geom_smooth(method = "loess", alpha = 0.1, size = 0.5) +
            theme_tq() +
            scale_color_tq() +
            scale_y_continuous(labels = scales::dollar)
        portfolio_plotly <- ggplotly(portfolio_plotly)
        
    })
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ PORTFOLIO ANALYSIS TAB END @@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CLUSTERING ANALYSIS TAB START @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    #First output: Plot Dendrogram
    output$clusterPlot <- renderPlot({
        
        #Select Year
        select_yearfilter <- combine %>%
            filter(Year == input$Year) %>% #select the year
            select('Ticker',input$variables ) %>%
            # '1-Year Return','1-Year Volatility','Average Dividend Yield','Return on Assets','Total Debt to Total Assets') %>% #select the variables to cluster
            mutate(across(where(is.numeric), round, 2))
        
        select_yearfilter <- select_yearfilter %>%
            column_to_rownames('Ticker') #make the Ticker column become rownames 
        
        #Do clustering
        clust_dist <- dist(select_yearfilter,method = input$dist_method)
        clust <- hclust(clust_dist , method = input$linkage_method)
        
        #Plot dendrogram
        plot(clust)
        
        rect.hclust(clust, k= input$clusters , border = "cadetblue")
        
    })
    
    #Second Output: Plot Line graph
    output$lineGraph <- renderPlotly({
        
        #Select Year
        select_yearfilter <- combine %>%
            filter(Year == input$Year) %>% #select the year
            select('Ticker',input$variables ) %>%
            # '1-Year Return','1-Year Volatility','Average Dividend Yield','Return on Assets','Total Debt to Total Assets') %>% #select the variables to cluster
            mutate(across(where(is.numeric), round, 2))
        
        select_yearfilter <- select_yearfilter %>%
            column_to_rownames('Ticker') #make the Ticker column become rownames 
        
        #Do clustering
        clust_dist <- dist(select_yearfilter,method = input$dist_method)
        clust <- hclust(clust_dist , method = input$linkage_method)
        
        #Get breakdown of stocks in each cluster
        groups<- data.frame(cutree(clust, k = input$clusters))
        
        #Merge the stocks with the variables
        cluster_breakdown <- merge(groups,select_yearfilter, by = 0, all = TRUE)
        
        cluster_breakdown <- subset(cluster_breakdown, select = -Row.names) 
        
        colnames(cluster_breakdown)[1] <- "Cluster"
        
        #Get average values of each variable for each cluster
        final_cluster_breakdown <- cluster_breakdown %>%
            group_by(Cluster) %>%
            summarise(across(everything(),mean))
        
        #Multiply 1-Year Return by 100 to make it in same scale as other variables
        final_cluster_breakdown$`1-Year Return`<- final_cluster_breakdown$`1-Year Return` * 100
        
        final_cluster_breakdown_melt <- melt(final_cluster_breakdown,id = "Cluster")
        
        p <- ggplot(data = final_cluster_breakdown_melt) +
            geom_line(aes(x=variable, y = value, colour = as.factor(Cluster), group = Cluster)) +
            labs(colour="Cluster" , y= "value (%)") +
            theme(axis.text.x  = element_text(angle=45, hjust = 1))
        
        fig <- ggplotly(p)
        
        fig
        
        
    })
    
    #Third Output: Table of constituent Stocks
    output$clustertable<- DT::renderDataTable(server = FALSE,{
        
        ### DO  clustering again
        #Select Year
        select_yearfilter <- combine %>%
            filter(Year == input$Year) %>% #select the year
            select('Ticker',input$variables ) %>%
            # '1-Year Return','1-Year Volatility','Average Dividend Yield','Return on Assets','Total Debt to Total Assets') %>% #select the variables to cluster
            mutate(across(where(is.numeric), round, 2))
        
        select_yearfilter <- select_yearfilter %>%
            column_to_rownames('Ticker') #make the Ticker column become rownames 
        
        #Do clustering
        clust_dist <- dist(select_yearfilter,method = input$dist_method)
        clust <- hclust(clust_dist , method = input$linkage_method)
        
        
        ###This section is to generate table showing stocks in each individual cluster
        
        #create dataframe of stock to cluster
        myclusters <- data.frame(Cluster = cutree(clust, k =input$clusters))
        
        myclusters <- tibble::rownames_to_column(myclusters,"Stock") %>%
            arrange(Cluster)
        
        #Get list of stocks in each cluster    
        agg=aggregate(myclusters$Stock, list(myclusters$Cluster), paste, collapse=",") %>%
            rename(
                Cluster = Group.1,
                Stocks = x
            )
        DT::datatable(data = agg, rownames = TRUE,
                      extensions = c("Buttons"),
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
                                     
                      ))
        
        
    })
    
    #Fourth Output: Table of constituent Stocks
    output$returnstable<- DT::renderDataTable(server = FALSE, {
        
        ### DO  clustering again
        #Select Year
        select_yearfilter <- combine %>%
            filter(Year == input$Year) %>% #select the year
            select('Ticker',input$variables ) %>%
            # '1-Year Return','1-Year Volatility','Average Dividend Yield','Return on Assets','Total Debt to Total Assets') %>% #select the variables to cluster
            mutate(across(where(is.numeric), round, 2))
        
        select_yearfilter <- select_yearfilter %>%
            column_to_rownames('Ticker') #make the Ticker column become rownames 
        
        #Do clustering
        clust_dist <- dist(select_yearfilter,method = input$dist_method)
        clust <- hclust(clust_dist , method = input$linkage_method)
        
        #Get breakdown of stocks in each cluster
        groups<- data.frame(cutree(clust, k = input$clusters))
        
        #Merge the stocks with the variables
        cluster_breakdown <- merge(groups,select_yearfilter, by = 0, all = TRUE)
        
        cluster_breakdown <- subset(cluster_breakdown, select = -Row.names) 
        
        colnames(cluster_breakdown)[1] <- "Cluster"
        
        #Get average values of each variable for each cluster
        final_cluster_breakdown <- cluster_breakdown %>%
            group_by(Cluster) %>%
            summarise(across(everything(),mean))
        
        #Multiply 1-Year Return by 100 to make it in same scale as other variables
        final_cluster_breakdown$`1-Year Return`<- final_cluster_breakdown$`1-Year Return` * 100
        
        final_cluster_breakdown$`Returns to Volatility` = final_cluster_breakdown$`1-Year Return` / final_cluster_breakdown$`1-Year Volatility`
        
        final_cluster_breakdown_ordered <- final_cluster_breakdown %>%
            arrange(desc(`Returns to Volatility`)) %>%
            select('Cluster','Returns to Volatility','1-Year Return','1-Year Volatility') %>%
            mutate(across(where(is.numeric), round, 2))
        
        DT::datatable(data = final_cluster_breakdown_ordered,
                      extensions = c("Buttons"),
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
                                     
                      ))
        
    })
    
    
    output$distriPlot <- renderPlotly({
        #Select Year
        select_yearfilter <- combine %>%
            filter(Year == input$Year) %>% #select the year
            select('Ticker',input$variables ) %>%
            # '1-Year Return','1-Year Volatility','Average Dividend Yield','Return on Assets','Total Debt to Total Assets') %>% #select the variables to cluster
            mutate(across(where(is.numeric), round, 2))
        
        select_yearfilter <- select_yearfilter %>%
            column_to_rownames('Ticker') #make the Ticker column become rownames 
        #Plot distributions
        select_yearfilter %>% gather() 
        
        histos <- ggplot(gather(select_yearfilter), aes(value)) + 
            geom_histogram() + 
            facet_wrap(~key, scales = 'free_x') + xlab("Value (%)") + ylab("Count")
        fig1 <- ggplotly(histos)
        fig1
        
        
    })
    
    
    output$correlPlot <- renderPlot({
        #Select Year
        select_yearfilter <- combine %>%
            filter(Year == input$Year) %>% #select the year
            select('Ticker',input$variables ) %>%
            # '1-Year Return','1-Year Volatility','Average Dividend Yield','Return on Assets','Total Debt to Total Assets') %>% #select the variables to cluster
            mutate(across(where(is.numeric), round, 2))
        
        select_yearfilter <- select_yearfilter %>%
            column_to_rownames('Ticker') #make the Ticker column become rownames 
        #Plot distributions
        M <- cor(select_yearfilter)
        corrplot(M, method ="number", type = "upper")
        
        
    })
    
    
    
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CLUSTERING ANALYSIS TAB END @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ FORECASTING ANALYSIS TAB START @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    forecast_ticker_data_daily <- reactive({
        
        # Date prep
        ######### DATA INPUT 1 AND 2 HERE. TICKER/START DATE
        ticker_data_daily <- tq_get(input$ticker1,
                                    get = "stock.prices",
                                    from = input$fc_start_date,
                                    to = Sys.Date())
        
    })
    
    forecast_splits <- reactive({
        
        splits <- time_series_split(forecast_ticker_data_daily(), assess = "2 weeks", cumulative = TRUE)
        
    })
    
    
    forecast_ticker_models <- reactive({
        
        recipe_spec <- recipe(adjusted ~ date, training(forecast_splits())) %>%
            step_timeseries_signature(date) %>%
            step_rm(matches("(.iso$)|(.xts$)")) %>%
            step_normalize(matches("(index.num$)|(_year$)")) %>%
            step_dummy(all_nominal()) %>%
            step_fourier(date, K = 1, period = 12)
        
        recipe_spec %>% prep() %>% juice()
        
        # 1 ARIMA
        
        model_spec_arima <- arima_reg() %>%
            set_engine("auto_arima")
        
        wflw_fit_arima <- workflow() %>%
            add_model(model_spec_arima) %>%
            add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
            fit(training(forecast_splits()))
        
        # 2 Prophet
        
        model_spec_prophet <- prophet_reg() %>%
            set_engine("prophet")
        
        wflw_fit_prophet <- workflow() %>%
            add_model(model_spec_prophet) %>%
            add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
            fit(training(forecast_splits()))
        
        # 3 Elastic Net
        
        model_spec_glmnet <- linear_reg(
            mixture = 0.9,
            penalty = 4.36e-6
        ) %>%
            set_engine("glmnet")
        
        wflw_fit_glmnet <- workflow() %>%
            add_model(model_spec_glmnet) %>%
            add_recipe(recipe_spec %>% step_rm(date)) %>%
            fit(training(forecast_splits()))
        
        # 4 Random Forest
        
        model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
            set_engine("randomForest")
        
        wflw_fit_rf <- workflow() %>%
            add_model(model_spec_rf) %>%
            add_recipe(recipe_spec %>% step_rm(date)) %>%
            fit(training(forecast_splits()))
        
        # 5 Boosted Prophet
        
        model_spec_prophet_boost <- prophet_boost() %>%
            set_engine("prophet_xgboost")
        
        wflw_fit_prophet_boost <- workflow() %>%
            add_model(model_spec_prophet_boost) %>%
            add_recipe(recipe_spec) %>%
            fit(training(forecast_splits()))
        
        # Creating table of models
        
        ticker_models <- modeltime_table(
            wflw_fit_arima,
            wflw_fit_prophet,
            wflw_fit_glmnet,
            wflw_fit_rf,
            wflw_fit_prophet_boost
            
        )
        
    })
    
    accuracies <- reactive({
        # Creating table of models for calibration and creating the accuracy table. Identify list of models to retain
        calibration_table <- forecast_ticker_models() %>%
            modeltime_calibrate(testing(forecast_splits()))
        
        accuracies <- calibration_table %>%
            modeltime_accuracy()
    })
    
    output$chart_forecast <- renderPlotly({
        
        ######## INPUT 3 HERE ACCURACY MEASURE
        
        retain_models <- accuracies() %>% # Feed in the previously calculated reactive expression here!
            top_n(-3,input$measure)
        
        
        retain_indexes <- list(retain_models$.model_id)
        
        # Creating an ensemble model with the retained models
        
        ensemble_fit <- forecast_ticker_models() %>%
            subset(.model_id %in% retain_indexes[[1]]) %>%
            ensemble_average(type = "mean")
        
        calibration_tbl <- modeltime_table(ensemble_fit) %>%
            modeltime_calibrate(testing(forecast_splits()))
        
        refit_tbl <- calibration_tbl %>%
            modeltime_refit(forecast_ticker_data_daily())
        
        ######### DATA INPUT 4 HERE FORECAST PERIOD
        
        forecast_period = paste(input$forecastperiod, "weeks")
        
        forecast_chart_title <- paste0("Forecasting Prices for: ", input$ticker1)
        
        refit_tbl %>%
            modeltime_forecast(
                h = forecast_period,
                actual_data = forecast_ticker_data_daily()
            ) %>%
            plot_modeltime_forecast(.title = forecast_chart_title,
                                    .legend_show = FALSE,
                                    .interactive = TRUE,
                                    .plotly_slider = TRUE)
        
    })
    
    output$accuracyTab <- renderDataTable(server = FALSE, {
        
        datatable(data = accuracies(),
                  
                  extensions = c("Buttons"),
                  options = list(scrollX = TRUE, scrollY = "200px",
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
        ) %>% 
            formatSignif(c('mae', 'mape', 'mase', 'smape', 'rmse', 'rsq'),digits = 8)
    })
    
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ FORECASTING ANALYSIS TAB END @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    
}

# ShinyApp call
shinyApp(ui = ui, server = server)

