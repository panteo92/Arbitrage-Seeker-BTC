library(jsonlite)
library(shinythemes)


if (interactive()) {
  
  ui <- fluidPage(
    theme = shinytheme("superhero"),
    navbarPage("Bitcoin Arbitrage Seeker"),
    numericInput("n",
                 "Invested Capital (BTC)",
                 min = 0,
                 step = 0.000001,
                 value = 1),
    wellPanel(verbatimTextOutput("BitcoinArbitrage"))
  )
  
  server <- function(input, output, session) {
    
    
    # Create a random name for the log file
    logfilename <- paste0('logfile',
                          floor(runif(1, 1e+05, 1e+06 - 1)),
                          '.txt')
    
    logwriter <- observe({
      # Invalidate this observer every second (1000 milliseconds)
      invalidateLater(10000, session)
      
      # Clear log file if more than 20 entries
      if (file.exists(logfilename) &&
          length(readLines(logfilename)) > 20) {
        unlink(logfilename)
      }
      
      # ---------- MARKET DATA FROM EXCHANGES ------------
      poloniex <- fromJSON("https://poloniex.com/public?command=returnOrderBook&currencyPair=USDT_BTC&lowestAsk&highestBid")  #(prendere $ask[1,1] o $bid[1,1])
      bitfinex <- fromJSON("https://api.bitfinex.com/v1/pubticker/BTCUSD") 
      kraken <- fromJSON("https://api.kraken.com/0/public/Ticker?pair=XBTUSD") #(a=ask array, b=bid array)
      gdax <- fromJSON("https://api.gdax.com/products/BTC-USD/ticker") 
      bitstamp <- fromJSON("https://www.bitstamp.net/api/v2/ticker/btcusd") 
      trt <- fromJSON("https://www.therocktrading.com/api/ticker/BTCUSD") 
      hitbtc <- fromJSON("https://api.hitbtc.com/api/2/public/ticker/BTCUSD") 
      cexio <- fromJSON("https://cex.io/api/ticker/BTC/USD") 
      
      # ---------- FEES FROM EXCHANGES ---------- 
      poloniex.fee <- 0.0025
      bitfinex.fee <- 0.002
      kraken.fee <- 0.0026
      gdax.fee <- 0.0025
      bistamp.fee <- 0.0025
      trt.fee <- 0.002
      hitbtc.fee <- 0.001
      cexio.fee <- 0.002
      
      # ---------- ASKs FROM EXCHANGES ----------
      poloniex.ask <- as.numeric(poloniex$asks[1,1]) - (poloniex.fee * as.numeric(poloniex$asks[1,1]))
      bitfinex.ask <- as.numeric(bitfinex$ask) - (bitfinex.fee * as.numeric(bitfinex$ask))
      kraken.ask <- as.numeric(kraken$result$XXBTZUSD$a[1]) - (kraken.fee * as.numeric(kraken$result$XXBTZUSD$a[1]))
      gdax.ask <- as.numeric(gdax$ask) - (gdax.fee * as.numeric(gdax$ask))
      bitstamp.ask <- as.numeric(bitstamp$ask) - (bistamp.fee * as.numeric(bitstamp$ask))
      trt.ask <- as.numeric(trt$result$ask) - (trt.fee * as.numeric(trt$result$ask))
      hitbtc.ask <- as.numeric(hitbtc$ask) - (hitbtc.fee * as.numeric(hitbtc$ask))
      cexio.ask <- as.numeric(cexio$ask) - (cexio.fee * as.numeric(cexio$ask))
      
      # ---------- BIDs FROM EXCHANGES ----------
      poloniex.bid <- as.numeric(poloniex$bids[1,1]) - (poloniex.fee * as.numeric(poloniex$bids[1,1]))
      bitfinex.bid <- as.numeric(bitfinex$bid) - (bitfinex.fee * as.numeric(bitfinex$bid))
      kraken.bid <- as.numeric(kraken$result$XXBTZUSD$b[1]) - (kraken.fee *  as.numeric(kraken$result$XXBTZUSD$b[1]))
      gdax.bid <- as.numeric(gdax$bid) - (gdax.fee * as.numeric(gdax$bid))
      bitstamp.bid <- as.numeric(bitstamp$bid) - (bistamp.fee * as.numeric(bitstamp$bid))
      trt.bid <- as.numeric(trt$result$bid) - (trt.fee * as.numeric(trt$result$bid))
      hitbtc.bid <- as.numeric(hitbtc$bid) - (hitbtc.fee * as.numeric(hitbtc$bid))
      cexio.bid <- as.numeric(cexio$bid) - (cexio.fee * as.numeric(cexio$bid))
      
      #Data management to visualize it
      AskVector <- c(poloniex.ask, bitfinex.ask, kraken.ask, gdax.ask, bitstamp.ask, trt.ask, hitbtc.ask, cexio.ask)
      BidVector <- c(poloniex.bid, bitfinex.bid, kraken.bid, gdax.bid, bitstamp.bid, trt.bid, hitbtc.bid, cexio.bid)
      
      BidMatrix <- matrix(data = BidVector, nrow=8, ncol=8,byrow = TRUE)
      
      AskVector1 <- AskVector[c(1,2,3,4,5,6,7,8)]
      AskVector2 <- AskVector[c(2,3,4,5,6,7,8,1)]
      AskVector3 <- AskVector[c(3,4,5,6,7,8,1,2)]
      AskVector4 <- AskVector[c(4,5,6,7,8,1,2,3)]
      AskVector5 <- AskVector[c(5,6,7,8,1,2,3,4)]
      AskVector6 <- AskVector[c(6,7,8,1,2,3,4,5)]
      AskVector7 <- AskVector[c(7,8,1,2,3,4,5,6)]
      AskVector8 <- AskVector[c(8,1,2,3,4,5,6,7)]
      
      AskMatrix <- matrix(data = c(AskVector1,AskVector2,AskVector3,AskVector4,AskVector5,AskVector6,AskVector7,AskVector8), nrow = 8,ncol = 8,byrow = TRUE)
      
      ArbitrageMatrix <- BidMatrix - AskMatrix
      ArbitrageProfit <- max(BidMatrix-AskMatrix)
      
      ArbitrageCol <- as.numeric(which(BidMatrix-AskMatrix==max(BidMatrix-AskMatrix), arr.ind = TRUE)[1,2])
      ArbitrageRow <- as.numeric(which(BidMatrix-AskMatrix==max(BidMatrix-AskMatrix), arr.ind = TRUE)[1,1])
      
      ExchangeNames <- c("Poloniex", "Bitfinex", "Kraken", "GDAX", "Bitstamp", "The Rock Trading", "HitBTC", "Cex.io")
      ExchangeNames1 <- ExchangeNames[c(1,2,3,4,5,6,7,8)]
      ExchangeNames2 <- ExchangeNames[c(2,3,4,5,6,7,8,1)]
      ExchangeNames3 <- ExchangeNames[c(3,4,5,6,7,8,1,2)]
      ExchangeNames4 <- ExchangeNames[c(4,5,6,7,8,1,2,3)]
      ExchangeNames5 <- ExchangeNames[c(5,6,7,8,1,2,3,4)]
      ExchangeNames6 <- ExchangeNames[c(6,7,8,1,2,3,4,5)]
      ExchangeNames7 <- ExchangeNames[c(7,8,1,2,3,4,5,6)]
      ExchangeNames8 <- ExchangeNames[c(8,1,2,3,4,5,6,7)]
      
      
      if (ArbitrageRow==2) {
        p<-paste("There's an arbitrage opportunity from",ExchangeNames2[ArbitrageCol],"to",ExchangeNames[ArbitrageCol],"with a profit of $",ArbitrageProfit*isolate(input$n),sep=" ", collapse = '\n')
      } else if (ArbitrageRow==3) {
        p<-paste("There's an arbitrage opportunity from",ExchangeNames3[ArbitrageCol],"to",ExchangeNames[ArbitrageCol],"with a profit of $",ArbitrageProfit*isolate(input$n),sep=" ", collapse = '\n')
      } else if (ArbitrageRow==4) {
        p<-paste("There's an arbitrage opportunity from",ExchangeNames4[ArbitrageCol],"to",ExchangeNames[ArbitrageCol],"with a profit of $",ArbitrageProfit*isolate(input$n),sep=" ", collapse = '\n')
      } else if (ArbitrageRow==5) {
        p<-paste("There's an arbitrage opportunity from",ExchangeNames5[ArbitrageCol],"to",ExchangeNames[ArbitrageCol],"with a profit of $",ArbitrageProfit*isolate(input$n),sep=" ", collapse = '\n')
      } else if (ArbitrageRow==6) {
        p<-paste("There's an arbitrage opportunity from",ExchangeNames6[ArbitrageCol],"to",ExchangeNames[ArbitrageCol],"with a profit of $",ArbitrageProfit*isolate(input$n),sep=" ", collapse = '\n')
      } else if (ArbitrageRow==7) {
        p<-paste("There's an arbitrage opportunity from",ExchangeNames7[ArbitrageCol],"to",ExchangeNames[ArbitrageCol],"with a profit of $",ArbitrageProfit*isolate(input$n),sep=" ", collapse = '\n')
      } else {
        p<-paste("There's an arbitrage opportunity from",ExchangeNames8[ArbitrageCol],"to",ExchangeNames[ArbitrageCol],"with a profit of $",ArbitrageProfit*isolate(input$n),sep=" ", collapse = '\n')
      } 
      
      
      
      cat(as.character(p), '\n', file = logfilename, append = TRUE)
    })
    
    # When the client ends the session, suspend the observer and remove the log file.
    session$onSessionEnded(function() {
      logwriter$suspend()
      unlink(logfilename)
    })
    
    fileReaderData <- reactiveFileReader(10000, session, logfilename, readLines)
    
    
    
    output$BitcoinArbitrage <- renderText({
      
      withProgress(message = 'Finding Arbitrage', value = 0, {
        # Number of times we'll go through the loop
        n <- 8
        
        for (i in 1:n) {
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Checking Exchange", i))
          
          # Pause for 1 seconds to simulate a long computation.
          Sys.sleep(1)
        }
      })
      
      
      text <- fileReaderData()
      length(text)<- 15
      text[is.na(text)]<-""
      paste(text, collapse = '\n')
    }) 
  }
  
  shinyApp(ui, server)
}