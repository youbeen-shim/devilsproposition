library(shiny)
library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Fetch SPY stock prices for the last 6 months
start_date <- Sys.Date() - months(6)
end_date <- Sys.Date()
# getSymbols("AAPL", src = "yahoo", from = start_date, to = end_date)
# spy_data <- data.frame(date = index(AAPL), coredata(AAPL))
# saveRDS(spy_data, "stock.rds")
spy_data <- readRDS("stock.rds")

# Calculate monthly returns
spy_data <- spy_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(open = first(AAPL.Open), close = last(AAPL.Close)) %>%
  mutate(perc_change = (close / lag(close) - 1) * 100)

# Utility functions
showSummaryModal <- function(total_income, starting_amount, ending_amount, account_growth, raw_investment_growth) {
  summary_text <- paste(
    "<h4>Summary</h4>",
    "<p><b>Starting Amount:</b> $10000</p>",
    "<p><b>Total Income:</b> $", total_income, "</p>",
    "<p><b>Ending Amount:</b> $", round(ending_amount, 2), "</p>",
    "<p><b>Account Growth (%):</b> ", round(account_growth, 2), "%</p>",
    "<p><b>Account Growth (Raw Investment %):</b> ", round(raw_investment_growth, 2), "%</p>"
  )
  showModal(modalDialog(
    title = "Game Summary",
    HTML(summary_text),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Close"),
      actionButton("continueJourney", "Continue Journey")
    )
  ))
}

# Define UI
ui <- fluidPage(
      titlePanel("one: how do i make money like an insider trader?"),
      tags$head(
        tags$style(HTML("
      .bubble-box {
        border: 1px solid #ccc;
        border-radius: 10px;
        padding: 20px;
        position: relative;
        box-shadow: 0px 0px 10px rgba(0,0,0,0.1);
        background-color: #fff;
        margin-bottom: 20px;
      }

      .close-btn {
        position: absolute;
        top: 10px;
        right: 10px;
        background: none;
        border: none;
        font-size: 20px;
        color: #888;
        cursor: pointer;
      }

      .close-btn:hover {
        color: #000;
      }

      .tooltip-icon {
        position: relative;
        display: inline-block;
        cursor: pointer;
        margin-left: 10px;
      }

      .tooltip-text {
        visibility: hidden;
        width: 200px;
        background-color: #f9f9f9;
        color: #333;
        text-align: left;
        border-radius: 4px;
        padding: 5px;
        position: absolute;
        z-index: 1;
        bottom: 125%; /* Position above the icon */
        left: 50%;
        margin-left: -100px; /* Center the tooltip */
        box-shadow: 0px 0px 10px 0px rgba(0,0,0,0.2);
      }

      .tooltip-icon:hover .tooltip-text {
        visibility: visible;
      }

      .introduction-step p {
        display: none;
      }
    "))
      ),
      tags$div(id = "row1", class = "bubble-box",
        fluidRow(
          column(12,
                 actionButton("closeRow1", "x", class = "close-btn"),
                 # h2("\"making money is easy, just ask Rumpelstiltskin\""),
                 h3("you are done playing fair.."),
                 HTML("<p>did you know that the richest 1% owns 49% of all stocks? the system is rigged, and you are 
                          tired of asking to play a fair game. you are ready to make some moves, big moves. but how does investing 
                          even work? you ask your finance-savvy friend who never seems to make a bad investment.<p>"),
                 HTML("<p>after some desperate proding, your friend finally gives in. <b>\"making money is easy, just ask Rumpelstiltskin.\"</b><p>"),
                 HTML("<p>you've heard about this sketchy, gnomeish man before, living off the last exit on the highway 
                          by the old mines. after briefly comtemplating, you cautiously crawl up, noticing an overwhelming aroma of what you hope was a really ripe durian. 
                          after a tough bargin (there was none to be done), you agree on a fair price - your to-be firstborn.<p>"),
                 HTML("<p>\"in return, i'll show you how any stock will move in the future.\"<p>"),
                 textInput("symb", "which stock interests you? (e.g. GOOG, NVDA, V)", "AAPL"),
                 actionButton("cheat", "sell ur firstborn")
                 )
          )
        ),
      tags$div(id = "row2", class = "bubble-box",
        fluidRow(
          h3("Stock Price Forcast"),
          plotOutput("stockPlot")
        )
      ),
      tags$div(id = "row1_2", class = "bubble-box",
               fluidRow(
                 column(12,
                        actionButton("closeRow1_2", "x", class = "close-btn"),
                        h3("Now it's all on you"),
                        HTML("<p>you now have access to the information that any investors would drool over: exact knowledge of 
                             how the stock prices will fluctuate in the next 6 months.<p>"),
                        HTML("<p>you have $10,000 at your disposal, and each month, you have one of two actions: buying or selling<p>"),
                        HTML("<p>what's the most amount of money you can make in the next 6 months?")
                 )
               )
      ),
      tags$div(id = "row3", class = "bubble-box",
        fluidRow(
          column(4,
                 uiOutput("investmentUI"),
                 br(),
                 # numericInput("sell", "Sell Amount:", value = 0, min = 0),
                 actionButton("submit", "Submit this Month's Order"),
                 tags$hr(style = "border: 0; border-top: 3px double #8c8c8c; margin: 20px 0;"),
                 actionButton("reset", "Reset Game"),
                 actionButton("end", "End Game"),
                 textOutput("error"), 
          ),
          column(8,
                 h3("Investment and Cash Account Values Over Time"),
                 plotOutput("accountPlot")
          )
        )
      ),
      tags$div(id = "row4", class = "bubble-box",
        fluidRow(
          column(12,
                 h3("Portfolio Performance"),
                 tableOutput("portfolioTable")
          )
        )
      )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$continueJourney, {
    showModal(modalDialog(
      title = "time for you to make a decision, sojourner",
      div(
        div(
          class = "choice-box",
          h3("more leverage = more money"),
          p("stocks are fun, but what if you want to make even more money with the same information?"),
          actionButton("warriorBtn", "learn about options")
        ),
        div(
          class = "choice-box",
          h3("wait, but my (future) baby..."),
          p("selling their firstborn isn't for everyone, how can you invest without knowing the future?"),
          actionButton("mageBtn", "learn about indexes & time-in-market")
        )
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("moreOption", "Neither of these! ")
      )
    ))
  })
  
  observeEvent(input$moreOption, {
    showModal(modalDialog(
      title = "curious one, aren't ya?",
      "well did you notice that your money grew in your savings account? how does that work?",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("ninjaBtn", "learn about interest rates & the federal reserve")
      )
    ))
  })
  
  observeEvent(input$warriorBtn, {
    showModal(modalDialog(
      title = "options, calls, and shorts",
      HTML('sorry, this lesson is still being built :( <a href="https://forms.gle/75fgQmE1f1JeaYis7" target="_blank">sign up to the email list here for updates!</a>'),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$mageBtn, {
    showModal(modalDialog(
      title = "etf, mutual funds, and time-in-market",
      HTML('sorry, this lesson is still being built :( <a href="https://forms.gle/75fgQmE1f1JeaYis7" target="_blank">sign up to the email list here for updates!</a>'),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$ninjaBtn, {
    showModal(modalDialog(
      title = "banks, federal reserve, and interest rates ",
      HTML('sorry, this lesson is still being built :( <a href="https://forms.gle/75fgQmE1f1JeaYis7" target="_blank">sign up to the email list here for updates!</a>'),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  observeEvent(input$maxInvestment, {
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      available_cash <- 10000
      income <- 0
      sell_amount <- 0
    } else {
      latest_entry <- tail(current_portfolio, 1)
      available_cash <- latest_entry$Cash
      income <- 0
      sell_amount <- latest_entry$MarketChangeValue
    }
    
    total_available <- available_cash + income
    updateNumericInput(session, "investment", value = total_available)
  })
  
  observeEvent(input$maxSell, {
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      sell_amount <- 0
    } else {
      latest_entry <- tail(current_portfolio, 1)
      sell_amount <- latest_entry$MarketChangeValue
    }
    
    updateNumericInput(session, "sell", value = floor(sell_amount))
  })
  
  observeEvent(input$closeRow1, {
    removeUI(selector = "#row1")
  })
  
  observeEvent(input$closeRow1_2, {
    removeUI(selector = "#row1_2")
  })
  
  observeEvent(input$reset, {
  portfolio(initial_portfolio)
  error_message("")
})
  
  # Initialize portfolio with empty data frame
  initial_portfolio <- data.frame(
    Month = as.Date(character()),
    Investment = numeric(),
    CumulativeInvestment = numeric(),
    Value = numeric(),
    MarketChangeValue = numeric(),
    Cash = numeric(),
    InterestAdjustedCash = numeric(),
    AmountAvailableForInvestment = numeric(),
    stringsAsFactors = FALSE
  )
  portfolio <- reactiveVal(initial_portfolio)
  
  # Error message
  error_message <- reactiveVal("")
  
  # Generate dynamic UI for investment input
  output$investmentUI <- renderUI({
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      available_cash <- 10000
      income <- 0
      sell_amount <- 0
    } else {
      latest_entry <- tail(current_portfolio, 1)
      available_cash <- latest_entry$Cash
      income <- 0
      sell_amount <- latest_entry$MarketChangeValue
    }
    
    total_available <- available_cash + income
    
    tagList(
      numericInput("investment", 
                   #paste("Investment Amount (out of $", round(available_cash, 2), " + $", income, "):", sep = ""), 
                   paste("Investment Amount (out of $", round(available_cash, 2), "):", sep = ""),
                   value = 1000, min = 0, max = total_available, step = 100),
      actionButton("maxInvestment", "max amount"),
      numericInput("sell", 
                   paste("Sell Amount (available: $", round(sell_amount, 2), "):", sep = ""), 
                   value = 0, min = 0, max = sell_amount, step = 100),
      actionButton("maxSell", "max amount")
    )
  })
  
  observeEvent(input$submit, {
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio) + 1
    error_message("")
    
    if (current_month == 1) {
      new_entry <- data.frame(
        Month = spy_data$month[current_month],
        Investment = 0,
        CumulativeInvestment = 0,
        Value = 0,
        MarketChangeValue = 0,
        Cash = 10000,
        InterestAdjustedCash = 10000 * 1.05,
        AmountAvailableForInvestment = 10000, #2500 * 1.05 + 1000,
        stringsAsFactors = FALSE
      )
      portfolio(rbind(current_portfolio, new_entry))
      current_portfolio <- portfolio()
      current_month <- nrow(current_portfolio) + 1
    }
    
    if (current_month <= nrow(spy_data)) {
      investment <- input$investment
      sell_amount <- input$sell
      last_cash <- if (current_month == 1) 0 else tail(current_portfolio$AmountAvailableForInvestment, 1)
      previous_value <- if (current_month == 1) 0 else tail(current_portfolio$MarketChangeValue, 1)
      
      if (investment > last_cash) {
        error_message("Error: Investment amount exceeds available cash.")
        return()
      }
      if (sell_amount > previous_value) {
        error_message("Error: Sell amount exceeds portfolio value.")
        return()
      }
      
      cash <- last_cash - investment + sell_amount
      interest_adjusted_cash <- cash * 1.05
      amount_available_for_investment <- interest_adjusted_cash + 0
      cumulative_investment <- sum(current_portfolio$Investment) + investment
      
      if (current_month == 1) {
        market_change_value <- investment - sell_amount + 0
      } else {
        change <- spy_data$close[current_month] / spy_data$close[current_month - 1]
        market_change_value <- (previous_value - sell_amount) * change + investment
      }
      
      value <- market_change_value
      
      new_entry <- data.frame(
        Month = spy_data$month[current_month],
        Investment = investment,
        CumulativeInvestment = cumulative_investment,
        Value = value,
        MarketChangeValue = market_change_value,
        Cash = cash,
        InterestAdjustedCash = interest_adjusted_cash,
        AmountAvailableForInvestment = amount_available_for_investment,
        stringsAsFactors = FALSE
      )
      portfolio(rbind(current_portfolio, new_entry))
      
      updateNumericInput(session, "sell", value = 0, max = market_change_value)
      latest_entry <- tail(portfolio(), 1)
      available_cash <- latest_entry$Cash
      income <- 1000
      total_available <- available_cash + income
      updateNumericInput(session, "investment", value = 0, max = total_available, label = paste("Investment Amount (out of $", round(available_cash, 2), "):", sep = ""))
    }
  })
  
  observeEvent(input$end, {
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio)
    
    if (current_month > 0) {
      # total_income <- (current_month - 1) * 1000
      total_income <- 0
      starting_amount <- 10000
      latest_entry <- tail(current_portfolio, 1)
      ending_amount <- latest_entry$MarketChangeValue + latest_entry$Cash
      account_growth <- (ending_amount - starting_amount) / starting_amount * 100
      raw_investment_growth <- (ending_amount - total_income - starting_amount) / starting_amount * 100
      
      showSummaryModal(total_income, starting_amount, ending_amount, account_growth, raw_investment_growth)
      
      # Reset sell input to 0
      updateNumericInput(session, "sell", value = 0)
      
      # Update investment input to reflect split amount
      available_cash <- latest_entry$Cash
      income <- 0
      total_available <- available_cash + income
      updateNumericInput(session, "investment", value = 0, max = total_available, label = paste("Investment Amount (out of $", round(available_cash, 2), "):", sep = ""))
    }
  })
  
  observeEvent(input$reset, {
    portfolio(initial_portfolio)
    error_message("")
    output$summary <- renderUI({
      HTML("<p>Game reset. Start a new game by making investments.</p>")
    })
  })
  
  output$error <- renderText({
    error_message()
  })
  
  output$stockPlot <- renderPlot({
    req(input$cheat)
    current_portfolio <- portfolio()
    invested_months <- as.Date(current_portfolio$Month)
    
    spy_data <- spy_data %>%
      mutate(point_color = ifelse(month %in% tail(invested_months, 1), "red", "grey"))
    
    ggplot(spy_data, aes(x = month, y = close)) +
      geom_line() +
      geom_point(aes(color = point_color), size = 3) +
      scale_color_identity() +
      labs(title = "AAPL stock prices for the next 6 months", x = "Date", y = "Price") +
      theme_minimal()
  })
  
  output$portfolioTable <- renderTable({
    req(input$submit)
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      return(data.frame("No data available yet."))
    }
    
    current_portfolio %>%
      mutate(
        Month = format(as.Date(Month), "%Y-%m"),
        Investment = round(Investment, 2),
        CumulativeInvestment = round(CumulativeInvestment, 2),
        Value = round(Value, 2),
        Cash = round(Cash, 2),
        InterestAdjustedCash = round(InterestAdjustedCash, 2),
        AmountAvailableForInvestment = round(AmountAvailableForInvestment, 2)
      )
  }, rownames = FALSE)
  
  output$spyChange <- renderUI({
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio) + 1
    
    if (current_month > 1 && current_month <= nrow(spy_data)) {
      spy_change <- spy_data$perc_change[current_month]
      HTML(paste("<h4>AAPL % Change from last month:</h4>", "<b>", round(spy_change, 2), "%</b>"))
    } else {
      HTML("<h4>AAPL % Change from last month:</h4> <b>Data will be displayed here.</b>")
    }
  })
  
  output$cashAvailable <- renderUI({
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      HTML("<h4>Cash available for next month:</h4> <b>Data will be displayed here.</b>")
    } else {
      latest_entry <- tail(current_portfolio, 1)
      HTML(paste("<h4>Cash available for next month:</h4>", "<b>$", round(latest_entry$AmountAvailableForInvestment, 2), "</b>"))
    }
  })
  
  output$accountPlot <- renderPlot({
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) > 0) {
      account_data <- current_portfolio %>%
        mutate(Total = Cash + MarketChangeValue) %>%
        select(Month, Cash, Investment = MarketChangeValue, Total) %>%
        pivot_longer(cols = c("Cash", "Investment", "Total"), names_to = "AccountType", values_to = "Amount")
      
      ggplot(account_data, aes(x = Month, y = Amount, color = AccountType)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Investment and Cash Account Values Over Time", x = "Month", y = "Amount ($)", color = "Account Type") +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


