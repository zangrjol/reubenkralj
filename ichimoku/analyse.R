library(tidyverse)
library(dplyr)
library(zoo)
library(smooth)
library(Mcomp)
library(ggplot2)
library("plot3D")
library(plotly)
load("ichimoku/data.rda")


# PREPARE TABLE: filter out to only when the position changes
    signal_ichimoku <- function(data, signal, base) {
      ichimoku(data, signal, base) %>% 
      filter(diff != 0) %>%
      mutate(signal = ifelse(diff > 0, 1, -1)) %>% 
      select(-diff) %>% 
      mutate(signal2 = rollmeanr(signal, 2, fill = 0)) %>%
      filter(signal2 ==0) %>% 
      select(start, open, signal)
    }
    
    ## calculate all combinations of ichimoku parameters
    
    ba <- seq(10,40,1)
    si <- seq(10,40,1)
    
    comb <- expand.grid(s = si, b = ba)
    
    comb %>%
      rowwise() %>%
      filter(s < b) %>% 
      mutate(pl = pl(signal_ichimoku(data, s, b), 0.00025)) -> final 
    
    #preparing for 3d plot
    final <- data.frame(final)
    final$s <- as.character(final$s)
    final$b <- as.character(final$b)
    #make matrix first
    a <- spread_(final, 
            key = "b",
            value = "pl"
    )
    a <- a %>% remove_rownames %>% column_to_rownames(var = "s")
    matrix_final <- data.matrix(a)
    
    ## plot
    plot_ly(x = ~rownames(matrix_final), y = ~ colnames(matrix_final), z = ~matrix_final) %>% add_surface()

    
### now choose the best value and present the performance
    best_signal <- final %>% filter(pl == max(pl)) %>% .$s %>% as.integer()
    best_base <- final %>% filter(pl == max(pl)) %>% .$b %>% as.integer()
    
    top_performance <- ichimoku_performance(data, best_signal, best_base, 0.00025)
    
    ## plot performance chart of the best ichimoku parameters
    top_performance %>% ggplot(aes(start, pl)) + geom_line()
    
### createPerformance Summary
    investment <- 10000
    trades <- signal_ichimoku(data, best_signal, best_base) %>% 
         mutate(pl_per_trade = lag(signal, default = signal[1])*(open - lag(open, default = open[1]))/lag(open, default = open[1]))
            
    
    performance <- trades_pl(trades, 0.00025) %>% 
          mutate(rolling_pl = (pl-lag(pl, default = pl[1]))*investment)
    
    ## profits
    net_profit <- investment*(top_performance$pl[nrow(top_performance)])

    gross_profit <- performance %>% 
      filter(rolling_pl>0) %>% 
      summarize(gross_profit = sum(rolling_pl))%>%  .$gross_profit

    gross_loss <- performance %>% 
      filter(rolling_pl<0) %>% 
      summarize(gross_loss = sum(rolling_pl))%>%  .$gross_loss
    
    profit_factor <- gross_profit/abs(gross_loss)
    
    
    ## trades
    total_trades <- nrow(performance)
    
    profit_trades <- performance %>% filter(pl_per_trade>0) %>%  nrow()
    
    loss_trades <- performance %>% filter(pl_per_trade<0) %>%  nrow()
    
    even_trades <- performance %>% filter(pl_per_trade==0) %>%  nrow()
    
    percent_profitable <- profit_trades/total_trades
    
    
    
    ## averages
    avg_trade_net_profit <- net_profit/total_trades

    avg_winning_trade <- gross_profit / profit_trades    

    avg_losing_trade <- gross_loss / loss_trades    

    largest_win <- max(performance$rolling_pl)  
    
    largest_loss <- min(performance$rolling_pl)
    
    ## consecutive
    cons <- performance %>% mutate(char = ifelse(rolling_pl > 0, 1, -1))
    
    max_cons_win <-cons %>% mutate( cons_plus = ave(cons$char, cumsum(c(F, abs(diff(cons$char)) == 2)), FUN=seq_along) - 1 ) %>% 
      filter(char == 1) %>%  summarize(cons_plus = max(cons_plus)) %>%  .$cons_plus
    
    max_cons_loss <-cons %>% mutate( cons_minus = ave(cons$char, cumsum(c(F, abs(diff(cons$char)) == 2)), FUN=seq_along) - 1 ) %>% 
      filter(char == -1) %>%  summarize(cons_minus = max(cons_minus)) %>%  .$cons_minus
    
    ## number of shares
    
    max_shares_held <- performance %>% mutate(shares_held = investment*pl/open) %>% summarise(max = max(shares_held)) %>%  .$max

    
    ## drawdown
    max_daily_percent_drawdown <- top_performance %>% mutate(start2 = as.Date(start), 
                            balance = investment*pl,
                            drawdown = balance - lag(balance, default = balance[1]),
                            drawdown = ifelse(drawdown <0, drawdown, 0)) %>% 
      group_by(start2) %>% 
      summarize(daily_drawdown = sum(drawdown), percent_balance = daily_drawdown/mean(balance)*100) %>% 
      filter(percent_balance == min(percent_balance)) %>% .$percent_balance

   
    
## create a summary table now:    
    
    
### visualisations of performance    
     #histogram of drawdowns
    drawdown_hist <- top_performance %>% mutate(start2 = as.Date(start), 
                                                balance = investment*pl,
                                                drawdown = balance - lag(balance, default = balance[1]),
                                                drawdown = ifelse(drawdown <0, drawdown, 0)) %>% 
      group_by(start2) %>% 
      summarize(daily_drawdown = sum(drawdown), percent_balance = daily_drawdown/mean(balance)*100) %>% 
      ggplot(aes(percent_balance)) + geom_histogram()
    
    
    
    
    
