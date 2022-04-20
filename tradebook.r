# author: Navami Shenoy <github.com/n-shenoy>

"""
    Functions for cleaning, sorting, visualizing and analyzing 
    tradebook data

"""

convert_cols <- function(df){
    # converts the classes of the columns in the dataframe
    Entry.Date <- as.Date(df$Entry.Date)
    Exit.Date <- as.Date(df$Exit.Date)
    Quantity <- as.numeric(df$Quantity)
    Buy.Value <- as.numeric(df$Buy.Value)
    Sell.Value <- as.numeric(df$Sell.Value)
    Profit <- as.numeric(df$Profit)
    Period.of.Holding <- as.numeric(df$Period.of.Holding)
    Fair.Market.Value <- as.numeric(df$Fair.Market.Value)
    Taxable.Profit <- as.numeric(df$Taxable.Profit)
    Turnover <- as.numeric(df$Turnover)
    df <- data.frame(Symbol = df$Symbol, ISIN = df$ISIN, Entry.Date, Exit.Date, Quantity,  Buy.Value, 
                     Sell.Value, Profit, Period.of.Holding, Fair.Market.Value, Taxable.Profit, Turnover)
    df
    
}


gross_cg_profit <- function(df){
    # calculates gross profit earned from capital gains in a financial year 
    sum(df$Profit[which(df$Profit > 0)])
}


gross_cg_loss <- function(df){
    # calculates total capital loss incurred in a financial year
    loss = df$Profit
    diff = df$Exit.Date - df$Entry.Date
    index <- c()
    for(i in seq_along(loss)){
        if(diff[i] != 0 && loss[i] < 0){ #ignore intraday trades
            index <- append(index, i, after = length(index))
        }
    }
    sum(loss[index])
}


stcg <- function(df){
    # calculates short term capital gains
    diff <- df$Exit.Date - df$Entry.Date
    gains <- c()
    for(i in seq_along(diff)){
        if(diff[i] < 365 && diff[i] != 0 && df[i , 8] > 0){
            profit <- df[i , 8]
            gains <- append(gains, profit, after = length(gains))
        }
    }
    sum(gains)
}


ltcg <- function(df){
    # calculates long term capital gains
    diff <- df$Exit.Date - df$Entry.Date
    loss <- c()
    
    for(i in seq_along(diff)){
        if(diff[i] >= 365 && diff[i] != 0 && df[i , 8] > 0){
            profit <- df[i , 8]
            loss <- append(loss, profit, after = length(loss))
        }
    }
    sum(loss)
}


total_dividends <- function(df){
    # calculates total dividends earned in a financial year
    div_df <- read.xlsx(filename, sheetName = 'Equity Dividends', startRow = 15) 
    div_df <- na.omit(div_df)
    sum(div_df$Net.Dividend.Amount)
}


st_trans_exps <- function(df){
    # calculates transfer expenses for short term capital gains
    tax <- read.xlsx(filename, sheetName = 'Equity', startRow = 6)
    tax1 <- tax[ , 1]
    end <- nrow(tax)
    ref <- 0
    
    for(i in seq_along(tax1)){
        if(tax1[i] == 'Short Term Trades'){
            cols <- tax[i+1 , ]
            sttax <- tax[i+2:end, ]
            colnames(sttax) <- cols
            ref <- i 
        }
        if(tax1[i] == 'Long Term Trades'){
            index <- i - ref - 2
            sttax <- sttax[1:index, ]
        }
    }
    
    for(i in c(6,7,8,9,10,11,13)){
        #sttax$Brokerage <- as.numeric(sttax$Brokerage)
        sttax[ , i] <- as.numeric(sttax[ , i])
    }
    
    summ <- lapply(sttax[ , c(6,7,8,9,10,11,13)], sum)
    
    summ <- as.vector(summ, mode = 'numeric')
    
    x <- sum(summ)
    
    
    sum2 <- 0
    
    other_charges <- read.xlsx(filename, sheetName = 'Other Debits and Credits', startRow = 15)
    
    other_charges <- na.omit(other_charges)
    #other_charges[ , 3] <-  as.numeric(other_charges[ , 3])
    
    
    for(i in seq_along(other_charges[ , 1])){
        if(grepl('DP Charges' , other_charges[i , 1], ignore.case = TRUE) == TRUE){
            sum2 <- sum2 + as.numeric(other_charges[i , 3])
        }  
    } 
    x + sum2
}
