############################################################
# 1) Load libraries and read data (OUTSIDE the function)
############################################################
library(readxl)
library(knitr)

# Adjust the path to your actual location if needed:
data <- read.csv("Preferences 2025.csv")

############################################################
# 2) Define the "big function"
############################################################
full_analysis <- function(data,
                          base_price = 2000, 
                          max_price = 2500, 
                          competitors_price = 2500, 
                          market_size = 100) {
  
  # ------------------------------------------------------
  # STEP 2: OLS from first principles (Partworths)
  # ------------------------------------------------------
  x <- as.matrix(data[, 4:8])  # 5 X variables
  y <- as.matrix(data[, 3])    # Preference Rank as Y
  
  n <- nrow(x)
  p <- ncol(x)
  
  colnames(x) <- c("Screen 75 inch", 
                   "Screen 85 inch", 
                   "Resolution 4K", 
                   "Sony", 
                   "Price")
  colnames(y) <- "Preference Rank"
  
  xx <- cbind(1, x)   # add intercept
  colnames(xx) <- c("Intercept", 
                    "Screen 75 inch", 
                    "Screen 85 inch", 
                    "Resolution 4K", 
                    "Sony", 
                    "Price")
  
  xpx  <- t(xx) %*% xx
  xpxi <- solve(xpx)
  xpy  <- t(xx) %*% y
  bhat <- xpxi %*% xpy
  
  rownames(bhat) <- c("Intercept", 
                      "Screen 75 inch", 
                      "Screen 85 inch", 
                      "Resolution 4K", 
                      "Sony", 
                      "Price")
  colnames(bhat) <- "Coefficients"
  
  # Compute SSE, Std. Errors, T-values
  yhat <- xx %*% bhat
  err  <- y - yhat
  sse  <- t(err) %*% err
  s2   <- sse / (n - p)
  
  se   <- sqrt(c(s2) * diag(xpxi))  # standard errors
  tval <- bhat / se                # t-values
  
  my_estimates <- cbind(bhat, se, tval)
  colnames(my_estimates) <- c("Estimates", "Std Errors", "t-values")
  
  cat("\n--- Partworths (OLS Estimates) ---\n")
  print(my_estimates)
  
  
  # Extract partworths as named variables
  Intercept        <- bhat["Intercept", 1]
  Screen_75_inch   <- bhat["Screen 75 inch", 1]
  Screen_85_inch   <- bhat["Screen 85 inch", 1]
  Resolution_4K    <- bhat["Resolution 4K", 1]
  Sony             <- bhat["Sony", 1]
  PriceCoeff       <- bhat["Price", 1]  # typically negative
  
  
  # ------------------------------------------------------
  # STEP 3: Attribute Importance
  # ------------------------------------------------------
  range_screen_size       <- round(Screen_85_inch - Screen_75_inch, 1)
  range_screen_resolution <- round(Resolution_4K - 0, 1)  # baseline = 0
  range_brand_name        <- round(Sony - 0, 1)          # baseline = 0
  range_price             <- round(0 - PriceCoeff, 1)    # baseline = 0
  
  ranges <- c(range_screen_size, 
              range_screen_resolution, 
              range_brand_name, 
              range_price)
  
  importance <- (ranges / sum(ranges)) * 100
  importance <- paste0(round(importance, 2), "%")
  
  attributes <- c("Screen Size", 
                  "Screen Resolution", 
                  "Brand Name", 
                  "Price")
  
  attribute_importances <- data.frame(
    Attributes = attributes,
    Range      = ranges,
    Importance = importance
  )
  
  cat("\n--- Attribute Importance ---\n")
  kable(attribute_importances, caption = "Attribute Importances of Each Attribute")
  
  
  # ------------------------------------------------------
  # STEP 3 (continued): Willingness To Pay
  # ------------------------------------------------------
  Sony_price    <- 2500
  Sharp_price   <- 2000
  price_savings <- Sony_price - Sharp_price
  one_util      <- price_savings / (0 - PriceCoeff)
  
  WTP_75_screen <- round(one_util * Screen_75_inch, 2)
  WTP_85_screen <- round(one_util * Screen_85_inch, 2)
  WTP_Sony      <- round(one_util * Sony, 2)
  WTP_4K        <- round(one_util * Resolution_4K, 2)
  
  cat("\n--- Willingness to Pay ---\n",
      "WTP for 75 inch screen:   $", WTP_75_screen, "\n",
      "WTP for 85 inch screen:   $", WTP_85_screen, "\n",
      "WTP for Sony brand name:  $", WTP_Sony, "\n",
      "WTP for 4K resolution:    $", WTP_4K, "\n",
      sep = "")
  
  
  # ------------------------------------------------------
  # STEPS 4–6: Optimal Price, Profit, Market Share, Plots
  # ------------------------------------------------------
  # Create partworths vector for the final profit function
  partworths_final <- c(
    Intercept,
    Screen_75_inch,
    Screen_85_inch,
    Resolution_4K,
    Sony,
    PriceCoeff
  )
  
  # Calculate best profit and market share
  calculate_profit_and_ms <- function(price) {
    my_design <- c(1, 0, 1, 0, 0, (price - base_price)/(max_price - base_price))
    sony      <- c(1, 1, 0, 1, 1, (competitors_price - base_price)/(max_price - base_price))
    sharp     <- c(1, 0, 1, 1, 0, (base_price - base_price)/(max_price - base_price))
    
    my_design_sum <- sum(partworths_final * my_design)
    sony_sum      <- sum(partworths_final * sony)
    sharp_sum     <- sum(partworths_final * sharp)
    
    my_design_attra <- exp(my_design_sum)
    sony_attra      <- exp(sony_sum)
    sharp_attra     <- exp(sharp_sum)
    total_attra     <- my_design_attra + sony_attra + sharp_attra
    
    my_design_ms <- my_design_attra / total_attra
    sales        <- my_design_ms * market_size
    margin       <- price - base_price
    profit       <- margin * sales
    
    return(list(profit = profit, ms = my_design_ms, sales = sales))
  }
  
  calculate_profit <- function(price) {
    calculate_profit_and_ms(price)$profit
  }
  
  # Optimize to find best price
  result         <- optimize(calculate_profit, 
                             interval = c(1, 10000), 
                             maximum = TRUE)
  best_price     <- result$maximum
  optimal_result <- calculate_profit_and_ms(best_price)
  
  # ------------------------------------------------------
  # STEPS 7-8: Plots
  # ------------------------------------------------------
  # Plot side-by-side
  prices  <- seq(1500, 2600, by = 100)
  results <- lapply(prices, calculate_profit_and_ms)
  
  df <- data.frame(
    price  = prices,
    sales  = sapply(results, function(x) x$sales),
    profit = sapply(results, function(x) x$profit)
  )
  
  old_par <- par(mfrow = c(1, 2))
  
  plot(df$price, df$sales, 
       type = "b", pch = 18, col = "blue",
       xlab = "Price", ylab = "Sales",
       main = "Sales = Share x Market Size")
  
  plot(df$price, df$profit, 
       type = "b", pch = 18, col = "blue",
       xlab = "Price", ylab = "Profit",
       main = "Profit = Margin x Sales")
  
  par(old_par)
  
  
  # ------------------------------------------------------
  # Return everything in a list
  # ------------------------------------------------------
  list(
    OLS_Estimates    = my_estimates,
    AttributeRanges  = attribute_importances,
    WTP              = list(
      WTP_75_inch = WTP_75_screen, 
      WTP_85_inch = WTP_85_screen, 
      WTP_Sony    = WTP_Sony, 
      WTP_4K      = WTP_4K
    ),
    Optimal_Price_Results = list(
      optimal_price = best_price,
      max_profit    = optimal_result$profit,
      market_share  = optimal_result$ms
    )
  )
}


############################################################
# 3) Example Usage
############################################################
# Call `full_analysis()` to do the whole analysis

result <- full_analysis(
  data,
  base_price       = 2000,
  max_price        = 2500,
  competitors_price= 2500,
  market_size      = 100
)

# Inspect the returned results
result
