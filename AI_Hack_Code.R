library(tidyverse)
library(caret)


# Load in Data ------------------------------------------------------------
ds_LOBs <- read_csv("LOBs.csv")

# Visualize the mid price for eth-usd for different exchanges
ggplot(ds_LOBs %>% filter(pair=="eth-usd"), aes(x=X1, y=mid_price)) + 
  geom_line(aes(color=exchange)) +
  facet_grid(cols=vars(exchange)) +
  ggtitle("ETH-USD Currency Pair Over Time for a Selection of Exchanges") +
  xlab("") + ylab("mid-price (USD)") +
  theme(legend.position = "none")
ggsave("eth-usd_time.PNG", width=9, height=4)


# Clean/Wrangle Data ------------------------------------------------------

data_processor <- function(data, pair = "eth-usd",
                           processor = c("full", "partial")) {
  # Filter for specific currency pair
  data_paired <- data %>%
    filter(pair=="eth-usd") %>%
    select(X1, 
           bid_vol = bid_volume0_1, ask_vol = ask_volume0_1, 
           bid_price = bid_price0_1, ask_price=ask_price0_1,
           exchange)
  
  # Find all the exchanges
  exchanges <- unique(data_paired$exchange); K <- length(exchanges)
  
  # split by the exchange to allow for inner join later 
  data_exchange_filtered <- lapply(exchanges, function(x)
                                   data_paired %>% filter(exchange==x) %>% select(-exchange))
  
  # Rename columns 
  for(i in 1:K) {
    colnames(data_exchange_filtered[[i]]) <- c("date",
                                          paste0("bid_vol_", exchanges[i]),
                                          paste0("ask_vol_", exchanges[i]),
                                          paste0("bid_price_", exchanges[i]),
                                          paste0("ask_price_", exchanges[i]))
  }
  
  # Inner join the different exchange's observations
  data_inner_joined <- data_exchange_filtered[[1]]
  for(i in 2:K) {
    data_inner_joined <- data_inner_joined %>% 
      inner_join(data_exchange_filtered[[i]], by="date")
  }
  
  if(processor=="partial") return(data_inner_joined)
  
  best_trades <- apply(data_inner_joined[, -1], 1, function(row) {
    max_score <- -Inf
    max_i <- NA
    max_j <- NA
    for(i in 1:K){
      for(j in setdiff(1:K, i)){
        score<- (row[4*(j-1) + 3] - row[4*(i-1) + 4]) * min(row[4*(j-1) + 1], row[4*(i-1) + 2])
        if(score > max_score){
          max_score <- score
          max_i <- i
          max_j <- j
        }
      }
    }
    c(exchanges[max_i], exchanges[max_j],
      row[4*(max_i-1) + 4], row[4*(max_j-1) + 3],
      row[4*(max_i-1) + 2], row[4*(max_j-1) + 1],
      max_score)
  }) %>% t()
  
  data_processed <- cbind(data_inner_joined$date, best_trades) %>% data.frame()
  colnames(data_processed) <- c("date", 
                                "seller", "buyer",
                                "ask_price", "bid_price",
                                "ask_vol", "bid_vol",
                                "profit")
  data_processed %>% mutate(date = as.POSIXct(as.numeric(date), origin="1970-01-01"),
                            trans_code=paste0(seller, "-", buyer),
                            profit = as.numeric(profit))
}

# Process the data
data_processed <- ds_LOBs %>% data_processor()
data_processed %>% head()

# If profit is less than zero then no trade needs to be made, remove this
data_processed <- data_processed %>% filter(profit > 0)


# EDA ---------------------------------------------------------------------

# Calculate the profits 
data_processed %>% summarise(total_profit = sum(profit))

# Make a plot of potential arbitrage profits against time
ggplot(data_processed, aes(date, profit)) + 
  geom_point(aes(color=trans_code), alpha=0.5) + 
  scale_color_discrete(name="[Seller]-[Buyer]") +
  xlab("Date") + ylab("Profit From Arbitrage Event") +
  ggtitle("Profit From Arbitrage Events Against Time")
ggsave("Arbitrage_Events_ETH_USD.PNG", width=10, height=5)

# Make a plot of the potential arbitrage events profit distribution 
ggplot(data_processed, aes(x=as.factor(1), y=profit)) + 
  geom_boxplot(outlier.alpha=0) +
  geom_jitter(aes(color=trans_code), alpha=0.5) + 
  scale_color_discrete(name="[Seller]-[Buyer]") +
  xlab("Date") + ylab("Profit From Arbitrage Event") +
  scale_y_log10() +
  ggtitle("Distribution of Arbitrage Event Profits")
ggsave("Arbitrage_Events_ETH_USD_dist.PNG", width=8, height=5)

# Make a plot of the [Seller]-[Buyer] pairs resbonsible for arbitrage events
ggplot(data_processed, aes(x=trans_code)) + 
  geom_bar(aes(fill=trans_code)) +
  xlab("[Seller]-[Buyer]") +
  theme(legend.position = "none") +
  ggtitle("Arbitrage Events By Exchange Buyer-Seller Counts")
ggsave("Arbitrage_Event_Counts.PNG", width=8, height=5)


# Preprocessing for Modeling ---------------------------------------------------------------
# The following seeks to transform the data into a model ready format
data_model <- ds_LOBs %>% data_processor(processor = "partial")
data_model <- data_model %>% 
  left_join(data_processed %>% 
              transmute(date, arbitrage = (profit>0)), 
            by = "date") %>%
  replace_na(list(arbitrage=FALSE))

# This will reshape the data such that each observation has the current arbitrage
# event indicator as a label, with the previous 5 measurements as covariates
transform_model_data <- function(data, lag=5) {
  N <- nrow(data)
  M <- ncol(data)
  model_matrix <- matrix(nrow=N-lag, ncol=lag*(M-2) + 1)
  for(i in 1:(N-lag)) {
    y <- data[i, "arbitrage"]
    x <- data[(i+1):(i+lag), setdiff(1:M, c(1, M))] %>% t() %>% as.vector()
    obs <- c(x, y) %>% unlist()
    model_matrix[i, ] <- obs
  }
  return(model_matrix)
}

data_matrix <- data_model %>% transform_model_data()

# Now need to generate an under-sampled data set to rectify class imbalance
# This means taking all observations with positive response and a randomly 
# sampled number of observations with negative response
# Set seed for reproducability 
set.seed(42)
num_pos <- data_model %>% summarise(sum(arbitrage)) %>% as.numeric() # 500
M <- ncol(data_matrix)
pos_data_matrix <- data_matrix[data_matrix[,M]==1,] # 500 observations
neg_data_matrix <- data_matrix[data_matrix[,M]==0,] # 40550 observations
rand_indicies <- sample(1:nrow(neg_data_matrix), num_pos) 
neg_data_matrix <- neg_data_matrix[rand_indicies, ] #take random subset of size 500
data_matrix_train <- rbind(pos_data_matrix, neg_data_matrix)
rand_indicies <- sample(1:nrow(data_matrix_train))
data_matrix_train <- data_matrix_train[rand_indicies, ] # Shuffle rows

# Now have a model ready dataset, all that reamins is to pass it into a dataframe
# with appropriate column names such that models can be trained
data_train <- data_matrix_train %>% 
  as.data.frame()
colnames(data_train)[M] <- "Y"
data_train <- data_train %>% mutate(Y=as.factor(Y))

# Split testing and training
split_ratio = 0.8
N <- nrow(data_train)
rand_indicies <- sample(1:N, floor(split_ratio*N))
data_test <- data_train[-rand_indicies, ]
data_train <- data_train[rand_indicies, ]


# Modeling ---------------------------------------------------------------
# Fit logistic regression using backwards elimination
my_logistic_reg <- train(
  form = Y ~ .,
  data = data_train,
  trControl = trainControl(method = "cv", number = 10),
  method = "glmStepAIC",
  family = "binomial")

my_logistic_reg_results <- my_logistic_reg$results

# Fit a QDA classifier
my_QDA <- train(
  form = Y ~ .,
  data = data_train,
  trControl = trainControl(method = "cv", number = 10),
  method = "stepQDA")

my_QDA_results <- my_QDA$results

# Fit a random forest classifier
my_random_forrest <- train(
  form = Y ~ .,
  data = data_train,
  tuneGrid=expand.grid(.mtry=2:8),
  trControl = trainControl(method = "cv", number = 10),
  method = "rf")

my_random_forrest_results <- my_random_forrest$results

# Fit SVM
my_svm <- train(
  form = Y ~ .,
  data = data_train,
  trControl = trainControl(method = "cv", number = 10),
  method = "svmRadial",
  preProcess = c("center","scale"),
  tuneGrid=expand.grid(.sigma=seq(0.1, 1, 0.1), .C=seq(0.1, 1, 0.1)))

my_svm_results <- my_svm$results

# Results
print(my_logistic_reg_results %>% filter(Accuracy == max(Accuracy)))
print(my_QDA_results %>% filter(Accuracy == max(Accuracy)))
print(my_random_forrest_results %>% filter(Accuracy == max(Accuracy)))
print(my_svm_results %>% filter(Accuracy == max(Accuracy)))

#Confusion matricies
confusionMatrix(my_logistic_reg)
confusionMatrix(my_QDA)
confusionMatrix(my_random_forrest)
confusionMatrix(my_svm)


# Testing -----------------------------------------------------------------
mean(predict(my_random_forrest, data_test) == data_test$Y)

