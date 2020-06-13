# Importing R libraries
library(data.table)           
library(readxl)               
library(tidyverse)
library(lubridate)
library(skimr)                
library(knitr)                
library(treemap)
library(recommenderlab)
library(pROC)

setwd("C:/Users/abhin/OneDrive - Harrisburg University/ANLY 699 - Applied Project/Recommender System Example Code")
retail = read_excel("Invoice_python_fulldata_2items.xlsx")
retail %>%  skim()

sapply(retail[ ,c('InvoiceNo','CustomerID')], function(x) length(unique(x)))

retail = retail %>%
  # Setting 'Description' and 'Country' as factors
  mutate(Description = as.factor(Description)) %>%
  mutate(Country = as.factor(Country)) %>% 
  # Changing 'InvoiceNo' type to numeric
  mutate(CustomerID = as.numeric(CustomerID)) %>% 
  # Extracting 'Date' and 'Time' from 'InvoiceDate'
  mutate(Date = as.Date(InvoiceDate)) %>% 
  mutate(Time = as.factor(format(InvoiceDate,"%H:%M:%S")))
glimpse(retail)


# Dataset Exploration ####

retail %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(Description, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
  coord_flip() +
  theme_grey(base_size = 12)
# Other-Portable is the most sold item

retail %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  mutate(pct=(count/sum(count))*100) %>% 
  arrange(desc(pct)) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)
# Top 10 items are 65% of all sales orders

retail %>% 
  ggplot(aes(wday(Date, 
                  week_start = getOption("lubridate.week.start", 1)))) + 
  geom_histogram(stat = "count" , fill = "forest green", colour = "dark green") +
  labs(x = "Day of Week", y = "") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                     labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_grey(base_size = 14)
# Similar sales on all days except for weekends as expected

treemap(retail,
        index      = c("Country"),
        vSize      = "Quantity",
        title      = "",
        palette    = "Set2",
        border.col = "grey40")
# MY has the bulk of orders


# Recommendation System ####

retail_subset <- retail %>% 
  # Create unique identifier
  mutate(InNo_Desc = paste(CustomerID, Description, sep = ' ')) 
# Filter out duplicates and drop unique identifier
retail_subset <- retail_subset[!duplicated(retail_subset$InNo_Desc), ]
retail_subset = retail_subset[,-11]

ratings_matrix <- retail_subset %>%
  # Select only needed variables
  select(CustomerID, Description) %>% 
  # Add a column of 1s
  mutate(value = 1) %>%
  # Spread into user-item format
  spread(Description, value, fill = 0) %>%
  select(-CustomerID) %>%
  # Convert to matrix
  as.matrix() %>%
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")
ratings_matrix
## 15004 x 103 rating matrix of class 'binaryRatingMatrix' with 21492 ratings.

scheme <- ratings_matrix %>% 
  evaluationScheme(method = "cross",k = 5, train = 0.8,  given = -1)
scheme
## Evaluation scheme using all-but-1 items
## Method: 'cross-validation' with 5 run(s).
## Good ratings: NA
## Data set: 24412 x 104 rating matrix of class 'binaryRatingMatrix' with 517354 ratings.

algorithms <- list(
  "association rules" = list(name  = "AR", 
                             param = list(supp = 0.01, conf = 0.01)),
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 5)),
  "user-based CF"     = list(name  = "UBCF", 
                             param = list(method = "Cosine", nn = 500))
)

results <- recommenderlab::evaluate(scheme, 
                                    algorithms, 
                                    type  = "topNList", 
                                    n     = c(1:103)
)

results

# Visualise the Results ####

# Pull into a list all confusion matrix information for one model 
tmp <- results$`user-based CF` %>%
  getConfusionMatrix()  %>%  
  as.list() 
# Calculate average value of 5 cross-validation rounds 
as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
  # Add a column to mark the number of recommendations calculated
  mutate(n = c(1:103)) %>%
  # Select only columns needed and sorting out order 
  select('n', 'precision', 'recall', 'TPR', 'FPR')

avg_conf_matr <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1:103)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}

# Using map() to iterate function across all models
results_tbl <- results %>%
  map(avg_conf_matr) %>% 
  # Turning into an unnested tibble
  enframe() %>%
  # Unnesting to have all variables on same level
  unnest()
results_tbl

results_tbl %>%
  ggplot(aes(FPR, TPR, 
             colour = fct_reorder2(as.factor(name), 
                                   FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves", colour = "Model") +
  theme_grey(base_size = 14) + ylab("True Positive Rate") + xlab("False Positive Rate")

results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  
                                   precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves", colour = "Model") +
  theme_grey(base_size = 14) + xlab("Recall") + ylab("Precision")

write.csv(results_tbl,"results_tbl_103items.csv", row.names = FALSE)

# Predictions for a New Customer ####

customer_order <- c("116-liter","58-liter","34-liter","103-liter","106-liter")
new_order_rat_matrx <- retail %>% 
  # Select item descriptions from retail dataset
  select(Description) %>% 
  unique() %>% 
  # Add a 'value' column with 1's for customer order items
  mutate(value = as.numeric(Description %in% customer_order)) %>% 
  # Spread into sparse matrix format
  spread(key = Description, value = value) %>% 
  # Change to a matrix
  as.matrix() %>% 
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")


recomm <- Recommender(getData(scheme, 'train'), 
                      method = "UBCF",  
                      param = list(k = 5))
recomm

pred <- predict(recomm, newdata = new_order_rat_matrx, n = 5)
as(pred, 'list')


# Making R-Shiny Web App for Recommendation ####

glimpse(retail)

past_orders_matrix <- retail %>%
  # Select only needed variables
  select(InvoiceNo, Description) %>% 
  # Add a column of 1s
  mutate(value = 1) %>%
  # Spread into user-item format
  spread(Description, value, fill = 0) %>%
  select(-InvoiceNo) %>% 
  # Convert to matrix
  as.matrix() %>% 
  # Convert to class "dgCMatrix"
  as("dgCMatrix")

# Creating a unique items list
item_list <- retail %>% 
  select(Description) %>% 
  unique()

saveRDS(past_orders_matrix, file = "past_orders_matrix.rds")

# put in a matrix format
new_order <- item_list %>%
  # Add a 'value' column with 1's for customer order items
  mutate(value = as.numeric(Description %in% customer_order)) %>%
  # Spread into sparse matrix format
  spread(key = Description, value = value) %>%
  # Change to a matrix
  as.matrix() %>% 
  # Convert to class "dgCMatrix"
  as("dgCMatrix")


all_orders_dgc <- t(rbind(new_order,past_orders_matrix))


# Calculating AUC ####
results_tbl_ar = read_excel("results_tbl_103items_ar.xlsx")
results_tbl_ar = transform(results_tbl_ar, dFPR = c(diff(FPR), 0), dTPR = c(diff(TPR), 0))
simple_auc = function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}
with(results_tbl_ar, simple_auc(TPR, FPR))

results_tbl_random = read_excel("results_tbl_103items_random.xlsx")
results_tbl_random = transform(results_tbl_random, dFPR = c(diff(FPR), 0), dTPR = c(diff(TPR), 0))
simple_auc = function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}
with(results_tbl_random, simple_auc(TPR, FPR))

results_tbl_popular = read_excel("results_tbl_103items_popular.xlsx")
results_tbl_popular = transform(results_tbl_popular, dFPR = c(diff(FPR), 0), dTPR = c(diff(TPR), 0))
simple_auc = function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}
with(results_tbl_popular, simple_auc(TPR, FPR))

results_tbl_ibcf = read_excel("results_tbl_103items_ibcf.xlsx")
results_tbl_ibcf = transform(results_tbl_ibcf, dFPR = c(diff(FPR), 0), dTPR = c(diff(TPR), 0))
simple_auc = function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}
with(results_tbl_ibcf, simple_auc(TPR, FPR))

results_tbl_ubcf = read_excel("results_tbl_103items_ubcf.xlsx")
results_tbl_ubcf = transform(results_tbl_ubcf, dFPR = c(diff(FPR), 0), dTPR = c(diff(TPR), 0))
simple_auc = function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}
with(results_tbl_ubcf, simple_auc(TPR, FPR))


rectangle <- function(x, y, width, height, density=12, angle=-45, ...) 
  polygon(c(x,x,x+width,x+width), c(y,y+height,y+height,y), 
          density=density, angle=angle, ...)

plot(0:10/10, 0:10/10, type='n', xlab="False Positive Rate", ylab="True Positive Rate")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")
with(results_tbl_ubcf, {
  mapply(rectangle, x=FPR, y=0,   
         width=dFPR, height=TPR, col="green", lwd=2)
  mapply(rectangle, x=FPR, y=TPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  
  lines(FPR, TPR, type='b', lwd=3, col="red")
})







