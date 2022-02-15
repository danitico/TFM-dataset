require(tidyverse)
require(lubridate)

dataset <- read_csv("data/data.csv")

dataset <- dataset %>% mutate(timestamp=as_datetime(timestamp))
dataset <- dataset %>% arrange(user_id, timestamp)
dataset <- dataset %>% mutate(timestamp=date(timestamp))


grouped_dataset <- dataset %>% group_by(user_id, timestamp) %>% select(-rating) %>% nest(items = item_id)

grouped_dataset$timestamp <- NULL

grouped_dataset <- grouped_dataset %>% group_by(user_id) %>% nest(newitems = items)

sequence_database <- grouped_dataset$newitems

spmf_database <- sapply(
    sequence_database,
    function (sequence_item) {
        transaction <- unlist(
            lapply(
                sequence_item[[1]],
                function (itemset) {
                    c(itemset[[1]], -1)
                }
            ),
            use.names = FALSE
        )
        
        transaction <- as.character(transaction)
        freq <- data.frame(table(transaction))
        duplicates <- freq[freq$Freq > 1 & freq$transaction != "-1",]

        if (dim(duplicates)[1] > 0) {
            for (index in 1:dim(duplicates)[1]) {
                for (index1 in 1:duplicates[index,]$Freq) {
                    transaction[
                        which(transaction == duplicates[index,]$transaction)[1]
                    ] <- paste(
                        duplicates[index,]$transaction,
                        index1,
                        sep="_"
                    )
                }
            }
        }
        
        return(paste(transaction, collapse = " "))
    }
)

write.table(
    spmf_database,
    "data2.spmf",
    row.names = F,
    col.names = F,
    quote = F,
    eol = " -2\n"
)
