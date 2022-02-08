require(tidyverse)
require(lubridate)

dataset <- read_csv("data/data.csv")

dataset <- dataset %>% mutate(timestamp=as_datetime(timestamp))
dataset <- dataset %>% arrange(user_id, timestamp)
dataset <- dataset %>% mutate(timestamp=date(timestamp))


meow <- dataset %>% group_by(user_id, timestamp) %>% select(-rating) %>% nest(items = item_id)

meow$timestamp <- NULL

meow <- meow %>% group_by(user_id) %>% nest(newitems = items)

sequence_database <- meow$newitems

spmf_database <- sapply(
    sequence_database,
    function (sequence_item) {
        paste(
            sapply(
                sequence_item[[1]],
                function (itemset) {
                    paste(itemset[[1]], collapse = " ")
                }
            ),
            collapse = " -1 "
        )
    }
)

write.table(
    spmf_database,
    "data.spmf",
    row.names = F,
    col.names = F,
    quote = F,
    eol = " -1 -2\n"
)
