#---------------------
#  Задание 1
#---------------------
data <- read.csv("lab1_e1.csv", header = TRUE)
sapply(data, typeof)

fix_data <- function(some_data){
  for (i in names(some_data)){
    if(length(grep('[a-z]', some_data[[i]], ignore.case = T))==0)
      some_data[[i]] <- as.double(gsub(" ", "", some_data[[i]]))
  }
  return (some_data)
}

data <- fix_data(data)
print(data)
sapply(data, typeof)

#---------------------
#  Задание 2
#---------------------

load('data/lab1_e2.Rdata')
print(all_data)

get_id <- function(df) {
  new_df <- do.call("rbind", df)
  mean_temp <- setNames(aggregate(temp ~ id, new_df, mean), c("id", "mean_temp"))
  id_count <- setNames(aggregate(temp ~ id, new_df, length), c("id", "count"))
  new_df <- merge(mean_temp, id_count, by = 'id')
  result <- subset(new_df, count == 7, select = c('id', 'mean_temp'))
  row.names(result) <- c(1:nrow(result))
  return (result)
}

data_e2 <- get_id(all_data)
print(data_e2)