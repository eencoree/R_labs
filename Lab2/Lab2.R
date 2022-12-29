data <- read.csv("Payment_and_value_of_Care-Hospital.csv", header = TRUE)
names(data)
data$Payment
data$Payment.Category
# очищаем от больницы без оценок и несостоятельных ячеек
clean_data <- subset(data, data$Payment != "Not Available")

#убираем знак доллара и запятые из чисел
clean_data$Payment <- as.numeric(gsub("[$,]","",clean_data$Payment))
clean_data$Lower.estimate <- as.numeric(gsub("[$,]", "", clean_data$Lower.estimate))
clean_data$Higher.estimate <- as.numeric(gsub("[$,]", "", clean_data$Lower.estimate))
head(clean_data$Payment)

# возможные услуги 
services <- unique(clean_data$Value.of.Care.Display.Name)
i = 1
hosps <- list()
for (elem in services){
  illness <- subset(clean_data, clean_data$Value.of.Care.Display.Name == elem)
  rec <- subset(illness, illness$Payment == min(illness$Payment))
  hosps[[i]] <- subset(rec, select=c("Facility.Name", "State", "County.Name", "City", "Payment"))
  cat("Показатели для ", elem,"\n")
  print(hosps[i])
  i <- i + 1
}
#______________________________________________________________________________________________________
data_2 = read.csv("data/RH_T.csv", header = TRUE)
names(data_2)
day_month = list()
i = 1
for (elem in unique(data_2$MM)){
  cnt = length(which(data_2$MM == elem))
  day_month[[i]] <- cnt
  i <- i + 1
}
day_month[[1]] <- day_month[[1]] - 1
print(day_month)
i = 4 # первый выходной
j = 1 # номер месяца
gl_min <- 100
x <- 0
y <- 0
while (j!=13){
  temp_1 <- subset(data_2, (data_2$MM == j) & (data_2$DD == i))$T2M
  if (i+1 > day_month[[j]]){
    j <- j + 1
    i <- 0
  }
  temp_2 <- subset(data_2, (data_2$MM == j) & (data_2$DD == i+1))$T2M
  minimum = temp_1+temp_2
  if (minimum < gl_min){
    gl_min <- minimum
    x <- i
    y <- j
  }
  
  i <- i + 7
  if (i > day_month[[j]]){
    i <- i - day_month[[j]]
    j <- j + 1
  }
}

min_row <- subset(data_2, (data_2$DD == x | data_2$DD == x+1) & data_2$MM == y)
print(min_row)
