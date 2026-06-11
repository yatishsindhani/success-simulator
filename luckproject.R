
set.seed(22)
#luck factor
l <- 0.05

IN = 0:9999999
luck = runif(10000000)
hardwork = runif(10000000)

lucky<- cbind(IN, luck, hardwork)

lucky <- as.data.frame(lucky)

lucky$IN <- lucky$IN + 1 

lucky$success <- (1-l)*lucky$hardwork + l*lucky$luck 

lucky[which.max(lucky$success),]

lucky_sorted<- lucky[order(lucky$success,decreasing = TRUE),]

lucky_sorted$lf <- isTRUE(pmax(lucky_sorted$luck,lucky_sorted$hardwork)==lucky_sorted$luck)
lucky_sorted$badluck <- isTRUE(lucky_sorted$luck<0.5)

length(isTRUE(head(lucky_sorted,100000)[,6] == "TRUE"))
length(isTRUE(head(lucky_sorted,100000)[,5] == "TRUE"))
