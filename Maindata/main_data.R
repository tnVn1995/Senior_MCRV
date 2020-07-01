arg=commandArgs(TRUE)
arg<-as.numeric(arg)
i = as.numeric(Sys.getenv("SGE_TASK_ID"))
cat('Starting run i =',i,'\n')




# 1 means no 2 means yes
get_ij <- function(data, cond){
  #' @description compute the count of ij (answer yes for W and no or yes for Y)
  #' @param data matrix data for operation
  #' @param cond int (1 or 2) either yes or no answer
  #' @param ws int number of Ws (e.g: 'ALT_TP31')
  #' @param ys int number of Ys (e.g: '')
  # W always equals 1 but Y can either be 1 or 0
  ij_count <- matrix(0, nrow = 16, ncol = 4) 
  Ws <- 1:16
  Ys <- 1:4
  # Iterate through each value of Ws
  for (i in seq_along(Ws)){
    # Iterate through each value of Ys
    for (j in seq_along(Ys)){
      ij_count[i,j] <-  ij_count[i, j] + dim(subset(data, (data[, 'ALT_TP31']== i & data[, j]==cond)))[1]
      ij_count[i,j] <-  ij_count[i, j] + dim(subset(data, (data[, 'ALT_TP32']== i & data[, j+4]==cond)))[1]
      ij_count[i,j] <-  ij_count[i, j] + dim(subset(data, (data[, 'ALT_TP33']== i & data[, j+8]==cond)))[1]
    }
  }
  return(ij_count)}
# Test case
# cond = 1
# subset(pp_data, (pp_data[, 'ALT_TP31']==1 & pp_data[, 1]==cond)) %>% 
#   dplyr::select(TP1_REC1, ALT_TP31) %>% 
#   dplyr::summarise_all(dplyr::funs(dplyr::n_distinct(.)))

get_i <- function(data){
  #' @description compute the count of ith level for W
  #' @param data matrix data for operation
  i_count <- matrix(0, nrow = 1, ncol = 16)
  for (i in 1:16){
    i_count[, i] <- i_count[, i] + dim(subset(data, data[, 'ALT_TP31'] == i))[1]
    i_count[, i] <- i_count[, i] + dim(subset(data, data[, 'ALT_TP32'] == i))[1]
    i_count[, i] <- i_count[, i] + dim(subset(data, data[, 'ALT_TP33'] == i))[1]
  }
  return(i_count)
}

# # Test case
# get_i(pp_data)
# subset(data, data[, 'ALT_TP31'] == 1) %>% 
#   select(ALT_TP31) %>% 
#   dplyr::n_distinct(.)

get_j <- function(data, cond){
  #' @description compute the count of jth level for Y
  #' @param cond int (1 or 2) either yes or no for value of Y
  j_count <- matrix(0, nrow = 1, ncol = 4)
  for (j in 1:4){
    cols <- c(j, j+4, j+8)
    filtered_data <- subset(data, (data[,j]==cond|data[,j+4]==cond|data[,j+8]==cond))
    for (indi in 1:nrow(filtered_data)){
      if (sum(is.na(filtered_data[indi, cols])) == 2){
        j_count[1, j] <- j_count[1, j] + 1
      }
      else if (sum(is.na(filtered_data[indi, cols])) == 1){
        j_count[1, j] <- j_count[1, j] + sum(filtered_data[indi, cols]==cond, na.rm = TRUE) / 2 
      }
      else {
        j_count[1, j] <- j_count[1, j] + sum(filtered_data[indi, cols]==cond) / 3
      }
    }
  }
  return(j_count)}

# get_j(pp_data, cond = 1)
# 
# # test case
# cond <- 1
# filtered_data <- subset(pp_data, (pp_data[,1]==cond|pp_data[,1+4]==cond|pp_data[,1+8]==cond))
# filtered_data
# cols <- c(1, 1+4, 1+8)
# when there's 1 na value
# assert("Check your code when there's 1 na value ", sum(filtered_data[4, cols]==1, na.rm = TRUE) == 2)
# assert("Check your code when there's 1 na value", sum(is.na(filtered_data[4, cols]==1)) == 1)
# # when there are 2 na values
# assert("Check your code when there are 2 na values ", sum(filtered_data[10, cols]==1, na.rm = TRUE) == 1)
# assert("Check your code when there are 2 na values ", sum(is.na(filtered_data[10, cols]==1))==2)

# Calculate chi-squared statistics
get_stats = function(data, summary=FALSE){
  n = dim(data)[1]
  ij_count1 = get_ij(data = data, cond = 1)
  notij_count1 = get_ij(data = data, cond = 2)
  i_count1 = get_i(data = data)
  yj_count1 = get_j(data=data, cond = 1)
  notj_count1 = get_j(data=data, cond = 2)
  pi1_i. = i_count1 / n
  # the grand total proportion of yeses and nos for the ith, jth and ijth values
  yp1_.j = yj_count1 / n
  np1_.j = notj_count1 / n
  ypi1_ij = ij_count1 / n
  npi1_ij = notij_count1 / n
  yp_grandstat1 = sum((ypi1_ij - t(pi1_i.) %*% yp1_.j)^2 / (t(pi1_i.) %*% yp1_.j))
  np_grandstat1 = sum((npi1_ij - t(pi1_i.) %*% np1_.j)^2 / (t(pi1_i.) %*% np1_.j))
  grandstat1 = (yp_grandstat1 + np_grandstat1) * n
  if (summary == TRUE){
    return (list(grandstat = grandstat1, yp_grandstat = yp_grandstat1, np_grandstat = np_grandstat1))
  }
  else {
    return (grandstat1)
  }
}

data <- read.csv('data.csv')
data <- as.matrix(data)
stat = get_stats(data)

n = dim(data)[1]
end = dim(data)[2]
# Sample from non-nan values for each level (w1,w2,y1,y2)
W = sample(x=1:n,size=n,replace=TRUE)
mm<- rbind(data[which(!is.na(data[,13])),c(1,2,3,4)], data[which(!is.na(data[,14])),c(5,6,7,8)],
           data[which(!is.na(data[,15])),c(9,10,11,12)]) 

Y1 <- sample(1:(dim(mm)[1]),size=n, replace=TRUE)
Y2 <- sample(1:(dim(mm)[1]),size=n,replace=TRUE)
Y3 <- sample(1:(dim(mm)[1]),size=n,replace=TRUE) 

c1 <- mm[Y1,1]
c2 <- mm[Y2,1]
c3 <- mm[Y3,1]
c4 <- mm[Y1,2]
c5 <- mm[Y2,2]
c6 <- mm[Y3,2]
c7 <- mm[Y1,3]
c8 <- mm[Y2,3]
c9 <- mm[Y3,3]
c10 <- mm[Y1,4]
c11 <- mm[Y2,4]
c12 <- mm[Y3,4]
data.star = cbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c12,data[W,c(13:15)])
data <- read.csv('data.csv')
data <- as.matrix(data)
stat = get_stats(data)

n = dim(data)[1]
end = dim(data)[2]
# Sample from non-nan values for each level (w1,w2,y1,y2)
W = sample(x=1:n,size=n,replace=TRUE)
mm<- rbind(data[which(!is.na(data[,13])),c(1,2,3,4)], data[which(!is.na(data[,14])),c(5,6,7,8)],
           data[which(!is.na(data[,15])),c(9,10,11,12)]) 

Y1 <- sample(1:(dim(mm)[1]),size=n, replace=TRUE)
Y2 <- sample(1:(dim(mm)[1]),size=n,replace=TRUE)
Y3 <- sample(1:(dim(mm)[1]),size=n,replace=TRUE) 

c1 <- mm[Y1,1]
c2 <- mm[Y2,1]
c3 <- mm[Y3,1]
c4 <- mm[Y1,2]
c5 <- mm[Y2,2]
c6 <- mm[Y3,2]
c7 <- mm[Y1,3]
c8 <- mm[Y2,3]
c9 <- mm[Y3,3]
c10 <- mm[Y1,4]
c11 <- mm[Y2,4]
c12 <- mm[Y3,4]
data.star = cbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,data[W,c(13:15)])
# If w's = 0, transform that row into nan-values
# data.star[data.star[,'W1']==0, c(3,5)] = NA
data.star[which(is.na(data.star[,'ALT_TP31'])),c(1,2,3,4)] <- NA
data.star[which(is.na(data.star[,'ALT_TP32'])),c(5,6,7,8)] <- NA
data.star[which(is.na(data.star[,'ALT_TP33'])),c(9,10,11,12)] <- NA
# data.star[data.star[,'W2']==0, c(4,6)] = NA
stat.star = get_stats(data.star)

outputName=paste("task-",i,".RData",sep="")
outputPath=file.path("Output",outputName)
save("stat.star",file=outputPath)