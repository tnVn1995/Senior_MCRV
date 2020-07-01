arg=commandArgs(TRUE)
arg<-as.numeric(arg)
i = as.numeric(Sys.getenv("SGE_TASK_ID"))
cat('Starting run i =',i,'\n')
requiredPackages = c('foreach','doMC','data.table')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p, repos="http://cran.us.r-project.org")
  library(p,character.only = TRUE)
}


#--- Generating simulated data

pi_R = c(0.4,0.6)
fun = function(p)((p/(0.4-p))/((0.5-p)/(0.1+p)) - 2)
pw.11 = uniroot(fun, lower=0.0, upper = 1.0)$root
pw.10 = 0.4 - pw.11
pw.01 = 0.5 - pw.11
pw.00 = 0.1 + pw.11
pw.11 + pw.10 + pw.01 + pw.00

# Generate marginal probabilities for Y_i
pi_C = c(0.2, 0.3)
py.11 = 0.085
py.10 = 0.2 - py.11
py.01 = pi_C[2] - py.11
py.00 = 0.5 + py.11
py.11 + py.00 + py.10 + py.01
# shuffle W - Alternative therapies probabilities
# orderw = cbind(c(00,10,11,01),c(pw.00,pw.10,pw.11,pw.01))
orderw = cbind(c(01,00,10,11),c(pw.01,pw.00,pw.10,pw.11))
# orderw1 = orderw[sample(nrow(orderw)),]
orderw2 = cbind(orderw, cumsum(orderw[,2]))

# shuffle Y - Recommendation sources probabilities
# ordery = cbind(c(00,10,11,01),c(py.00,py.10,py.11,py.01))
ordery = cbind(c(01,00,10,11),c(py.01,py.00,py.10,py.11))
# ordery1 = ordery[sample(nrow(ordery)),]
ordery2 = cbind(ordery, cumsum(ordery[,2]))


#-------------------------------------
# Simulate data using computed marginal probabilities

generate_y = function(proby, data, Uy, row, pos1){
  #' @description Generate simulated data for the Y variable using specified probabilities
  #' @param proby matrix. A 1x3 matrix containing cumulative probability and its corresponding cluster
  #' @param data matrix. A 1x6 empty matrix to store data simulated for the Y variables
  #' @param Uy decimal. A decimal number randomly generated from a uniform distribution.
  #' @param pos1 integer. Integer numbers specifying columns to fill in data
  #' @param row integer. Interger number specifying the row to fill in data
  #' @usage generate_y(proby, data, Uy)
  #' @return a 500x6 matrix that contains simulated data from Y
  if (Uy >= proby[1,3] & Uy < proby[2,3]){
    data[row,pos1] = 0
    data[row, pos1+1] = 0
  } else if (Uy >= proby[2,3] & Uy < proby[3,3]){
    data[row, pos1] = 1
    data[row, pos1+1] = 0
  } else if (Uy >= proby[3,3] & Uy < proby[4,3]){
    data[row, pos1] = 1
    data[row, pos1+1] = 1
  } else {
    data[row, pos1] = 0
    data[row, pos1+1] = 1
  }
  return(data)
}
generate_cluster = function(probw,proby, Uw, Uy1, Uy2, i, pos1, pos2, row, data=NULL){
  #' @description Generate simulated data for W, Y using spcified probabilities
  #' @param probw,proby matrix. A 1x3 matrix containing cumulative probability and its corresponding cluster
  #' @param data matrix. A 500x6 empty matrix to store simulated data
  #' @param Uw,Uy1,Uy2 float. A float number generated from a uniform distribution
  #' @param pos1,pos2 integer. Numbers specifying the columns to fill in the data with
  #' @param i integer. A number specifying the row to fill in the data with
  #' @usage generated_cluster(prob, data, U)
  #' @return a 500x6 matrix that contains simulated data
  if(is.null(data)){
    data = matrix(NA, nrow=500, ncol=6)
    colnames(data) = c('W1','W2','Y11','Y12','Y21','Y22')
  }
  if (Uw >= probw[1,3] & Uw < probw[2,3]){
    data[i,1] = 0
    data[i,2] = 0
    data[i,3:6] = NA
  }
  else if (Uw >= probw[2,3] & Uw < probw[3,3]){
    data[i, 1] = 1
    data[i, 2] = 0
    data = generate_y(proby, data, Uy1,row=i, pos1)
  }
  else if (Uw >= probw[3,3] & Uw < probw[4,3]){
    data[i, 1] = 1
    data[i, 2] = 1
    data = generate_y(proby, data, Uy1, row=i, pos1)
    data = generate_y(proby, data, Uy2, row=i, pos2)
  }
  else {
    data[i,1] = 0
    data[i,2] = 1
    data = generate_y(proby, data, Uy2, row=i, pos2)
  }
  return (data)
}

# Simulating Data for Experiment
simulate_data = function(filepath, n=500){
  #' @description Simulating and Saving data to filepath
  #' @filepath character Path to save simulated data
  count = 1
  Uw = runif(1, 0, 1)
  Uy1 = runif(1, 0, 1)
  Uy2 = runif(1, 0, 1)
  simulated_data = generate_cluster(probw = orderw2, proby = ordery2, Uw = Uw, Uy1=Uy1, Uy2=Uy2, i= count, pos1=3, pos2=5)
  while (count < n){
    Uw = runif(1, 0, 1)
    Uy1 = runif(1, 0, 1)
    Uy2 = runif(1, 0, 1)
    simulated_data = generate_cluster(probw = orderw2, proby = ordery2, Uw = Uw, Uy1=Uy1, Uy2=Uy2, i= count+1, pos1=3, pos2=5, data=simulated_data)
    count=count+1
  }
  # Change the order of columns to reflect the true order of the data due to the way Y's were generated
  simulated_data[,c(1,2,3,4,5,6)] = simulated_data[,c(1,2,3,5,4,6)]
  # Save simulated data to filepath
  write.csv(simulated_data, file = filepath, row.names = FALSE)
  return (simulated_data)
}

# Execute create simulated data and save to filepath
no = i
filepath = paste('./', no,'.csv',sep='')
print(filepath)

# Read in generate d data
simulated_data = simulate_data(filepath = filepath)

#--- Helper functions
get_ij = function(data, cond){
  #' @description Calculate the count of ij
  ij_count = matrix(0, nrow=2, ncol=2)
  # W always equals 1 but Y can either be 1 or 0 
  # This leads to a combination of 10, 11 for either W.
  ij_count[1,1] = dim(subset(data,(data[,1]==1 & data[,3]==cond)))[1]
  ij_count[1,2] = dim(subset(data,(data[,1]==1 & data[,5]==cond)))[1]
  ij_count[2,1] = dim(subset(data,(data[,2]==1 & data[,4]==cond)))[1]
  ij_count[2,2] = dim(subset(data,(data[,2]==1 & data[,6]==cond)))[1]
  return (ij_count)
}
get_i = function(data){
  #' @description Calculate the count of i
  i_count = matrix(0, nrow = 1,ncol = 2)
  i_count[,1] = dim(data[data[,1]==1,])[1]
  i_count[,2] = dim(data[data[,2]==1,])[1]
  return (i_count)
}

get_j = function(data, cols, cond, j, j_count = NULL){
  #' @description Calculate the count of j
  #' @data dataframe data to be processed
  #' @cols vector denotes the columns to process
  #' @cond int 1 or 0. Denotes whether the observation specify yes (1) or no (0)
  #' @j 
  #' @j_count matrix matrix denoting the count of j
  
  # Filtering dataframe, containing rows that satisfy cond
  # for either col[1] or col[2] or both
  test_data = subset(data, (data[,cols[1]]==cond | data[,cols[2]]==cond))
  # If j_count is null, create a new matrix to store count of j
  if (is.null(j_count)){
    j_count = matrix(0, nrow = 1, ncol = 2)
  }
  for (indi in 1:nrow(test_data)){
    # When either Y1 or Y2 is available,
    # add 1 to j_count
    if (is.na(sum(test_data[indi,cols]))){
      j_count[1,j] = j_count[1,j] + 1
    }
    else {
      # When both are available,
      # add the proportion that satisfy the condition 
      j_count[1,j] = j_count[1,j] + sum(test_data[indi,cols]==cond) / 2
    }
  }
  return (j_count)
}
# Calculate chi-squared statistics
get_stats = function(data, sequence=FALSE){
  n = dim(data)[1]
  ij_count1 = get_ij(data=data, cond= 1)
  notij_count1 = get_ij(data=data, cond= 0)
  i_count1 = get_i(data=data)
  yj_count1 = get_j(data=data, cols=c(5,6), cond=1, j=2)
  yj_count1 = get_j(data=data, cols=c(3,4), cond=1, j=1, j_count=yj_count1)
  notj_count1 = get_j(data=data, cols=c(5,6), cond=0, j=2)
  notj_count1 = get_j(data=data, cols=c(3,4), cond=0, j=1, j_count=notj_count1)
  pi1_i. = i_count1 / n
  # the grand total proportion of yeses and nos for the ith, jth and ijth values
  yp1_.j = yj_count1 / n
  np1_.j = notj_count1 / n
  ypi1_ij = ij_count1 / n
  npi1_ij = notij_count1 / n
  yp_grandstat1 = sum((ypi1_ij - t(pi1_i.) %*% yp1_.j)^2 / (t(pi1_i.) %*% yp1_.j))
  np_grandstat1 = sum((npi1_ij - t(pi1_i.) %*% np1_.j)^2 / (t(pi1_i.) %*% np1_.j))
  grandstat1 = (yp_grandstat1 + np_grandstat1) * n
  if (sequence==TRUE){
    return (list(grandstat = grandstat1, yp_grandstat = yp_grandstat1, np_grandstat = np_grandstat1))
  }
  else {
    return (grandstat1)
  }
}
#---

# Parallel computing
# There are 36 cores in each node in the HPCC, 
# running parallel on all 36 cores,
# executing the experiment on about 250 nodes concurrently (250 tasks with 36 cores each)
cores = detectCores()
registerDoMC(cores)

# Using a copy version of generated data
data = simulated_data
data <- read.csv('data.csv')
data <- as.matrix(data)
paste('The number of cores used is', getDoParWorkers())



X = 2

# Bootstrapping for 1999 times
trials <- 1999
stat = get_stats(data)
ptime = system.time({r <- foreach(icount(trials), .combine=rbind) %dopar% {
  n = dim(data)[1]
  end = dim(data)[2]
  W = sample(x=1:n,size=n,replace=TRUE)
  # Sample from non-nan values for each level (w1,w2,y1,y2)
  mm<- rbind(data[which(!is.na(data[,3])),c(3,5)], data[which(!is.na(data[,4])),c(4,6)]) 
  
  Y3 = sample(1:(dim(mm)[1]),size=n,replace=TRUE)
  Y4 = sample(1:(dim(mm)[1]),size=n,replace=TRUE) 
  
  
  c3 = mm[Y3,1]
  c4 = mm[Y4,1]
  c5 = mm[Y3,2]
  c6 = mm[Y4,2]
  data.star = cbind(data[W,1:X],c3,c4,c5,c6)
  # If w's = 0, transform that row into nan-values
  data.star[data.star[,'W1']==0, c(3,5)] = NA
  data.star[data.star[,'W2']==0, c(4,6)] = NA
  stat.star = get_stats(data.star)
  
}})

# save p-value in a list
results = list(task = i, p_value = mean(r >= stat), test_stat = stat, bootstrap_teststat=r)

# Save output for later report
outputName=paste("task-",i,".RData",sep="")
outputPath=file.path("Output",outputName)
save("results",file=outputPath)