library(foreign)
library(tidyverse)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(testit) # python-like assert statement
file_path = 'C:/Users/tnguy/OneDrive - Texas Tech University/Summer_Project/2012 NHIS Adult Sample_CAM_HIT paper 2017-2-17.sav'
###Load in dataset
data = read.spss(file_path, to.data.frame=TRUE)


### ALT data
###------
# top 3 alternative therapies
ALT = data %>% select('ALT_TP31', 'ALT_TP32', 'ALT_TP33')


### TP_REC
### ----
# top 3 alternative therapies; recommended by ...
Tp_REC = data %>% select('TP1_REC1', 'TP1_REC2', 'TP1_REC3', 'TP1_REC4')



### 1_INFF
### -----
TP_INF = data %>% select(TP1_INF1, TP1_INF2, TP1_INF3, TP1_INF4, TP1_INF5, TP1_INF6)




###----

### Select 2 factors of interest which are ALT_TP and TP_REC
ALT_Tp = data %>% select('TP1_REC1', 'TP1_REC2', 'TP1_REC3', 'TP1_REC4','TP2_REC1', 'TP2_REC2', 'TP2_REC3', 'TP2_REC4',
                         'TP3_REC1', 'TP3_REC2', 'TP3_REC3', 'TP3_REC4','ALT_TP31', 'ALT_TP32', 'ALT_TP33')

### Handling missing data
##----- 
#Drop NA from the data
ATL_dropna = na.omit(ALT_Tp)



## Catergorical Encoding
## ----
for (i in colnames(ATL_dropna)){
  ATL_dropna[i] = as.numeric(unlist(ATL_dropna[i]))
}

### Calculate chi
# 1 = Yes; 2 = No;
cal_X_squ = function(data){
# The number of observations in the data
n = dim(data)[1]
# Matrices containing relative frequencies of yeses and nos count for each ith rec
yp_i = matrix(0, nrow=1,ncol=4)
np_i = matrix(0, nrow=1, ncol=4)
# Matrices containing count and relative frequencies of individuals who pick jth therapy
j_count=matrix(0,nrow=4,ncol=16)
p_j=matrix(0,nrow=1,ncol=16)
# Matrices containing counts and relative frequencies of individuals who pick jth therapy and ith rec
ij_count = matrix(0, nrow=4,ncol=16)
p_ij = matrix(0, nrow=4,ncol=16)
# Test statistics
ygrand_stats = 0
ngrand_stats = 0
# Iterate through each ith reccommendation
for (i in 1:4){
  # Initialize each ith's test stat with 0
  yes_stats =0
  no_stats = 0
  # Initialize each ith rec's yes and no count with zeros
  nn_i = 0
  yn_i = 0
  # Iterate through each observation to calculate test-statitstic
  for (indi in c(1:n)){
    # Need to refine missing data later
    # For now the denominator will equal 3
    ### The denominator varies according to the number of top choices each individual specifies
    yni_denominator = 3 #FOr this dataset, the denominator = 3
    # calculate the number of yes corresponding to the ith value of TP_REC for each individual
    yni_numerator=sum(c(i,i+4,i+8) %in% which(data[indi,1:12]==1))
    nni_numerator=sum(c(i,i+4,i+8) %in% which(data[indi,1:12]==2))
    # Calculate the proportion of yes and nos corresponding to the ith value of TP_REC for individuals in the data
    yn_i = yn_i + (yni_numerator / yni_denominator)
    nn_i = nn_i + (nni_numerator / yni_denominator)
    assert("Maximum proportion value must be less than or equal to 1", yni_numerator <= yni_denominator)
    # Iterate through the each jth therapy to compute each jth's count of individuals who pick yes
    for (j in data[indi,13:15]){
        j_count[i,j] = j_count[i,j] + 1
        # Calculate the frequency of individuals who choose ith rec and jth therapy
        if (sum(c(i,i+4,i+8) %in% which(data[indi,1:12]==1)) > 0){
          ij_count[i,j] = ij_count[i,j] + 1
        }
      }
    
  }
  # the grand total proportion of yeses and nos for the ith, jth and ijth values
  yp_i[1,i] = yn_i / n
  np_i[1,i] = 1 - (yn_i / n)
  # Relative frequencies of individuals who pick jth therapy and who pick jth therapy and ith rec
  p_j = j_count[1,] / n
  p_ij[i,] = ij_count[i,] / n
  # Calculate chisquare score 
  for (therap in (1:16)){
    yes_stats = yes_stats + (p_ij[i,j] - yp_i[1,i]*p_j[j])^2 / (yp_i[1,i]*p_j[j])
    no_stats = no_stats + (p_ij[i,j] - np_i[1,i]*p_j[j])^2 / (np_i[1,i]*p_j[j])
  }
  # The final results for X_M,1,1) and X_M,1,0)
  ygrand_stats = (ygrand_stats + yes_stats)
  ngrand_stats = (ngrand_stats + no_stats)
}
# return X_S
return(sum(n*ygrand_stats,n*ngrand_stats))
}
#Bootstrap method to calculate p_value (resample 1999 times)
cal_p_boot = function(data, X, B.max=1999, FUN=cal_X_squ){
  stat = FUN(data=data)
  X.sq.S.star = numeric(length(B.max))
  counter = 0
  b = 0
  while(b <= B.max){
    b = b+1
    n = dim(data)[1]
    end = dim(data)[2]
    W = sample(x=1:n,size=n,replace=TRUE)
    Y = sample(x=1:n,size=n,replace = TRUE)
    data.star = cbind(data[W,1:X],data[Y,(X+1):end])
    stat.star = FUN(data.star)
    
    counter = counter + 1
    X.sq.S.star[counter] = stat.star
  }
  p.value.boot = mean(X.sq.S.star >= stat)
  return(list(p_value=p.value.boot, X.sq=X.sq.S.star))
}

# Calculate p-value
cal_p_boot(ATL_dropna, X=12)



