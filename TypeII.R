# Loading dependencies

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

# Generate marginal probabilities for Y_i, W1
piw1_C = c(0.2, 0.3)
pyw1.11 = 0.085
pyw1.10 = 0.2 - pyw1.11
pyw1.01 = piw1_C[2] - pyw1.11
pyw1.00 = 0.5 + pyw1.11
pyw1.11 + pyw1.00 + pyw1.10 + pyw1.01

# Generate marginal probabilities for Y)i, W2

piw2_C = c(0.2, 0.8)

pyw2.11 = 0.1754
pyw2.10 = 0.2 - pyw2.11
pyw2.01 = piw2_C[2] - pyw2.11
pyw2.00 = pyw2.11
pyw2.11 + pyw2.00 + pyw2.10 + pyw2.01



