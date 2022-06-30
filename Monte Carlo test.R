# This program is about the Monte Carlo approach for randomization test
# 1. Why we want to use this approach?
# -> if the sample is too large, the combination of sample we become even more large
#    so we instead using a number that we want to reduce the computing time.

# same thing as we use in "randomization test.R"
# The data we use
y <- c(26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6) #wheat yields
x <- c("B","A","B","A","B","B","B","A","A","A","B","A")             #fertilizer assignment

# g.obs is the observed test statistic
g.obs <- abs(mean(y[x=="B"])-mean(y[x=="A"])) 
g.obs

# Monte Carlo approach
S <- 1e4             # how many time we want to check
set.seed(1)          # for reproducibility
g.null <- numeric(S) # create a list to store results

for(s in 1:S){
  xsim <- sample(x)  # sample without replacement == rearrange the order
  g.null[s] <- abs(mean(y[xsim=="B"])-mean(y[xsim=="A"]))
}
# get p-value
mean(g.null>=g.obs)  # almost have the same result with randomzation test
                     # still fail to reject H0
hist(g.null)