## This program is just a sample of randomization test
#-> The idea is to use sample to use "choose" to get all the 
#   combination of observation and check how many proportion
#   of result is bigger or equal to the observation result.

# The data we use
y <- c(26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6) #wheat yields
x <- c("B","A","B","A","B","B","B","A","A","A","B","A")             #fertilizer assignment

#g.obs is the observed test statistic
g.obs <- abs(mean(y[x=="B"])-mean(y[x=="A"])) 
g.obs

# randomizaiton test
n.combs <- choose(12,6)           # find out how many possible situation we have
all.combinations <- combn(12,6)   # get all the combination
g.null <- numeric(n.combs)        # create a list to store the result

for(i in 1:n.combs){
  A.indices <- all.combinations[,i]
  # the difference between two group
  g.null[i] <- abs(mean(y[A.indices])-mean(y[-A.indices]))
}
# get p value
mean(g.null>=g.obs)  # the ans is bigger than 0.05, so we fail to reject H0
                     # if our confidence level is 95%
hist(g.null)