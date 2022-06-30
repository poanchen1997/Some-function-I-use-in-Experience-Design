# This function is use to create a latin square
latin <- function(n, nrand = 20){
  x = matrix(LETTERS[1:n], n, n) # use "LETTERS" to get the alphabet 
                                 # and then repeat it for n times to be a n X n matrix
                                 # matrix("data", "nrow", "ncol")
  x = t(x)                       # transpose it
  
  # rotate the row
  for (i in 2:n){
    x[i, ] = x[i, c(i:n, 1:(i - 1))]
  }
  # randomize the row and column to make sure the latin square we create
  # is different.
  if (nrand > 0) {
    for (i in 1:nrand) {
      x = x[sample(n), ]
      x = x[, sample(n)]
    }
  }
  x
}

# test result
latin(5)
