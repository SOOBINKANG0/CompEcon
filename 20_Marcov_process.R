rm(list=ls())
source("source.R")

##Marcov matrix, P_ij
P = matrix(c(0.5, 0.4, 0.1,
                    0.4, 0.5, 0.1,
                    0.2, 0.2, 0.6), byrow = T, ncol = 3)

P[2,]
psi = t(matrix(c(0.2, 0.3, 0.5), ncol = 1))

## Marcov multiplication
multi_func = function(x, mat, n){
    
    temp <- x
    
    for(i in 1:n){
        temp <- temp %*% mat
    }
    
    return(temp)
}

multi_func(x = arbi_mat, mat = Marcov_mat, n = 100)

## Marcov simulation
draw = function(x){
    sample(1:3, size = 1, prob = psi)
}

marcov_sim = function(psi, n){ 
    
    bin = numeric(n)
    bin[1] = sample(1:3, size = 1, prob = psi)
    
    for(i in 2:n){
        bin[i] <- draw(P[bin[i-1], ])
    }
    
    return(bin)
}

marcov_sim(psi = psi, n = 15)