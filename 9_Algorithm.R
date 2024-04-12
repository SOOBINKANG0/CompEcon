rm(list=ls())
library(tidyverse); library(data.table)

##procedural programming 1
fib = function(x, x1, x2){
    f = numeric()
    f[1] <- x1; f[2] <-x2
    
    for(i in 3:x){
        f[i] = f[i-1] + f[i-2]
    }
    
    return(f)
}

##procedural programming 2
result = month.name[1]

for(i in 2:length(month.name)){
    result = paste0(result, "+", month.name[i])
}

result

## Functional programming

pasteadd = function(...){
    paste(..., sep = "+")
}

Reduce(pasteadd, month.name)

## evaluate the polynomial
## x^0 + x^1 + x^2+x^3+x^4+...

poly_sum = function(x, x_prime){ #는 x에 들어갈, x_prime은 x^()에 들어감
    
    result = 1
    
    for(i in 1:x_prime){

        result= result +x^i
        
    }
    
    return(result)
}
poly_sum(2,5)

