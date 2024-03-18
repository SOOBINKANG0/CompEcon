# Monte_carlo simulation
rm(list=ls())

#1. 원의 넓이 구하기
pi_func = function(n){
    
    bin = numeric(n)
    
    for(i in 1:n){
        
        x = runif(1, min = -1, max = 1)
        y = runif(1, min = -1, max = 1)
        
        temp = (x^2)+(y^2)
        
        if(temp <= 1){
            bin[i] <- 1
        } else {
            bin[i] <- 0
        }
        
    }
    
    return(4*(1/n)*sum(bin))
}

pi_func(10000)

## 2. 몬테카를로 적분
## grid method without generating random number with grid btw 1 and 3
obj_func = function(x){
    return(2+cos(x + sin(atan(x)) + 3))
}


x_grid = seq(from = 1, to = 3, by = 0.0001)
temp = numeric()

for(i in 1:length(x_grid)){
    temp[i] <- obj_func(x_grid[i])
}

print(1/length(x_grid) * sum(temp)) # 2.731847