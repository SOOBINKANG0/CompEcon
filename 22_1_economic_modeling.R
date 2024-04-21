## Economic modeling exercise
rm(list=ls())

## difficulty: 1/10
## 1 period consumption-saving model

## max U(c) = (c^(1-rho)-1)/(1-rho)
## s.t. Y = C+S
## Y = 100, rho= 2
## by hand, solution is C* = 0, S* = 100

U <- function(C, rho){
    if (rho == 1) {
        return(log(C))  # 로그 유틸리티 처리
    } else {
        return((C^(1-rho) - 1) / (1-rho))  # 수정된 부분
    }
}

my_func <- function(Y, rho){
    
    f <- function(C) {
        return(-U(C, rho))  # 유틸리티 함수의 부호를 반전
    }
    
    result <- optimize(f = f, interval = c(0, Y), maximum = TRUE)
    print(result)
}

my_func(Y = 100, rho = 2)

# $maximum
# [1] 4.627768e-05 # very small amount
# 
# $objective
# [1] 21607.69


## difficulty: 2/10

## 2 Period consumpion saving problem
##  Max U u(c0)+beta*U(c1)
## s.t. Y1 = c1+s1, c2 = s1(1+r) + Y2
r = 0.1; rho = 2; beta = 0.9
y1 = 1000; y2= 100

C1_grid = seq(from = 0, to = 1000, 0.1)
C2_grid = (y1-C1_grid)*(1+r) +y2

Utility2 <- function(C1, C2, rho){
    if (rho == 1) {
        return(log(C1)+beta*log(C2))  # 로그 유틸리티 처리
    } else {
        return((C1^(1-rho) - 1) / (1-rho)+beta*((C2^(1-rho) - 1) / (1-rho)))  # 수정된 부분
    }
}

bin = numeric(length(C1_grid))

for(i in 1:length(C1_grid)){
    bin[i] <- Utility2(C1_grid[i], C2_grid[i], rho = 2)
}

res = which.max(bin)
result = list(C1 = C1_grid[res], C2 = C2_grid[res], 
              Util = Utility2(C1_grid[res], C2_grid[res], rho))
print(result)
# $C1
# [1] 572.8
# 
# $C2
# [1] 569.92
# 
# $Util
# [1] 1.896675

## 2) optim
Utility <- function(c) {
    c1 <- c
    s1 <- y1 - c1
    c2 <- s1 * (1 + r) + y2
    
    if (rho == 1) {
        return(-(log(c1) + beta * log(c2)))  # 로그 유틸리티, 최대화를 위해 부호 반전
    } else {
        return(-((c1^(1-rho) - 1) / (1-rho) + beta * ((c2^(1-rho) - 1) / (1-rho))))  # 일반 유틸리티, 최대화를 위해 부호 반전
    }
}

result_optim <- optim(par = c(500), fn = Utility, method = "Brent", lower = 0, upper = y1)
print(list(result_optim$par, -result_optim$value))

## production problem
## Max profit 10*Q - 10L-15W, Q=1.5*L^0.5*K^0.5 s.t. 10L+15K=1000
product = function(L, K){
    L = L
    K = (1000-10*L)/15
    
    Q = 1.5*L^(0.5)*K^(0.5)
    profit = 10*Q - 10*L -15*K
    
    return(profit)
}


optimize(f = product, maximum = TRUE, c(0, 100))