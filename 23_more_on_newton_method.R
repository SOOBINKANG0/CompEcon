rm(list=ls())

## 1. Multiple solutions
## 2. Failure: diversion, cycle, functional, No gradient
## 3. Reduced performance

## basic function
f2 = function(x){
    return(-4*x^3+5*x+1)
}

f2_grad = function(x){
    return(-12*x^2+5)
}

grid = seq(from = -2, to =2, 0.1)

plot(x = grid , y =f2(grid), type = "l")
abline(h=0) ## multiple solutions

## one_variable newton
newton_raphson = function(initial, max_iter, tol){
    
    x0 = initial
    
    for(i in 1:max_iter){
        
        x1 = x0 - f2(x0)/f2_grad(x0)
        
        if(abs(x1-x0)<tol){
            break
        }
        
        x0 <- x1
        
    }
    return(list(iter = i, solution = x0))
}

## multiple solutions
newton_raphson(initial = -0.595, max_iter = 100,tol = 0.01) # 1.20
newton_raphson(initial = -2, max_iter = 100,tol = 0.01) # -1.001589

rm(f2, f2_grad)
## for f = arctan -> diversion
## when initial x0 = -0.56898425... -> not stable
## for f = log(x), f>0 f(x)=0 do not exist and x0 = 2.9, 
## suboptimal f = x^9, x0 = 1 -> Too slow becasue of numerical error

## combine robust but slow solver(to get into  domain of atrraction) 
## with newton method(successive approx.)

## multivariate newton
## x, y로 정의 하는 것이 아니라 x[1], x[2]로 정의

my_func = function(x){
    return(2.575 - 2*cos(x[1])*cos(x[2]+pi)-0.575*cos(1.25*pi - 2*x[1]))
}

G <- function(x) {
    c(2 * sin(x[1]) * cos(x[2] + pi) - 2 * 0.575 * sin(1.25 * pi - 2 * x[1]),
      2 * cos(x[1]) * sin(x[2] + pi))
}

H <- function(x) {
    matrix(c(2 * cos(x[1]) * cos(x[2] + pi) - 2 * 0.575 * sin(1.25 * pi - 2 * x[1]), 
             -2 * sin(x[1]) * sin(x[2] + pi),
             -2 * sin(x[1]) * sin(x[2] + pi),
             2 * cos(x[1]) * cos(x[2] + pi)), nrow = 2, ncol = 2, byrow = TRUE)
}

## using optim()
optim(par = c(0,0), fn = my_func, method= "BFGS")

## multi_Newton-raphson for max_min
## for multivariate_newton_raphson, we should use Hessian and Gradient
multi_NR = function(initial, max_iter, tol){
    
    x0 <- initial
    
    ##
    for(i in 1:max_iter){
        
        ##
        x1 <- x0 - solve(H(x0)) %*% G(x0) # univariate: x1 = x0 - f2(x0)/f2_grad(x0)
        
        if(abs(sum(x1-x0)^2)<tol){
            break
        }
        
        x0 <- x1
        
    }
    return(list(iter = i, solution = x0))
}

##
multi_NR(initial =c(0,0), max_iter = 1000,tol = 0.01)
