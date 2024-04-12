## Computational Econ 9~11

## 1. binary search("up"and "down" game, find the sum of 'up' and down number)
up_and_down = function(x){
    
    #initial
    n = 1 
    
    
    bin = seq(1:100)
    random = sample(bin, 1)
    
    while(TRUE){
        
        if(x > random){
            
            bin = bin[bin > random]
            random = sample(bin, 1)
            print("up")
            
        } else if(x < random) {
            
            bin = bin[bin < random]
            random = sample(bin, 1)
            print("down")
            
        } else if(x == random){
            print("equal")
            break
        }
        
        n = n+1
    }
    return(paste0(n,' is the number counted'))
}

up_and_down(99)

## 2. bisection_method(solver)
## plot 
f = function(x){
    return(x^3 -4*x^2+ 3*x -24)
}

grid = seq(from = 0, to = 8, 0.1)

plot(x= grid, y = f(grid), type = "l", main = "graphically solve")
abline(h = 0) # solution is around "4~5"

##
bisection = function(x, max_iter, tol){ # x: interval, iter: max_iter, tol: tol
    
    a = x[1]
    b = x[2]
    
    if( f(a)*f(b)>0 ){
        print("input a new interval")
    } # exist solution btw interval
    
    for(i in 1:max_iter){
    
        m = (a+b)/2        
        error = abs(b-a)
            
        if(f(a)*f(m)<0){
            b <- m
        } else if(f(a)*f(m)>0) {
            a <- m
        }
            
        if(error < tol){
            break
        }
    }
    return(list(iter = i,solution = m))
}

## test
bisection(c(4,5), max_iter = 50, tol = 0.1)

## 3. newton_raphson method(solver)
if (!requireNamespace("Deriv", quietly = TRUE)) {
    install.packages("Deriv")
}
library(Deriv)


## function to be solved
f2 = function(x){
    return(x^3-4*x^2+10*x+23)
}

f2_grad = function(x){
    return(3*x^2-8*x+10)
}
## graphically sketch
grid = seq(from = -3, to =3, 0.1)

plot(x = grid , y =f2(grid), type = "l")
abline(h=0) ## solution exists btw -1 and -2

##
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

newton_raphson(-2, 100, 0.00001)
