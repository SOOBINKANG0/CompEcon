rm(list=ls())
## 22_fixed_point_iteration(successive approximation)

## root finding method
## g(x) =  0 <-> g(x)+x = x
## iterate on x_i+1 = F(x_i) x_i+1 is close to to x_i

## Limitation
## Only stable solutions can be computed with SA
## Starting values matter for convergence!
## only linear (slow) convergence to the solution when it does converge

##
initial_f= function(x){
    return(1/2-exp(-(x-2)^2))
}

fixed_F = function(x){
    return(x-exp(-(x-2)^2)+1/2)
}

gradient_F = function(x){
    return(1 + exp(-((x-2)^2)*2*(x-2))) 
}


my_func =  function(init, max_iter, tol){
    
    x0 = init

    for(i in 1:max_iter){
        
        x1 = fixed_F(x0)

        err = abs(x1-x0)
        
        if(err < tol){
            break()
        }
        
        x0 = x1
    }
    return(list(iter = i, solution = x0, err = err))
}

my_func(init = 2.8, max_iter = 20000, tol = 0.000000001)


