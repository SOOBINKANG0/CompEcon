## linear programming
## 선형

## max profit y+2x
## s.t. x>=0, y>=0, x+2y<=14, y-x<= 4, 2x-y<=8
## 중요한 것은 어떻게 matrix form으로 만드느냐
install.packages("lpSolve")
library(lpSolve)

# lpsolve
f.obj = c(2, 1)
f.dir = c(rep(">=", 2), rep("<=", 3))
f.con = matrix(c(1, 0, 0, 1, 1,2, -1, 1, 2, -1),nrow = 5, ncol =2, byrow = T)
f.rhs = c(0,0,14,4,8)

lpSolve::lp('max', f.obj, f.con, f.dir, f.rhs) ## 16

# Quadprog::QP.solve
## https://henrywang.nl/quadratic-programming-with-r/
rm(list=ls())
library(quadprog)

return = c(0.21, 0.3, 0.08)
cov_m = matrix(c(0.25^2, 0.04  , -0.01 ,
                 0.04  , 0.45^2, -0.005,
                 -0.01 , -0.005, 0.05^2), 
               ncol= 3, nrow = 3)

# Dmat: 목적함수의 2차계수행렬
# dvec: 목적함수의 선형 계수 벡터
# Amat : 제약조건의 계수행렬, Ax=b 's A
# bvec : 제약조건의 우변 벡터, Ax=b's b

## min risk
## s.t. w1 >= 0, w2>= 0, w3>= 0,
## w1+w2+w3  = 1
## 21w1 + 30w2+ 8w3 >= 18
dvec = matrix(rep(0,3), nrow = 3)
Dmat = 2*cov_m
Amat = t(rbind(c(1,1,1), diag(1, nrow = 3, ncol = 3), c(21,30,8)))
bvec = c(1,0, 0, 0, 18)

res = solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
print(round(res$solution,2))




