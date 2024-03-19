## linear programming
## 선형

## max profit y+2x
## s.t. x>=0, y>=0, x+2y<=14, y-x<= 4, 2x-y<=8
## 중요한 것은 어떻게 matrix form으로 만드느냐
install.packages("lpSolve")
library(lpSolve)

# lpsolve
f.obj = c(2, 1)
f.dir = rep("<=", 5)
f.con = Amat
f.rhs = bvec

lpSolve::lp('max', f.obj, f.con, f.dir, f.rhs) ## 16

# Quadprog::QP.solve
# Dmat: 목적함수의 2차계수행렬
# dvec: 목적함수의 선형 계수 벡터
# Amat : 제약조건의 계수행렬, Ax=b 's A
# bvec : 제약조건의 우변 벡터, Ax=b's b

## https://henrywang.nl/quadratic-programming-with-r/
rm(list=ls())
library(quadprog)


return = c(0.21, 0.3, 0.08)
cov_m = matrix(c(0.25^2, 0.04  , -0.01 ,
                 0.04  , 0.45^2, -0.005,
                 -0.01 , -0.005, 0.05^2), 
               ncol= 3, nrow = 3)

## min risk
## s.t. w>= 0, w1+w2+w3  = 1
## 21w1 + 30w2+ 8w3 >= 18

dvec = -return
Dmat = 2*cov_m
Amat = cbind(1, diag(1, nrow = 3, ncol = 3))
Amat = cbind(Amat, c(21, 30, 8))
bvec = c(1, 0, 0, 0 , 18)

solve.QP(dvec = dvec, Dmat = Dmat, Amat = Amat, bvec = bvec, meq = 1)