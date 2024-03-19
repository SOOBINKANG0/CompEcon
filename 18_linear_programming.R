## linear programming
## 선형

## max profit y+2x
## s.t. x>=0, y>=0, x+2y<=14, y-x<= 4, 2x-y<=8

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