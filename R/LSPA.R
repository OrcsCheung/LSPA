LSPA<-function(x,a,b){
  matrixA<-matrix(0,x,x)
  for (i in 1:x){
    for(j in 1:x){
      matrixA[i,j]=(b^(i+j-1)-a^(i+j-1))/(i+j-1)

    }

  }
  print(matrixA)

  matrixB<-matrix(0,x,1)
  for (i in 1:x){


    matrixB[i]<-integrate(function(y){(y^(i-1))*(log(y)*y)},lower = a,upper = b)[1]

  }
  print(matrixB)


  ans<-matrix(0,x,1)
  ans<-solve(matrixA,matrixB)
  print(ans)
}
