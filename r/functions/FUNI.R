FUN = function(i,j){    
  chisq.test(matrix(c(Matriz[i,1], Matriz[i,2],
                      Matriz[j,1], Matriz[j,2]),
                    nrow=2,
                    byrow=TRUE))$ p.value
}
