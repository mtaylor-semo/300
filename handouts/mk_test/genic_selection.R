library(DescTools)

# Input =("
#  ADH       fixed  polymorphic
#  Replacement    7          2
#  Synonymous   17          42
# ")

Input =("
 PSE       fixed  polymorphic
 Replacement    38          16
 Synonymous   30          2
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
f
GTest(Matriz, correct = "none")

N <- sum(Matriz)
ab <- 7 + 2
cd <- 17 + 42
ac <- 7 + 17
bd <- 2 + 42

q <- 1 + (((N/ab + N/cd - 1)*(N/ac + N/bd - 1))/(6*N))
  

row.tot <- col.tot <- 0
for(i in 1:nrow(Matriz)){
  row.tot <- row.tot + 1/sum(Matriz[i,])
  col.tot <- col.tot + 1/sum(Matriz[,i])
}


q <- 1 + ((N*row.tot-1)*(N*col.tot - 1))/(6*N*(nrow(Matriz)-1)*(ncol(Matriz)-1))

x <- 7*log(7) + 17*log(17) + 2*log(2) + 42*log(42) + N*log(N)
x
y <-  9*log(9) + 59*log(59) + 24*log(24) + 44*log(44)
y

G <- 2*(x - y)
G

G/q
