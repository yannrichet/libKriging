## This benchmark is dedicated to compare behavior of OptimLib and optim (for R) on a given objective function (Rosenbrock)

a=.5
b=50
rosenbrock_fun = function(X) (a-X[,1])^2+b*(X[,2]-X[,1]^2)^2
# min: rosenbrock_fun(cbind(a,a^2))==0

x=seq(0,1,,21)
contour(x,x,matrix(rosenbrock_fun(expand.grid(x,x)),nrow=length(x)),nlevels = 50)
points(a,a^2)

rosenbrock_grad = function(X) cbind(
  -2*(a-X[,1]) + 4*b*(X[,1]^3 - X[,2]*X[,1]),
  2*b*(X[,2] - X[,1]^2)
)
# min: rosenbrock_grad(cbind(a,a^2)) == 0 0
delta=.001
for (x in seq(0,1,,11))
  for (y in seq(0,1,,11)) {
    g = rosenbrock_grad(cbind(x,y))
    arrows(x,y,x-delta*g[,1],y-delta*g[,2],length = .1)
  }




set.seed(123)
X0 = matrix(runif(20),ncol=2)

for (ix0 in 1:nrow(X0)) {
  x0 = X0[ix0,]

  if (abs(rosenbrock_fun(matrix(x0,ncol=2)) - f_optim(x0))>1E-7) stop("Wrong f eval")
  if (any(abs(rosenbrock_grad(matrix(x0,ncol=2)) - t(grad_optim(x0)))>1E-7)) stop("Wrong g eval")
    
  hist_x = NULL
  f = function(x) {
    x=matrix(x,ncol=2); 
    if(exists('last_x'))
       if(!is.null(last_x)) 
          lines(x=c(last_x[,1],x[,1]),y=c(last_x[,2],x[,2]),lty=2,col=rgb(1,0,0,.2)); 
    last_x <<- x;
    hist_x <<- rbind(hist_x,x) 
    points(x,col=rgb(1,0,0,.2)); 
    f_optim(x) #rosenbrock_fun(x)
  }
  g = function(x) {
    x=matrix(x,ncol=2);
    gr=t(grad_optim(x)) #rosenbrock_grad(x); 
    arrows(x[,1],x[,2],x[,1]-delta*gr[1],x[,2]-delta*gr[2],length = .1,col=rgb(1,0,0,.2)); 
    return(gr)
  }
  
  o=optim(x0,f,g,method = "L-BFGS-B", control=list(maxit=10),lower = c(0,0),upper=c(1,1))
  points(o$par[1],o$par[2],col='red',pch='x')
  
  X = bench_optim(x0)
  points(X[-1,1],X[-1,2],col=rgb(0,0,1,.2),pch=20)
  lines(X[-1,],col=rgb(0,0,1,.2),lty=2)
  points(X[nrow(X),1],X[nrow(X),2],col='blue',pch='x')
  
}






