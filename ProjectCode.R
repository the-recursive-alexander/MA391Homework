library(ma391helpfuncs)

#example function
test = function(x) {
  c(-x[1]^3 - 4*x[1] - x[2], 3*x[1])
}
linmdl = function(f,x = c(0,0),h=0.001) {
  n = length(x)
  delF = array(0,dim=c(n,n))
  for (i in 1:n){
    xhp = xhm = x

    xhp[i]=xhp[i]+h
    xhm[i]=xhm[i]-h
    delF[,i] = (f(xhp)-f(xhm))/(2*h)
  }
  return (delF)
}
diffsolve = function(f, t, C) {
  e = eigen(linmdl(f))
  c(C[1]*(e$vectors[1,1])*exp(e$values[1]*t) + C[2]*(e$vectors[1,2])*exp(e$values[2]*t),
    C[1]*(e$vectors[2,1])*exp(e$values[1]*t) + C[2]*(e$vectors[2,2])*exp(e$values[2]*t))
}
givepts = function(f,n,C, h = 0.1) {
  t = seq(0, n, h)
  pts = array(0, dim = c((n/h),2))
  for(i in 1:(n/h)) {
    pts[i,] = f(t[i],C)
  }
  return(pts)
}
PhasePortrait = function(fun, n, xlim, ylim) {
  solfun = function(t, C) {diffsolve(fun,t,C)}
  VectorField(fun,xlim,ylim)
  r = seq(-1,1,0.01)
  for(i in 1:n) {
    rand = sample(r,2)
    pts = givepts(solfun, 10, c(rand[1]*xlim[1],rand[2]*xlim[2]))
    points(pts, type='l',col = 'red')
    arrows(pts[2,1],pts[2,2], pts[3,1], pts[3,2], angle = 30, length = .1, col='red')
  }
}


#Example
PhasePortrait(test,10, xlim = c(-3,3),ylim=c(-3,3))


