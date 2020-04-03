library(ma391helpfuncs)
#################################################################
##########################Chapter 4##############################
#################################################################

#####Problem 4######
#B = number of blue whales
#F = number of fin whales
#gB = growth rate of blue whales
#gF = growth rate of fin whales
##assume##
rB = 0.05
rF = 0.08
alpha = 10^-8
##model##
simplemdl = function(P) {
  c(rB*P[1] - alpha*P[1]*P[2],
    rF*P[2] - alpha*P[1]*P[2])
}
##solve##
VectorField(simplemdl, xlim = c(0,200000), ylim = c(0,450000))
points(path(simplemdl,c(5000,70000)))
#the vectorfield (b) only yeilds one discerible zero, we find it:
#we also see given 5000 blue whales and 70000 fin whales, 
#the population would increase indefinately under this model(d)
Zeros(simplemdl,c(500,500))
#We arrive at the result (0,0)
#there is one equilibrium point at (0,0). it is unstable (c)
#We conclude that these two species can coexist under this simplified 
#model. (a)







#####Problem 5#####
#B = number of blue whales
#F = number of fin whales
#gB = growth rate of blue whales
#gF = growth rate of fin whales
##assume##
rB = 0.05
rF = 0.08
cB = 3000
cF = 15000
kB = 150000
kF = 400000
alpha = 10^-8
##model##
complexmdl = function(P) {
  c(rB*((P[1]-cB)/(P[1]+cB))*(1-(P[1]/kB)) - alpha*P[1]*P[2],
    rF*((P[2]-cF)/(P[2]+cF))*(1-(P[2]/kF)) - alpha*P[1]*P[2])
}
#solve#
#similar to before, we analyze the vector field. Solve for zeros, 
#and classify those points.
VectorField(complexmdl, xlim = c(0,200000), ylim = c(0,450000))
points(path(simplemdl,c(5000,70000)))
Zeros(complexmdl,c(200,200))


#################################################################
#############################Chapter 5###########################
#################################################################

######Problem 1######
#a see above
#b 
A = Hessian(simplemdl,c(0,0))
eigen(A)
#one eigen value is negative so the equilibrium is unstable
#can't get my Phase portrait function to work, but we would use
#it here to sketch the phase portrait
PhasePortrait(simplemdl, 10, xlim = c(0,20000),ylim=c(0,400000))



