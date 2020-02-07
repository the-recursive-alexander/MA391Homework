#problem1
#a
profit = function(rebate) {(1500 - 100*rebate)*(1+.15*rebate)}
dprofit = function(rebate){fprime(profit,rebate)}
x = seq(0,10,0.1)
plot(x,profit(x))
newton(dprofit,2)
profit(4.16666)
#A rebate of $417 will maximize profit to $1760.42

#b
saleIncrease = seq(0.05, 0.50, 0.01)
bestRebate = 0
highestProfit = 0
for(i in 1:length(saleIncrease)) {
  profit = function(rebate) {(1500 - 100*rebate)*(1+saleIncrease[i]*rebate)}
  dprofit = function(rebate){fprime(profit,rebate)}
  bestRebate[i] = newton(dprofit, 0)
  highestProfit[i] = profit(bestRebate)
}

result = data.frame(saleIncrease = saleIncrease, bestRebate = bestRebate, highestProfit = highestProfit)
print(result)

#c
saleIncrease = seq(0.1, 0.15, 0.01)
bestRebate = 0
highestProfit = 0
for(i in 1:length(saleIncrease)) {
  profit = function(rebate) {(1500 - 100*rebate)*(1+saleIncrease[i]*rebate)}
  dprofit = function(rebate){fprime(profit,rebate)}
  bestRebate[i] = newton(dprofit, 0)
  highestProfit[i] = profit(bestRebate)
}

result = data.frame(saleIncrease = saleIncrease, bestRebate = bestRebate, highestProfit = highestProfit)
print(result)
#a 10% increase in sales for each $100 dollar rebate means a best rebate of $250 and a maximum profit of $1562.50

#d
#A reduction in profit would occur whenever the rebate offer counteracted the increase in sales that it created

#Problem2
#Sensitivity analysis
cost = seq(0.35, 0.55, 0.01)
whenSell = 0
maxProfit = 0
for(i in 1:length(cost)) {
  profit = function(days) {(0.65-0.01*days)*(200+5*days)-cost[i]*days}
  dprofit = function(days) {fprime(profit,days)}
  whenSell[i] = newton(dprofit, 0)
  maxProfit[i] = profit(whenSell)
}
result = data.frame(cost = cost, sellOnDay = whenSell, maxProfit = maxProfit)
print(result)
#the cost to keep the pig does not really effect the maximum profit too much
#it averages around 133 dollars. It does, however, effect the date to sell
#on by a moderate amount, varying from nine to seven days.

#Switch feed?
days = seq(0, 20,1)
profit = function(days) {(0.65-0.01*days)*(200+7*days)-.60*days}
dprofit = function(days) {fprime(profit,days)}
plot(days, profit(days), type = "l")
ans = newton(dprofit,0)
print(ans)
print(profit(ans))

#under the new feed, you should sell after 13 days and will make
#a profit of 143 dollars so you should switch becuase you are gaining
#more profit potentially

#minimum improvement?
growthrate = seq(5,7,0.1)
maxProfit = 0
for(i in 1:length(growthrate)) {
  newfeedprofit = function(days) {(0.65-0.01*days)*(200+growthrate[i]*days)-.60*days}
  dnewfeedprofit = function(days) {fprime(newfeedprofit,days)}
  ans = newton(dnewfeedprofit,5)
  maxProfit[i] = newfeedprofit(ans)
}
result = data.frame(growth_rate = growthrate, whenSell = ans,maxProfit = maxProfit)
print(result)
#the new feed should increase growth rate by at least .5 to have any increase in
#profit, however an increase by 2 will make the increase in profit more worthwhile
#as that increases profit by 10 dollars

##Lesson5
#Problem 1
#f = population of Fin whales
#b = population of Blue whales
rF = 0.08 #intrinsic growth rate of Fin whales
rB = 0.05 #intrinsic growth rate of Blue Whales
KF = 400000 #maximum sustainable population of Fin whales
KB = 150000 #maximum sustainable population of Blue whales
a = 0.00000001 #effects ofcompetition
#W = number of whales total
#t = time (years)
#assume: W = B + F

#objective functions:
dF = function(f, b) {rF*f*(1-(f/KF) - a*f*b)}
dB = function(f, b) {rB*b*(1-(b/KB) - a*f*b)}

#a) maximize dW = dF + dB
W = function(f,b) {dF(f,b) + dB(f,b)}
dW = function(f,b) {gradient(W,a,b)}


##Lesson7
problem1 = function(x1,x2) {x1*(600-3*x1+x2) + x2*(800-2*x2+x1)}


##Lesson9
#problem1
obj = function(x) {
  (600-3*x[1]+x[2])*x[1]
  +(800-2*x[2]+x[1])*x[2]
}
Aeq = matrix(c(12,5),nrow = 1)
Beq = matrix(2500)

X = list(x = seq(100,500), y = seq(100,500))
Z = Outer(obj, X)

contour(X$x,X$y, -Z)
abline(a = 500,b = -12/5, col = "red",lwd = 3)


x0 = c(100,500)

#ans = solnl(x0, objfun = obj,Aeq = Aeq,Beq = Beq)

#problem5
obj = function(x) {
  (x*(10+22*x[1]^-0.5+1.3*x[2]^-0.1)) -
    (18*x[1]) +
    (x[2]*(5+15*x[2]^-0.5+0.8*x[1]^-0.08)) -
    (10*x[2])
}
X = list(x = seq(0,500), y = seq(0,500))
#Z = Outer(obj, X)



