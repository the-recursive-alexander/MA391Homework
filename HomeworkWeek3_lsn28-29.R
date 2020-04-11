###########################################################
########################Chapter 8##########################
###########################################################

###Problem 10###
#a) For this model, we will assume a pure birth/death process
#with the cars coming in being considered births and 
#the outgoing cars being considered deaths. We make the assumption
#that 
lambda = 30#cars/hour
#and
mu = 27#cars/hour
#we assume a slower rate of leaving than of arriving due to
#the fact that pumping gas takes more time than driving up to the
#station. We further assume that all lambdas are the same and 
#all mus are the same because there is no reason that the rates 
#would change based on the number of cars present. Furthermore
#we assume that at a line of 5 cars, the sixth car to arrive will
#balk and take there bussiness elsewhere which translates to a
n = 10#states
#From these assumptions, we can use a formula to determine the 
#proportion of time we are in state 0:
P0 = (1 - (lambda/mu))/(1 - (lambda/mu)^(n+1))
#now we can determine a state probability vector
P = matrix(nrow = n,ncol = 1)
for(i in 0:n) {
  Pi = ((lambda/mu)^(i-1))*P0
  P[i,] = Pi
}
print(P)
barplot(c(P[1,],P[2,],P[3,],P[4,],P[5,],P[6,],P[7,],P[8,],P[9,],P[10,]),names.arg = 0:9)
#Now we have the probabilities for each state. Here, the value
#marked with 1 on the matrix is state 0.
#looking at the matrix we see there is a
sum(c(P[6,],P[7,],P[8,],P[9,],P[10,]))*100
#percent chance that there will be a line of at least one
#calculating the expected value of how many cars there will be,
#we find that we expect there to be
expectedV = 0
for(i in 1:n) {
  expectedV = expectedV + P[i,]*(i-1)
}
print(expectedV)#cars at any one time
#b) Given that there is a 
P[10,]#percent chance that the cars form a line so long that
#customers will balk and 28 cars come in an hour, there are
lost = P[10,]*mu;print(lost)#customers lost per hour or
(lost/mu)*100#percent of business lost
#c) As above, the station manager could simply record the number of 
#customers that arrive at the station per hour to obtain an 
#estimate of demand
#d) I would recommend that the station purchase additional pumps 
#in the event that the percentage of lost business calculated 
#above reaches 25 percent


###Problem 11###

#a) Once again we will model a birth/death process with
lambda = .9#we say here that lambda is .9 because the
#probability of moving up a state is 90 percent. Multiply that
#by the rate of 1 reproduction every hour and we have lambda.
#likewise:
mu = .1
#we maintain 
n = 10#but in the context of this problem, this means that 10 hours
#have gone by. Similar to above, we find that
P0 = (1 - (lambda/mu))/(1 - (lambda/mu)^(n+1));print(P0)
#and after 10 hours, we expect there to be
P = matrix(nrow = n,ncol = 1)
for(i in 0:n) {
  Pi = ((lambda/mu)^(i-1))*P0
  P[i,] = Pi
}
print(P)
#b) from this information, we can predict that over time the 
#population would steadily increase
#c) lastly, one issue using this model is that n is
#practically infinite, so we cannot predict any steady state in
#particular







