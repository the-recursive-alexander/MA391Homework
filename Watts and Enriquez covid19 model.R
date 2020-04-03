##################
#CorOnAViRUs
##################
#FACTS from research
#First reported US case is January 22nd
#Last date for case numbers is April 1st
#Our model will look to fit these two dates case numbers
#starts at 1 patient, then goes to 213,144 cases by April first 
#United States Population (when writing) 330,534,135
#virus incubates for 5.2 days before showing symptoms, however...
#people are able to 'shed' the virus and infect others before they show symptoms, basically as soon as getting sick
#infected individuals typically are symptomatic for 2 weeks, so they have the disease for 2.74 weeks
#virus has an r naught r0=2.38 in perfect conditions, every sick person infects 2.38 healthy people
#novel virus meaning no immunity to this virus, 100% of population is susceptible to catching it
#we will model using
#x[1]=susceptible
#x[2]=infected
#x[3]=recovered
#x[4]=dead

#ASSUMPTIONS
#Only person to person contact can transmit the disease
#this means we are assuming the coronavirus can only live on hosts, not surfaces
#initially we are assuming no quarantine measures
#we also assume r0 of 2.2 based on Early Transmission Dynamics article and research
#we will model this disease weekly, each interval of time is one week passing
#the last assumption is that all cases originated from one infected individual, our 'patient zero'

#MODEL:
#alpha is the interaction coefficient. By making it equal to 
#2.2*1/330534135, when it is multiplied by our initial x[1] of the US population and x[2]
#which is one sick person, it equals 2.2 new cases which is exactly what we want
alpha=2.2*1/330534135
#beta is the recovery coefficient. By making it equal to the percentage of people who
#recover 96.6% and dividing by the amount of weeks they have the disease (2.74), we have a recovery rate percentage per week
beta=.966/2.74
#omega is the death coefficient. 3.4% of people who contract coronavirus statistically will die.
#we are setting omega equal to .034 divided by the amount of time it takes for the average patient to recover (2.74 weeks)
#because the infected individuals will be infected for more than one week so we must divide our death percentage
#by the time it takes people to either recover or die so that our model does not predict an unrealistic death statistic
omega=.034/2.74
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]-beta*x[2]-omega*x[2],beta*x[2],omega*x[2])}
x0=c(330534135,1,0,0)
#below, the path and norm commands are adapted for a 4 variable system so we can model accurately
norm2 = function(v){sqrt(v[1]^2+v[2]^2+v[3]^2+v[4]^2)}
path = function(f,x0,deltat=0.01,N=1000,tol=1E-4){
  points=matrix(0,ncol=4)
  points[1,] = x0
  n = 0
  p = c(1,1,1,1)
  while(norm2(p)>tol & n<N){
    n=n+1
    p = f(x0)*deltat
    x0=x0+p
    points = rbind(points,x0)
  }
  
  rownames(points)=0:n
  return(points)
}
#the command below runs 19 weeks of supposed spread, and puts the peak at 19 weeks
x=path(f,x0,deltat=1,N=30,tol=1)
print(x)

#this plot gives the curve of the infected individuals with this disease
plot(x[,2])

#say quarantine measures were announced started right now for our model and 
#went into effect week 12. Then we use week 12 as our x0 and lower the alpha to 
#show how the r0 (number of people 1 infected individual can infect) would go down.
#We will arbitrarily say that this makes the virus 50% as infectious to give our new alpha.
alpha2=alpha/2;print(alpha2)
#the quarantine model is below
f=function(x){c(-alpha2*x[1]*x[2],alpha2*x[1]*x[2]-beta*x[2]-omega*x[2],beta*x[2],omega*x[2])}
x0=x[13,];print(x0)
y=path(f,x0,deltat=1,N=30,tol=1)
print(y)
plot(y[,2])
#this model makes week 13 the peak (to have the infected population in the 200,000s to depict the case number now)
#and dramatically shows how quarantine measures literally flatten the curve!
plot(x[,2],type="l",main="COVID-19 Model",ylab="infected",xlab="week")
plot(y[,2],type="l",main="COVID-19 Quarantine Model",ylab="infected",xlab="week")
