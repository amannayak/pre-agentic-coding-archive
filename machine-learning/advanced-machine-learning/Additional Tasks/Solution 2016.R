
#2016 paper

library(bnlearn)
library(gRain)
library(Rgraphviz)
library(gRbase)

#1,1
data("asia")
set.seed(123)
#learn network
dagAsia = hc(x = asia , restart = 10, score="bde",iss=10)
bnlearn::graphviz.plot(dagAsia , main = "Learned Network")
asiaFit = bn.fit(x = dagAsia , data =  asia , method = "bayes")
grainObject = bnlearn::as.grain(asiaFit)
compiled = compile(object = grainObject)
Evidence_Grain = gRain::setEvidence(object = compiled , 
                                    nodes = c("X" , "B") , 
                                    states = c("yes" , "yes"))
queryGrain = querygrain(object = Evidence_Grain,
                        #object = compiled_Grain , evidence = Evidence_Grain ,
                        nodes = "A" , 
                        type = "marginal")

queryGrain


#1,2
#library(bnlearn)
set.seed(123)
ss<-50000
x<-random.graph(c("A","B","C","D","E"),num=ss,method="melancon",every=50,burn.in=30000)

y<-unique(x)
z<-lapply(y,cpdag)

r=0
for(i in 1:length(y)) {
  if(all.equal(y[[i]],z[[i]])==TRUE)
    r<-r+1
}
length(y)/r


#1,3 follow MIT doc
#![alt text here](path-to-image-here)

#Question 2 

#outbound question 

# You are asked to build a HMM to model a weather forecast system. 
# The system is based on the following information. 
# 
# If it was rainy (respectively sunny) the last two days, then it will be rainy (respectively sunny) today with probability 0.75 and sunny (respectively rainy) with probability 0.25. 
# 
# If the last two days were rainy one and sunny the other, then it will be rainy today with probability 0.5 and sunny with probability 0.5.
# 
# Moreover, the weather stations that report the weather back to the system malfunction with probability 0.1, i.e.  	
# they report rainy weather when it is actually sunny and vice versa. 
# 
# Implement the weather forecast system described using the HMM package. Sample 10 observations from the HMM built. 
# 
# Hint: You may want to have hidden random variables with four states encoding the weather in two consecutive days.
# 



library(HMM)

#States : Day Before Yesterday , Yesterday, Today and Tomorrow 
State = c("RR" , "RS" , "SR" , "SS") # States are RR , RS ,SR , SS 
Symbol = c("R" , "S")

# transProbs=matrix(c(0 , 0.75 , 0.25 , 0,
#                     0.5 , 0 , 0 , 0.5,
#                     0.5 , 0, 0 , 0.5,
#                     0 , 0.25 , 0.75 , 0) , 
#                     byrow = TRUE , nrow = 4 )

transProbs=matrix(c(0.75 , .25 , 0 , 0,
                    0 , 0 , 0.5 , 0.5,
                    0.5 , 0.5, 0 , 0,
                    0 , 0 , 0.25 , 0.75) , 
                  byrow = TRUE , nrow = 4 )

#R , S 
emissionProbs=matrix(c(0.9,0.1,  
                       0.1 ,0.9,
                       0.9,0.1,
                       0.1,0.9),
                     byrow = TRUE , nrow = 4)


hmm=initHMM(State,Symbol,transProbs,emissionProbs , startProbs = c(0.25,0.25,0.25,0.25))

#

#QUestion 2,1
library(HMM)

States=1:10 # Sectors
Symbols=1:11 # Sectors + malfunctioning
transProbs=matrix(c(.5,.5,0,0,0,0,0,0,0,0,
                    0,.5,.5,0,0,0,0,0,0,0,
                    0,0,.5,.5,0,0,0,0,0,0,
                    0,0,0,.5,.5,0,0,0,0,0,
                    0,0,0,0,.5,.5,0,0,0,0,
                    0,0,0,0,0,.5,.5,0,0,0,
                    0,0,0,0,0,0,.5,.5,0,0,
                    0,0,0,0,0,0,0,.5,.5,0,
                    0,0,0,0,0,0,0,0,.5,.5,
                    .5,0,0,0,0,0,0,0,0,.5), nrow=length(States), ncol=length(States), byrow = TRUE)

emissionProbs=matrix(c(.1,.1,.1,0,0,0,0,0,.1,.1,.5,
                       .1,.1,.1,.1,0,0,0,0,0,.1,.5,
                       .1,.1,.1,.1,.1,0,0,0,0,0,.5,
                       0,.1,.1,.1,.1,.1,0,0,0,0,.5,
                       0,0,.1,.1,.1,.1,.1,0,0,0,.5,
                       0,0,0,.1,.1,.1,.1,.1,0,0,.5,
                       0,0,0,0,.1,.1,.1,.1,.1,0,.5,
                       0,0,0,0,0,.1,.1,.1,.1,.1,.5,
                       .1,0,0,0,0,0,.1,.1,.1,.1,.5,
                       .1,.1,0,0,0,0,0,.1,.1,.1,.5), nrow=length(States), ncol=length(Symbols), byrow = TRUE)

startProbs=c(.1,.1,.1,.1,.1,.1,.1,.1,.1,.1)

hmm=initHMM(States,Symbols,startProbs,transProbs,emissionProbs)

obs=c(1,11,11,11)
posterior(hmm,obs)
viterbi(hmm,obs)


#QUestion 2,2


States=1:5
Symbols=1:5
#now since it is staying in same band for 2 time unit so it will move to next unit only at third time
#unit so its probability of being in stage will change to .67 , .33 
#rounding .66666 to .67 for 1 else .66 and .33  
transProbs=matrix(c(.67,.33,0,0,0,
                    0,.67,.33,0,0,
                    0,0,.67,.33,0,
                    0,0,0,.67,.33,
                    .33,0,0,0,.67), nrow=length(States), ncol=length(States), byrow = TRUE)

emissionProbs=matrix(c(0.33 , 0.33 , 0 , 0 , 0.33,
                       0.33 , 0.33 , 0.33 ,0 , 0 ,
                       0 , 0.33 , 0.33 , 0.33 ,0 ,
                       0 , 0 , 0.33 , 0.33 ,0.33 ,
                       0.33 , 0 , 0 , 0.33 , 0.33
                      ), nrow=length(States), ncol=length(States), byrow = TRUE)

startProbs=c(.2,.2,.2,.2,.2)

hmm2 = initHMM(States,Symbols,startProbs,transProbs,emissionProbs)

hmm2


#Question 3 

#2018 Question 


