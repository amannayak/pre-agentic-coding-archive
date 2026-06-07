
linReg = function(trainY,trainX , testY, testX){
  modelMatrix    = as.matrix(cbind(1,trainX))
  y              = as.matrix(trainY)
  #Computing beta for test parameter 
  beta           = solve(t(modelMatrix)%*%modelMatrix)%*%t(modelMatrix)%*%y
  
  #Computing Y pred and residual of same 
  predMatrix     = as.matrix(cbind(1,testX))
  fittedValues   = predMatrix%*%beta
  residual       = as.matrix(testY) - fittedValues
  return(residual)  
}#linReg = function(){


myCV=function(X,Y,Nfolds){
  n=length(Y)
  p=ncol(X)
  set.seed(12345)
  ind=sample(n,n)
  X1=X[ind,]
  Y1=Y[ind]
  sF=floor(n/Nfolds)
  MSE=numeric(2^p-1)
  Nfeat=numeric(2^p-1)
  Features=list()
  Features_axis <- c(0,0,0,0,0)
  curr=0
  
  #we assume 5 features.
  
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1)
          { 
            model= c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            
            SSE=0

            Xpred = data.frame(X1[,which(model == 1)])
            Y1 = data.frame(Y1)            
            knode_s = 1
            knode_e = sF
            for (k in 1:Nfolds)
            {
              trainX = Xpred[-(knode_s:knode_e),]
              testX  = Xpred[knode_s:knode_e, ]
              trainY = Y1[-(knode_s:knode_e),]
              testY  = Y1[knode_s:knode_e,]
              
              print(trainX)
              #print(trainY)
              
              residual = linReg(trainY,trainX ,testY, testX)
              
              SSE=SSE+sum((residual)^2)
              if(k != Nfolds)
              {  
                knode_s = sF * k +1
                kndoe_e = sF * (k + 1)
              }#if(k != Nfolds)
            }#for (k in 1:Nfolds)
            
            curr=curr+1
            n1 = length(testY)
            MSE[curr]=SSE/n1
            Nfeat[curr]=sum(model)
            Features[[curr]]=model
            Features_axis[curr] <- sum(model) #calculating total number of features used 
          }#for(f5 in 0:1)
  
  MSEplot = plot(MSE, type="o", col="blue", xaxt = "n" ,ann=FALSE)
            title(main="MSE vs Features", xlab = "Number of Features",
            col.main="green",
            font.main=4)
            axis(1,at=1:31, labels = Features_axis)
   
   i=which.min(MSE)
   return(list(CV=MSE[i], Features=Features[[i]] , MSEplot))
}

myCV(swiss[,2:6] , swiss[,1] , 5)
