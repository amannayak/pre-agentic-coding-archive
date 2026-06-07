#' The class linreg is similar to lm function in R which is used for linear modelling.
#' @param formula Formula used for calculation of dependent factor. 
#' @param data is a Data frame.
#' @return linear model based on \code{formula} and \code{data}
#' @examples 
#' \dontrun{
#' a = linreg(formula,data)
#' a$print()
#' a$plot()
#' a$summary()
#' }
#' @details 
#' This class use RC Class in order to make Object oriented programming.
#' 
#' We are here making linear model custom way which is based on lm function which already exit in R. 
#' 
#' Please refer below doc to read more on lm function
#' \url{https://www.rdocumentation.org/packages/stats/versions/3.6.0/topics/lm}
#' @import ggplot2 
#' @import gridExtra
#' @export linreg
#' @export


linreg = setRefClass("linreg",
   fields = list(
     formula = "formula", 
     data    = "data.frame",
     RegCof = "matrix",
     #coefficients = "matrix",
     fittedVal = "matrix",
     residual = "matrix",
     degfree = "numeric",
     residualVar = "numeric",
     varRegCoeff = "matrix",
     tValue = "matrix",
     f_formula = "character",
     data_name = "character"
     # #mm = "matrix"
     #replace with 
   ), 
   methods = list(
     #linreg = function(formula,data)
     initialize = function(formula,data)
     {
       tryCatch( 
         expr =
           {
             mm = model.matrix(formula,data)
             vact = as.vector(all.vars(formula))
             f_formula<<-Reduce(paste, deparse(formula))
             data_name<<-deparse(substitute(data))
             len = length(vact)
             vact = vact[1]
             Dep_Location = which(colnames(data) == vact)
             y = matrix(data[,as.numeric(Dep_Location)])
             
             #Regression Coefficients
             #assign(RegCof , solve(t(mm)%*%mm)%*%t(mm)%*%y)
             RegCof <<- (solve(t(mm)%*%mm)%*%t(mm)%*%y)
             #coefficients <<- t(RegCof)
             # when multiple <<- R consider it missspel and since I have multiple <<- so in order to avoid
             # this replaced this with assign function
             
             #Fitted Values
             fittedVal <<- (mm%*%RegCof)
            
             #Residuals:
             #assign(residual , y-fittedVal)
             residual <<- (y-fittedVal)
         
             #Degrees of Freedom:
             #assign(degfree , as.vector(length(residual) - ((len)+1)))
             degfree <<- as.vector(length(residual) - len)
             #+1 is done to include intercept  
             
             #The residual variance: sigma^2
             #assign(residualVar , as.vector((t(residual)%*%residual)/degfree))
             residualVar <<- as.vector((t(residual)%*%residual)/degfree)
             
             #Variance of Regression Coefficients:
             #assign(varRegCoeff , diag(solve(t(mm)%*%mm) * residualVar))
             varRegCoeff <<- solve(t(mm)%*%mm) * residualVar
             
             #t-Value
             #assign(tValue , (RegCof / (sqrt(varRegCoeff))))
             tValue <<- RegCof / as.double(sqrt(diag(varRegCoeff)))
             
              return(.self)
             
           },#expr
         error = function(e)
         {
           return("Function : linreg is getting error out " + e)
         }
       )#tryCatch(
     }#initialize = function(data,formula)  
     , 
     print = function(){
       cat("Call:\n")
       text_str<-paste("linreg(formula = ",f_formula,", data = ",data_name,")",sep = "")
       cat(text_str,"\n")
       cat("Coefficients: \n\n")
       disp<-c(round(RegCof,digits = 2))
       cat(row.names(RegCof),"\n")
       cat(disp)
       
     }#print = function(){
     ,
     plot = function()
     {
       tryCatch(
         expr =
           {
             library(ggplot2)
              
             #Added theme as required 
              liu_blue <- "#54D8E0"
              theme_liu <- theme(plot.margin = unit(c(1,1,1,1), "cm"), 
                                 panel.background = element_rect(fill="white"),
                                 panel.grid.major.y = element_blank(),
                                 panel.grid.minor.y = element_blank(),
                                 panel.grid.major.x = element_blank(),
                                 panel.grid.minor.x = element_blank(),
                                 axis.line = element_line(color= "#58585b", size=0.1),
                                 axis.text.x = element_text(color="Black", size="10"),
                                 axis.text.y = element_text(color="Black", size="10"),
                                 axis.title.x = element_text(color="Black", size="10", face="bold"),
                                 axis.title.y = element_text(color="Black", size="10", face="bold"),
                                 axis.ticks.y = element_blank(),
                                 axis.ticks.x = element_line(color = "#58585b", size = 0.3),
                                 plot.title = element_text(color="Black", face="bold", size="14", hjust = 0.5),
                                 legend.position="bottom", legend.title = element_blank(),
                                 legend.key = element_blank(),
                                 legend.text = element_text(color="Black", size="10"))
              
             
             plot1 = qplot(x = fittedVal , y = residual , xlab = "Fitted Value" , ylab = "Residuals") 
             plot1 = plot1 + stat_summary(fun.y = median , color = "red" , geom = "line" , size = 1)
             plot1 = plot1 + ggtitle(label = "Residuals vs Fitted")
             plot1 = plot1 + theme_liu
             
             sqrt_stdResi = sqrt(abs((residual - mean(residual))/sqrt(residualVar)))
             
             plot2 = qplot(x = fittedVal , y = sqrt_stdResi , xlab = "Fitted Value" , ylab = "Standardized Residuals") 
             plot2 = plot2 + stat_summary(fun.y = mean , color = "red" , geom = "line" , size = 1)
             plot2 = plot2 + ggtitle(label = "Scale−Location")
            plot2 = plot2 + theme_liu
             
             library(gridExtra)
             grid.arrange(plot1, plot2, nrow = 1,ncol = 2)
             
           },#expr
         error = function(e)
         {
           return("Function : plot is getting error out " +  e)
         }
       )#tryCatch(
     }#plot = function()
     ,
     resid = function()
     {
       tryCatch( 
         expr =
           {
             return(residual)		
           },#expr
         error = function(e)
         {
           return("Function : resid is getting error out " + e)
         }
       )#tryCatch(
     }#resid = function()
     ,
     pred = function()
     {
       tryCatch( 
         expr =
           {
             return(fittedVal)		
           },#expr
         error = function(e)
         {
           return("Function : pred is getting error out " + e)
         }
       )#tryCatch(
     }#pred = function()
     ,
     coef = function()
     {
       tryCatch( 
         expr =
           {
             return(RegCof)		
           },#expr
         error = function(e)
         {
           return("Function : coef is getting error out " + e)
         }
       )#tryCatch(
     }#coef = function()
     ,
     summary = function()
     {
       tryCatch( 
         expr =
           {
             p_val = 2*pt(-abs(tValue),degfree)
             summary_df = data.frame(
               estimate = round(RegCof,2),
               std.error = round(sqrt(diag(varRegCoeff)),2),
               t_value = round(tValue,2),
               p_value = round(p_val,4)
             )#ended data frame creation
             
             #initaliznig for loop in order to append * as per expectation of unit test case
             for(i in 1:nrow(summary_df))
             {
               if(summary_df$p_value[i] == 0){
                 #paste(c(summary_df$p_value[i] , "***") , collapse = "")
                 summary_df$p_value[i] =  "***"
               }else if(summary_df$p_value[i] > 0 && summary_df$p_value[i] <= 0.001){
                 #paste(c(summary_df$p_value[i] , "**") , collapse = "")
                 summary_df$p_value[i] =  "**"
               }else if(summary_df$p_value[i] > 0.001 && summary_df$p_value[i] <= 0.01){
                 #paste(c(summary_df$p_value[i] , "*") , collapse = "")
                 summary_df$p_value[i] =  "*"
               }else if(summary_df$p_value[i] > 0.01 && summary_df$p_value[i] <= 0.05){
                 #paste(c(summary_df$p_value[i] , ".") , collapse = "")
                 summary_df$p_value[i] =  "."
               }else if(summary_df$p_value[i] > 0.05 && summary_df$p_value[i] <= 0.1){
                 #paste(c(summary_df$p_value[i] , " ") , collapse = "")
                 summary_df$p_value[i] =  " "
               }else if(summary_df$p_value[i] > 0.1){
                 summary_df$p_value[i] = " "
               }
               
             }#for(i in 1:nrow(summary_df))
             names(summary_df)<-NULL
             base::print(summary_df)
             
             cat("Residual standard error:", round(sqrt(diag(varRegCoeff)[1]),1),"on",degfree,"degrees of freedom")
             
           },#expr
         error = function(e)
         {
           return("Function : summary is getting error out " + e)
         }
       )#tryCatch(
     }#summary = function()
   )#methods = list
)#linreg = setRefClass(

