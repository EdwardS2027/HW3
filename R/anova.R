#' Anova for Linear Regression
#'
#' @description
#' Performs ANOVA of the linear regression model
#'
#' @usage
#' anova2(formula,data,subset,na.action,intercept)
#'
#' @param formula
#' A framework for how should the linear regression model be given.
#'
#' @param data
#' A matrix of (nxp) whose data will be used to form the model.
#'
#' @param na.action
#' How will the missing values be handled. It is defauled to "omit" value.
#'
#' @param intercept
#' A boolean value if the linear regression model will have an
#' intercept or not. It is defaulted to TRUE.
#'
#' @return
#' A matrix that contains degrees of freedom, sum of squares, mean sum of
#' squares, f statistics, p values for each of the covariates in the fitted
#' linear regression model.
#'
#' @examples
#' data = read.csv("data/melb_data.csv")
#' output = anova2(data$Price~data$Bedroom2+data$Bathroom+data$Landsize,data,
#' na.action="omit",intercept=TRUE)
#' ss = output[,2]
#' f = output[,4]
#'
#' @export
#'

anova2 <-function(formula,data,na.action = "omit",intercept= TRUE)
{
  # Runs the lm2() function to get the needed values for anova
  output = lm2(formula,data,na.action,intercept)

  # Intializes the neede values from the return of lm2()
  y = output$y
  x = output$x
  SSE = output$SSE
  SSR = output$SSR
  SSY = SSR+SSE

  # Initializes empty vectors for desired outputs
  df = c()
  ss = c()
  MS = c()
  f = c()
  p = c()

  #Considers how SS will be calucated depending on if intercept was used or not.
  if(intercept==TRUE)
  {
    start = 2
  }
  else
  {
    start = 1
  }

  # For each covariate, calculates the sequential sum of squares and binds to
  # correct output vector.
  previous = 0
  for( i in (start:ncol(x)))
  {
    df = c(df,1)
    if(i ==1 && intercept==FALSE)
    {
      new_x= x[,1,drop=FALSE]

    }
    else if(i ==2&& intercept==TRUE)
    {
      new_x = x[,1:2,drop=FALSE]
    }
    else
    {
      new_x=x[,1:i,drop=FALSE]
    }
    inverse_prod = solve(t(new_x)%*%new_x)
    betas = inverse_prod%*%t(new_x)
    betas = betas%*%y
    fitted <- as.vector(new_x%*%betas)
    res = y - fitted

    SSY = sum((y-mean(y))^2)
    SSE = sum(res^2)
    SSR = SSY-SSE
    s = SSR - previous
    ss=c(ss,s)
    MS=c(MS,s)
    previous = SSR

  }

  # Converts the sequential sum of squares to the f statistics values
  f= ss/(SSE/(nrow(x)-ncol(x)))

  # For each f statistic value, convert it into p values
  for(i in (1:length(f)))
  {
    p=c(p,pf(f[i],1,nrow(x)-ncol(x),lower.tail=FALSE))
  }

  # For residuals, correctly add it to the output vectors.
  df = c(df,(nrow(x)-ncol(x)))
  ss= c(ss,SSE)
  MS=c(MS,(SSE/(nrow(x)-ncol(x))))
  f = c(f,NA)
  p=c(p,NA)

  # Creates the output matrix for the return
  output=cbind(df,ss)
  output=cbind(output,MS)
  output=cbind(output,f)
  output=cbind(output,p)
  colnames(output)=c("Df","Sum Sq","Mean Sq","F value","Pr(>F)")


  return(output)
}