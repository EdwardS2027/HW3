#' Linear Regression and Hypothesis Testing
#'
#' @description
#' Preprocess the data, fits a specficed linear regression model, and performs
#' basic hypothesis testing
#'
#' @param formula
#' A framework for how should the linear regression model be given.
#'
#' @param data
#' A matrix of (nxp) whose data will be used to form the model.
#'
#' @param subset
#' The conditions of which rows in data will be used.
#'
#' @param na.action
#' How will the missing values be handled. It is defauled to "omit" value.
#'
#' @param intercept
#' A boolean value if the linear regression model will have an
#' intercept or not. It is defaulted to TRUE.
#'
#' @return
#' coefficients of the linear regression model, residuals, sigma, coefficient
#' of determination, adjusted coefficient of determination, unscaled covariance,
#' f statistic, variance-covariance matrix, SSE, SSR, SSY, design matrix, and
#' response
#'
#' @examples
#' data = read.csv("data/melb_data.csv")
#' output = lm2(data$Price~data$Bedroom2+data$Bathroom+data$Landsize,data,
#' na.action="omit",intercept=TRUE)
#' betas = output$coefficients
#' res = output$residuals
#'
lm2 <- function(formula,data,subset,na.action = "omit",intercept= TRUE)
{

  # Uses the subset parameter to take the subset of the data
  if(missing(subset)==FALSE)
  {
    data = data[,subset]
  }


  # Considers how the missing values in the data will be handled
  if(any(is.na(data)))
  {
    if(na.action=="omit")
    {
      which_row = which(is.na(data[[1]]))
      data = data[-which_row,]
    }
    else if(na.action == "fail")
    {
      return(data)
    }
  }

  #Formula
  new_data = model.matrix(formula,data)

  frame <- model.frame(formula, data=data)
  y = frame[1][,1]
  x = new_data
  if(intercept==FALSE)
  {
    x = x[,-1]
  }

  # Size of the design matrix x
  n = nrow(x)
  p = ncol(x)


  # Estimation of the coefficients for the specficed linear regression model
  inverse_prod = solve(t(x)%*%x)
  betas = inverse_prod%*%t(x)
  betas = betas%*%y


  # Estimation of the residuals and sum of squares
  fitted <- as.vector(x%*%betas)
  res = y - fitted

  SSY = sum((y-mean(y))^2)
  SSE = sum(res^2)
  SSR = SSY-SSE
  r_squared = SSR/SSY
  adjusted_r_squared = 1- ( ((1-r_squared)*(n-1))/(n-p))
  s = sqrt(SSE/(n-p))
  unscaled_cov = solve(t(x) %*% x)
  var_cov= solve(t(x) %*% x)*(s^2)
  std_error = sqrt(diag(solve(t(x) %*% x)))*s


  # Performs hypothesis Testing (T-test/F-test)
  t = betas / std_error
  p_value = 2*pt(abs(t),(n-p),lower.tail=FALSE)
  f = (SSR/(p-1))
  f = f/(SSE/(n-p))
  fstatistic=cbind(f,p-1)
  fstatistic = cbind(fstatistic,n-p)
  colnames(fstatistic)=c("value","numdf","dendf")

  Coefficients = cbind(betas,std_error)
  Coefficients = cbind(Coefficients,t)
  Coefficients = cbind(Coefficients,p_value)
  colnames(Coefficients) = c("Estimate", "Std. Error", "t value","Pr(>|t|)")
  return(list("coefficients"=Coefficients,"residuals"= res,"sigma"=s,"r.squared"=(SSR/SSY),"adj.r.squared"=adjusted_r_squared,"cov.unscaled"=unscaled_cov,"fstatistic"=fstatistic,"var_cov"=var_cov,"SSE"=SSE,"SSR"=SSR,"SSY"=SSY,"x"=x,"y"=y))
}

