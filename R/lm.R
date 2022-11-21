#' Linear Regression and Hypothesis Testing
#'
#' @description
#' Preprocess the data, fits a specified linear regression model, and performs
#' basic hypothesis testing. It is deisgned to copy some of the values from the
#' summary of the lm().
#'
#' @usage
#' lm2(formula,data,na.action)
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
#'
#' @return
#' A list that contains the following for the fitted linear regression model:
#' coefficients of the linear regression model, residuals, sigma, coefficient
#' of determination, adjusted coefficient of determination, unscaled covariance,
#' f statistic, variance-covariance matrix, SSE, SSR, SSY, design matrix, and
#' the response
#'
#' @examples
#' data = read.csv("data/melb_data.csv")
#' output = lm2(data$Price~data$Bedroom2+data$Bathroom+data$Landsize,melb_data,
#' na.action="omit")
#' betas = output$coefficients
#' res = output$residuals
#'
#' @export
#'
lm2 <- function(formula,data,na.action = "omit")
{

  # Considers how the missing values in the data will be handled
  if(any(is.na(data)))
  {
    if(na.action=="omit")
    {
      data = data[complete.cases(data),]
    }
    else if(na.action == "fail")
    {
      return(data)
    }
  }

  # Subset the data based on the covariates used in the formula
  covariates = all.vars(formula)[-1]
  #where = which(colnames(data)==covariates)
  where = 1:length(covariates)
  for(i in 1:length(where))
  {
     where[i] = which(colnames(data)==covariates[i])
   }
  data = data[,where]


  # Parse the formula parameter to get design matrix x and repsonse y
  new_data = model.matrix(formula,data)
  frame <- model.frame(formula, data)
  y = frame[1][,1]
  x = new_data


  # Size of the design matrix x
  n = nrow(x)
  p = ncol(x)

  # Estimation of the coefficients for the specficed linear regression model
  inverse_prod = solve(t(x)%*%x)
  betas = inverse_prod%*%t(x)
  betas = betas%*%y


  # Estimation of the residuals and sum of squares
  fitted <- as.vector(x%*%betas)
  res = as.vector(y - x%*%betas)

  # Estimation of sum of squares
  SSY = sum((y-mean(y))^2)
  SSE = sum(res^2)
  SSR = SSY-SSE

  # Coefficient of determination
  r_squared = SSR/SSY
  adjusted_r_squared = 1- ( ((1-r_squared)*(n-1))/(n-p))

  #Sigma and variance/standard error
  s = sqrt(SSE/(n-p))
  unscaled_cov = solve(t(x) %*% x)
  var_cov= solve(t(x) %*% x)*(s^2)
  std_error = sqrt(diag(solve(t(x) %*% x)))*s


  # Performs hypothesis Testing (T-test/F-test)

  # T-test
  t = betas / std_error
  p_value = 2*pt(abs(t),(n-p),lower.tail=FALSE)

  # F-test
  f = (SSR/(p-1))
  f = f/(SSE/(n-p))
  fstatistic=cbind(f,p-1)
  fstatistic = cbind(fstatistic,n-p)
  colnames(fstatistic)=c("value","numdf","dendf")

  # Creates the coefficient matrix from the values for each of the coefficient
  Coefficients = cbind(betas,std_error)
  Coefficients = cbind(Coefficients,t)
  Coefficients = cbind(Coefficients,p_value)
  colnames(Coefficients) = c("Estimate", "Std. Error", "t value","Pr(>|t|)")

  # Returns as a list for similar values from lm()
  return(list("coefficients"=Coefficients,"residuals"= res,"sigma"=as.numeric(s)
              ,"r.squared"=(SSR/SSY),"adj.r.squared"=adjusted_r_squared,
              "cov.unscaled"=unscaled_cov,"fstatistic"=fstatistic,
              "var_cov"=var_cov,"SSE"=SSE,"SSR"=SSR,"SSY"=SSY,"x"=x,"y"=y))
}
