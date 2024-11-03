#' @name ridgereg
#' @title A Reference Class for Calculation of Ridge Regression using LR and QR Decomposition
#' @description This class performs ridge regression using least square estimation and QR decomposition.
#' @param formula Formula. The equation to be fitted.
#' @param data Data.Frame. The data frame capturing the data.
#' @param lambda Numeric. Captures the value of lambda.
#' @param qrDecomposition Logical. Default=FALSE or least square estimation. Specifies if QR Decomposition is to be used for estimation.
#' @rawNamespace import(MASS, except = select)
#' @import methods
#' @import caret
#' @import mlbench
#' @import leaps
#' @importFrom methods setClass setMethod new
#' @export
#' 
#' @examples
#' data("iris")
#' 
#' # Using Least Square Estimation Method for estimation
#' ## With Intecept
#' a<-ridgereg$new(formula=Petal.Length~Sepal.Width+Sepal.Length,data=iris,lambda=1)
#' 
#' #Scaled Coeffecients:
#' a$coef()
#' 
#' #Un-Scaled Coeffecients
#' a$unScaledCoefficients
#' 
#' #Scale Factor:
#' a$scales
#' 
#' #Predicted Values:
#' a$predict()
#' 
#' #Residual Values:
#' a$resid()
#' 
#' #Print Fitted Equation:
#' a$print()
#' 
#' ## Without Intecept
#' a<-ridgereg$new(formula=Petal.Length~Sepal.Width+Sepal.Length-1,data=iris,lambda=1)
#' 
#' # Using QR Decomposition Method for estimation
#' a<-ridgereg$new(formula=Petal.Length~Sepal.Width+Sepal.Length,data=iris,lambda=1,
#'     qrDecomposition=TRUE)
#' 
#' @seealso
#' \url{https://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression}

options(repos = list(CRAN="https://cloud.r-project.org/"))

ridgereg <- setRefClass("ridgereg",
                      fields=list(
                        formula="formula",
                        data="data.frame",
                        lambda="numeric",
                        qrDecomposition="logical",
                        data_str="character",
                        formula_str="character",
                        X="matrix",
                        y="numeric",
                        xs="matrix",
                        scales="numeric",
                        coefficients="matrix",
                        unScaledCoefficients="vector",
                        residuals="matrix",
                        y_hat="matrix",
                        newData="data.frame"
                      ),
                      methods = list(
                        initialize=function(formula,data,lambda,qrDecomposition=FALSE){
                          stopifnot(all(all.vars(formula) %in% colnames(data)))
                          stopifnot(is.data.frame(data))
                          stopifnot((is.numeric(lambda))&&(is.atomic(lambda)))
                          formula<<-as.formula(formula)
                          data_str <<- deparse(substitute(data))
                          formula_str <<- deparse(substitute(formula))
                          y<<-data[[attr(terms(formula),which="variables")[2][[1]]]]
                          X<<-model.matrix(formula,data)
                          p<-ncol(X)
                          n<-nrow(X)
                          xs<<-X
                          if(attr(terms(formula), "intercept")){
                            p<-p-1
                            X<<-X[,-1]
                            xs<<-(X-rep(colMeans(X), rep(n, p)))
                          }
                          variance<-((apply(xs^2,2,sum)))/n
                          scales<<-sqrt(variance)
                          xs<<-xs/matrix(scales,nrow = n,ncol = p,byrow = TRUE)
                          if(qrDecomposition){
                            QR<-qr(xs)
                            Q<-qr.Q(QR)
                            R<-qr.R(QR)
                            QtQ <- t(Q) %*% Q
                            coefficients <<- backsolve(((R%*%QtQ)+lambda*(QtQ%*%solve(t(R)))), (t(Q)%*%y))
                            rownames(coefficients)<<-colnames(xs)
                          }else{
                            coefficients<<-solve(crossprod(as.matrix(xs))+lambda*diag(p))%*%t(xs)%*%(y)
                          }
                          y_hat<<-as.matrix(xs)%*%coefficients
                          if(attr(terms(formula), "intercept")){
                            y_hat<<-y_hat + mean(y)
                            unScaledCoefficients<<-as.vector(c(mean(y)-t(coefficients/scales)%*%colMeans(X),coefficients/scales))
                            names(unScaledCoefficients)<<-c("(Intercept)",rownames(coefficients))
                          }else{
                            unScaledCoefficients<<-as.vector(coefficients/scales)
                            names(unScaledCoefficients)<<-rownames(coefficients)
                            }
                          residuals<<-(y-y_hat)
                          },
                        show = function(self.unScaledCoefficients,self.data_str,self.formula_str) {
                          cat(paste0("ridgereg(formula = ",
                                     gsub("~", " ~ ",
                                          gsub("\\+", " \\+ ",
                                               gsub("\\.", "\\.", 
                                                    gsub(" ", "", formula_str)))), 
                                     ", data = ", data_str, ")"))
                          cat("\n",names(unScaledCoefficients), "\n",unScaledCoefficients)
                          },
                        coef = function(self.X,self.coefficients){
                          coefficient_output<-as.vector(coefficients)
                          names(coefficient_output)<-colnames(X)
                          cat("Coefficients: \n")
                          coefficient_output
                          },
                        predict = function(self.y_hat,self.formula,self.unScaledCoefficients,newData=NULL){
                          if(is.null(newData)){
                            as.vector(y_hat)
                          }else{
                            nd<-model.matrix(formula,newData)
                            as.vector(nd%*%as.matrix(myModelRidreg$unScaledCoefficients))
                            }
                          },
                        resid = function(self.residuals) {
                          residuals
                          }
                      )
)