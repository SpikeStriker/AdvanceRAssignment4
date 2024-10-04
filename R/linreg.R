#' @name linreg
#' @title A Reference Class for Calculation of Linear Regression using QR Decomposition
#' @description This class performs linear regression using QR decomposition.
#' @field formula Formula. The equation to be fitted.
#' @field data Data.Frame. The data frame capturing the data.
#' @import methods
#' @import ggplot2
#' @import gridExtra
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom gridExtra grid.arrange
#' @export
#' 
options(repos = list(CRAN="https://cloud.r-project.org/"))


if (!requireNamespace("methods", quietly = TRUE)) {
  install.packages("methods")
}
library(methods)
if(!require("ggplot2")) install.packages("xtable", dependencies = TRUE)
library(ggplot2)
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}
library(gridExtra)


linreg <- setRefClass("linreg",
                      fields=list(
                        formula="formula",
                        data="data.frame",
                        data_str="character",
                        formula_str="character",
                        X="matrix",
                        y="numeric",
                        Q="matrix",
                        R="matrix",
                        coefficients="matrix",
                        residuals="numeric",
                        y_hat="matrix",
                        std_error="numeric",
                        dof="numeric",
                        std_dev="numeric",
                        var_beta="matrix",
                        t_value="matrix",
                        p_value="matrix",
                        residual_std_error="numeric",
                        summary_output="matrix"
                      ),
                      methods = list(
                        initialize=function(formula,data){
                          stopifnot(all(all.vars(formula) %in% colnames(data)))
                          data_str <<- deparse(substitute(data))
                          formula_str <<- deparse(substitute(formula))
                          y<<-data[[attr(terms(formula),which="variables")[2][[1]]]]
                          X<<-model.matrix(formula,data)
                          QR<-qr(X)
                          Q<<-qr.Q(QR)
                          R<<-qr.R(QR)
                          coefficients <<- backsolve(R, crossprod(Q,y))
                          y_hat<<-tcrossprod(Q)%*%y
                          residuals<<-as.vector(y-y_hat)
                          dof<<-length(y)-QR$rank
                          std_dev<<-sum((y-y_hat)^2)/dof
                          residual_std_error<<-sqrt(std_dev)
                          var_beta <<- std_dev*chol2inv(qr.R(QR))
                          std_error <<- sqrt(diag(var_beta))
                          t_value<<-coefficients/std_error
                          p_value<<-2*(1-pt(abs(t_value),dof))
                          summary_output<<-cbind(coefficients,std_error,t_value,p_value)
                          rownames(summary_output)<<-colnames(X)
                          colnames(summary_output)<<-c("Coefficients","Std.Error", "t-value","p-value")
                        },
                        print = function(self.X,self.coefficients,self.data_str,self.formula_str) {
                          coefficient_output<-as.vector(coefficients)
                          names(coefficient_output)<-colnames(X)
                          # cat("linreg (formula =",trimws(formula_str),", data =",trimws(data_str),")\n")
                          cat(paste0("linreg(formula = ",
                                     gsub("~", " ~ ",
                                          gsub("\\+", " \\+ ",
                                               gsub("\\.", "\\.", 
                                                    gsub(" ", "", formula_str)))), 
                                     ", data = ", data_str, ")"))
                          cat(names(coefficient_output), "\n",coefficient_output)
                        },
                        coef = function(self.X,self.coefficients){
                          coefficient_output<-as.vector(coefficients)
                          names(coefficient_output)<-colnames(X)
                          cat("Coefficients: \n")
                          coefficient_output
                        },
                        pred = function(self.y_hat){
                          as.vector(y_hat)
                        },
                        resid = function(self.residuals) {
                          residuals
                        },
                        summary = function(self.summary_output,self.dof,self.residual_std_error) {
                          cat(paste(colnames(summary_output), collapse = " "), "\n")
                          for (i in 1:nrow(summary_output)) {
                            if (summary_output[i,4] < 0.001) {
                              cat(rownames(summary_output)[i], summary_output[i, ],"***")
                            } else if (summary_output[i,4] < 0.01) {
                              cat(rownames(summary_output)[i], summary_output[i, ],"**")
                            } else if (summary_output[i,4] < 0.05) {
                              cat(rownames(summary_output)[i], summary_output[i, ],"*")
                            } else {
                              cat(rownames(summary_output)[i], summary_output[i, ])
                            }
                            cat("\n")
                          }
                          cat("Residual standard error:", residual_std_error, "on", dof, "degrees of freedom")
                        },
                        plot = function(self.data,self.y_hat,self.residuals,self.formula_str){
                          par(mfrow = c(2, 1))
                          p1<-ggplot(data,aes(x=round(y_hat,3),y=round(residuals,3)))+geom_point(size=0.5)+stat_summary(
                            fun=median,colour="red",geom="path",size=1)+labs(
                              title="Residuals vs Fitted",
                              caption=paste("lm(",trimws(formula_str),")"),
                              x="Fitted values",
                              y="Residuals")+
                            theme(plot.title = element_text(hjust = 0.5),
                                  plot.caption = element_text(hjust = 0.5))
                          
                          p2<-ggplot(data,aes(x=round(y_hat,3),y=round(sqrt(residuals^2),3)))+geom_point(size=0.5)+stat_summary(
                            fun=median,colour="red",geom="path",size=1)+
                            labs(title="Scale Location",
                                 caption=paste("lm(",trimws(formula_str),")"),
                              x="Fitted values",
                              y="Standardized Residuals")+
                            theme(plot.title = element_text(hjust = 0.5),
                                  plot.caption = element_text(hjust = 0.5))
                          grid.arrange(p1, p2, ncol=1)
                        }
                      )
)
