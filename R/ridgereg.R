#'@title Do you want to make a Ridge Regression?
#'@description This function make a "Reference Class" with class=ridgereg for a Ridge Regression
#'
#'
#'
#' @field formula The formula for the model
#' @field data A object of class data.frame
#' @field lambda The lambda value
#' 
#'@examples
#' data(iris)
#' ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,0.5)$print()
#' ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,0.5)$predict()
#' ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,0.5)$coef()
#' @export ridgereg
#' @export 
#' 
#' 
ridgereg<-setRefClass("ridgereg", fields = list(formula="formula", 
                                                Fits="list",
                                                Coef="list",
                                                data="data.frame",
                                                Call="character",
                                                lambda="numeric"),
                      methods = list(
                        
                        initialize = function(formula, data, lambda=0,normalize=TRUE){
                          
                          
                          ##### Beräkningar #####
                          x<-model.matrix(formula,data)
                          if(normalize==TRUE){
                            x[,2:ncol(x)]<-apply(x[,2:ncol(x)],2,function(a) (a-mean(a))/sd(a))
                          }
                          y_namn<-all.vars(formula)[1]
                          y<-as.matrix(data[,names(data)==y_namn])
                          data1<-data.frame(cbind(x[,2:ncol(x)],y))
                          names(data1)[ncol(data1)]<-y_namn
                          data<<-data1
                          
                          ############
                          # data<-data1
                          ############
                          ###
                          # data<-data1
                          # i=1
                          ###
                          
                          fits_lista<-list()
                          beta_lista<-list()
                          for ( i in 1:length(lambda)){
                            
                            anvanda_lambda<-lambda[i]
                            
                            I_lambda<-matrix(nrow=ncol(x),ncol=ncol(x),data = 0)
                            diag(I_lambda)<-anvanda_lambda
                            
                            
                            b_hat<-(solve((t(x)%*%x)+I_lambda))%*%(t(x)%*%y)
                            y_fits<-x%*%b_hat
                            e<-y-y_fits
                            
                            coef<-as.numeric(b_hat)
                            names(coef)<-rownames(b_hat)
                            
                            b_hat1<-as.numeric(b_hat)
                            names(b_hat1)<-rownames(b_hat)
                            
                            fits_lista[[i]]<-as.numeric(y_fits)
                            beta_lista[[i]]<-b_hat1
                          }
                          
                          
                          
                          ########### Spara beräkningar ###########
                          Call1<-character()
                          Call1[1] <-deparse(substitute(data))
                          Call1[2] <-Reduce(paste,deparse(formula))
                          Call1[3] <-deparse(substitute(lambda))
                          Call<<-paste("linreg(formula = ",Call1[2],", data = ",Call1[1],", lambda=",Call1[3],")",sep="")
                          
                          Fits<<-fits_lista
                          Coef<<-beta_lista
                          lambda<<-lambda
                          
                          #################################
                          # Call<-paste("linreg(formula = ",Call1[2],", data = ",Call1[1],", lambda=",Call1[3],")",sep="")
                          # Fits<-fits_lista
                          # Coef<-beta_lista
                          # lambda<-lambda
                          #################################
                        },
                        print = function(){
                          
                          beta_avrund<-lapply(Coef, function(a) round(a,5))
                          
                          namn<-c("lambda",names(beta_avrund[[1]]))
                          
                          i=1
                          j=1
                          langd_namn<-nchar(namn)
                          for(i in 1:length(beta_avrund)){
                            beta_avrund[[i]]<-as.character(c(lambda[i],beta_avrund[[i]]))
                            for(j in 1:length(namn)){
                              beta_avrund[[i]][j]<-format(beta_avrund[[i]][j], width=langd_namn[j],justify = c("right"))
                            }
                            
                          }
                          
                          cat("Call:",sep="\n")
                          cat(Call, sep="\n")
                          cat(sep="\n")
                          cat("Coefficients:",sep="\n")
                          cat(paste(namn,collapse = "  "),sep="",collapse="\n")
                          for(i in 1:length(beta_avrund)){
                            cat(paste(beta_avrund[[i]],collapse = "  "),sep="",collapse="\n")
                          }
                        },
                        predict = function(values=NULL){
                          
                          
                          if(is.null(values)==TRUE){
                            if(length(Fits)==1){
                              svar<-Fits[[1]]
                            }
                            else svar<-Fits
                            return(svar)
                          
                          }
                          
                          
                          
                          if (is.null(values)==FALSE){
                            if((all(names(values)==names(Coef[[1]][2:length(Coef[[1]])])))==FALSE){
                              stop("Vaiables not matchs")
                            }
                            
                            values2<-cbind("(Intercept)"=1,values)
                            
                            
                            predict<-list()
                            for(i in 1: length(lambda)){
                              predict[[i]]<-as.numeric((Coef[[i]])%*%t(as.matrix(values2)))
                            }
                            return(predict)
                          }
                            
                            
                            
                            
                            
                         
                        },
                        coef = function(){
                          
                          if(length(Coef)==1){
                            svar<-Coef[[1]]
                          }
                          else svar<-Coef
                          return(svar)
                        })

)
