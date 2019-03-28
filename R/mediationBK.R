#' Perform mediation analysis by Baron and Kenny Method
#' @param X name of independent variable
#' @param M name of mediator variable
#' @param Y name of dependent variable
#' @param labels An optional list of variable names
#' @param data A data.frame
#' @param silent Logical. Whether or not show summary of regression tests
#' @param indirect.test Logical. Whether or not show results of bda::mediation.test
#' @importFrom bda mediation.test
#' @export
#' @examples
#' labels=list(X="wt",M="hp",Y="mpg")
#' mediationBK(labels=labels,data=mtcars)
mediationBK=function(X=NULL,M=NULL,Y=NULL,labels=list(),data,silent=TRUE,indirect.test=TRUE){

    if(is.null(X)) X=labels$X
    if(is.null(M)) M=labels$M
    if(is.null(Y)) Y=labels$Y

    dataname=substitute(data)
    Paths=c("Path C (Total Effect)","Path A (X on M)","Path B (M on Y, controlling for X)","Reversed Path C (Y on X, controlling for M)")
    if(!silent) cat("Step 1: ",Paths[1],"\n-Estimate the relationship between X on Y\n")
    temp1=paste0("lm(",Y,"~",X,",data=",dataname,")")
    fit1=eval(parse(text=temp1))
    if(!silent) print(summary(fit1))
    if(!silent) cat("\nStep 2:",Paths[2],"\n-Estimate the relationship between X on M\n")
    temp2=paste0("lm(",M,"~",X,",data=",dataname,")")
    fit2=eval(parse(text=temp2))
    if(!silent) print(summary(fit2))
    if(!silent) cat("\nStep 3:",Paths[3],"\n-Estimate the relationship between M on Y, controlling for X\n")
    temp3=paste0("lm(",Y,"~",M,"+", X,",data=",dataname,")")
    fit3=eval(parse(text=temp3))
    if(!silent) print(summary(fit3))
    if(!silent) cat("\nStep 4:",Paths[4],"\n-Estimate the relationship between Y on X, controlling for M\n")
    temp4=paste0("lm(",X,"~",Y ,"+", M,",data=",dataname,")")
    fit4=eval(parse(text=temp4))
    if(!silent) print(summary(fit4))
    fit=list(fit1,fit2,fit3,fit4)
    equations=list(temp1,temp2,temp3,temp4)
    results=list()
    results[[1]]=ifelse(summary(fit[[1]])$coef[2,4]<0.05,"Acceptable","Not satisfied")
    results[[2]]=ifelse(summary(fit[[2]])$coef[2,4]<0.05,"Acceptable","Not satisfied")
    results[[3]]=ifelse(summary(fit[[3]])$coef[2,4]<0.05,"Acceptable","Not satisfied")
    results[[4]]=ifelse(summary(fit[[4]])$coef[2,4]>0.05,"Acceptable","Not satisfied")
    cat("\nResults of Baron and Kenny Method\n")
    for(i in 1:4){
        cat("Step",i,":", Paths[i],"-", results[[i]],"\n")
    }
    indirect=bda::mediation.test(data[[M]],data[[X]],data[[Y]])
    if(indirect.test){
        cat("\nResults of bda::mediation.test\n")
        print(indirect)
    }
    invisible(list(fit=fit,equations=equations,results=results,indirect=indirect))
}
