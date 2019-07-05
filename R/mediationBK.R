#' Perform mediation analysis by Baron and Kenny Method
#' @param X name of independent variable
#' @param M name of mediator variable
#' @param Y name of dependent variable
#' @param labels An optional list of variable names
#' @param data A data.frame
#' @param silent Logical. Whether or not show summary of regression tests
#' @param indirect.test Logical. Whether or not show results of bda::mediation.test
#' @param sig significant level. default value is 0.05
#' @importFrom bda mediation.test
#' @importFrom stats pnorm qnorm
#' @export
#' @examples
#' labels=list(X="cond",M="pmi",Y="reaction")
#' result=mediationBK(labels=labels,data=pmi,silent=FALSE)
#' result
mediationBK=function(X=NULL,M=NULL,Y=NULL,labels=list(),data,silent=TRUE,indirect.test=TRUE,sig=0.05){

        # X=NULL;M=NULL;Y=NULL;silent=FALSE;indirect.test=TRUE;data=pmi
    if(is.null(X)) X=labels$X
    if(is.null(M)) M=labels$M
    if(is.null(Y)) Y=labels$Y

    dataname=substitute(data)
    Paths=c("Path C (Total Effect)","Path A (X on M)","Path B (M on Y, controlling for X)","Path C' (Direct Effect, X on Y, controlling for M)")
    if(!silent) cat("Step 1: ",Paths[1],"\n-Estimate the relationship between X on Y\n")
    temp1=paste0("lm(",Y,"~",X,",data=",dataname,")")
    fit1=eval(parse(text=temp1))
    if(!silent) print(summary(fit1))
    if(!silent) cat("\nStep 2:",Paths[2],"\n-Estimate the relationship between X on M\n")
    temp2=paste0("lm(",M,"~",X,",data=",dataname,")")
    fit2=eval(parse(text=temp2))
    if(!silent) print(summary(fit2))
    a=fit2$coef[2]
    sea=summary(fit2)$coef[2,2]
    if(!silent) cat("\nStep 3:",Paths[3],"\n-Estimate the relationship between M on Y, controlling for X\n")
    temp3=paste0("lm(",Y,"~",M,"+", X,",data=",dataname,")")
    fit3=eval(parse(text=temp3))
    if(!silent) print(summary(fit3))
    b=fit3$coef[2]
    seb=summary(fit3)$coef[2,2]
    if(!silent) cat("\nStep 4:",Paths[4],"\n-Estimate the relationship between Y on X, controlling for M\n")
    fit=list(fit1,fit2,fit3)
    equations=list(temp1,temp2,temp3)
    results=list()
    coef=c()
    pvalue=c()
    for(i in 1:3){
        coef=c(coef,summary(fit[[i]])$coef[2,1])
        pvalue=c(pvalue,summary(fit[[i]])$coef[2,4])
    }
    coef=c(coef,summary(fit[[i]])$coef[3,1])
    pvalue=c(pvalue,summary(fit[[i]])$coef[3,4])
    results[[1]]=ifelse(pvalue[1]<0.05,"Acceptable","Not satisfied")
    results[[2]]=ifelse(pvalue[2]<0.05,"Acceptable","Not satisfied")
    results[[3]]=ifelse(pvalue[3]<0.05,"Acceptable","Not satisfied")
    if(pvalue[4]>0.05) {
        results[[4]]="Complete mediation"
    } else if(coef[4]<coef[1]) {
        results[[4]]="Partial mediation"
    } else results[[4]]="Not satisfied"

    # cat("\nResults of Baron and Kenny Method\n")
    # name=c("c","a","b","c'")
    # for(i in 1:4){
    #     temp=pvalue[i]
    #     if(temp<0.001) temp="< 0.001"
    #     else temp=paste0("= ",sprintf("%0.3f",temp))
    #     cat("Step",i,":", Paths[i],":",name[i],"=",sprintf("%0.3f",coef[i]),"( p",temp,")\n")
    # }
    # cat("Result :",results[[4]],"\n")
    indirect=bda::mediation.test(data[[M]],data[[X]],data[[Y]])
    # if(indirect.test){
    #     cat("\nResults of bda::mediation.test\n\n")
    #     print(indirect)
    # }
    seab=sqrt((a^2)*(seb^2)+(b^2)*(sea^2)+(sea^2)*(seb^2))
    Z=(a*b)/seab
    normalTheory=c(ab=a*b,seab=seab,z=Z,p=pnorm(Z,lower.tail = FALSE)*2,
                   ci.lower=a*b+qnorm(sig/2)*seab,ci.upper=a*b-qnorm(sig/2)*seab)

    names(normalTheory)=c("ab","seab","z","p","ci.lower","ci.upper")
    result=list(labels=labels,fit=fit,equations=equations,coef=coef,pvalue=pvalue,results=results,indirect=indirect,normalTheory=normalTheory)
    class(result)="mediationBK"
    invisible(result)
}

#' S3 method for class mediationBK
#' @param x An object of class mediationBK
#' @param ... Further arguments to be passed to print()
#' @export
print.mediationBK=function(x,...){
    Paths=c("Path C (Total Effect)","Path A (X on M)",
            "Path B (M on Y, controlling for X)","Path C' (Direct Effect, X on Y, controlling for M)")
    cat("\nResults of Baron and Kenny Method\n")
    name=c("c","a","b","c'")
    for(i in 1:4){
        temp=x$pvalue[i]
        if(temp<0.001) temp="< 0.001"
        else temp=paste0("= ",sprintf("%0.3f",temp))
        cat("Step",i,":", Paths[i],":",name[i],"=",sprintf("%0.3f",x$coef[i]),"( p",temp,")\n")
    }
    cat("Result :",x$results[[4]],"\n")
    cat("\nNormal theory test for indirect effect(s) :\n\n")
    print(x$normalTheory)
    cat("\nResults of bda::mediation.test\n\n")
    print(x$indirect)

}

#' S3 method for class mediationBK
#' @param object An object of class mediationBK
#' @param ... Further arguments to be passed to summary()
#' @export
summary.mediationBK=function(object,...){
    modelsSummary(labels=object$labels,object$fit)
}

#' S3 method for class mediationBK
#' @param x An object of class mediationBK
#' @param ... Further arguments to be passed to plot()
#' @importFrom rlang enexprs
#' @examples
#' labels=list(X="cond",M="pmi",Y="reaction")
#' result=mediationBK(labels=labels,data=pmi)
#' plot(result,type=1)
#' plot(result)
#' plot(result,type=1,whatLabel="label",arrowslabels="c",addprime=FALSE)
#' plot(result,whatLabel="label",arrowslabels=c("a","b","c"))
#' @export
plot.mediationBK=function(x,...){

    vars=rlang::enexprs(...)

    type=0
    if(!is.null(vars$type)){
        if(vars$type==1) type=1
    }
    whatLabel="est"
    if(!is.null(vars$whatLabel)){
        whatLabel=vars$whatLabel
    }

    if(type==1){
        lty=ifelse(x$pvalue[1]<0.05,1,2)
        if(whatLabel=="est"){
        statisticalDiagram(0,labels=x$labels,arrowslabels=round(x$coef[1],3),arrowslty=lty,whatLabel = "label2",...)
        } else{
            statisticalDiagram(0,labels=x$labels,...)
        }

    } else{
        lty=c()
        for(i in 2:4){
            lty=c(lty,ifelse(x$pvalue[i]<0.05,1,2))
        }
        if(whatLabel=="est"){
        statisticalDiagram(4,labels=x$labels,arrowslabels=round(x$coef[2:4],3),arrowslty=lty,whatLabel = "label2",...)
        } else{
         statisticalDiagram(4,labels=x$labels,...)
        }
    }
}
