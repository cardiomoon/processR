#' Calculate difference of R2 and adjusted R2
#' @param fit An object of class lm
#' @param mode Integer If 1, remove all interaction. If 2, remove variaables one by one
#' @param digits Integer indicating the number of decimal places
#' @importFrom stringr str_detect
#' @importFrom stats anova
#' @export
#' @examples
#' fit=lm(mpg~wt*hp,data=mtcars)
#' r2diff(fit)
#' r2diff(fit,mode=2)
r2diff=function(fit,mode=1,digits=3){

      # mode=1; digits=3;all.interaction=TRUE
    fit.r2=summary(fit)$r.squared
    fit.adj.r2=summary(fit)$adj.r.squared
    vars=names(fit$coef)[-1]
    df=fit$model
    r2=c()
    adj.r2=c()
    f=c()
    df1=c()
    df2=c()
    p=c()
    if(mode==1){
        temp=vars[!str_detect(vars,":")]
        eq=paste0(names(df)[1],"~",paste0(temp,collapse="+"))
        fit1=lm(as.formula(eq),data=df)
        r2=c(r2,summary(fit1)$r.squared)
        adj.r2=c(adj.r2,summary(fit1)$adj.r.squared)
        result=anova(fit1,fit)
        f=c(f,result$F[2])
        df1=c(df1,result$Df[2])
        df2=c(df2,summary(fit)$df[2])
        p=c(p,result$`Pr(>F)`[2])

    } else{
    for(i in 1:length(vars)){
        temp=vars[-i]
        eq=paste0(names(df)[1],"~",paste0(temp,collapse="+"))
        fit1=lm(as.formula(eq),data=df)
        r2=c(r2,summary(fit1)$r.squared)
        adj.r2=c(adj.r2,summary(fit1)$adj.r.squared)
        result=anova(fit1,fit)
        f=c(f,result$F[2])
        df1=c(df1,result$Df[2])
        df2=c(df2,summary(fit)$df[2])
        p=c(p,result$`Pr(>F)`[2])

    }
    }
    r2diff=fit.r2-r2
    ar2diff=fit.adj.r2-adj.r2
    df=data.frame(r2diff=r2diff,ar2diff=ar2diff,F=f,df1=df1,df2=df2,p=p)
    df
    if(mode!=1) rownames(df)=vars
    if(!is.null(digits)){
    for(i in c(1,2,3,6)){
      df[[i]]=myformat(df[[i]],digits)
    }
    df[[6]]=pformat(df[[6]])
    }
    if(mode==1){
        tempvars=vars[str_detect(vars,":")]
        cat("Test of highest order unconditional interaction(s)\n\n")

        if(length(tempvars)>0){
        cat("Removing : ",paste0(tempvars,collapse=","),"\n")
        temp=paste0("r2diff = ",df$r2diff,", F(",df$df1,",",df$df2,") = ",df$F,", p = ",df$p)
        cat(temp,"\n")
        } else{
            cat("There is no interaction in the model.")
        }

        invisible(df)
    } else{
        df
    }
}

