#'Summary of moderation effect
#'@param fit An object of class lm
#'@param X Name of independent variable
#'@param W Name of the first moderator variable
#'@param Z Name of the second moderator variable
#'@param labels Optional list of variable names
#'@param modx.values Values of moderator variable
#'@param mod2.values Values of the second moderator variable
#'@param rangemode Integer. 1 or 2.
#'@param maxylev maximum unique length of variable to be treated as a categorical variable
#'@param digits integer indicating the number of decimal places
#'@export
#'@examples
#'fit=lm(govact~negemot*sex+negemot*age+posemot+ideology,data=glbwarm)
#'modSummary3(fit,mod2.values=c(30,50,70))
#'fit1=lm(govact~negemot*sex*age+posemot+ideology,data=glbwarm)
#'modSummary3(fit1,rangemode=1)
#'fit=lm(mpg~hp*wt,data=mtcars)
#'modSummary3(fit)
modSummary3=function(fit,X=NULL,W=NULL,Z=NULL,labels=NULL,modx.values=NULL,
                     mod2.values=NULL,rangemode=2,maxylev=6,digits=3){

     # X="hp";W="wt";Z=NULL;modx.values=NULL;labels=NULL
     # rangemode=2;maxylev=6;digits=3;mod2.values=NULL

    data=fit$model
    if(is.null(X)){ X=labels$X }
    if(is.null(X)){ X=names(data)[2]}
    if(is.null(W)){ W=labels$W }
    if(is.null(W)){ W=names(data)[3]}
    if(is.null(Z)){ Z=labels$Z }
    if(is.null(Z)) {
        if(length(names(data))>3) Z=names(data)[4]
    }



    addz=TRUE
    if(is.null(Z)) addz=FALSE
    labelW=W
    labelZ=Z
    vars=names(fit$coef)
    select=str_detect(vars,X)
    coef=fit$coef[select]
    count=length(coef)

    vars1<- vars[select] %>%
        str_replace(W,"W") %>%
        str_replace_all(":","*") %>%
        str_replace(X,"1")

    if(addz) vars1=str_replace(vars1,Z,"Z")

    temp<-paste0("coef[",1:count,"]*",vars1) %>%
        str_replace("\\*1","") %>%
        paste0(collapse="+")

    temp

    temp1=paste0(round(coef,digits=3),"*",vars1) %>%
        str_replace("\\*1","") %>%
        paste0(collapse="+") %>%
        str_replace("\\+-","-")
    temp1

    cat("Conditional Effect = ",temp1,"\n")


    if(is.null(modx.values)){
        W1=getRepValues(data,W,rangemode=rangemode,maxylev=maxylev,digits=digits)
    } else{
        W1=modx.values
    }
    if(addz){
    if(is.null(mod2.values)){
        Z1=getRepValues(data,Z,rangemode=rangemode,maxylev=maxylev,digits=digits)
    } else{
        Z1=mod2.values
    }
    }

    w<-z<-y<-c()
    W1


    if(addz) {
        for(i in seq_along(W1)){
            for(j in seq_along(Z1)){
                W=W1[i]
                if(addz) Z=Z1[j]
                w=c(w,W1[i])
                if(addz) z=c(z,Z1[j])
                y=c(y,eval(parse(text=temp)))
            }
        }
        df=data.frame(W=w,Z=z,slope=round(y,digits))
        colnames(df)=c(paste0(labelW,"(W)"),paste0(labelZ,"(Z)"),"slope")
    } else{
        for(i in seq_along(W1)){
                W=W1[i]
                w=c(w,W1[i])
                y=c(y,eval(parse(text=temp)))

        }
        df=data.frame(W=w,slope=round(y,digits))
        colnames(df)=c(paste0(labelW,"(W)"),"slope")
    }
    attr(df,"eq")=temp1
    df

}

#' Get representative values
#' @param data A data.frame
#' @param colname Name of column
#' @param rangemode Integer. 1 or 2
#' @param maxylev Integer. Maximum unique length of variable to be treated as a categorical variable
#'@param digits integer indicating the number of decimal places
#' @export
getRepValues=function(data,colname,rangemode=2,maxylev=6,digits=digits){
    modx=colname
    if(length(unique(data[[modx]]))<=maxylev) {
        values=unique(data[[modx]])
    } else if(rangemode==1){
        values=mean(data[[modx]],na.rm=T)+c(-1,0,1)*sd(data[[modx]],na.rm=T)
        values=round(values,digits)
    } else{
        values=quantile(data[[modx]],probs=c(0.16,0.5,0.84),type=6)
        values=round(values,digits)
    }
    values
}

#' Make flextable summarizing moderation effect
#' @param x An object
#' @param vanilla logical
#' @param ... Further argument to be passed to modSummary3
#' @export
#' @examples
#' fit=lm(govact~negemot*sex+negemot*age+posemot+ideology,data=glbwarm)
#' modSummary3Table(fit,mod2.values=c(30,50,70))
modSummary3Table=function(x,vanilla=TRUE,...){
    if("lm" %in% class(x)) x=modSummary3(x,...)
    rrtable::df2flextable(x,vanilla=vanilla,digits=3) %>%
        add_footer_lines(attr(x,"eq") ) %>%
        align(align="right",part="footer")
}

