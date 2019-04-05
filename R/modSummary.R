#' Make table summarizing moderation effect
#'@param fit An object of class lm
#'@param pred name of predictor variable
#'@param modx name of moderator variable
#'@param pred.values Values of predictor variables
#'@param modx.values Values of modifier variables
#'@param rangemode integer. 1 or 2
#'@param digits integer indicating the number of decimal places
#'@export
#'@examples
#'fit=lm(justify~skeptic*frame,data=disaster)
#'modSummary(fit)
modSummary=function(fit,pred=NULL,modx=NULL,pred.values=NULL,modx.values=NULL,
                    rangemode=2,digits=3){
    data=fit$model

    dep=colnames(data)[1]
    if(is.null(pred)) pred=colnames(data)[2]
    if(is.null(modx)) modx=colnames(data)[3]



    if(is.null(pred.values)){
        if(rangemode==1) {
            pred.values=mean(data[[pred]],na.rm=TRUE)+c(-1,0,1)*sd(data[[pred]],na.rm=TRUE)
        } else if(rangemode==2) {
            pred.values=quantile(data[[pred]],probs=c(0.16,0.5,0.84),type=6)
        }
    }

    if(is.null(modx.values)){
        if(length(unique(data[[modx]]))<6) {
            modx.values=unique(data[[modx]])
        } else if(rangemode==1){
            modx.values=mean(data[[modx]],na.rm=T)+c(-1,1)*sd(data[[modx]],na.rm=T)
        } else{
            modx.values=quantile(data[[modx]],probs=c(0.16,0.84),type=6)
        }
    }

    df1=calEquation(fit,modx.values = modx.values)
    df1[[modx]]=modx.values

    df=tidyr::crossing(pred.values,modx.values)
    names(df)=c(pred,modx)

    df<-dplyr::left_join(df,df1)

    df[[dep]]=df$slope*df[[pred]]+df$intercept

    df<-df[c(pred,modx,dep)]
    colnames(df)=c(paste0(pred,"(X)"),paste0(modx,("(W)")),"\u0176")
    df[]=lapply(df,myformat,digits)
    coef=round(fit$coef,digits)

    temp=paste0("\u0176 = ",coef[1], coef2str(coef[2]),"X",
                coef2str(coef[3]),"W",coef2str(coef[4]),"XW")
    attr(df,"eq")=temp
    class(df)=c("modSummary","data.frame")
    df
}

#'coonvert number to signed string
#'@param x A number
#'@export
coef2str=function(x){
    paste0(ifelse(sign(x)>=0," + "," - "),abs(x))
}

#'S3 method of print
#'@param x An object of class modSummary
#'@param ... Further argument to be passed to print()
#'@export
print.modSummary=function(x,...){
    cat("\nSummary of moderation effect\n")
    cat("\nEquation: ",attr(x,"eq"),"\n\n")
    class(x)="data.frame"
    print(x)
}


#' Make flextable summarizing moderation effect
#' @param x An object
#' @param vanilla logical
#' @param ... Further argument to be passed to modSummary
#' @export
#' @importFrom flextable add_footer_lines align
#'@examples
#'fit=lm(justify~skeptic*frame,data=disaster)
#'modSummaryTable(fit)
modSummaryTable=function(x,vanilla=TRUE,...){
    if("lm" %in% class(x)) x=modSummary(x,...)
    rrtable::df2flextable(x,vanilla=vanilla) %>%
        add_footer_lines(attr(x,"eq")) %>%
        align(align="right",part="footer")
}


