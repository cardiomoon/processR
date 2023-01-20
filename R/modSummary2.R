#' Make table summarizing moderation effect
#' @param fit An object of class lm
#' @param rangemode An integer. If 1, mean+c(-1,0,1)*sd used. If 2, 16th, 50th and 84th percentiles are used
#' @param pred.values Values of predictor variables
#' @param summarymode An integer. 1 or 2. Summarizing method of variables. If 1, typical values are used. If 2, mean values are used
#' @param maxylev An integer. Maximum length of predictor variables to be treated as a categorical variable.
#' @param digits An integer indicating the number of decimal places
#' @param labels Optional list of labels of variables
#' @param ... Further arguments to be passed to predict3d::fit2newdata()
#' @importFrom predict3d fit2newdata
#' @importFrom stringr str_detect
#' @export
#' @examples
#' labels=list(X="negemot",W="sex",Z="age",Y="govact",C1="posemot",C2="ideology")
#' fit=lm(govact~negemot*sex+negemot*age+posemot+ideology,data=glbwarm)
#' modSummary2(fit,rangemode=2,mod2.values=c(30,50,70),summarymode=2)
#' modSummary2(fit,mod2.values=c(30,50,70),summarymode=1,labels=labels)
#' labels=list(X="frame",W="skeptic",Y="justify")
#' fit=lm(justify~frame*skeptic,data=disaster)
#' modSummary2(fit,labels=labels)
modSummary2=function(fit,rangemode=2,pred.values=NULL,summarymode=2,maxylev=6,digits=3,labels=NULL,...){
    # rangemode=2;pred.values=NULL;summarymode=2;maxylev=6;digits=3
    temp=names(fit$coef)[str_detect(names(fit$coef),":")]
    vars=unique(unlist(strsplit(temp,":")))
    if(is.null(pred.values)){
        x=fit$model[[vars[1]]]
        if(length(unique(x))<=maxylev){
            pred.values=sort(unique(x))
        } else if(rangemode==1) {
            pred.values=mean(x,na.rm=TRUE)+c(-1,0,1)*sd(x,na.rm=TRUE)
        } else{
            pred.values=quantile(x,probs=c(0.16,0.50,0.84),type=6)
        }
    }
    df=fit2newdata(fit,predictors=vars,mode=rangemode,pred.values=pred.values,
                summarymode=summarymode,...)
    # df=fit2newdata(fit,predictors=vars,mode=rangemode,pred.values=pred.values,
                 # summarymode=summarymode)

    df[]=lapply(df,myformat,digits=digits)
    if(!is.null(labels)) {
        finalNames=changeLabelName(colnames(df),labels,add=TRUE)
        colnames(df)=finalNames
    }
    df
}

#' Make flextable summarizing moderation effect
#' @param x An object
#' @param vanilla logical
#' @param ... Further argument to be passed to modSummary3
#' @export
#' @examples
#' fit=lm(govact~negemot*sex+negemot*age+posemot+ideology,data=glbwarm)
#' modSummary2Table(fit)
modSummary2Table=function(x,vanilla=TRUE,...){
    if("lm" %in% class(x)) x=modSummary2(x,...)
    rrtable::df2flextable(x,vanilla=vanilla)
}

