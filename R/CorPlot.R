#' Make a table with correlation
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @importFrom mycor mycor
#' @export
corTable=function(fit){
    data=fit@Data@X[[1]]
    colnames(data)=fit@Data@ov$name
    data=data.frame(data)
    data
    result=mycor::mycor(data)
    result
    resp=p2asterisk(result$p)
    res=paste0(sprintf("%.2f",result$r),resp)
    res=matrix(res,ncol(result$r))
    res[upper.tri(res)]=""
    res[row(res)==col(res)]<-"1"
    rownames(res)=colnames(data)
    colnames(res)=colnames(data)
    res=as.data.frame(res)
    # str(res)
    res
    values=unique(as.vector(resp))
    temp=""
    if("***" %in% values) temp=pastecolon(temp,"***p<0.001")
    if("**" %in% values) temp=pastecolon(temp,"**p<0.01")
    if("*" %in% values) temp=pastecolon(temp,"*p<0.05")
    attr(res,"footer")=temp
    res
}

#' Make a table with correlation
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param vanilla Logical. If true, vanilla.table is returned
#' @param addFooter Logical. If true, footer added
#' @param seek string to look for
#' @param replace A string of replacement
#' @importFrom rrtable df2flextable
#' @importFrom flextable align color add_footer merge_at
#' @export
corTable2=function(fit,vanilla=TRUE,addFooter=FALSE,seek=NULL,replace=NULL){

    res=corTable(fit)
    if(!is.null(seek)){
        for(i in seq_along(seek)) {
    colnames(res)[colnames(res)==seek[i]]=replace[i]
    rownames(res)[rownames(res)==seek[i]]=replace[i]
        }
    }
    Table=rrtable::df2flextable(res,vanilla=vanilla,add.rownames=TRUE)
    Table=flextable::align(Table,align="center",part="all")
    Table=flextable::color(Table,i=1,j=1,color=ifelse(vanilla,"white","#5B7778"),part="header")

    if(addFooter){
        Table<-flextable::add_footer(Table,rowname=attr(res,"footer"))
        Table<-flextable::merge_at(Table,j=1:(ncol(res)+1),part="footer")
        Table=flextable::align(Table,align="right",part="footer")
    }
    Table
}


#' paste two character with colon
#' @param temp a character
#' @param x a character
pastecolon=function(temp,x){
    if(temp=="") res=x
    else res=paste0(temp," ; ",x)
    res
}

#' Convert p values to asterisk
#' @param x a numeric vector or matrix
p2asterisk=function(x){
    ifelse(x<0.001,"***",ifelse(x<0.01,"**",ifelse(x<0.05,"*","")))
}



#'Extract model fit measures to data.frame
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param digits integer indicating the number of decimal places to be used.
#' @param names names of statistic to be extracted
#'
#' @importFrom lavaan fitMeasures
#' @export
#' @return A data.frame
modelFitTable=function(fit,digits=2,names=NULL){
    # digits=2;names=NULL
    if(is.null(names)) {
        names=c("chisq","df","pvalue","cfi","gfi","agfi","tli","rmr","srmr","rmsea","rmsea.ci.lower","rmsea.ci.upper","aic","bic")
        newnames=c("chisq","df","p","CFI","GFI","AGFI","TLI","RMR","SRMR","RMSEA","lower","upper","AIC","BIC")

    }
    names
    value=fitMeasures(fit)[names]
    value
    res=data.frame(rbind(value))
    if(!is.null(newnames)) colnames(res)=newnames
    rownames(res)="statistic"
    res$x2df=res$chisq/res$df
    res=round(res,digits)
    res$RMSEA=paste0(res$RMSEA,"(",res$lower,"-",res$upper,")")
    colnames(res)[10]="RMSEA(95% CI)"
    res=res[c(1,2,15,3:10,13,14)]
    res
}


#'Extract model fit measures to flextable
#'@param fit An object of class lavaan. Result of sem function of package lavaan
#'@param vanilla Logical
#'@param ... Further arguments to be passed to modelFitTable()
#'@export
modelFitTable2=function(fit,vanilla=FALSE,...){
    result=modelFitTable(fit,...)
    df2flextable(result,vanilla=vanilla)
}


#'convert parameterEstimates to data.frame
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param latent whether the latent variables be included in result
#' @param regression whether the regressions be included in result
#' @param mediation whether the mediation effects be included in result
#' @param covar whether the covariances be included in result
#' @param ci If TRUE, confidence intervals are added to the output
#' @param standardized Logical. If TRUE, standardized estimates are added to the output
#' @param digits integer indicating the number of decimal places to be used.
#'
#' @export
estimatesTable=function(fit,latent=TRUE,regression=TRUE,mediation=FALSE,covar=FALSE,ci=FALSE,standardized=TRUE,digits=2){
    # latent=TRUE;regression=TRUE;mediation=FALSE;covar=FALSE;ci=TRUE;standardized=TRUE;digits=2
    # cat("digits=",digits,"\n")
    result=parameterEstimates(fit,ci=ci,standardized=standardized)
    result
    if(mediation){
       result=result[result$label!="",]
       result=result[-c(2)]
    } else{
        include=c()
        if(latent) include=c(include,"=~")
        if(regression) include=c(include,"~")
        if(covar) include=c(include,"~~")
        result=result[result$op %in% include,]
        #result=result[-c(2,4)]
        result=result[-c(2)]
    }
    no=ncol(result)
    result=result[-c(no-2,no)]
    result
    if(ci){
        result$est=paste0(round(result$est,digits),"(",round(result$ci.lower,digits),"-",round(result$ci.upper,digits),")")
        no=ncol(result)
        result=result[-c(no-1,no-2)]
    } else{
        result$est=round(result$est,digits)
    }
    result
    result$se=round(result$se,digits)
    result$z=round(result$z,digits)
    result$std.all=round(result$std.all,digits)
    result$pvalue=convertPvalue(result$pvalue)
    result
    if(mediation){
        result$lhs[substr(result$lhs,1,3)=="ind"]="indirect effect"
        result$lhs[substr(result$lhs,1,5)=="total"]="total effect"
        colnames(result)=c("Variables","Predictors","label","B","SE","z","p","\u03B2")

    } else{
        result=result[,names(result)!="label"]
        colnames(result)=c("Variables","Predictors","B","SE","z","p","\u03B2")
    }
    result[is.na(result)]=""
    result
}


#'convert parameterEstimates to flextable
#'@param fit An object of class lavaan. Result of sem function of package lavaan
#'@param vanilla Logical
#'@param digits integer indicating the number of decimal places to be used.
#' @param seek string to look for
#' @param replace A string of replacement
#'@param ... Further arguments to be passed to estimatesTable()
#'@export
estimatesTable2=function(fit,vanilla=FALSE,digits=2,seek=NULL,replace=NULL,...){
    result=estimatesTable(fit,digits=digits,...)
    if(!is.null(seek)){
        for(i in seq_along(seek)){
        result$Predictors[result$Predictors==seek[i]]=replace[i]
        }
    }
    df2flextable(result,vanilla=vanilla,digits=digits)
}


#' convert vector of p values to string
#'
#' @param x vector of p values
convertPvalue=function(x){
    x=sprintf("%0.3f",x)
    x[x=="NA"]=""
    x[x=="0.000"]="< 0.001"
    x
}



#' Draw correlation plot
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param label if 0, no label(default), if 1, use r value as label, if 2, use r value with significant mark as label
#' @param yreverse Logical. if true, reverse the order of y axis.
#' @param xangle axis.x.text.angle
#' @param seek string to look for
#' @param replace A string of replacement
#' @param ... Further arguments to be passed on to geom_text
#'
#' @importFrom ggiraphExtra ggCor
#' @export
#'
#' @return A ggplot
corPlot=function(fit,label=2,yreverse=TRUE,xangle=45,seek=NULL,replace=NULL,...){
    data=fit@Data@X[[1]]
    colnames(data)=fit@Data@ov$name
    data=data.frame(data)
    data
    if(!is.null(seek)) {
        for(i in seq_along(seek)) {
        colnames(data)[colnames(data)==seek[i]]=replace[i]
        }
    }
    ggiraphExtra::ggCor(data=data,label=label,yreverse=yreverse,xangle=xangle,...)
}


#' Make a Cronbach alpha table
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @param digits integer indicating the number of decimal places to be used.
#'
#' @importFrom psych alpha
#' @importFrom stringr str_flatten
#' @export
fit2alpha=function(fit,digits=3){
        df=as.data.frame(fit@ParTable,stringsAsFactors = FALSE)
        df=df[df$op=="=~",c("lhs","rhs")]

        latentVars=unique(df$lhs)
        indicators=c()
        data=fit@Data@X[[1]]
        fit@Data@X[[1]]
        colnames(data)=fit@Data@ov$name
        data=data.frame(data)
        data
        alpha<-lambda6<-c()

        for(i in seq_along(latentVars)){
            vars=df$rhs[df$lhs==latentVars[i]]
            indicators=c(indicators,stringr::str_flatten(vars,"+"))
            data[vars]
            result=psych::alpha(data[vars],warnings=FALSE)
            alpha=c(alpha,result$total$raw_alpha)
            lambda6=c(lambda6,result$total$`G6(smc)`)
        }
        alpha=round(alpha,digits)
        lambda6=round(lambda6,digits)
        data.frame(latentVars,indicators,alpha,lambda6,stringsAsFactors = FALSE)
}

