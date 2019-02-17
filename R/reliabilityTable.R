#' make discriminant Validity Table
#' @param fit An object of a class lavaan
#' @importFrom semTools reliability
#' @importFrom lavaan inspect
#' @export
discriminantValidityTable=function(fit){
    result=tryCatch(semTools::reliability(fit),error= function(e) "error")
    if("character" %in% class(result)) {
        result<-NULL
    } else{
        result=rbind(result,sqrtave=sqrt(result[5,]))
        df=as.data.frame(t(result[,-ncol(result)]))

        colnames(df)[5]="AVE"
        colnames(df)[6]="sqrt(AVE)"
        result1=inspect(fit,"cor.lv")
        diag(result1)<-NA

        discriminantValidity<-as.character(df[[6]]>apply(result1,1,max,na.rm=TRUE))
        #discriminantValidity
        diag(result1)<-1
        rdf=as.data.frame(result1)

        result=cbind(rdf,df[5:6])
        result=round(result,3)
        result=cbind(result,discriminantValidity)

    }
    result
}

#' make discriminant Validity Table in flextable format
#' @param fit An object of a class lavaan
#' @param vanilla Logical
#' @export
discriminantValidityTable2=function(fit,vanilla=FALSE){
    result=tryCatch(semTools::reliability(fit),error= function(e) "error")

    if("character" %in% class(result)) {
        result<-NULL
    } else{
        result=discriminantValidityTable(fit)
        result<-df2flextable(result,add.rownames = TRUE,vanilla=vanilla)
    }
    result
}


#' make reliability Table
#' @param fit An object of a class lavaan
#' @export
reliabilityTable=function(fit){
    result=tryCatch(semTools::reliability(fit),error= function(e) "error")
    if("character" %in% class(result)) {
        result<-NULL
    } else{
        result=rbind(result,sqrtave=sqrt(result[5,]))
        df=as.data.frame(round(t(result),3))
        colnames(df)[5]="AVE"
        colnames(df)[6]="sqrt(AVE)"
        df$Reliablity=as.character((df$omega>=0.7)&(df$alpha>=0.7))
        df$convergenceValidity=as.character(df$AVE>=0.5)
        result<-df
    }
    result
}

#' make reliability Table in flextable format
#' @param fit An object of a class lavaan
#' @param vanilla Logical
#' @importFrom flextable width
#' @export
reliabilityTable2=function(fit,vanilla=FALSE){
    result=tryCatch(semTools::reliability(fit),error= function(e) "error")
    result
    if("character" %in% class(result)) {
        result<-NULL
    } else{
        df<-reliabilityTable(fit)
        result<-df2flextable(df,add.rownames=TRUE,vanilla=vanilla)
        result<-width(result,j=1:(ncol(df)+1),width=c(1,1,1,1,1,1,1.5,1.5,1.5))
    }
    result
}

#' Model fit guide table
#' @export
modelFitGuideTable=function(){

    x2df="< 3"
    p="> 0.05"
    CFI="> 0.9"
    GFI="> 0.9"
    AGFI="> 0.9"
    TLI="> 0.9"
    RMR="< 0.05"
    SRMR="< 0.05"
    RMESA="< 0.1(< 0.05)"
    AIC="the lower, the better"
    BIC="the lower, the better"
    result=data.frame(x2df,p,CFI,GFI,AGFI,TLI,RMR,SRMR,RMESA,AIC,BIC)
    result
}

#' Model fit guide table
#' @param vanilla Logical
#' @export
modelFitGuideTable2=function(vanilla=FALSE){

    result=modelFitGuideTable()
    df2flextable(result,vanilla=vanilla)
}

