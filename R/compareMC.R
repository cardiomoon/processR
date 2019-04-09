#' Compare effects of mean-centering and standardization of model
#' @param fit An object of class 'lm'
#' @param mode integer
#' @return if mode is 1, an object of modelSummary2. Otherwise lift of models
#' @export
#' @examples
#' fit=lm(govact~negemot*age,data=glbwarm)
#' compareMC(fit)
#' compareMC(fit,mode=2)
compareMC=function(fit,mode=1){
    data=fit$model

    xvar=colnames(data)[2]
    wvar=colnames(data)[3]
    yvar=colnames(data)[1]
    Zyvar=paste0("Z",yvar)
    labels=list(X=xvar,W=wvar,X.c=paste0(xvar,".c"),W.c=paste0(wvar,".c"),
                Zx=paste0("Z",xvar),Zw=paste0("Z",wvar))
    fitlabels=c("Original Data","After Mean-Centering","After Standardization")

    data=meanCentering(data=data,colnames(data[2:3]))
    data=standardizeDf(data,colnames(data[1:3]))

    fit1=fit
    temp=paste0("lm(",yvar,"~",labels[["X.c"]],"*",labels[["W.c"]],",data=data)")
    fit2=eval(parse(text=temp))
    temp=paste0("lm(",Zyvar,"~",labels[["Zx"]],"*",labels[["Zw"]],",data=data)")
    fit3=eval(parse(text=temp))

    res=list(fit1,fit2,fit3)
    if(mode==1) {
        res=modelsSummary2(res,labels=labels,fitlabels = fitlabels)
        res$name1=str_replace_all(res$name1,"X.c","X'")
        res$name1=str_replace_all(res$name1,"W.c","W'")

    }
    res

}

#' Make table comparing effects of mean-centering and standardization of model
#' @param fit An object of class 'lm'
#' @param vanilla logical.
#' @export
compareMCTable=function(fit,vanilla=TRUE){
    res=compareMC(fit)
    ft<-modelsSummary2Table(res,vanilla=vanilla)
    ft %>% bold(i=c(1,6,11),part="body") %>%
        bold(i=1,part="header")
}


#' Summarizing correlation, tolerance and variance inflation factors of a model
#' @param fit An object of class lm
#' @param mode integer. one of 1:2
#' @param namemode integer. One of 1:3
#' @param digits logical
#' @importFrom mycor mycor
#' @importFrom car vif
#' @importFrom stats var
#' @export
#' @examples
#' fit=lm(govact~negemot*age,data=glbwarm)
#' fit2vif(fit)
fit2vif=function(fit,mode=1,namemode=1,digits=3){
    data=fit$model
    temp=c(names(data)[2:3],paste0(names(data)[2],":",names(data)[3]))
    data[["XW"]]=data[[2]]*data[[3]]
    data=data[2:4]
    colnames(data)=c("X","W","XW")
    df=as.data.frame(mycor::mycor(data,digits=digits)$r)
    variance=unlist(lapply(data,var))
    VIF=car::vif(fit)
    tol=1/VIF
    tol
    rownames(df)=temp
    df=cbind(df,variance,tol,VIF)
    df[]=lapply(df,myformat,digits)
    df[1,2]=""
    df[1,3]=""
    df[2,3]=""
    df
    if(mode==2) {
        df=rbind(names(df),df)
        if(namemode==1) {
            rownames(df)[1]="Original Data"
            temp=c("X","W","XW")
            rownames(df)[2:4]=paste0(rownames(df)[2:4],"(",temp,")")
        } else if(namemode==2) {
            rownames(df)[1]="After Mean-Centering"
            temp=c("X'","W'","X'W'")
            rownames(df)[2:4]=paste0(rownames(df)[2:4],"(",temp,")")
            df[1,1:3]=temp
        } else if(namemode==3) {
            rownames(df)[1]="After Standardization"
            temp=c("Zx","Zw","ZxZw")
            rownames(df)[2:4]=paste0(rownames(df)[2:4],"(",temp,")")
            df[1,1:3]=temp
        }
    }
    df
}


#' Compare correlation, tolerance, vif of mean-centered and standardized models
#' @param fit An object of class lm
#' @importFrom purrr pmap
#' @export
#' @examples
#' fit=lm(govact~negemot*age,data=glbwarm)
#' compareVIF(fit)
compareVIF=function(fit){
    fit=compareMC(fit,mode=2)
    mode=list(2,2,2)
    namemode=list(1,2,3)
    args=list(fit,mode,namemode)
    df<-args %>% pmap(fit2vif) %>% reduce(rbind)
    class(df)=c("compareVIF","data.frame")
    df
}


#'S3 method of class compareVIF
#'@param x An object of class compareVIF
#'@param ... Further arguments to be passed to print
#'@export
print.compareVIF=function(x,...){
    width=c(20,8,8,8,10,10,10)
    width[1]=max(20,max(nchar(rownames(x))+2))
    total=sum(width)
    cat("\n")

    count=nrow(x)/4
    for(i in 1:count){
        no=(i-1)*4+1
        cat(paste(rep("=",total),collapse = ""),"\n")
        cat(paste0(centerPrint(rownames(x)[no],width[1]),
                   rightPrint(x$X[no],width[2]),
                   rightPrint(x$W[no],width[3]),
                   rightPrint(x$XW[no],width[4]),
                   rightPrint(x$variance[no],width[5]),
                   rightPrint(x$tol[no],width[6]),
                   rightPrint(x$VIF[no],width[7])),"\n")
        cat(paste(rep("-",total),collapse = ""),"\n")
        for(j in 1:3 ){
            no=no+1
            cat(paste0(rightPrint(rownames(x)[no],width[1]),
                       rightPrint(x$X[no],width[2]),
                       rightPrint(x$W[no],width[3]),
                       rightPrint(x$XW[no],width[4]),
                       rightPrint(x$variance[no],width[5]),
                       rightPrint(x$tol[no],width[6]),
                       rightPrint(x$VIF[no],width[7])),"\n")
        }
    }
    cat(paste(rep("=",total),collapse = ""),"\n")
}


#' Make table comparing correlation, tolerance, vif of mean-centered and standardized models
#' @param fit An object of class lm
#' @param vanilla logical
#' @importFrom flextable delete_part
#' @export
#' @examples
#' fit=lm(govact~negemot*age,data=glbwarm)
#' compareVIFTable(fit)
#' compareVIFTable(fit,vanilla=FALSE)
compareVIFTable=function(fit,vanilla=TRUE){

    if("lm" %in% class(fit)){
        df=compareVIF(fit)
    } else if("compareVIF" %in% class(fit)){
        df=fit
    }

    std_border = fp_border(color="gray")

    df %>% rrtable::df2flextable(vanilla=vanilla,add.rownames=TRUE) %>%
        delete_part(part="header") %>%
        hline(i=c(4,8),border=std_border,part="body") %>%
        hline(i=c(1,5,9),j=c(2:7),border=std_border,part="body") %>%
        bold(i=c(1,5,9),j=1) %>%
        italic(i=c(1,5,9),j=c(2,3,4,6,7)) %>%
        align(i=c(1,5,9),align="center",part="body") %>%
        width(j=1,width=2)


}
