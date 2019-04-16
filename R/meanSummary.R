#' Make mean summary table
#' @param data A data.frame
#' @param X Name of independant variable
#' @param Y Name of dependant variable
#' @param M Name of mediator variable
#' @param W Name of moderator variable
#' @param labels A list of labels
#' @param digits Integer indicating the number of decimal places
#' @param xlabels Otional string
#' @export
#' @examples
#'labels=list(X="cond",Y="reaction",M="pmi")
#'xlabels=c("Front Page","Interior Page")
#'meanSummary(data=pmi,labels=labels,xlabels=xlabels)
#'labels=list(X="frame",Y="justify",W="skeptic")
#'xlabels=c("Natural causes condition","Climate change condition")
#'meanSummary(data=disaster,labels=labels,xlabels=xlabels)
meanSummary=function(data,X=NULL,Y=NULL,M=NULL,W=NULL,labels=labels,digits=3,xlabels=NULL){

    # X=NULL;Y=NULL;M=NULL;W=NULL;digits=3;xlabels=NULL
    # data=disaster
    if(is.null(X)) X=labels[["X"]]
    if(is.null(Y)) Y=labels[["Y"]]
    if(is.null(M)) M=labels[["M"]]
    if(is.null(W)) W=labels[["W"]]

    values=sort(unique(data[[X]]))
    count=length(values)
    data
    y=getMeanSd(data=data,X=X,Y=Y,digits=digits)

    if(!is.null(M)) {
        m=getMeanSd(data=data,X=X,Y=M,digits=digits)
        yhat=getYhat1(data=data,X=X,M=M,Y=Y,digits=digits)
        yhat
        adjY=rep("",(length(values)+1)*2)
        for(i in seq_along(yhat)){
            adjY[2*i-1]=yhat[i]
        }
        name=rep(c("Mean","SD"),count+1)
        xvalues=paste0(X,"(X) = ",values)
        X=rep("",(length(values)+1)*2)
        if(!is.null(xlabels)){
           for(i in seq_along(xvalues)){
               X[2*i-1]=xlabels[i]
               X[2*i]=paste0(xvalues[i])
           }
        } else{
            for(i in seq_along(xvalues)){
                X[2*i-1]=xvalues[i]
            }
        }

        df=data.frame(X=X,name=name,Y=y,M=m,adjY=adjY,stringsAsFactors = FALSE)
        df
        attr(df,"M")=M
    }
    if(!is.null(W)) {
        w=getMeanSd(data=data,X=X,Y=W,digits=digits)
        name=rep(c("Mean","SD"),count+1)
        xvalues=paste0(X,"(X) = ",values)
        X=rep("",(length(values)+1)*2)
        if(!is.null(xlabels)){
            for(i in seq_along(xvalues)){
                X[2*i-1]=xlabels[i]
                X[2*i]=paste0(xvalues[i])
            }
        } else{
            for(i in seq_along(xvalues)){
                X[2*i-1]=xvalues[i]
            }
        }
        data
        df=data.frame(X=X,name=name,Y=y,W=w,stringsAsFactors = FALSE)
        df
        attr(df,"W")=W
    }
    attr(df,"X")=X
    attr(df,"Y")=Y
    class(df)=c("meanSummary","data.frame")
    df
}


#'S3 method of class meanSummary
#'@param x An object of class meanSummary
#'@param ... Further argumants to be passed to print()
#'@export
print.meanSummary=function(x,...){

    mode=1
    if(ncol(x)>4) mode=2

    width=c(20,rep(8,ncol(x)-1))
    width[1]=max(20,max(nchar(x[["X"]]))+2)
    width[3]=max(width[3],max(nchar(attr(x,"Y"))+2))
    if(mode==1){
       width[4]=max(width[4],max(nchar(attr(x,"W"))+2))
    } else{
        width[4]=max(width[4],max(nchar(attr(x,"M"))+2))
        width[5]=max(width[4],10)
    }
    total=sum(width)

    cat(paste(rep("=",total),collapse = ""),"\n")
    temp=paste0(centerPrint("",width[1]),centerPrint("",width[2]),
               centerPrint(names(x)[3],width[3]),centerPrint(names(x)[4],width[4]))
    if(mode==2) {
        temp=paste0(temp,centerPrint("Y",width[5]))
    }
    cat(temp,"\n")
    if(mode==1){
        temp=paste0(centerPrint("",width[1]),centerPrint("",width[2]),
                    centerPrint(attr(x,"Y"),width[3]),centerPrint(attr(x,"W"),width[4]))
    } else{
        temp=paste0(centerPrint("",width[1]),centerPrint("",width[2]),
                    centerPrint(attr(x,"Y"),width[3]),centerPrint(attr(x,"M"),width[4]),
                    centerPrint("adjusted",width[5]))
    }
    cat(temp,"\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in 1:(nrow(x)-2)){
        temp=""
        for(j in 1:ncol(x)){
           temp=paste0(temp,centerPrint(x[i,j],width[j]))
        }
        cat(temp,"\n")
    }
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in (nrow(x)-1):nrow(x)){
        temp=""
        for(j in 1:ncol(x)){
            temp=paste0(temp,centerPrint(x[i,j],width[j]))
        }
        cat(temp,"\n")
    }
    cat(paste(rep("=",total),collapse = ""),"\n")
}

#' Make mean summary table
#' @param ... Further argumants to be passed to meanSummary
#' @param vanilla logical
#' @importFrom rrtable df2flextable
#' @export
#' @examples
#'labels=list(X="cond",Y="reaction",M="pmi")
#'xlabels=c("Front Page","Interior Page")
#'meanSummaryTable(data=pmi,labels=labels,xlabels=xlabels)
#'labels=list(X="frame",Y="justify",W="skeptic")
#'xlabels=c("Natural causes condition","Climate change condition")
#'meanSummaryTable(data=disaster,labels=labels,xlabels=xlabels)
meanSummaryTable=function(...,vanilla=TRUE){
    x=meanSummary(...)
    mode=ifelse(ncol(x)==4,1,2)
    ft<- rrtable::df2flextable(x,vanilla=vanilla)
    if(mode==1) {
        headerlabels=list(X="",name=" ",Y="Y",W="W")
        headerrow=c("","",attr(x,"Y"),attr(x,"W"))
    } else{
        headerlabels=list(X="",name=" ",Y="Y",M="M",adjY="Y")
        headerrow=c("","",attr(x,"Y"),attr(x,"M"),"adjusted")
    }
    ft<- ft %>% width(j=1,width=2.5) %>%
        set_header_labels(values=headerlabels) %>%
        add_header_row(values=headerrow,top=FALSE,colwidths=rep(1,ncol(x))) %>%
        align(align="center",part="header") %>%
        italic(part="header") %>%
        hline(i=1,border=fp_border(color="#EDBD3E",width=0),part="header") %>%
        hline(i=nrow(x)-2,j=2:ncol(x),border=fp_border(color="black",width=1),part="body") %>%
        fontsize(size=12,part="all")

    ft
}

#' get mean and sd
#' @param data A data.frame
#' @param X Name of independant variable
#' @param Y Name of dependant variable
#' @param digits Integer indicating the number of decimal places
getMeanSd=function(data,X,Y,digits){
    values=sort(unique(data[[X]]))
    y=c()
    for(i in seq_along(values)) {
        y=c(y,mean(data[data[[X]]==values[i],Y],na.rm=T),
            sd(data[data[[X]]==values[i],Y],na.rm=T))
    }
    y=c(y,mean(data[[Y]],na.rm=T),sd(data[[Y]],na.rm=T))
    sprintf(paste0("%0.",digits,"f"),y)
}

#' Get Yhat value from simple mediation
#' @param data A data.frame
#' @param X Name of independant variable
#' @param M Name of moderator variable
#' @param Y Name of dependant variable
#' @param digits Integer indicating the number of decimal places
getYhat1=function(data,X,M,Y,digits=3){
    temp=paste0("lm(",Y,"~",M,"+",X,",data=data)")
    fit=eval(parse(text=temp))
    summary(fit)


    values=sort(unique(data[[X]]))
    yhat=fit$coef[1]+fit$coef[2]*mean(data[[M]],na.rm=T)+fit$coef[3]*values
    round(yhat,digits)
}
