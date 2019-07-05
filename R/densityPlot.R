#' Get bootstrapped values
#' @param semfit An object of class lavaan
#' @param what Character. What needs to be inspected/extracted?
#' @param ... Further argument to be passed to lavaan::lavTech()
#' @importFrom lavaan lavTech
#' @export
#' @examples
#' require(lavaan)
#' labels=list(X="cond",M="pmi",Y="reaction")
#' model=tripleEquation(labels=labels)
#' set.seed(1234)
#' semfit=sem(model,data=pmi,se="boot",bootstrap=100)
#' getBootData(semfit)
getBootData=function(semfit,what="coef.boot",...){
    as.data.frame(lavTech(semfit, what=what, add.labels = TRUE,...))
}


#' Draw Smoothed Kernel density plot
#' @param x A numeric vector
#' @param sig significant level. Default value is 0.05
#' @param digits Integer indicating the number of decimal places
#' @param xlab character. x axis label
#' @param ylab character. y axis label
#' @importFrom ggplot2 ggplot geom_histogram annotate labs geom_line
#' @importFrom predict3d theme_bw2
#' @export
#' @examples
#' require(lavaan)
#' labels=list(X="cond",M="pmi",Y="reaction")
#' model=tripleEquation(labels=labels)
#' set.seed(1234)
#' semfit=sem(model,data=pmi,se="boot",bootstrap=100)
#' bootData=getBootData(semfit)
#' bootData$indirect=bootData$a*bootData$b
#' densityPlot(bootData$indirect)
densityPlot=function(x,sig=0.05,digits=3,xlab="Indirect effect(ab)",ylab=NULL){
    if(is.null(ylab)){
        ylab=paste0("Smoothed Kernel density estimates \nin ",length(x)," bootstrap samples")
    }
    df=as.data.frame(x=x)
    xintercept=quantile(x,probs=c(sig/2,1-sig/2),type=6)
    labels=sprintf(paste0("%0.",digits,"f"),xintercept)

    p<-ggplot(data=df,aes_string(x="x",y="..density.."))+
        geom_histogram(color="grey60",fill="cornsilk")+
        geom_line(stat="density",color="red")

    res=getAspectRatio(p)

    p<-p+ geom_vline(xintercept=xintercept[1],lty=2)+
        geom_vline(xintercept=xintercept[2],lty=2)+
        annotate("text",x=xintercept[1],y=res$ymax,label=labels[1],hjust=1.1)+
        annotate("text",x=xintercept[2],y=res$ymax,label=labels[2],hjust=-0.1)+
        labs(x=xlab,y=ylab)+
        theme_bw2()
    p
}
