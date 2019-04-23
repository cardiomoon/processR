#' Draw conditional plot for moderated moderation
#' @param fit An object of class lm
#' @param mod2 name of moderator variable
#' @param mod2.values values od moderator variable
#' @param rangemode integer. 1 or 2
#' @param digits integer indicating the number of decimal places
#' @param addlabel logical
#' @importFrom predict3d ggPredict
#' @export
#' @examples
#' fit=lm(govact~negemot*sex*age+posemot+ideology,data=glbwarm)
#' condPlot2(fit)
#' condPlot2(fit,mod2.values = c(30,50,70))
condPlot2=function(fit,mod2=NULL,mod2.values=NULL,rangemode=1,digits=3,addlabel=TRUE){

    # mod2=NULL;mod2.values = c(30,50,70);rangemode=1;digits=3

    if(is.null(mod2)) mod2=names(fit$model)[4]
    if(is.null(mod2.values)){
        if(rangemode==1){
            mod2.values=mean(fit$model[[mod2]],na.rm=TRUE)+c(-1,0,1)*sd(fit$model[[mod2]],na.rm=TRUE)
            mod2.values=round(mod2.values,digits)
        } else{
            mod2.values=quantile(fit$model[[mod2]],probs=c(0.16,0.5,0.84),type=6)
            mod2.values=round(mod2.values,digits)
        }
    }

    vjust=rep(c(-0.5,1.5),each=length(mod2.values))
    res=ggPredict(fit,show.point=FALSE,mod2.values = mod2.values,
                  vjust=vjust,digits=digits,plot=FALSE)
    info=res$aspectRatio

    df=makeLabel(fit,mod2.values=mod2.values,digits=digits)
    p<- res$p + guides(colour=FALSE)

    if(addlabel){
        p<-p +geom_text(data=df,aes_string(label="label",colour=NULL,fill=NULL,group=NULL),
                  x=(info$xmin+info$xmax)/2,y=info$ymax,parse=TRUE,vjust=1)
    }
    p
}

#' Make Labels
#' @param fit An object of class lm
#' @param mod2.values values od moderator variable
#' @param digits integer indicating the number of decimal places
#' @export
makeLabel=function(fit,mod2.values = c(30,50,70),digits=3){
    temp=paste0(deparse(fit$call),collapse="")
    varname=names(fit$model)[4]
    varname
    est=c()
    p=c()
    for(i in 1:length(mod2.values)){
        newvar=paste0("I(",varname,"-",mod2.values[i],")")
        temp1=stringr::str_replace(temp,varname,newvar)
        fit1=eval(parse(text=temp1))
        est=c(est,summary(fit1)$coef[7,1])
        p=c(p,summary(fit1)$coef[7,4])
    }
    df=data.frame(mod2.values,est,p)
    names(df)[1]=varname
    df[[2]]=myformat(df[[2]],digits=digits)
    df[[3]]=myformat(df[[3]],digits=digits)
    df[[3]]=pformat(df[[3]])
    df[[3]]=ifelse(nchar(df[[3]])==4,paste0(" = ",df[[3]]),paste0(" ",df[[3]]))
    df$label1=paste0("theta[italic(X)*italic(W)%->%italic(Y)] ==",df$est)
    df$label2= paste0("italic(p),'",df$p,"'")
    df$label=paste0("paste(",df$label1,",', ',",df$label2,")")
    df[["mod2group"]]=paste0(varname," = ",df[[varname]])
    df
}
