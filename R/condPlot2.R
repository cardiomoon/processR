#' Draw conditional plot for moderated moderation
#' @param fit An object of class lm
#' @param pred name of the predictor variable
#' @param modx name of the moderator variable
#' @param mod2 name of the second moderator variable
#' @param mod2.values values od moderator variable
#' @param vjust integer
#' @param rangemode integer. 1 or 2
#' @param digits integer indicating the number of decimal places
#' @param addlabel logical
#' @param xvar character. "Z" or "W"
#' @param ... Further arguments to be passed to predict3d::ggPredict()
#' @importFrom predict3d ggPredict
#' @export
#' @examples
#' fit=lm(govact~negemot*sex*age+posemot+ideology,data=glbwarm)
#' condPlot2(fit)
#' condPlot2(fit,mod2.values = c(30,50,70))
#' fit1=lm(govact~negemot*age*sex+posemot+ideology,data=glbwarm)
#' condPlot2(fit1,pred="negemot",modx="sex",mod2="age",mod2.values = c(30,50,70),xvar="W")
condPlot2=function(fit,pred=NULL,modx=NULL,mod2=NULL,mod2.values=NULL,
                   rangemode=1,vjust=NULL,digits=3,addlabel=TRUE,xvar="Z",...){


      # fit=lm(govact~negemot*age*sex+posemot+ideology,data=glbwarm)
      # pred="negemot";modx="sex"; mod2="age";mod2.values =NULL;rangemode=1;digits=3
    if(is.null(pred)) pred=names(fit$model)[2]
    if(is.null(modx)) modx=names(fit$model)[3]
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

    if(is.null(vjust)) vjust=rep(c(-0.5,1.5),each=length(mod2.values))
    temp=paste0("ggPredict(fit,pred=",pred,",modx=",modx,",mod2=",mod2,",show.point=FALSE,mod2.values = mod2.values,
                vjust=vjust,digits=digits,plot=FALSE,...)")
    res=eval(parse(text=temp))
    info=res$aspectRatio

    df=makeLabel(fit,pred=pred,modx=modx,mod2=mod2,mod2.values=mod2.values,xvar=xvar,digits=digits)
    p<- res$p + guides(colour=FALSE)

    if(addlabel){
        p<-p +geom_text(data=df,aes_string(label="label",colour=NULL,fill=NULL,group=NULL),
                  x=(info$xmin+info$xmax)/2,y=info$ymax,parse=TRUE,vjust=1)
    }
    p
}

#' Make Labels
#' @param fit An object of class lm
#' @param pred name of the predictor variable
#' @param modx name of the moderator variable
#' @param mod2 name of the second moderator variable
#' @param mod2.values values od moderator variable
#' @param xvar character. "Z" or "W"
#' @param digits integer indicating the number of decimal places
#' @export
makeLabel=function(fit,pred,modx,mod2,mod2.values = c(30,50,70),xvar="Z",digits=3){
    temp=paste0(deparse(fit$call),collapse="")
    varname=mod2

    est=c()
    p=c()
    for(i in 1:length(mod2.values)){
        newvar=paste0("I(",varname,"-",mod2.values[i],")")
        temp1=stringr::str_replace(temp,varname,newvar)
        fit1=eval(parse(text=temp1))
        select=which(names(fit1$coef)==paste0(pred,":",modx))
        if(length(select)==0) select=which(names(fit1$coef)==paste0(modx,":",pred))
        est=c(est,summary(fit1)$coef[select,1])
        p=c(p,summary(fit1)$coef[select,4])
    }
    df=data.frame(mod2.values,est,p)
    names(df)[1]=varname
    df[[2]]=myformat(df[[2]],digits=digits)
    df[[3]]=myformat(df[[3]],digits=digits)
    df[[3]]=pformat(df[[3]])
    df[[3]]=ifelse(nchar(df[[3]])==4,paste0(" = ",df[[3]]),paste0(" ",df[[3]]))
    if(xvar=="Z"){
       df$label1=paste0("theta[italic(X)*italic(W)%->%italic(Y)] ==",df$est)
    } else{
        df$label1=paste0("theta[italic(X)*italic(Z)%->%italic(Y)] ==",df$est)
    }
    df$label2= paste0("italic(p),'",df$p,"'")
    df$label=paste0("paste(",df$label1,",', ',",df$label2,")")
    df[["mod2group"]]=paste0(varname," = ",df[[varname]])
    df
}
