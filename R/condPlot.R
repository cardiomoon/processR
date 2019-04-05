#'Draw conditional effect plot
#'@param fit An onject of class lm
#'@param pred name of predictor variable
#'@param modx name of moderator variable
#'@param pred.values Values of predictor variables
#'@param modx.values Values of modifier variables
#'@param labels labels of regression lines
#'@param mode integer. one of 1:3.
#'@param rangemode integer. 1 or 2
#'@param ypos integer. label y position.
#'@param hjust hjust of label
#'@param linecolor name of color of vline and hline
#'@param linetype linetype of arrow
#'@param linesize size of regression line
#'@param arrowsize size of arrow
#'@param digits integer indicating the number of decimal places
#'@param ... further arguments to be passed to add_lines
#'@importFrom predict3d add_lines calEquation
#'@importFrom tidyr crossing spread
#'@importFrom ggplot2 geom_vline geom_segment geom_hline labs
#'@importFrom grid arrow unit
#'@importFrom stats sd
#'@export
#'@examples
#'fit=lm(justify~skeptic*frame,data=disaster)
#'condPlot(fit,rangemode=2,xpos=0.7,labels=c("Climate change(X=1)","Natural causes(X=0)"))
#'condPlot(fit,mode=2,xpos=0.6)
#'condPlot(fit,mode=3,rangemode=2,xpos=0.5,ypos=c(0,2))
#'fit=lm(mpg~hp*vs,data=mtcars)
#'condPlot(fit,rangemode=2,xpos=0.6)
#'condPlot(fit,mode=2,xpos=0.5)
#'condPlot(fit,mode=3,rangemode=2)
#'\donttest{
#'fit=lm(govact~negemot*age+posemot+ideology+sex,data=glbwarm)
#'condPlot(fit,hjust=c(-0.1,-0.1,1.1))
#'condPlot(fit,modx.values=c(30,70),hjust=c(-0.1,-0.1,1.1))
#'condPlot(fit,mode=2,modx.values=c(30,50,70),xpos=0.2)
#'condPlot(fit,mode=2,xpos=0.2)
#'condPlot(fit,mode=3,xpos=0.5,hjust=c(-0.1,1.1))
#'condPlot(fit,pred.values=c(2,3,4),mode=3,xpos=0.6)
#'}
condPlot=function(fit,pred=NULL,modx=NULL,pred.values=NULL,modx.values=NULL,labels=NULL,
                  mode=1,rangemode=1,ypos=NULL,hjust=NULL,linecolor="gray60",linetype=2,
                  linesize=1,arrowsize=1,digits=3,...){
  # fit=lm(govact~negemot*age+posemot+ideology+sex,data=glbwarm)
  # pred=NULL;modx=NULL;pred.values=NULL;modx.values=NULL
  # mode=2;rangemode=2;digits=3
  # # modx.values=c(30,70)


  data=fit$model

  dep=colnames(data)[1]
  if(is.null(pred)) pred=colnames(data)[2]
  if(is.null(modx)) modx=colnames(data)[3]
  if(length(colnames(data)[3])>3) {
    mod2=colnames(data)[4]
  } else {
    mod2=NULL
  }

  if(mode==1){
  if(is.null(pred.values)){
    if(rangemode==1) {
      pred.values=mean(data[[pred]],na.rm=TRUE)+c(-1,0,1)*sd(data[[pred]],na.rm=TRUE)
    } else if(rangemode==2) {
      pred.values=quantile(data[[pred]],probs=c(0.16,0.5,0.84),type=6)
    }
  }
  coef<-pvalue<-c()
  for(i in seq_along(pred.values)){
    equation=deparse(fit$call)
    temp=stringr::str_replace(equation,pred,paste0("I(",pred,"-",pred.values[i],")"))
    fit1=eval(parse(text=temp))
    coef=c(coef,fit1$coef[3])
    pvalue=c(pvalue,summary(fit1)$coef[3,4])
  }

  effectDf=data.frame(pred.values,coef,pvalue)
  colnames(effectDf)[1]="x"
  effectDf

  if(is.null(modx.values)){
    if(length(unique(data[[modx]]))<6) {
      modx.values=unique(data[[modx]])
    } else if(rangemode==1){
      modx.values=mean(data[[modx]],na.rm=T)+c(-1,1)*sd(data[[modx]],na.rm=T)
    } else{
      modx.values=quantile(data[[modx]],probs=c(0.16,0.84),type=6)
    }
  }
  modx.values=modx.values[c(1,length(modx.values))]
  p=ggplot(data,aes_string(x=pred,y=dep))
  df1=calEquation(fit,modx.values = modx.values)
  df1
  if(!is.null(labels)){
      if(nrow(df1)==length(labels)) df1$label=labels
  }

   # p<-add_lines(p,df1)
    p<-add_lines(p,df1,size=linesize,...)
  p
  info=getAspectRatio(p)
  ratio=info$ratio
  df1$slope2=df1$slope*ratio
  df1$radian=atan(df1$slope2)
  df1$angle=df1$radian*180/pi

  df1[[modx]]=modx.values
  df1

  df=tidyr::crossing(pred.values,modx.values)
  names(df)=c(pred,modx)
  df

  df<-dplyr::left_join(df,df1)

  df[[dep]]=df$slope*df[[pred]]+df$intercept

  df
  df<-df[c(pred,modx,dep)]
  df %>% spread(key=2,value=3) -> df2

  colnames(df2)=c("x","y","yend")
  df2
  df3<-left_join(effectDf,df2)
  df3$coef=myformat(df3$coef,digits)
  df3$label3=paste0("italic(W) ==",round(df3$x,digits))
  df3$p1=myformat(df3$pvalue,digits=3)
  df3$p1=pformat(df3$p1)
  df3$p2=ifelse(df3$p1=="0.000","< 0.001",paste0("== ",df3$p1))

  df3$label=paste0("theta[italic(X) %->% italic(Y)] == ",df3$coef," ( italic(p) ",df3$p2,")")
  df3$angle1=df1$angle[1]
  df3$angle2=df1$angle[2]
  df3$vjust1=ifelse(df3$y>df3$yend,1.2,-0.2)
  df3$vjust2=ifelse(df3$y>df3$yend,-0.2,1.2)
  df3$y1=(df3$y+df3$yend)/2
  df3$labely=df3$yend
  if(!is.null(ypos)){
      if(length(ypos)==1) ypos=rep(ypos,nrow(df3))
      for(i in seq_along(ypos)){
          if(ypos[i]==0) {
              df3$labely[i]=df3$y[i]
          } else if(ypos[i]==1) {
              df3$labely[i]=df3$yend[i]
          } else if(ypos[i]==2) {
              df3$labely[i]=df3$y1[i]
          }
      }
  }

  df3$hjust=-0.1
  if(!is.null(hjust)){
    df3$hjust=hjust
  }

  for(i in seq_along(df2$x)){
    p<-p+geom_vline(xintercept=df2$x[i],color=linecolor,linetype=2)
  }
  p
  p<-p+geom_segment(data=df2,aes_string(x="x",y="y",xend="x",yend="yend"),
                    arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),
                    color="red",linetype=linetype,size=arrowsize)
  p+geom_text(data=df3,aes_string(x="x",y="labely",label="label",hjust="hjust",vjust=1),
              parse=TRUE) +
    geom_text(data=df3,aes_string(x="x",y=info$ymin,label="label3"),
              parse=TRUE) + xlab(paste0(pred,"(W)"))
  } else if(mode==2) {
    coef<-pvalue<-c()
    if(is.null(modx.values)){
      if(length(unique(data[[modx]]))<6) {
        modx.values=unique(data[[modx]])
      } else if(rangemode==1){
        modx.values=mean(data[[modx]],na.rm=T)+c(-1,0,1)*sd(data[[modx]],na.rm=T)
      } else{
        modx.values=quantile(data[[modx]],probs=c(0.16,0.5,0.84),type=6)
      }
    }

    for(i in seq_along(modx.values)){
      equation=deparse(fit$call)
      temp=stringr::str_replace(equation,modx,paste0("I(",modx,"-",modx.values[i],")"))
      fit1=eval(parse(text=temp))
      coef=c(coef,fit1$coef[2])
      pvalue=c(pvalue,summary(fit1)$coef[2,4])
    }
    effectDf=data.frame(modx.values,coef,pvalue)
    effectDf
    colnames(effectDf)[1]=modx

    p=ggplot(data,aes_string(x=pred,y=dep))
    df1=calEquation(fit,modx.values = modx.values)
    df1[[modx]]=modx.values
    df1
    df1<-dplyr::left_join(df1,effectDf)
    df1$coef=myformat(df1$coef,digits=digits)
    df1$label=paste0("theta[italic(X) %->% italic(Y)] == ",df1$coef,
                     "~( ", modx,"== ",round(df1[[modx]],digits),")")
    df1
    if(!is.null(labels)){
        if(nrow(df1)==length(labels)) df1$label=labels
    }
    p<-add_lines(p,df1,parse=TRUE,size=linesize,...)
    p
  } else if(mode==3){
    if(is.null(pred.values)){
      if(rangemode==1) {
        pred.values=mean(data[[pred]],na.rm=TRUE)+c(-1,1)*sd(data[[pred]],na.rm=TRUE)
      } else if(rangemode==2) {
        pred.values=quantile(data[[pred]],probs=c(0.16,0.84),type=6)
      }
    }

    coef<-pvalue<-c()
    for(i in seq_along(pred.values)){
      equation=deparse(fit$call)
      temp=stringr::str_replace(equation,pred,paste0("I(",pred,"-",pred.values[i],")"))
      fit1=eval(parse(text=temp))
      coef=c(coef,fit1$coef[3])
      pvalue=c(pvalue,summary(fit1)$coef[3,4])
    }

    effectDf=data.frame(pred.values,coef,pvalue)
    colnames(effectDf)[1]="x"
    df=effectDf
    df
    b1=fit$coef[modx]
    b3=fit$coef[paste0(pred,":",modx)]
    fun=function(x){b1+b3*x}

    df$yend=0
    df$y=fun(df$x)
    df$labely=df$y
    if(!is.null(ypos)){
        if(length(ypos)==1) ypos=rep(ypos,nrow(df))
        for(i in seq_along(ypos)){
            if(ypos[i]==1) {
                df$labely[i]=df$yend[i]
            } else if(ypos[i]==2) {
                df$labely[i]=(df$y[i]+df$yend[i])/2
            }
        }
    }

    df1=data.frame(slope=b3,intercept=b1,label=paste0("theta[italic(X)%->%italic(Y)]==",
                                                      round(b1,digits),ifelse(b3>=0,"+",""),round(b3,digits),"*italic(W)"))
    if(!is.null(labels)){
        if(nrow(df1)==length(labels)) df1$label=labels
    }
    df$label=paste0("italic(W) ==",round(df$x,digits))
    df$label2=paste0("theta[italic(X)%->%italic(Y)]==",round(df$coef,digits),"(italic(p) ==",round(df$pvalue,3),")")
    df$hjust=-0.1
    if(!is.null(hjust)){
        df$hjust=hjust
    }
    p<-ggplot(data=data,aes_string(x=pred))
          # p<-add_lines(p,df1,parse=TRUE,vjust=-0.2)
    p<-add_lines(p,df1,parse=TRUE,vjust=-0.3,size=linesize,...)
    p
    info=getAspectRatio(p)
    for(i in seq_along(pred.values)){
       p<-p+geom_vline(xintercept=pred.values[i],color=linecolor,linetype=2)
    }
    p<-p+ geom_text(data=df,aes_string(x="x",label="label"),y=info$ymin,parse=TRUE)+
      geom_text(data=df,aes_string(x="x",y="labely",label="label2",hjust="hjust"),
                parse=TRUE)
    p
    p<-p+geom_hline(yintercept=0,color=linecolor,linetype=3)
    p<-p + geom_segment(data=df,aes_string(x="x",y="y",xend="x",yend="yend"),
                     arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),
                     color="red",linetype=linetype,size=arrowsize)
    p+labs(x=paste(pred,"(W)"),
           y=expression(paste("Conditional Effect (", theta[italic(X) %->%italic(Y)],")")))

  }

}

#' Draw johnson_neyman plot
#' @param fit A regression model
#' @param pred name of predictor variable
#' @param modx name of moderator variable
#' @param digits integer indicating the number of decimal places
#' @param plot logical. Whether or not draw plot
#' @param ... Further argumant to be passed to interactions::johnson_neyman()
#' @importFrom interactions johnson_neyman
#' @export
#' @examples
#' fit=lm(mpg~hp*wt,data=mtcars)
#' jnPlot(fit)
#' fit=lm(justify~skeptic*frame,data=disaster)
#' res=jnPlot(fit,plot=FALSE)
#' res$plot
jnPlot=function(fit,pred=NULL,modx=NULL,digits=3,plot=TRUE,...){
  data=fit$model

  if(is.null(pred)) pred=colnames(data)[2]
  if(is.null(modx)) modx=colnames(data)[3]

  temp=paste0("interactions::johnson_neyman(fit,pred=",modx,",modx=",pred,
              ",digits=",digits,",...)")
  res=eval(parse(text=temp))
  p<-res$plot
  info=getAspectRatio(p)
  label=paste0("italic(W) ==",sprintf(paste0("%0.",digits,"f"),res$bounds))
  df=data.frame(x=res$bounds,y=info$ymin,label=label,stringsAsFactors = FALSE)
  ylab=expression(paste("Conditional Effect (",theta[italic(X) %->% italic(Y)],")"))
  xlab=paste(pred,"(W)")
  p<-p+geom_text(data=df,aes_string(x="x",y="y",label="label"),parse=TRUE)+
    labs(y=ylab,x=xlab)
  if(plot) print(p)
  res$plot<-p
  res
}


