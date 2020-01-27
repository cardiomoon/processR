#'Draw conditional effect plot
#'@param fit An onject of class lm
#'@param xmode integer. 1 or 2.
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
#'@param depM logical If true, label M instead of X
#'@param ... further arguments to be passed to add_lines
#'@importFrom predict3d add_lines calEquation
#'@importFrom tidyr crossing spread
#'@importFrom ggplot2 geom_vline geom_segment geom_hline labs
#'@importFrom grid arrow unit
#'@importFrom stats sd
#'@export
#'@examples
#'fit=lm(justify~frame*skeptic,data=disaster)
#'condPlot(fit,rangemode=2,xpos=0.7,labels=c("Climate change(X=1)","Natural causes(X=0)"))
#'\donttest{
#'condPlot(fit,mode=2,xpos=0.6)
#'condPlot(fit,mode=3,rangemode=2,xpos=0.5)
#'condPlot(fit,xmode=2)
#'condPlot(fit,xmode=2,mode=2)
#'condPlot(fit,xmode=2,mode=3)
#'fit=lm(mpg~vs*hp,data=mtcars)
#'condPlot(fit,rangemode=2,xpos=0.6)
#'condPlot(fit,mode=2,xpos=0.5)
#'condPlot(fit,mode=3,rangemode=2)
#'fit=lm(govact~negemot*age+posemot+ideology+sex,data=glbwarm)
#'condPlot(fit,xmode=2,hjust=c(-0.1,-0.1,1.1))
#'condPlot(fit,xmode=2,pred.values=c(30,70),hjust=c(-0.1,-0.1,1.1),xpos=0.5)
#'condPlot(fit,xmode=2,mode=2,pred.values=c(30,50,70),xpos=0.2)
#'condPlot(fit,xmode=2,mode=3,xpos=0.5,hjust=c(-0.1,-0.1,1.1))
#'condPlot(fit,xmode=2,modx.values=c(2,3,4),mode=3,xpos=0.6)
#'}
condPlot=function(fit,xmode=1,pred=NULL,modx=NULL,pred.values=NULL,modx.values=NULL,labels=NULL,
                  mode=1,rangemode=1,ypos=NULL,hjust=NULL,linecolor="gray60",linetype=2,
                  linesize=1,arrowsize=1,digits=3,depM=FALSE,...){
          # fit=lm(justify~skeptic*frame,data=disaster)
            # fit=lm(justify~frame*skeptic,data=disaster)
  # fit=lm(govact~negemot*age+posemot+ideology+sex,data=glbwarm)
  # pred=NULL;modx=NULL;pred.values=NULL;modx.values=NULL
  # mode=3;rangemode=2;digits=3;xmode=1;depM=FALSE
  # ypos=NULL;linecolor="gray60";linetype=2;linesize=1;arrowsize=1;digits=3
  # # # modx.values=c(30,70)


  data=fit$model
  data
  dep=colnames(data)[1]
  if(is.null(pred)) {
      pred=ifelse(xmode==1,colnames(data)[2],colnames(data)[3])
  }
  if(is.null(modx)) modx=setdiff(colnames(data)[2:3],pred)
  if(length(colnames(data)[3])>3) {
    mod2=colnames(data)[4]
  } else {
    mod2=NULL
  }

  if(is.null(modx.values)){
    if(length(unique(data[[modx]]))<6) {
      modx.values=unique(data[[modx]])
    } else if(rangemode==1){
      modx.values=mean(data[[modx]],na.rm=T)+c(-1,0,1)*sd(data[[modx]],na.rm=T)
    } else{
      modx.values=quantile(data[[modx]],probs=c(0.16,0.5,0.84),type=6)
    }
  }
  if(is.null(pred.values)){
    if(length(unique(data[[pred]]))<6) {
      pred.values=unique(data[[pred]])
    } else if(rangemode==1) {
      pred.values=mean(data[[pred]],na.rm=TRUE)+c(-1,0,1)*sd(data[[pred]],na.rm=TRUE)
    } else if(rangemode==2) {
      pred.values=quantile(data[[pred]],probs=c(0.16,0.5,0.84),type=6)
    }
  }


  if(mode==1){

  coef<-pvalue<-c()

  for(i in seq_along(modx.values)){
    equation=deparse(fit$call)
    temp=stringr::str_replace(equation,modx,paste0("I(",modx,"-",modx.values[i],")"))
    fit1=eval(parse(text=temp))
    coef=c(coef,fit1$coef[pred])
    pvalue=c(pvalue,summary(fit1)$coef[pred,4])
  }

  effectDf=data.frame(modx.values,coef,pvalue)
  colnames(effectDf)[1]="x"
  effectDf


  pred.values=pred.values[c(1,length(pred.values))]
  p=ggplot(data,aes_string(x=modx,y=dep))
  df1=calEquation(fit,pred=modx,modx.values = pred.values)
  df1
  if(!is.null(labels)){
      if(nrow(df1)==length(labels)) df1$label=labels
  }

       # p<-add_lines(p,df1)
    p<-add_lines(p,df1,size=linesize,...)
     # p<-add_lines(p,df1,size=linesize)
  p
  info=getAspectRatio(p)
  ratio=info$ratio
  df1$slope2=df1$slope*ratio
  df1$radian=atan(df1$slope2)
  df1$angle=df1$radian*180/pi

  df1[[pred]]=pred.values
  df1

  df=tidyr::crossing(pred.values,modx.values)
  df
  names(df)=c(pred,modx)
  df

  df<-dplyr::left_join(df,df1)

  df[[dep]]=df$slope*df[[modx]]+df$intercept  #

  df
  df<-df[c(modx,pred,dep)]
  df %>% spread(key=2,value=3) -> df2

  colnames(df2)=c("x","y","yend")
  df2
  df3<-left_join(effectDf,df2)
  df3$coef=myformat(df3$coef,digits)
  if(xmode==1) {
    df3$label3=paste0("italic(W) ==",round(df3$x,digits))
  } else{
    if(depM){
      df3$label3=paste0("italic(M) ==",round(df3$x,digits))
    } else{
      df3$label3=paste0("italic(X) ==",round(df3$x,digits))
    }
  }
  df3$p1=myformat(df3$pvalue,digits=3)
  df3$p1=pformat(df3$p1)
  df3$p2=ifelse(df3$p1=="<.001","<.001",paste0("= ",df3$p1))

  if(depM){
    df3$label=paste0("theta[italic(M) %->% italic(Y)] == ",df3$coef)
  } else{
  df3$label=paste0("theta[italic(X) %->% italic(Y)] == ",df3$coef)
  }

  df3$label2= paste0("italic(p),'",df3$p2,"'")
  df3$label=paste0("paste(",df3$label,",', ',",df3$label2,")")
  df3$angle1=df1$angle[1]
  df3$angle2=df1$angle[2]
  df3$vjust1=ifelse(df3$y>df3$yend,1.2,-0.2)
  df3$vjust2=ifelse(df3$y>df3$yend,-0.2,1.2)
  df3$y1=(df3$y+df3$yend)/2
  df3$labely=df3$yend
  if(!is.null(ypos)){
      if(length(ypos)==1) ypos=rep(ypos,nrow(df3))
      for(i in seq_along(ypos)){
           df3$labely[i]=min(df3$y[i],df3$yend[i])+abs(df3$y[i]-df3$yend[i])*ypos[i]
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
  p<-p+geom_text(data=df3,aes_string(x="x",y="labely",label="label",hjust="hjust",vjust=1),
              parse=TRUE) +
    geom_text(data=df3,aes_string(x="x",y=info$ymin,label="label3"),
              parse=TRUE)

  p<-p + xlab(paste0(modx,ifelse(xmode==1,"(W)",ifelse(depM,"(M)","(X)"))))
  p

  } else if(mode==2) {
    # coef<-pvalue<-c()


    df1= calEquation(fit,pred=modx, modx.values = pred.values)

    df1[[pred]]=pred.values
    df1
    if(xmode==1){
    df1$label1=paste0("hat(Y) ==",round(df1$slope,digits),
                     "*italic(W)",ifelse(df1$intercept>=0,"+","-"),
                     abs(round(df1$intercept,digits)))
    df1$label2=paste0(" | ",pred,"=",round(df1[[pred]],digits))
    } else{
      df1$label1=paste0("theta[italic(",ifelse(depM,"M","X"),") %->% italic(Y)] == ",
                        sprintf("%0.3f",round(df1$slope,digits)))
      df1$label2=paste0(" | ",pred,"(W) = ",round(df1[[pred]],digits))
    }
    df1$label=paste0("paste(",df1$label1,",'",df1$label2,"')")
    p=ggplot(data,aes_string(x=modx,y=dep))
    p<-add_lines(p,df1,parse=TRUE,size=linesize,...)
    p + xlab(paste(modx,ifelse(xmode==1,"(W)",ifelse(depM,"(M)","(X)"))))

  } else if(mode==3){

    coef<-pvalue<-c()
    for(i in seq_along(modx.values)){
      equation=deparse(fit$call)
      temp=stringr::str_replace(equation,modx,paste0("I(",modx,"-",modx.values[i],")"))
      fit1=eval(parse(text=temp))
      coef=c(coef,fit1$coef[pred])
      pvalue=c(pvalue,summary(fit1)$coef[pred,4])
    }

    effectDf=data.frame(modx.values,coef,pvalue)
    colnames(effectDf)[1]="x"
    df=effectDf
    df
    b1=fit$coef[pred]
    b1
    interaction=paste0(pred,":",modx)
    if(!(interaction %in% names(fit$coef))) interaction=paste0(modx,":",pred)
    interaction

    b3=fit$coef[interaction]
    b3
    fun=function(x){b1+b3*x}

    df$yend=0
    df$y=fun(df$x)
    df$labely=df$y
    if(!is.null(ypos)){
        if(length(ypos)==1) ypos=rep(ypos,nrow(df))
        for(i in seq_along(ypos)){
            df$labely[i]=min(df$y[i],df$yend[i])+abs(df$y[i]-df$yend[i])*ypos[i]
        }
    }
    df1=data.frame(slope=b3,intercept=b1)

    if(depM){
      df1$label=paste0("theta[italic(M)%->%italic(Y)]==",round(b1,digits),
                       ifelse(b3>=0,"+",""),round(b3,digits),"*italic(W)")
    } else{
    df1$label=paste0("theta[italic(X)%->%italic(Y)]==",round(b1,digits),
                     ifelse(b3>=0,"+",""),round(b3,digits),"*italic(W)")
    }
    df1
    if(!is.null(labels)){
        if(nrow(df1)==length(labels)) df1$label=labels
    }
    if(xmode==1) {
      df$label=paste0("italic(W) ==",round(df$x,digits))
    } else{
      df$label=paste0("italic(",ifelse(depM,"M","X"),") ==",round(df$x,digits))
    }
    df$p1=myformat(df$pvalue,digits=3)
    df$p1=pformat(df$p1)
    df$p2=ifelse(df$p1=="<.001","<.001",paste0("= ",df$p1))


    df$label3=paste0("theta[italic(",ifelse(depM,"M","X"),
                     ")%->%italic(Y)]==",round(df$coef,digits))
    df$label4= paste0("italic(p),'",df$p2,"'")
    df$label2=paste0("paste(",df$label3,",', ',",df$label4,")")
    df$hjust=-0.1
    if(!is.null(hjust)){
        df$hjust=hjust
    }

    p<-ggplot(data=data,aes_string(x=modx))
    # p<-add_lines(p,df1,parse=TRUE,vjust=-0.3,size=linesize)
    p<-add_lines(p,df1,parse=TRUE,vjust=-0.3,size=linesize,...)
    p
    info=getAspectRatio(p)
    for(i in seq_along(modx.values)){
       p<-p+geom_vline(xintercept=modx.values[i],color=linecolor,linetype=2)
    }
    p<-p+ geom_text(data=df,aes_string(x="x",label="label"),y=info$ymin,parse=TRUE)+
      geom_text(data=df,aes_string(x="x",y="labely",label="label2",hjust="hjust"),
                parse=TRUE)
    p
    p<-p+geom_hline(yintercept=0,color=linecolor,linetype=3)
    p<-p + geom_segment(data=df,aes_string(x="x",y="y",xend="x",yend="yend"),
                     arrow=arrow(angle=20,length=unit(0.3,"cm"),type="closed"),
                     color="red",linetype=linetype,size=arrowsize)
    if(depM){
      p<-p+labs(x=paste(modx,ifelse(xmode==1,"(W)","(M)")),
             y=expression(paste("Conditional Effect (", theta[italic(M) %->%italic(Y)],")")))

    } else{
    p<-p+labs(x=paste(modx,ifelse(xmode==1,"(W)","(X)")),
           y=expression(paste("Conditional Effect (", theta[italic(X) %->%italic(Y)],")")))
    }
    p

  }

}

#' Draw johnson_neyman plot
#' @param fit A regression model
#' @param pred name of predictor variable
#' @param modx name of moderator variable
#' @param digits integer indicating the number of decimal places
#' @param plot logical. Whether or not draw plot
#' @param mode integer 1 or 2
#' @param addEq logical
#' @param xvar Name of xvar
#' @param ... Further argumant to be passed to interactions::johnson_neyman()
#' @importFrom interactions johnson_neyman
#' @export
#' @examples
#' fit=lm(mpg~hp*wt,data=mtcars)
#' jnPlot(fit)
#' \donttest{
#' fit=lm(justify~frame*skeptic,data=disaster)
#' res=jnPlot(fit)
#' res$plot
#' fit=lm(govact~negemot*sex*age+posemot+ideology,data=glbwarm)
#' jnPlot(fit,pred="negemot:sex",modx="age",mode=2,addEq=TRUE)
#' }
jnPlot=function(fit,pred=NULL,modx=NULL,digits=3,plot=FALSE,mode=1,xvar="Z",addEq=FALSE,...){
  data=fit$model
  # pred=NULL;modx=NULL;digits=3;plot=FALSE;mode=1;addEq=TRUE
  # pred="negemot:sex";modx="age";mode=2;addEq=TRUE

  if(is.null(pred)) pred=colnames(data)[2]
  if(is.null(modx)) modx=colnames(data)[3]


  if(str_detect(pred,":")){
    vars=unlist(strsplit(pred,":"))
    vars
    newvar=str_replace(pred,":","")
    newvar
    data=fit$model
    data[[newvar]]=data[[vars[1]]]*data[[vars[2]]]
    indep=names(fit$coef)[-1]
    indep=str_replace(indep,pred,newvar)
    indep

    temp=unlist(strsplit(deparse(fit$call),"~"))[1]
    equation=paste0(temp,"~",paste0(indep,collapse="+"),",data=data)")
    fit=eval(parse(text=equation))
    pred=newvar
  }

  temp=paste0("interactions::johnson_neyman(fit,pred=",pred,",modx=",modx,
              ",digits=",digits,",...)")
  # temp=paste0("interactions::johnson_neyman(fit,pred=",pred,",modx=",modx,
  #             ",digits=",digits,")")
  res=eval(parse(text=temp))
  p<-res$plot
  info=getAspectRatio(p)

  if(mode==1){
    label=paste0("italic(W) ==",sprintf(paste0("%0.",digits,"f"),res$bounds))
    ylab=expression(paste("Conditional Effect (",theta[italic(X) %->% italic(Y)],")"))
    xlab=paste(modx,"(W)")
  } else{
    if(xvar=="Z"){
    label=paste0("italic(Z) ==",sprintf(paste0("%0.",digits,"f"),res$bounds))
    ylab=expression(paste("Conditional Effect (",theta[italic(X)*italic(W) %->% italic(Y)],")"))
    xlab=paste(modx,"(Z)")
    } else{
      label=paste0("italic(W) ==",sprintf(paste0("%0.",digits,"f"),res$bounds))
      ylab=expression(paste("Conditional Effect (",theta[italic(X)*italic(Z) %->% italic(Y)],")"))
      xlab=paste(modx,"(W)")
    }
  }
  df=data.frame(x=res$bounds,y=info$ymin,label=label,stringsAsFactors = FALSE)
  p<-p+geom_text(data=df,aes_string(x="x",y="y",label="label"),parse=TRUE)+
    labs(y=ylab,x=xlab)
  p
  if(addEq) {
    x1=res$cbands[1,1]
    y1=res$cbands[1,2]
    count=nrow(res$cbands)
    x2=res$cbands[count,1]
    y2=res$cbands[count,2]
    eq=getEq2p(x1,y1,x2,y2)
    temp=paste0(round(eq$intercept,digits),ifelse(eq$slope>=0," + "," - "),
                round(abs(eq$slope),digits))
    if(mode==1) {
      label=paste0("theta[italic(X) %->% italic(Y)] == ",temp,"*W")
    } else{
      if(xvar=="Z"){
         label=paste0("theta[italic(X)*italic(W) %->% italic(Y)] == ",temp,"*Z")
      } else{
        label=paste0("theta[italic(X)*italic(Z) %->% italic(Y)] == ",temp,"*W")
      }
    }
    df1=data.frame(x=(info$xmin+info$xmax)/2,y=info$ymax,
                   label=label,stringsAsFactors = FALSE)
    p<-p+geom_text(data=df1,aes_string(x="x",y="y",label="label"),parse=TRUE,vjust=1)

  }
  if(plot) print(p)
  res$plot<-p
  res
}

#'get slope and intercept with 2 points
#'@param x1 x coordinate of the first point
#'@param y1 y coordinate of the first point
#'@param x2 x coordinate of the second point
#'@param y2 y coordinate of the second point
#'@export
getEq2p=function(x1,y1,x2,y2){
  slope=(y1-y2)/(x1-x2)
  intercept=y1-slope*x1
  list(slope=slope,intercept=intercept)
}

#'Make Slopes Plot
#'@param ss An object of class sim_slopes
#'@param color Name of color
#'@param size size of pointrange
#'@param digits An integer indicating the number of decimal places
#'@importFrom ggplot2 geom_pointrange scale_x_discrete theme element_blank coord_flip
#'@export
#'@return A ggplot
plotCoef=function(ss,color="deepskyblue2",size=0.75,digits=1){
  df=ss$slopes
  pred=attr(ss,"pred")
  modx=attr(ss,"modx")
  colnames(df)=c("x","y","se","ymin","ymax","t","p")
  df=data.frame(df)
  df$x=factor(round(df$x,digits=digits))
  ggplot(data=df,aes_string(x="x",y="y",ymin="ymin",ymax="ymax"))+
    geom_pointrange(color=color,fill="white",pch=21,size=size)+theme_bw()+
    geom_hline(yintercept=0,lty=2,color="darkgray")+
    scale_x_discrete(breaks=df$x,labels=paste0(modx," = ",df$x))+
    theme(panel.grid.minor.x=element_blank())+
    labs(y=paste0("Slope of ",pred),x="")+
    coord_flip()
}

