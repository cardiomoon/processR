#' Make simple regression model with one categorical variable
#' @param labels Named list of variables
#' @param yvar Label of the dependent variable. Either "Y"(default) or "M".
#' @param total logical. If true, model include mediator variable.
#' @param data A data.frame
#' @param addvars logical. Whether or not add categorical variables to the data
#' @param maxylev maximal unique length of categorical variable
#' @param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#' @importFrom stats as.formula
#' @export
#' @return An object of class lm
#' @examples
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' data1=addCatVars(protest,"protest")
#' makeCatModel(labels=labels,data=data1)
makeCatModel=function(labels=labels,data,yvar="Y",total=FALSE,addvars=TRUE,maxylev=6,mode=1){
    # labels=list(X="protest",W="sexism",M="respappr",Y="liking")
    # yvar="Y";total=TRUE;data=protest;addvars=TRUE;maxylev=6;mode=1
  X=labels$X
  W=labels$W
  Y=ifelse(yvar=="Y",labels$Y,labels$M)
  if(total==TRUE){
     moderator=list(name=labels$W,site=list(c("a","c")))
     eq=catMediation(X=X,M=labels$M,Y=Y,moderator=moderator,data=data,maxylev=maxylev,mode=1)

     model=unlist(strsplit(eq,"\n"))[2]
  } else{

    model=makeCatEquation(X=X,Y=Y,W=W,data=data,mode=1)

  }

  if(addvars){
    if(length(unique(data[[X]]))<=maxylev) catVar="X"
    if(length(unique(data[[W]]))<=maxylev) catVar="W"
    if(catVar=="X") data1=addCatVars(data,X,mode=mode)
    if(catVar=="W") data1=addCatVars(data,W,mode=mode)
  } else{
    data1=data
  }
  fit=lm(as.formula(model),data=data1)
  fit
}

#' Make data summarizing conditional effects
#' @param labels Named list of variables
#' @param data A data.frame
#' @param yvar Label of the dependent variable. Either "Y"(default) or "M".
#' @param total logical. If true, model include mediator variable.
#' @param addvars logical. Whether or not add categorical variables to the data
#' @param maxylev maximal unique length of categorical variable
#' @param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#' @param rangemode rangemode. 1 or 2.
#' @export
#' @examples
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' data1=addCatVars(protest,varnames="protest",mode=1)
#' makeCEDf(labels=labels,data=protest,mode=1)
makeCEDf=function(labels=labels,data,yvar="Y",total=FALSE,addvars=TRUE,
                  maxylev=6,mode=1,rangemode=2){
  X=labels$X
  W=labels$W
  Y=ifelse(yvar=="Y",labels$Y,labels$M)
  if(length(unique(data[[X]]))<=maxylev) catVar="X"
  if(length(unique(data[[W]]))<=maxylev) catVar="W"

  if(catVar=="W"){
    W=labels$X
    X=labels$W
  }

  if(rangemode==2) {
      wvalues=quantile(data[[W]],probs=c(0.16,0.5,0.84),type=6)
  } else{
      wvalues=mean(data[[W]],na.rm=TRUE)+c(-1,0,1)*sd(data[[W]],na.rm=TRUE)
  }
  if(total==TRUE){
    moderator=list(name=labels$W,site=list(c("a","c")))
    eq=catMediation(X=X,M=labels$M,Y=Y,moderator=moderator,data=data,maxylev=maxylev,mode=1)
    unlist(strsplit(eq,"\n"))[2]
    model=unlist(strsplit(eq,"\n"))[2]
  } else{

    model=makeCatEquation(X=X,Y=Y,W=W,data=data,mode=1)

  }
  model
  if(addvars) {
    data1=addCatVars(data,varnames=X,mode=mode)
  } else{
    data1=data
  }

  fit<-list()
  d1<-d2<-p1<-p2<-c()
  for(i in 1:3){
    temp=str_replace_all(model,W,paste0("I(",W,"-",wvalues[i],")"))
    fit[[i]]<-lm(as.formula(temp),data=data1)
    d1=c(d1,fit[[i]]$coef["D1"])
    d2=c(d2,fit[[i]]$coef["D2"])
    p1<-c(p1,summary(fit[[i]])$coef["D1",4])
    p2<-c(p2,summary(fit[[i]])$coef["D2",4])
  }
  df=data.frame(W=wvalues,d1=d1,p1=p1,d2=d2,p2=p2)
  df
}


#' Make data summarizing ANOVA results
#' @param labels Named list of variables
#' @param data A data.frame
#' @param yvar Label of the dependent variable. Either "Y"(default) or "M".
#' @param total logical. If true, model include mediator variable.
#' @param addvars logical. Whether or not add categorical variables to the data
#' @param maxylev maximal unique length of categorical variable
#' @param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#' @param rangemode rangemode. 1 or 2.
#' @export
#' @examples
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' makeAnovaDf(labels=labels,data=protest,total=TRUE,mode=3)
makeAnovaDf=function(labels,data,yvar="Y",total=FALSE,addvars=TRUE,maxylev=6,mode=1,rangemode=2){

  # labels=list(X="protest",W="sexism",M="respappr",Y="liking")
  # data=protest
  # yvar="Y";total=TRUE;addvars=TRUE;maxylev=6;mode=3;rangemode=2

  X=labels$X
  W=labels$W
  Y=ifelse(yvar=="Y",labels$Y,labels$M)
  if(length(unique(data[[X]]))<=maxylev) catVar="X"
  if(length(unique(data[[W]]))<=maxylev) catVar="W"

  if(catVar=="W"){
    W=labels$X
    X=labels$W
  }
  if(rangemode==2) {
    wvalues=quantile(data[[W]],probs=c(0.16,0.5,0.84),type=6)
  } else{
    wvalues=mean(data[[W]],na.rm=TRUE)+c(-1,0,1)*sd(data[[W]],na.rm=TRUE)
  }
  if(total==TRUE){
    moderator=list(name=labels$W,site=list(c("a","c")))
    eq=catMediation(X=X,M=labels$M,Y=Y,moderator=moderator,data=data,maxylev=maxylev,mode=1)
    unlist(strsplit(eq,"\n"))[2]
    model=unlist(strsplit(eq,"\n"))[2]
  } else{

    model=makeCatEquation(X=X,Y=Y,W=W,data=data,mode=1)

  }


  if(addvars) {
    data1=addCatVars(data,X,mode=mode)
  } else{
    data1=data
  }

  fit<-list()
  for(i in 1:3){
    temp=str_replace_all(model,W,paste0("I(",W,"-",wvalues[i],")"))
    fit[[i]]<-lm(as.formula(temp),data=data1)
  }

  temp=unlist(strsplit(model,"~"))[2]
  temp2=unlist(strsplit(temp,"\\+"))
  temp3=temp2[str_detect(temp2,W)|str_detect(temp2,":")]
  temp3
  if(total==TRUE) temp3=c(temp3,labels$M)
  model1=paste0(unlist(strsplit(model,"~"))[1],"~",paste0(temp3,collapse="+"))
  fit1<-list()
  Fvalue=p=c()
  for(i in 1:3){
    temp=str_replace_all(model1,W,paste0("I(",W,"-",wvalues[i],")"))
    temp
    fit1[[i]]<-lm(as.formula(temp),data=data1)
    res=anova(fit1[[i]],fit[[i]])
    Fvalue=c(Fvalue,res$F[2])
    p=c(p,res$`Pr(>F)`[2])
  }
  df3=data.frame(W=wvalues,F=Fvalue,p=p,df=res$Df[2],df2=res$Res.Df[2])
  df3[[2]]=myformat(df3[[2]])
  df3[["p1"]]=myformat(df3[[3]])
  df3[["p1"]]=pformat(df3[["p1"]])
  df3$label1=paste0("italic(F)(",df3$df,",",df3$df2,") ==",df3$F)
  df3$label2=paste0("italic(p),' ",ifelse(df3$p1=="<.001","<.001",paste0("= ",df3$p1)),"'")
  df3$label=paste0("paste(",df3$label1,",', ',",df3$label2,")")
  df3$label3=paste0("F = ",df3$F,"\np ",ifelse(df3$p1=="<.001","<.001",paste0("= ",df3$p1)))
  df3
}



#' Make data summarizing regression slopes and intercepts
#' @param labels Named list of variables
#' @param data A data.frame
#' @param yvar Label of the dependent variable. Either "Y"(default) or "M".
#' @param total logical. If true, model include mediator variable.
#' @param addvars logical. Whether or not add categorical variables to the data
#' @param add.label logical
#' @param maxylev maximal unique length of categorical variable
#' @param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#' @param rangemode rangemode. 1 or 2.
#' @export
#' @examples
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' getCatSlopeDf(labels=labels,yvar="M",data=protest,mode=3)
#' getCatSlopeDf(labels=labels,yvar="M",data=protest,mode=1)
getCatSlopeDf=function(labels=NULL,data,yvar="Y",total=FALSE,addvars=TRUE,
                       add.label=FALSE,
                       maxylev=6,mode=1,rangemode=2){

        # labels=list(X="protest",W="sexism",M="respappr",Y="liking")
        # data=protest;yvar="Y";total=TRUE;addvars=TRUE
        # add.label=FALSE;maxylev=6;mode=3;rangemode=2


  # data1=addCatVars(protest,"protest",mode=3)
  # labels1=list(X="protest",Y="respappr",W="sexism")
  # labels1
  # labels=labels1
  # data=data1
  # add.label=FALSE;maxylev=6
  #

    fit=makeCatModel(labels=labels,data=data,yvar=yvar,total=total,addvars=addvars,mode=mode)

      # summary(fit)

    X=labels$X
    W=labels$W
    Y=ifelse(yvar=="Y",labels$Y,labels$M)
    if(length(unique(data[[X]]))<=maxylev) catVar="X"
    if(length(unique(data[[W]]))<=maxylev) catVar="W"

    if(catVar=="W"){
        W=labels$X
        X=labels$W
    }

    if(total==TRUE){
      moderator=list(name=labels$W,site=list(c("a","c")))
      eq=catMediation(X=X,M=labels$M,Y=Y,moderator=moderator,data=data,maxylev=maxylev,mode=1)
      model=unlist(strsplit(eq,"\n"))[2]
      temp=unlist(strsplit(model,"~"))[2] %>%
        strsplit("\\+") %>% unlist()
      model=paste0(Y,"~",paste(paste0("b",1:length(temp),"*",temp),collapse="+"))
    } else{

      model=makeCatEquation(X=X,Y=Y,W=W,data=data,mode=0)

    }

    model
    count=length(unique(data[[X]]))

    ratio=getRatioTable(count,mode=mode)
    ratio
    colnames(ratio)=paste0("D",1:(count-1))
    eq=unlist(strsplit(model,"~"))[2]
    eq
    eq=str_replace_all(eq,":","*")
    eq=str_replace_all(eq,W,"W")
    if(total==TRUE) {
       eq=str_replace_all(eq,labels$M,as.character(mean(data[[labels$M]],na.rm=TRUE)))
    }
    eq=paste0("b0+",eq)
    eq1=unlist(strsplit(eq,"\\+"))
    eq1
    ratio

    intercept<-slope<-list()

    for(i in 1:count){
        temp=eq1
        temp
        ncol(ratio)
        for(j in 1:ncol(ratio)){
            temp=str_replace(temp,paste0("D",j),as.character(round(ratio[i,j],3)))
        }
        temp


        temp1=temp[str_detect(temp,"W")]
        temp2=strGrouping(temp1,"W")$yes
        slope[[i]]=paste0(temp2,collapse="+")
        intercept[[i]]=paste0(temp[!str_detect(temp,"W")],collapse="+")
    }

    for(i in 1:length(fit$coef)){
        assign(paste0("b",i-1),fit$coef[i])
    }
    slope
    intercept
    slope1=unlist(lapply(slope,function(x){eval(parse(text=x))}))
    intercept1=unlist(lapply(intercept,function(x){eval(parse(text=x))}))
    if(mode==3) {
       slope1=c(slope1,(slope1[2]+slope1[3])/2)
       intercept1=c(intercept1,(intercept1[2]+intercept1[3])/2)
    }
    df=data.frame(slope=slope1,intercept=intercept1)
    df
    if(add.label) {
        df$label=paste0(sprintf("%0.3f",df$intercept),ifelse(df$slope>=0," + "," - "),
                        sprintf("%0.3f",abs(df$slope)),"*italic(W)")
    } else{
        df$label=""
    }

    df
}


#' Make conditional effect plot with data including a categorical variable
#' @param labels Named list of variables
#' @param data A data.frame
#' @param yvar character. "Y"(default) or "M"
#' @param total logical. If true, model include mediator variable.
#' @param addvars logical
#' @param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#' @param rangemode rangemode. 1 or 2.
#' @param maxylev maximal unique length of categorical variable
#' @param catlabels optional string of labels for the categorical variable
#' @param add.slopelabel logical
#' @param xpos  numeric. x position of slope labels
#' @param add.point logical. If true, add point to the plot
#' @param add.vlines logical. If true, add vlines to the plot
#' @param add.vlines.text logical. If true, add vlines.text to the plot
#' @param add.anova logical. If true, add results of ANOVA to the plot
#' @param ypos optional. Y position of anova results
#' @param add.arrow logical. If true, add conditional effects to the plot
#' @param xinterval Integer. Width of angled arrow
#' @param hjust1 optional. hjust of conditional effects 1
#' @param hjust2 optional. hjust of conditional effects 2
#' @param ypos2 optional. Y position of conditional effects 1
#' @param ypos3 optional. Y position of conditional effects 2
#' @param ceno integer. 1 or 2
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot geom_point geom_curve
#' @importFrom ggrepel geom_text_repel
#' @export
#' @examples
#' \donttest{
#' library(ggplot2)
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' catlabels=c("No protest","Individual protest","Collective protest")
#' catlabels2=c("No protest","Individual protest","Collective protest","Any protest")
#' condPlotCat(labels=labels,yvar="M",data=protest,mode=3,ypos=c(0.2,0.15,0.1))
#' condPlotCat(labels=labels,yvar="M",data=protest,mode=3,ceno=c(1,2),add.vlines.text=FALSE)
#' condPlotCat(labels=labels,catlabels=catlabels,yvar="M",data=protest,mode=3,
#'      add.arrow=FALSE,addvars=FALSE)
#' condPlotCat(labels=labels,yvar="M",data=protest,mode=3,catlabels=catlabels2,ceno=c(1,2))
#' condPlotCat(labels=labels,data=protest,catlabels=catlabels,add.slopelabel=TRUE,
#'      xpos=c(0.3,0.7,0.7),add.point=FALSE,add.vlines=FALSE,add.anova=FALSE,add.arrow=FALSE)
#' condPlotCat(labels=labels,data=protest,catlabels=catlabels,add.anova=FALSE,add.arrow=FALSE)
#' condPlotCat(labels=labels,data=protest,catlabels=catlabels,add.anova=FALSE)+xlim(c(3.5,6.5))
#' condPlotCat(labels=labels,data=protest,add.anova=TRUE,ypos=c(0.2,0.2,0.5),add.arrow=FALSE)
#' condPlotCat(labels=labels,data=protest,catlabels=catlabels,add.anova=FALSE,ceno=1)
#' condPlotCat(labels=labels,data=protest,catlabels=catlabels,add.anova=FALSE,ceno=2)
#' condPlotCat(labels=labels,data=protest,total=TRUE,catlabels=catlabels,ypos=0.1,
#'       add.arrow=FALSE)+xlim(c(4,6))
#' condPlotCat(labels=labels,data=protest,total=TRUE,catlabels=catlabels2,add.anova=FALSE,
#'       ceno=c(1,2),xinterval=0.05,hjust1=c(-0.05,-0.05,1.05),hjust2=c(-0.05,1.05,1.05),
#'       ypos2=c(0.5,0.1,0.3),ypos3=c(0.2,0.4,0.4),mode=3)+xlim(c(4,6))
#' }
condPlotCat=function(labels=list(),yvar="Y",total=FALSE,data,addvars=TRUE,mode=1,rangemode=2,maxylev=6,
                     catlabels=NULL,add.slopelabel=FALSE,
                     xpos=0.5,
                     add.point=TRUE,add.vlines=TRUE,add.vlines.text=TRUE,add.anova=TRUE,ypos=NULL,
                     add.arrow=TRUE,xinterval=NULL,hjust1=NULL,hjust2=NULL,ypos2=NULL,ypos3=NULL,ceno=1){

  # labels=list(X="protest",M="respappr",Y="liking",W="sexism")
  # data=protest;yvar="M";addvars=TRUE;mode=3;rangemode=2
  # maxylev=6;xpos=0.5;total=FALSE
  # catlabels=NULL;add.slopelabel=FALSE
  # add.point=TRUE;add.vlines=TRUE;add.anova=TRUE;ypos=NULL
  # add.arrow=TRUE;hjust=NULL;ypos2=NULL;ceno=1;xinterval=NULL

  fit=makeCatModel(labels=labels,data=data,yvar=yvar,total=total,
                   addvars=TRUE,maxylev=6,mode=mode)

  X=labels$X
  W=labels$W
  Y=ifelse(yvar=="M",labels$M,labels$Y)
  if(length(unique(data[[X]]))<=maxylev) catVar="X"
  if(length(unique(data[[W]]))<=maxylev) catVar="W"

  if(catVar=="W"){
    W=labels$X
    X=labels$W
  }


  slopeDf=getCatSlopeDf(labels=labels,data=data,yvar=yvar,total=total,addvars=TRUE,
                        mode=mode,rangemode=rangemode,add.label=add.slopelabel,
                        maxylev=maxylev)
  if(!addvars) slopeDf=slopeDf[-nrow(slopeDf),]
  p<-ggplot(data=data,aes_string(x=W,y=Y))

  p<-add_lines(p,slopeDf,add.coord.fixed=add.slopelabel,size=1,xpos=xpos,parse=TRUE)
  p


  count=nrow(slopeDf)
  x=quantile(data[[W]],probs=c(0.16,0.5,0.84),type=6)
  df=data.frame(x=rep(x,count))
  df$color=rep(1:count,each=3)
  df
  df$slope=slopeDf$slope[rep(1:count,each=3)]
  df$intercept=slopeDf$intercept[rep(1:count,each=3)]
  df$y=df$x*df$slope+df$intercept
  df
  info=getAspectRatio(p)
  info

  if(is.null(catlabels)) {
    catlabels=paste0("D",0:(count-1))
  }
  df$group=rep(catlabels,each=3)
  df$group1=reorder(df$group,df$color)
  df$label=sprintf("%0.3f",df$y)
  if(add.vlines){
    if(catVar=="X"){
      df2=data.frame(x=x,y=info$ymin,
                     label=paste0("W = ",sprintf("%0.3f",x)))
    } else{
      df2=data.frame(x=x,y=info$ymin,
                     label=paste0("X = ",sprintf("%0.3f",x)))
    }

    df2
    p<-p+geom_vline(xintercept=x,lty=2,color="gray")
    if(add.vlines.text) {
      p<-p+geom_text(data=df2,aes_string(x="x",y="y",label="label"),family="Times",fontface="italic")
    }
  }
  p
  if(add.point){
    p<-p+ geom_point(data=df,aes_string(x="x",y="y",color="group1"),size=2)+
      geom_text_repel(data=df,aes_string(x="x",y="y",label="label",color="group1"),
                      box.padding=1)+
      theme(legend.position="top",legend.title = element_blank())
  }
  p
  if(add.anova){
    df3=makeAnovaDf(labels=labels,data=data,yvar=yvar,total=total,addvars=TRUE,
                    maxylev=maxylev,mode=mode,rangemode=rangemode)
    df3
    if(is.null(ypos)) ypos=c(0.2,0.2,0.2)
    df3$ypos=info$ymin+(info$ymax-info$ymin)*ypos
    #p<-p+geom_text(data=df3,aes(x=W,y=ypos,label=label),parse=TRUE)
    p<-p+geom_text(data=df3,aes_string(x="W",y="ypos",label="label3"),family="Times",fontface="italic")
  }
  p
  if(add.arrow){
    if(is.null(xinterval)) {
      info=getAspectRatio(p)
      xinterval=(info$xmax-info$xmin)/30
    }
    df4 <-eval(parse(text="df %>% select(x,color,y) %>% spread(color,y)"))
    colnames(df4)=c("x","y1","y2","y3")

    if(1 %in% ceno){

      # p<-p+geom_curve(data=df4,aes_string(x="x",y="y1",xend="x",yend="y2"),
      #                 curvature=0.2,
      #              arrow=arrow(length=unit(0.1,"inches"),angle=15,ends="last",type="closed"))

      df4$x2=df4$x+xinterval

      if(mode==1){
      p<-p+geom_segment(data=df4,aes_string(x="x",y="y1",xend="x2",yend="y1"))+
        geom_segment(data=df4,aes_string(x="x2",y="y1",xend="x2",yend="y2"))+
        geom_segment(data=df4,aes_string(x="x2",y="y2",xend="x",yend="y2"),
                     arrow=arrow(length=unit(0.1,"inches"),
                                 angle=15,ends="last",type="closed"))
      } else if(mode==3){

        df4$y4=(df4$y2+df4$y3)/2
        p<-p+geom_segment(data=df4,aes_string(x="x",y="y1",xend="x2",yend="y1"))+
          geom_segment(data=df4,aes_string(x="x2",y="y1",xend="x2",yend="y4"))+
          geom_segment(data=df4,aes_string(x="x2",y="y4",xend="x",yend="y4"),
                       arrow=arrow(length=unit(0.1,"inches"),
                                   angle=15,ends="last",type="closed"))

      }
    }
    if(2 %in% ceno){
      # p<-p+geom_curve(data=df4,aes_string(x="x",y="y1",xend="x",yend="y3"),
      #                 curvature=0.2,
      #                 arrow=arrow(length=unit(0.1,"inches"),angle=15,ends="last",type="closed"))
      if(mode==1){
      df4$x3=df4$x-xinterval
      p<-p+geom_segment(data=df4,aes_string(x="x",y="y1",xend="x3",yend="y1"))+
        geom_segment(data=df4,aes_string(x="x3",y="y1",xend="x3",yend="y3"))+
        geom_segment(data=df4,aes_string(x="x3",y="y3",xend="x",yend="y3"),
                     arrow=arrow(length=unit(0.1,"inches"),
                                 angle=15,ends="last",type="closed"))
      } else if(mode==3){
        df4$x3=df4$x-xinterval
        p<-p+geom_segment(data=df4,aes_string(x="x",y="y2",xend="x3",yend="y2"))+
          geom_segment(data=df4,aes_string(x="x3",y="y2",xend="x3",yend="y3"))+
          geom_segment(data=df4,aes_string(x="x3",y="y3",xend="x",yend="y3"),
                       arrow=arrow(length=unit(0.1,"inches"),
                                   angle=15,ends="last",type="closed"))
      }

    }
    df5=makeCEDf(labels=labels,data=data,yvar=yvar,total=total,addvars=addvars,maxylev=maxylev,
                 mode=mode,rangemode=rangemode)
    df5$y1=df4$y1
    df5$y2=df4$y2
    df5$y3=df4$y3

    if(1 %in% ceno){
      df5$label1=paste0("theta[italic(D[1]) %->% italic(",yvar,")] == ",sprintf("%0.3f",df5$d1))
      if(mode==1){
         df5$y=(df5$y1+df5$y2)/2
         if(!is.null(ypos2)) df5$y= ifelse(df5$y1>df5$y2,df5$y2+(df5$y1-df5$y2)*ypos2,
                                        df5$y1+(df5$y2-df5$y1)*ypos2)
      } else if(mode==3){
          df5$y4=df4$y4
          df5$y=(df5$y1+df5$y4)/2
          if(!is.null(ypos2)) df5$y= ifelse(df5$y1>df5$y4,df5$y4+(df5$y1-df5$y4)*ypos2,
                                            df5$y1+(df5$y4-df5$y1)*ypos2)
      }
      df5$hjust1=-0.05
      if(!is.null(hjust1)) df5$hjust1=hjust1

      df5$W1=df5$W+xinterval
      p<-p+geom_text(data=df5,aes_string(x="W1",y="y",label="label1",hjust="hjust1"),parse=TRUE)

    }
    if(2 %in% ceno){
      df5$label2=paste0("theta[italic(D[2]) %->% italic(",yvar,")] == ",sprintf("%0.3f",df5$d2))
      if(mode==1){
      df5$y=(df5$y1+df5$y3)/2
      if(!is.null(ypos2)) df5$y= ifelse(df5$y1>df5$y3,df5$y3+(df5$y1-df5$y3)*ypos2,
                                        df5$y1+(df5$y3-df5$y1)*ypos2)
      } else if(mode==3){
        df5$y=(df5$y2+df5$y3)/2
        if(!is.null(ypos3)) df5$y= ifelse(df5$y2>df5$y3,df5$y3+(df5$y2-df5$y3)*ypos3,
                                          df5$y2+(df5$y3-df5$y2)*ypos3)
      }
      df5$hjust2=1.05
      if(!is.null(hjust2)) df5$hjust2=hjust2

      df5$W2=df5$W-xinterval
      p<-p+geom_text(data=df5,aes_string(x="W2",y="y",label="label2",hjust="hjust2"),
                     parse=TRUE)
    }
  }
  p
}

