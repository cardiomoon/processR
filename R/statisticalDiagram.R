#' Get nodes data with model no
#' @param no model number
getNodes=function(no=25){
    res=no
    for( i in 1:4) {
       res=c(res,moreModels$no1[moreModels$no %in% res])
       res=unique(res)
    }
    res
    nodes[nodes$no %in% res,]
}

#' Get arrows data with no
#' @param no model number
getArrows=function(no=25){
  res=no
  for( i in 1:4) {
    res=c(res,moreModels$no1[moreModels$no %in% res])
    res=unique(res)
  }
  res
  parrows[parrows$no %in% res,]
}


#' Make estimateTable with a list of lm object
#' @param fit A list of lm object
#' @param labels A list
#' @param digits integer indicating the number of decimal places
#' @export
#' @examples
#' labels=list(X="frame",M="justify",Y="donate",W="skeptic")
#' moderator=list(name="skeptic",site=list(c("a","c")))
#' eq=tripleEquation(labels=labels,moderator=moderator,data=disaster,mode=1)
#' fit=eq2fit(eq,data=disaster)
#' fit2table(fit=fit,labels=labels)
fit2table=function(fit,labels=labels,digits=3){
  count=length(fit)
  Y=c()
  i=1
  for(i in 1:count){
    Y=c(Y,names(fit[[i]]$model)[1])
    if(i==1) {
      df=as.data.frame(summary(fit[[i]])$coef)
      df=df[-1,]
      df$Predictors=rownames(df)
    } else{
      temp=as.data.frame(summary(fit[[i]])$coef)
      temp=temp[-1,]
      temp$Predictors=rownames(temp)
      df=rbind(df,temp)
    }
  }
  Y
  df
  res=modelsSummary2(fit,labels=labels)
  class(res)="data.frame"
  res
  if(count==1){
    nrow1=nrow(res)-1
    Variables=rep(Y[1],nrow1)
  } else if(count>1) {
    xcount=unlist(lapply(fit,function(x) {length(x$coef)-1}))
    xcount
    Variables=c()
    for(i in 1:length(Y)){
      Variables=c(Variables,rep(Y[i],xcount[i]))
    }
  }
  df$Variables=Variables
  res=res[res$name1!="Constant",]
  res$label=str_replace(res$name,"'","")
  df$label=res$label
  colnames(df)[1:4]=c("B","SE","t","p")
  df$B=round(df$B,digits=digits)
  df$SE=round(df$SE,digits=digits)
  df$t=round(df$t,digits=digits)
  df
}


#'Draw statistical diagram
#'@param no process macro model number
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xmargin horizontal margin of plot
#'@param arrowlabel logical whether or not draw arrowlabel
#'@param arrowslabels A character vector
#'@param arrowslty linetype of arrows
#'@param labels A list of character string
#'@param nodeslabels A list of character string
#'@param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","std","name","label")
#'@param fit A list of class lm or an object of lacc lavaan
#'@param estimateTable A data.frame
#'@param digits Integer indicating the number of decimal places
#'@param covar Optional list of covariates
#'@param addCovar Logical. Whether or not include covariates
#'@param type An integer
#'@param includeLatentVars A logical
#'@param addprime logical. Whether or not add prime to label "c"
#'@param box.col fill  color of the box
#' @param xlim the x limits (min,max) of the plot
#' @param ylim the y limits (min,max) of the plot
#'@importFrom dplyr left_join
#'@export
#'@examples
#'statisticalDiagram(no=1)
#'covar=list(name=c("posemot","ideology","sex"),site=list(c("Y"),c("Y"),c("Y")))
#'statisticalDiagram(no=1,covar=covar)
#'covar=list(name=c("posemot","ideology","sex"),site=list(c("M","Y"),c("Mi","Y"),c("Mi","Y")))
#'covar=list(name=c("C1","C2"),site=list(c("M","Y"),"Y"))
#'statisticalDiagram(no=4,covar=covar)
#'statisticalDiagram(no=8,covar=covar)
#'labels=list(X="wintense",Mi="cogapp",Y="emotion")
#'nodeslabels=list(X="Work\nIntensity",Mi="Cognitive\nAppraisal",Y="Emotional\nExhaustion")
#'statisticalDiagram(4,labels=labels)
#'statisticalDiagram(4,labels=nodeslabels)
#'statisticalDiagram(4,labels=labels,nodeslabels=nodeslabels)
#'labels=list(X="GDP\nper inhabitant",M="Illiteracy Rate",Y="Mean Life\nExpectation")
#'statisticalDiagram(4,labels=labels)
#'statisticalDiagram(4,labels=labels,arrowslabels=c("e","f","g"),whatLabel="label")
statisticalDiagram=function(no=1,radx=0.10,rady=0.04,xmargin=0.01,arrowlabel=TRUE,arrowslabels=NULL,
                            arrowslty=NULL,
                            labels=list(),nodeslabels=list(),whatLabel="name",fit=NULL,
                            estimateTable=NULL,
                            digits=3,covar=list(),addCovar=TRUE,type=NULL,
                            includeLatentVars=FALSE,addprime=TRUE,box.col="white",xlim=c(0,1),ylim=NULL){
#
  # no=4;radx=0.10;rady=0.04;xmargin=0.01;arrowlabel=TRUE;arrowslabels=NULL
  # arrowslty=NULL
  # labels=list();nodeslabels=list();whatLabel="name";fit=NULL;estimateTable=NULL
  # digits=3;covar=list();addCovar=TRUE;type=NULL
  # includeLatentVars=FALSE;addprime=TRUE
    # arrowslabels=c("e","f","g");whatLabel="label";ylim=NULL

  # covar=list(name=c("C1","C2"),site=list(c("M","Y"),"Y"));xlim=c(0,1);ylim=c(0,1)

  # labels=list(X="frame",M="justify",Y="donate",W="skeptic")
  # moderator=list(name="skeptic",site=list(c("a","c")))
  # covar=NULL
  # model=tripleEquation(labels=labels,moderator=moderator,data=disaster,rangemode=2)
  # semfit=sem(model,data=disaster)
  # fit=semfit;whatLabel="est";no=8
  #
  #

  if(!is.null(fit)) {
    if(is.null(estimateTable)) {
      if(class(fit)=="list") {
          estimateTable<-fit2table(fit,labels=labels,digits=digits)
      } else if(class(fit)=="lavaan"){
          estimateTable<-estimatesTable(fit,digits=digits)
      }

    }
  }


  if(no==1.1) {
        nodes=est2Nodes(estimateTable)
    } else {
        nodes=getNodes(no)
    }
    if(no==1.1){
        arrows1=est2Arrows(estimateTable)
    } else{
        arrows1=getArrows(no)
    }
    nodes
    arrows1
    # Add covariates
    if(addCovar){
    if(no!=1.1) nodes=addNodes(nodes,covar,radx=radx,rady=rady,no=no)
    }
      # print(nodes)
    arrows1
    covar
    if(no==1.1) {
        arrows2=arrows1
    } else {
        if(addCovar){
        arrows2=addArrows(arrows1,covar)
        } else{
            arrows2=arrows1
        }
    }
    arrows2

    # print(arrows)

    if( !is.null(estimateTable)) {
        if(no==1.1){
            arrows2$Predictors=arrows2$start
        } else{
           arrows2$Predictors=findNames(labels,names=arrows2$start)
        }
        arrows2$Variables=findNames(labels,names=arrows2$end)

        labels
        arrows2
        estimateTable
        # temp=c()
        # for(i in 1:nrow(estimateTable)){
        #    temp=c(temp,names(labels)[str_detect(labels,estimateTable$Variables[i])])
        # }
        # temp
        # estimateTable$start=temp
        # estimateTable
        arrows2
        if(includeLatentVars){
          arrows3<-full_join(arrows2,estimateTable,by=c("Predictors","Variables"))
          arrows3
          arrows3$no=  arrows3$no[1]
          arrows3$name[is.na(arrows3$name)]=""
          arrows3$start[is.na(arrows3$start)]=arrows3$Predictors[is.na(arrows3$start)]
          arrows3$end[is.na(arrows3$end)]=arrows3$Variables[is.na(arrows3$end)]
          arrows3$labelpos=0.5
          arrows3$arrpos=0.84
          arrows3$end=changeLabelName(arrows3$end,labels)

        } else{
          arrows3<-left_join(arrows2,estimateTable,by=c("Predictors","Variables"))
        }
        if(is.null(arrowslty)) {
           arrows3$lty=ifelse(arrows3$p<0.05,1,3)
           if(whatLabel=="name") arrows3$lty=1
        } else{
           arrows3$lty=arrowslty
        }
         # print(arrows3)
    }  else{
      arrows2$lty=1
      arrows3<-arrows2
      if(!is.null(arrowslty)) arrows3$lty=arrowslty
    }


    if(arrowlabel){
        if(whatLabel=="name") {
            arrows3$label=arrows3$name
        } else if(whatLabel=="est"){
            arrows3$label=arrows3$B
        } else if(whatLabel %in% c("label","label2")){
            if((!is.null(arrowslabels))&(length(arrowslabels)==nrow(arrows3))){
              arrows3$label=arrowslabels

            } else{
              arrows3$label=""
            }
        } else{
            arrows3$label=arrows3[,ncol(arrows3)-1]
        }
    } else {
        arrows3$label=""
    }

    if((!is.null(fit))&(includeLatentVars)){
      nodes=addLatentNodes(nodes,fit,labels)
      nodes=adjustPosNodes(nodes)

    }

    if(length(labels)>0){
       nodes$name=str_replace(nodes$name,"i$","")
       arrows3$name=str_replace(arrows3$name,"i$","")
       arrows3$start=str_replace(arrows3$start,"i$","")
       arrows3$end=str_replace(arrows3$end,"i$","")
       arrows3$label=str_replace(arrows3$label,"i$","")
    }
    # print(nodes)
    if(is.null(ylim)) ylim=c(min(nodes$ypos-rady-0.01),max(nodes$ypos+rady+0.01))
    if(ylim[1]>0.2) ylim[1]=0.2
    if(ylim[2]<0.8) ylim[2]=0.8


    if(whatLabel %in% c("est","std","label2")) arrows3$label=as.numeric(arrows3$label)

    drawStatDiagram(no=no,arrows=arrows3,nodes=nodes,labels=labels,nodeslabels=nodeslabels,
                    xmargin=xmargin,radx=radx,rady=rady,fit,addprime=addprime,box.col=box.col,
                    xlim=xlim,ylim=ylim)
    # openplotmat()
    #
    # drawArrows(arrows3,nodes,xmargin=xmargin,rady=rady,radx=radx)
    #
    # for(i in 1:nrow(nodes)){
    #     xpos=nodes$xpos[i]
    #     xpos=adjustxpos(xpos,xmargin,radx)
    #     mid=c(xpos,nodes$ypos[i])
    #
    #    # label=ifelse(is.null(labels[[nodes$name[i]]]),nodes$name[i],labels[[nodes$name[i]]])
    #     label=ifelse(no==1.1,nodes$name[i],findName(labels,nodes$name[i]))
    #
    #     drawtext(mid,radx=radx,rady=rady,lab=label,latent=FALSE)
    #     if(no==1.1){
    #         if(i<=nrow(nodes)){
    #             label=findName(labels,nodes$name[i])
    #             if(label!=nodes$name[i]) textplain(mid+c(0,-0.07),radx=radx,rady=rady,lab=label,latent=FALSE)
    #         }
    #     }
    # }

}


#'Change Label Names
#'@param x A character vector
#'@param labels A list
#'@param add A logical
#'@export
#'@examples
#'labels=list(X="frame:test",Mi="empathy",Y="intervention",W="frame",Z="test")
#'x=c("skeptic","test","empathy","skeptic:frame:test","D1:frame","frame:test")
#'changeLabelName(x,labels)
#'changeLabelName(x,labels,add=TRUE)
#'x=c("baby","milk","baby:milk")
#'labels=list(X="baby",M=c("wine","tent","sand"),Y="tile",W="milk")
#'changeLabelName(x,labels)
changeLabelName=function(x,labels,add=FALSE){

  res=c()
  unlist(labels)
   # i=5;add=FALSE
  for(i in 1:length(x)){
    if(x[i] %in% unlist(labels)){
      if(add){

        temp=names(unlist(labels))[which(unlist(labels)==x[i])]
        res=c(res,paste0(x[i],"(",temp,")"))
      } else{
        res=c(res,names(unlist(labels))[which(unlist(labels)==x[i])])
      }
    } else if(str_detect(x[i],":")){
         temp=unlist(strsplit(x[i],":"))
         temp
         temp2=c()
         for(j in 1:length(temp)){
           temp3=names(unlist(labels))[which(unlist(labels)==temp[j])]
           if(length(temp3)==0) temp3=temp[j]
           temp2=c(temp2,temp3)
         }
         temp2

         if(length(temp2)>1){
           temp2=paste0(temp2,collapse=":")
           if(add){
              res=c(res,paste0(x[i],"(",temp2,")"))
           } else{
              res=c(res,temp2)
           }
         } else{
            temp=x[i]
            temp
            for(j in 1:length(labels)){
               if(add) {
                 temp=str_replace(x[i],labels[[j]],paste0(labels[[j]],"(",names(labels)[j],")"))
               } else{
                 temp=str_replace(x[i],labels[[j]],names(labels)[j])
               }
            }
            temp
            res=c(res,temp)
         }
      }  else {
        res=c(res,x[i])
      }
  }
  res
}


#'Adjust position of nodes
#'@param nodes A data.frame
adjustPosNodes=function(nodes){
   if(min(nodes$xpos)<0){
    nodes$xpos[(nodes$xpos<0.3)&(nodes$xpos>=0)]=nodes$xpos[(nodes$xpos<0.3)&(nodes$xpos>=0)]+0.2
    nodes$xpos[nodes$xpos<0]=0

  }
  if(max(nodes$xpos)>1){
    nodes$xpos[(nodes$xpos>0.7)&(nodes$xpos<=1)]=nodes$xpos[(nodes$xpos>0.7)&(nodes$xpos<=1)]-0.2
    nodes$xpos[nodes$xpos>1]=1
  }

  nodes
}

#'Extract Latent Variables Names
#'@param fit An object of class lavaan. Result of lavaan::sem()
extractLatentVarName=function(fit){
  res=parameterEstimates(fit)
  res=res[res$op=="=~",]
  unique(res$lhs)
}

#'Extract Latent Variables Data
#'@param fit An object of class lavaan. Result of lavaan::sem()
#'@param labels A list
extractLatentVar=function(fit,labels){
  res=parameterEstimates(fit)
  res=res[res$op=="=~",]
  if(nrow(res)>0){
  temp=c()
  for(i in 1:nrow(res)){
    temp=c(temp,names(labels)[str_detect(labels,res$lhs[i])])
  }
  temp
  res$name=temp
  }
  res
}


#'Add latent nodes information to nodes
#'@param nodes A data.frame
#'@param fit An object of class lavaan. Result of lavaan::sem()
#'@param labels A list
addLatentNodes=function(nodes,fit,labels){
  nodes
  res=extractLatentVar(fit,labels)
  no<-name<-xpos<-ypos<-c()
  yinterval=0.12

  count=length(res$name[res$name=="X"])
  start=ifelse(count>4,0.1+yinterval*(count-1),nodes$ypos[nodes$name=="X"]+yinterval*(count-1)/2)
  ypos=seq(start,by=-yinterval,length.out = count)
  no=rep(nodes$no[1],count)
  xpos=rep(-0.1,count)
  name=res$rhs[res$name=="X"]
  df=data.frame(no,name,xpos,ypos,stringsAsFactors = FALSE)
  nodes<-rbind(nodes,df)

  count=length(res$name[res$name=="Mi"])
  start=ifelse(count>4,0.1,nodes$xpos[nodes$name=="Mi"]-0.1*(count-1)/2)
  xpos=seq(start,by=0.1,length.out = count)
  no=rep(nodes$no[1],count)
  ypos=rep(nodes$ypos[nodes$name=="Mi"]+0.1,count)
  name=res$rhs[res$name=="Mi"]
  df=data.frame(no,name,xpos,ypos,stringsAsFactors = FALSE)
  df
  nodes<-rbind(nodes,df)

  count=length(res$name[res$name=="Y"])
  start=ifelse(count>4,0.1+yinterval*(count-1),nodes$ypos[nodes$name=="Y"]+yinterval*(count-1)/2)
  ypos=seq(start,by=-yinterval,length.out = count)
  no=rep(nodes$no[1],count)
  xpos=rep(1.1,count)
  name=res$rhs[res$name=="Y"]
  df=data.frame(no,name,xpos,ypos,stringsAsFactors = FALSE)
  df
  nodes<-rbind(nodes,df)
  nodes
}

#'draw StatDiagram
#'@param no process macro model number
#'@param arrows A data.frame
#'@param nodes A data.frame
#'@param labels  A list
#'@param nodeslabels A list
#'@param xmargin horizontal margin of plot
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param fit An object of class lavaan. Result of lavaan::sem()
#'@param addprime logical Whether add prime to label "c"
#'@param box.col fill color of the box
#' @param xlim the x limits (min,max) of the plot
#' @param ylim the y limits (min,max) of the plot
#'@export
drawStatDiagram=function(no,arrows,nodes,labels,nodeslabels=list(),xmargin,radx,rady,fit=NULL,addprime=TRUE,box.col="white",xlim=c(0,1),ylim=c(0,1)){

  # print(nodes)
  # print(arrows)
  openplotmat(xlim=xlim,ylim=ylim)
  drawArrows(arrows,nodes,xmargin=xmargin,rady=rady,radx=radx,addprime=addprime)
  LVnames=c()
  if(!is.null(fit)) {
     if(class(fit)=="lavaan") LVnames=extractLatentVarName(fit)
  }
  for(i in 1:nrow(nodes)){
    xpos=nodes$xpos[i]
    xpos=adjustxpos(xpos,xmargin,radx)
    mid=c(xpos,nodes$ypos[i])
    # label=ifelse(is.null(labels[[nodes$name[i]]]),nodes$name[i],labels[[nodes$name[i]]])
    label=ifelse(no==1.1,nodes$name[i],findName(labels,nodeslabels=nodeslabels,name=nodes$name[i]))
    # label=eval(parse(text=paste0("expression(italic(",label,"))")))
    drawtext(mid,radx=radx,rady=rady,lab=label,latent=ifelse(label %in% LVnames,TRUE,FALSE),box.col=box.col)
    if(no==1.1){
      if(i<=nrow(nodes)){
        label=findName(labels,nodeslabels=nodeslabels,name=nodes$name[i])
        if(label!=nodes$name[i]) textplain(mid+c(0,-0.07),radx=radx,rady=rady,lab=label,latent=FALSE)
      }
    }
  }

}

#' Make arrows from estimatesTable
#' @param res A data.frame, result of estimatesTable
est2Arrows=function(res){
    no=rep(1.1,nrow(res))
    name=paste0("b",1:nrow(res))
    start=res$Predictors
    end=res$Variables
    labelpos=rep(0.5,nrow(res))
    arrpos=rep(0.84,nrow(res))
    data.frame(no,name,start,end,labelpos,arrpos,stringsAsFactors = FALSE)
}

#' Make nodes from estimatesTable
#' @param res A data.frame, result of estimatesTable
#' @param lastxno A numeric
est2Nodes=function(res,lastxno=2){
    res
    count=nrow(res)-1
    count
    yinterval=-0.8/(count-1)
    start=0.9
    y=seq(0.9,by=yinterval,length.out = count)
    y
    y=c(y,0.1,0.5)
    x=c(rep(0,count-1),seq(from=0.05,by=0.1,length.out=2),1)
    x
    no=rep(1.1,nrow(res)+1)
    name=c(res$Predictors,res$Variables[1])
    data.frame(no=no,name=name,xpos=x,ypos=y,stringsAsFactors = FALSE)
}


#'Draw arrows
#'@param arrows A data.frame
#'@param nodes A data.frame
#'@param xmargin horizontal margin of plot
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param addprime logical Whether add prime to label "c"
drawArrows=function(arrows,nodes,xmargin=0.01,radx=0.10,rady=0.04,addprime=TRUE){
      # print(arrows)
      # nodes

    for(i in 1:nrow(arrows)){

    if(is.na(arrows$lty[i])){
        myarrow2(nodes,from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=arrows$labelpos[i],arr.pos=arrows$arrpos[i],addprime=addprime)

    } else{
        myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=arrows$labelpos[i],arr.pos=arrows$arrpos[i],lty=arrows$lty[i],addprime=addprime)
    }
}
}


#'Add covariates to nodes
#'@param nodes A data.frame
#'@param covar A list of covariates
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param no A numeric
#'@export
addNodes=function(nodes,covar,radx=0.10,rady=0.04,no=NULL){

    if(length(covar$name)>0){
        if(no==1.1){

        }
        number<-name<-xpos<-ypos<-c()
        minypos=min(nodes$ypos)
        maxxpos=min(nodes$xpos[nodes$ypos==minypos])
        if(nodes$no[1]==4.2) {
            maxxpos=-(radx/2)
            minypos=0.4
        }
        for(i in 1:length(covar$name)){
            number=c(number,nodes$no[1])
            name=c(name,covar$name[i])
            xpos=c(xpos,maxxpos+radx/2*i)
            ypos=c(ypos,minypos-(rady*2+0.02)*i)
        }
        df=data.frame(no=number,name=name,xpos=xpos,ypos=ypos)
        nodes=rbind(nodes,df)
        nodes=adjustNodes(nodes)
    }

    nodes
}

#'Add covariates to arrows
#'@param arrows A data.frame
#'@param covar A list of covariates
#'@export
addArrows=function(arrows,covar){
    if(length(covar$name)>0){
        number<-name<-start<-end<-labelpos<-arrpos<-c()
        fcount<-gcount<-1
        for(i in 1:length(covar$name)){
            for(j in 1:length(covar$site[[i]])){
                start=c(start,covar$name[i])
                end=c(end,covar$site[[i]][j])
                number=c(number,arrows$no[1])
                prefix=ifelse(covar$site[[i]][j]=="Y","g","f")
                count=ifelse(covar$site[[i]][j]=="Y",gcount,fcount)
                name=c(name,paste0(prefix,count))
                labelpos=c(labelpos,0.5)
                arrpos=c(arrpos,0.84)
                if(covar$site[[i]][j]=="Y"){
                    gcount=gcount+1
                } else{
                    fcount=fcount+1
                }

            }

        }
        number
        name
        start
        end[end=="M"]="Mi"
        labelpos
        arrpos
        df=data.frame(no=number,name=name,start=start,end=end,labelpos=labelpos,arrpos=arrpos)
        arrows=rbind(arrows,df)

    }
    if(!("f2" %in% arrows$name)) {
      arrows$name[arrows$name=="f1"]="f"
    }
    if(!("g2" %in% arrows$name)) {
      arrows$name[arrows$name=="g1"]="g"
    }
    arrows
}
#'Adjust y position of nodes
#'@param nodes A data.frame
adjustNodes=function(nodes){
    miny=min(nodes$ypos)
    if(miny<0.05){
        if(miny>=0) {
            nodes$ypos=nodes$ypos+0.05
        } else{
            nodes$ypos=nodes$ypos+0.05-miny
        }
    }
    if(max(nodes$ypos)>0.95) {
        nodes$ypos=nodes$ypos*0.95/max(nodes$ypos)
    }
    nodes
}

#'convert a vector of names with list
#'@param labels A named list
#'@param nodeslabels A named list
#'@param names A character vector to look for
#'@param exact A logical
#'@export
#'@examples
#'labels=list(X="wt",Mi="am",Y="mpg");names=c("X","MiX","Y")
#'findNames(labels,names=names)
findNames=function(labels,nodeslabels=list(),names,exact=FALSE){
    result=c()
    for(i in 1:length(names)){
        result=c(result,findName(labels,nodeslabels=nodeslabels,name=names[i],exact=exact))
    }
    result
}

#'convert name with list
#'@param labels A named list
#'@param nodeslabels A named list
#'@param name A name to look for
#'@param exact A logical
#'@export
#'@examples
#'labels=list(X="wt",M="am",Y="mpg");name="MiX"
#'nodeslabels=list(X="weight",M="automatic",Y="milepergallon")
#'findName(labels=labels,nodeslabels=nodeslabels,name="MiX")
#'findName(labels=labels,name="MiX")
#'findName(labels=labels,nodeslabels=nodeslabels,name="M")
#'labels=list(X="GDPpp",M="Illit",Y="LifeEx")
#'nodeslabels=list(X="GDP\nper inhabitant",M="Illiteracy Rate",Y="Mean Life\nExpectation")
#'findName(labels=labels,name="Mi")
#'findName(labels=labels,nodeslabels=nodeslabels,name="Mi")
#'labels=list(X="GDPpp",Mi="Illit",Y="LifeEx")
#'nodeslabels=list(X="GDP\nper inhabitant",Mi="Illiteracy Rate",Y="Mean Life\nExpectation")
#'findName(labels=labels,name="M")
#'findName(labels=labels,nodeslabels=nodeslabels,name="M")
#'labels=list(X="cond",M=c("import","pmi"),Y="reaction")
#'findName(labels=labels,name="M1")
findName=function(labels,nodeslabels=list(),name="MiX",exact=FALSE){

    # labels=list(X="wt",M="am",Y="mpg")
    # nodeslabels=list()
     # name="Mi"
     # exact=FALSE
     #


    result=NULL
    if(length(labels)==0) {
        result=name
    } else if(length(nodeslabels)>0){
        if(!is.null(nodeslabels[[name]])) {
           result=nodeslabels[[name]]
        }
        if(is.null(result)){
        if(name=="Mi"){
            result=nodeslabels[["M"]]
        } else if(name=="M"){
          result=nodeslabels[["Mi"]]
        } else if(name=="M1"){
          result=nodeslabels[["M"]][1]
        } else if(name=="M2"){
          result=nodeslabels[["M"]][2]
        } else if(name=="M3"){
          result=nodeslabels[["M"]][3]
        }
        }
    } else if(name %in% names(labels)) {
        if(is.null(result)) result=labels[[name]]
    }

    if(is.null(result)){

    if(name=="Mi"){
        if("M" %in% names(labels)) result=labels$M
    } else if(name=="M"){
      if("Mi" %in% names(labels)) result=labels$Mi
    } else if(name=="M1"){
      result=labels[["M"]][1]
    } else if(name=="M2"){
      result=labels[["M"]][2]
    } else if(name=="M3"){
      result=labels[["M"]][3]
    }
    }


    if(is.null(result)){
    if(!exact){
        temp=c()
        for(i in seq_along(nodeslabels)){
            grep(names(nodeslabels)[i],name)
            if(length(grep(names(nodeslabels)[i],name))>0)
                temp=c(temp,nodeslabels[[names(nodeslabels[i])]])
            temp
        }
        if(length(temp)==0){
        for(i in 1:length(labels)){
            grep(names(labels)[i],name)
            if(length(grep(names(labels)[i],name))>0)
                temp=c(temp,labels[[names(labels[i])]])
            temp
        }
        }
        temp
        if(length(temp)<1) {
            result=name
        } else{
            result=paste0(temp,collapse=":")
        }
    } else{
        result=name
    }
    }
    result
}

#'Adjust x position
#'@param xpos x position
#'@param xmargin horizontal margin of plot
#'@param radx horizontal radius of the box
#'@param xspace numeric. horizontal interval
#'@param mode integer adjust mode
#'@export
adjustxpos=function(xpos,xmargin=0.01,radx=0.12,xspace=NULL,mode=1){
  if(is.null(xspace)) xspace=xmargin+2*radx
  if(mode==1){
    result=ifelse(xpos==0.5,0.5,
         ifelse(xpos>0.5,
                1-xmargin-radx-(1.0-xpos)*10*xspace,
                xmargin+radx+(xpos%/%0.1)*(xmargin+2*radx)+(xpos%%0.1)*10*xspace))
  } else{
    result=ifelse(((xpos>0.2)|(xpos<0.8)),xpos,
                  ifelse(xpos>0.5,
                         1-xmargin-radx-(1.0-xpos)*10*xspace,
                         xmargin+radx+(xpos%/%0.1)*(xmargin+2*radx)+(xpos%%0.1)*10*xspace))
  }
  result
}

#' Draw arrow with adjustment of a position
#' @param nodes A data.frame
#' @param from coordinates (x,y) of the point *from* which to draw arrow.
#' @param to coordinates (x,y) of the point *to* which to draw arrow.
#' @param label label to display
#' @param no process macro model number
#' @param radx horizontal radius of the box.
#' @param rady vertical radius of the box.
#' @param xmargin horizontal margin of plot
#' @param label.pos label position
#' @param arr.pos arrow position
#'@param addprime logical Whether add prime to label "c"
#'@param xspace numeric horizontal space between nodes
#'@param mode integer mode for adjustxpos
#' @param ... Further argument to be passed to straightarrow()
myarrow2=function(nodes,from,to,label="",no,radx=0.12,rady=0.04,xmargin=0.01,label.pos=0.5,arr.pos=NULL,addprime=TRUE,xspace=NULL,mode=1,...){

    #nodes=nodes[nodes$no==no, ]
    # from="X";no=1;to="Y";label="66"
    xpos=nodes$xpos[nodes$name==from]
    xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace,mode=mode)
    ypos=nodes$ypos[nodes$name==from]
    start=c(xpos,ypos)

    xpos=nodes$xpos[nodes$name==to]
    xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace,mode=mode)
    ypos=nodes$ypos[nodes$name==to]
    end=c(xpos,ypos)
    if(!is.numeric(label)){
    if(nchar(label)>1) {

        if(addprime) {
          prime=ifelse(substr(label,1,1)=="c","^minute","")
        }  else {
          prime=""
        }
        # if(nchar(label==3)){
        #     temp1=paste0("expression(italic(",substr(label,1,1),")[",substr(label,2,2),"]","[",
        #                  substr(label,3,nchar(label)),"]",prime,")")
        #
        # } else{
        if(!str_detect(label,"\n")){
        temp2=substr(label,2,nchar(label))
        temp2
        temp1=paste0("expression(italic(",substr(label,1,1),")[",temp2,"]",prime,")")
        temp1
        # }
        temp=eval(parse(text=temp1))
        label=temp
        }
    } else if(nchar(label)==1){
        if(label=="c") {
          if(addprime) {
            label=expression(italic(c)^minute)
          } else {
            label=expression(italic(c))
          }
        } else {
            temp=paste0("expression(italic(",label,"))")
            label=eval(parse(text=temp))
        }
    }
    }
    myarrow(from=start,to=end,label=label,label.pos=label.pos,arr.pos=arr.pos,radx=radx,rady=rady,...)
    # myarrow(from=start,to=end,label=label,label.pos=label.pos,radx=radx,rady=rady,...)

}



