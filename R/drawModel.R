#'Adjust y position
#'@param ypos y position
#'@param ymargin verical margin of plot
#'@param rady vertical radius of the box
#' @param maxypos maximal y position of X or W variables
#' @param minypos minimal y position of X or W variables
#' @param totalOnly logical if TRUE, arrange ypos with center 0.5
#' @export
#' @examples
#' ypos=c(0.5,0.9,1,1,2,3)
#' adjustypos(ypos)
#' adjustypos(ypos,totalOnly=TRUE)
adjustypos=function(ypos,ymargin=0.02,rady=0.06,maxypos=0.6,minypos=0,totalOnly=FALSE){

  if(totalOnly){
     ymargin=rady
     count=max(ypos)
     yinterval=ymargin+2*rady
     starty=0.5-yinterval*(count-1)/2
     result=ifelse(ypos<1,ypos,starty+(ypos-1)*yinterval)
  } else{
    yinterval=ymargin+2*rady
    starty=minypos+ymargin+rady
    yinterval=max((maxypos-starty)/(max(ypos)-1),yinterval)
     result=ifelse(ypos<1,ypos,starty+(ypos-1)*yinterval)
  }
     result
}


#' Draw statistical diagram with an object of class lavaan or a list of class lm
#' @param semfit An object of class lavaan or a list of class lm
#' @param labels list of variable names
#' @param equation Optional string contains equation
#' @param vars A list
#' @param moderator A list
#' @param covar A list
#' @param data A data.frame
#' @param nodelabels list of nodes names
#' @param arrowslabels list of arrows names
#' @param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","name","estSE")
#' @param mode integer If 1, models with categorical X
#' @param nodemode integer If 1, separate node name and node label
#' @param xmargin horizontal margin between nodes
#' @param radx horizontal radius of the box.
#' @param ymargin vertical margin between nodes
#' @param xlim the x limits (min,max) of the plot
#' @param ylim the y limits (min,max) of the plot
#' @param box.col fill color of the box
#' @param palette character. palette name
#' @param reverse logical. Reverse otr not palatte.
#' @param rady vertical radius of the box.
#' @param maxypos maximal y position of X or W variables
#' @param minypos minimal y position of X or W variables
#' @param ypos  The x and y position of Y node. Default value is c(1,0.5)
#' @param mpos The x and y position of M node. Default value is c(0.5,0.9)
#' @param xinterval numeric. Horizontal intervals among labels for nodes and nodes
#' @param yinterval numeric. Vertical intervals among labels for nodes and nodes
#' @param xspace numeric. Horizontal distance bewteen nodes
#' @param node.pos Optional list of node position
#' @param arrow.pos Optional list of arrow label position
#' @param interactionFirst logical If true, place nodes with interaction first
#' @param totalOnly logical If true, draw total effect model only
#' @param parallel logical If true, draw parallel multiple mediation model
#' @param parallel2 logical If true, draw parallel2 multiple mediation model
#' @param parallel3 logical If true, draw parallel3 multiple mediation model
#' @param kmediator logical If true, draw parallel multiple mediation model with k mediator
#' @param serial Logical. If TRUE, serial variables are added
#' @param bmatrix integer specifying causal relations among mediators
#' @param label.pos Integer Position of nodelabels. Choices are one of 1:2
#' @param curved.arrow Optional list of curved arrow
#' @param segment.arrow Optional list of curved arrow
#' @param digits integer indicating the number of decimal places
#' @param showPos logical If true print node position
#' @param drawCovar  logical If true, draw covariates
#' @param drawbox  logical If true, draw rectangle
#' @importFrom dplyr arrange
#' @importFrom diagram segmentarrow curvedarrow
#' @importFrom ztable palette2colors
#' @export
#' @examples
#' library(lavaan)
#' labels=list(X="frame",Y="donate")
#' drawModel(labels=labels)
#' drawModel(labels=labels,arrowslabels=list(c="c"))
#' labels=list(X="frame",W="skeptic",M="justify",Y="donate")
#' moderator=list(name="skeptic",site=list(c("a","c")))
#' model=tripleEquation(labels=labels,moderator=moderator,data=disaster)
#' semfit=sem(model=model,data=disaster)
#' drawModel(semfit,labels=labels,interactionFirst=TRUE)
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' moderator=list(name="sexism",site=list(c("a","c")))
#' data1=addCatVars(protest,"protest",mode=3)
#' model=catMediation(X="protest",M="respappr",Y="liking",moderator=moderator,data=protest,maxylev=6)
#' semfit=sem(model,data=data1)
#' nodelabels=list(D1="Ind.Protest",D2="Col.Protest",W="sexism",M="respappr",Y="liking")
#' drawModel(semfit,labels=labels,nodelabels=nodelabels,whatLabel="name",
#'        xlim=c(-0.4,1.3))
#' drawModel(semfit,labels=labels)
#' labels=list(X="cyl",M=c("am","wt","hp"),Y="mpg",W="vs")
#' moderator=list(name=c("vs"),site=list(c("a1","b1")))
#' model=multipleMediation(labels=labels,moderator=moderator,data=mtcars)
#' semfit=sem(model=model,data=mtcars)
#' drawModel(semfit,labels=labels,maxypos=0.5)
#' labels=list(X="X",M=c("M1","M2","M3"),Y="Y")
#' nodelabels=c(X="Intervention\n(vs.control)",
#'    M=c("Restrained\nEating","Emotional\nEating","Perceived\nBarriers to\nExercise"),Y="Weight Loss")
#' drawModel(labels=labels,nodelabels=nodelabels,whatLabel="none",parallel=TRUE,
#' ylim=c(-0.3,1.2),label.pos=2)
#' labels=list(X="X",M=c("M1","M2","Mk-1","Mk"),Y="Y")
#' drawModel(labels=labels,parallel=TRUE,kmediator=TRUE,nodemode=2,
#'     arrow.pos=list(c=0.4),serial=FALSE,radx=0.08)
#' labels=list(X="cond",M=c("import","pmi"),Y="reaction")
#' drawModel(labels=labels,parallel=TRUE)
#' drawModel(labels=labels,parallel=TRUE,serial=TRUE)
#' model=multipleMediation(labels=labels,data=pmi,serial=TRUE)
#' model=multipleMediation(labels=labels,data=pmi)
#' cat(model)
#' semfit=sem(model=model,data=pmi)
#' drawModel(semfit,labels=labels,parallel=TRUE)
#' drawModel(semfit,labels=labels,whatLabel="est",parallel=TRUE)
#' labels=list(X="X",M=c("M1","M2"),Y="Y")
#' drawModel(labels=labels,serial=TRUE,nodemode=4)
#' labels=list(X="X",M=c("M1","M2","M3"),Y="Y")
#' drawModel(labels=labels,serial=TRUE)
#' equation='M1~X
#' M2~X+M1
#' M3~X+M1
#' Y~X+M1+M2+M3'
#' node.pos=list(X=c(0,0.5),M1=c(0.5,0.5),M2=c(0.75,0.9),M3=c(0.75,0.1),Y=c(1,0.5))
#' curved.arrow=list(a2=-0.1,a3=0.1,c=-0.15)
#' drawModel(equation=equation,nodemode=2,node.pos=node.pos,curved.arrow=curved.arrow)
#' equation='M1~X
#' M2~X
#' M3~X
#' M4~X+M1+M2+M3
#' Y~X+M1+M2+M3+M4'
#' node.pos=list(X=c(0,0.5),M1=c(0.35,0.9),M2=c(0.35,0.5),M3=c(0.35,0.1),M4=c(0.7,0.5),Y=c(1,0.5))
#' curved.arrow=list(a4=0.15,b2=0.15)
#' segment.arrow=list(c=0.5)
#' drawModel(equation=equation,nodemode=2,node.pos=node.pos,radx=0.08,curved.arrow=curved.arrow,
#' segment.arrow=segment.arrow)
#' labels=list(X="baby",M="wine",Y="tile")
#' moderator=list(name=c("milk"),site=list("a"))
#' covar=list(name=c("milk","tent","sand"),site=list(c("Y"),c("M","Y"),c("M","Y")))
#' drawModel(labels=labels,moderator=moderator,covar=covar,palette="Set3")
drawModel=function(semfit=NULL,labels=NULL,equation=NULL,
                   vars=list(),moderator=list(),covar=NULL,data=NULL,
                   nodelabels=NULL,arrowslabels=NULL,
                   whatLabel="name",mode=1,
                      nodemode=1,
                      xmargin=0.02,radx=NULL,
                      ymargin=0.02,xlim=NULL,ylim=NULL,
                   box.col="white",palette=NULL,reverse=FALSE,
                   rady=0.06,maxypos=NULL,minypos=0,ypos=c(1,0.5),mpos=c(0.5,0.9),
                   xinterval=NULL,yinterval=NULL,xspace=NULL,node.pos=list(),arrow.pos=list(),
                   interactionFirst=FALSE,totalOnly=FALSE,
                   parallel=FALSE,parallel2=FALSE,parallel3=FALSE,kmediator=FALSE,
                   serial=FALSE,bmatrix=NULL,label.pos=1,curved.arrow=list(),
                   segment.arrow=list(),
                   digits=3,showPos=FALSE,drawCovar=TRUE,drawbox=FALSE){

   # nodelabels=NULL;whatLabel="name";semfit=NULL;parallel=TRUE;covar=NULL;data=NULL
   # equation=NULL
   # labels=list(X="cond",M=c("import","pmi","age","M4"),Y="reaction")
   # xmargin=0.01;radx=NULL;mode=2;nodemode=1
   # ymargin=0.02;xlim=NULL;ylim=NULL
   # rady=0.04;maxypos=0.6;minypos=0;ypos=c(1,0.5);mpos=c(0.5,0.9)
   # xinterval=NULL;yinterval=NULL;xspace=NULL;arrow.pos=list()
   # digits=3;serial=FALSE
   # labels=list(X="X",M=c("M1","M2"),Y="Y");parallel=TRUE;serial=TRUE;
   # vars=list();moderator=list();covar=NULL;bmatrix=NULL
   # interactionFirst=TRUE;totalOnly=TRUE;semfit=NULL;moderator=list();kmediator=TRUE
   # parallel=FALSE;kmediator=FALSE
   # labels=list(X="X",M="M",Y="Y")
   # vars=list(name=list(c("W","Z")),site=list("a"),arr.pos=list(c(0.5)))
   # labels=list(X="cond",M="pmi",Y="reaction");data=pmi
   # vars=list(); moderator=list();semfit=NULL;equation=NULL;covar=NULL
     # data=NULL;  bmatrix=NULL; serial=FALSE;arrowslabels=NULL
      # curved.arrow=list();  segment.arrow=list()
       # node.pos=list();arrow.pos=list()
       # parallel2=FALSE;parallel3=FALSE
   # labels=list(X="baby",M="wine",Y="tile")
   # moderator=list(name=c("milk"),site=list("a"))
   # covar=list(name=c("milk","tent","sand"),site=list(c("Y"),c("M","Y"),c("M","Y")))

    if(is.null(radx)) radx=ifelse(nodemode %in% c(1,4),0.08,0.12)

    labels=appendLabels(labels,vars,moderator,covar)
     # labels

    if(is.null(semfit)){
        if(is.null(data)){
           df1=labels2table(labels=labels,vars=vars,moderator=moderator,
                            covar=covar,serial=serial,bmatrix=bmatrix,
                            eq=equation)
           df1$end=df1$Variables
           df1$start=df1$Predictors
           # print(df1)
        } else{
           eq=tripleEquation(labels=labels,vars=vars,moderator=moderator,covar=covar,data=data,mode=1)
           semfit=eq2fit(eq,data=data)
        }
    }

    if(!is.null(semfit)){
      if(class(semfit)=="lavaan"){
        res=parameterEstimates(semfit)
        res=res[res$op=="~",]
        res
        res=res[c(1,3,4,5,6,8)]

      } else if(class(semfit)=="list"){
        res=fit2table(semfit,labels=labels,digits=digits)
        res=res[c(6,5,7,1,2,4)]
      }
    }
    if(!is.null(semfit)){
      colnames(res)=c("end","start","name","est","SE","p")
      res
      res$start=changeLabelName(res$start,labels,add=FALSE)
      res$end=changeLabelName(res$end,labels,add=FALSE)
      df1=res
    }
    df1
    if(totalOnly){
       df1=df1[df1$end=="Y",]
       df1=df1[df1$start!="M",]
    }
    if(kmediator){
       kcount=sum(str_detect(df1$name,"^a"))
       df1$name[df1$name==paste0("a",kcount-1)]="ak-1"
       df1$name[df1$name==paste0("a",kcount)]="ak"
       df1$name[df1$name==paste0("b",kcount-1)]="bk-1"
       df1$name[df1$name==paste0("b",kcount)]="bk"
       # df1$start[df1$start==paste0("M",kcount-1)]="Mk-1"
       # df1$start[df1$start==paste0("M",kcount)]="Mk"
       # df1$end[df1$end==paste0("M",kcount-1)]="Mk-1"
       # df1$end[df1$end==paste0("M",kcount)]="Mk"
    }


    name=unique(c(df1$start,df1$end))
    nodes=data.frame(name=name,stringsAsFactors = FALSE)
    nodes$no=4
    nodes$no[nodes$name=="Y"]=1
    nodes$no[str_detect(nodes$name,"^M[0-9]?$")]=2
    nodes$no[str_detect(nodes$name,"^X[0-9]?$")]=3
    nodes$no[str_detect(nodes$name,"^C[0-9]?$")]=8
    if(is.null(maxypos)){
        if(length(nodes$no[nodes$no>2])==1) {
          maxypos=0.5
        } else{
          maxypos=0.6
        }
    }
    count=length(nodes$no[nodes$no==4])

    nodes$no[(nodes$no==4)&(str_detect(nodes$name,":"))]=6
    nodes$no[(nodes$no==6)&(str_count(nodes$name,":")==2)]=7
    nodes$no[(nodes$no==6)&(str_detect(nodes$name,"^X"))]=5
    temp="dplyr::arrange(nodes,no,name)"
    nodes<-eval(parse(text=temp))
    nodes
    if(interactionFirst){
        nodes$no[nodes$no==5]=0
        nodes$no[nodes$no==4]=5
        nodes$no[nodes$no==3]=4
        nodes$no[nodes$no==0]=3
    }
    icount=length(which(nodes$no %in% c(5,6,7)))
    icount
    covarcount=length(which(nodes$no ==8))
      nodes$xpos=0
      nodes$xpos[nodes$name=="Y"]=ypos[1]

      mcount=nrow(nodes[nodes$no==2,])
      if(mcount>1) {
         if(mcount==2){
            mympos=seq(from=0.2,to=0.8,length.out=mcount)
         } else{
            mympos=seq(from=0.05,to=0.95,length.out=mcount)
         }
         for(i in 1:mcount){
           labels[[paste0("M",i)]]=labels$M[i]
         }
      } else {
         mympos=0.5
      }
      nodes$xpos[nodes$no==2]=mympos



      xcount=nrow(nodes[nodes$no==3,])
      if(xcount>1) {
        for(i in 1:xcount){
          labels[[paste0("X",i)]]=labels$X[i]
        }
      }
      nodes
      icount
      if(icount>0) {
        nodes$xpos[nodes$no %in% c(5,6,7)]=seq(from=0.1,to=0.9,length.out=icount)
      }
      if(covarcount>0) {
        nodes$xpos[nodes$no==8]=seq(from=0.1,by=0.1,length.out=covarcount)
      }
      if(totalOnly) {
        nodes$xpos[nodes$no!=1]=0
      }
      nodes$ypos=1
      nodes$ypos[nodes$no==1]=ypos[2]
      nodes$ypos[nodes$no==2]=mpos[2]
      if(mcount>2){
          starty=mpos[2]-0.12
          nodes$ypos[nodes$no==2]=c(starty,rep(mpos[2],mcount-2),starty)
      }
      temp="dplyr::arrange(nodes,no)"
      nodes=eval(parse(text=temp))
      select1=which(nodes$no<=2)
      select=setdiff(which(nodes$no<5),select1)
      select
      nodes$ypos[select]=seq(to=2,by=-1,length.out = length(select))
      select=which(nodes$no==8)
      nodes$ypos[select]=seq(from=min(nodes$ypos)-0.05,by=-0.15,length.out = length(select))

    # nodes$xpos1=adjustxpos(nodes$xpos,xmargin=xmargin,radx=radx)
    nodes$ypos=adjustypos(nodes$ypos,ymargin=ymargin,rady=rady,
                          maxypos=maxypos,minypos=minypos,totalOnly=totalOnly)
    if(parallel){
      # nodes$xpos[nodes$name=="M1"]=0.5
      # nodes$xpos[nodes$name=="M2"]=0.5
      select2=setdiff(which(str_detect(nodes$name,"^M")),which(str_detect(nodes$name,":")))

      nodes$xpos[select2]=0.5
      mcount=length(select2)
      # nodes$ypos[nodes$name=="M2"]=0
      if(mcount==2) {
        nodes$ypos[select2]=seq(0.9,0,length.out=mcount)
        # nodes$ypos[nodes$name=="X"]=0.45
        nodes$ypos[nodes$name=="Y"]=ypos[2]
      } else if(mcount>2) {
        tempypos=seq(0.9,0,length.out=mcount+1)
        if(kmediator){
          select=(mcount+1)%/%2+1
          nodes$ypos[select2]=tempypos[-select]
          nodes$ypos[nodes$name=="X"]=tempypos[select]

        } else{
          # select=(mcount+1)%/%2+1
          select1=mcount+1
          nodes$ypos[select2]=tempypos[-select1]
          # nodes$ypos[nodes$name=="X"]=ypos[select]
          nodes$ypos[nodes$name=="Y"]=ypos[2]
        }
      }

    } else if(parallel2){
      mcount=length(nodes$no[nodes$no==2])
      mrow=mcount%/%2+ifelse(mcount%%2==0,0,1)
      newpos=seq(0,1,length.out=(mrow+2))[2:(mrow+1)]
      newpos=rep(newpos,each=2)[1:mcount]
      nodes$xpos[nodes$no==2]=newpos
      tempy=rep(c(mpos[2],minypos),mcount/2+1)[1:mcount]
      nodes$ypos[nodes$no==2]=tempy
    } else if(parallel3){
      mrow=mcount%/%2+ifelse(mcount%%2==0,0,1)
      newpos=seq(0,1,length.out=(mrow+2))[2:(mrow+1)]
      newpos=rep(newpos,2)[1:mcount]
      nodes$xpos[nodes$no==2]=newpos
      mrow=mcount%/%2+ifelse(mcount%%2==0,0,1)
      tempy=rep(c(mpos[2],minypos),each=mrow)[1:mcount]
      nodes$ypos[nodes$no==2]=tempy

    }

    nodes

    arrows=df1
    arrows$labelpos=0.5
    arrows$arrpos=0.8
    arrows$no=1
    arrows$lty=1
    arrows$label1=arrows$name
    # print(arrows)
    if(!is.null(arrowslabels)){
        arrows$label=arrowslabels[[arrows$name]]
        addprime=FALSE
    } else if(whatLabel=="name") {
        arrows$label=arrows$name
        addprime=ifelse(totalOnly,FALSE,TRUE)
    } else if(whatLabel=="none"){
      arrows$label=""
      addprime=FALSE
    } else if(whatLabel=="est"){
        arrows$label=round(arrows$est,digits)
        arrows$lty=ifelse(arrows$p<0.05,1,3)
        addprime=FALSE
    } else{
      arrows$label=paste0(round(arrows$est,digits),"\n(",round(arrows$SE,digits),")")
      arrows$lty=ifelse(arrows$p<0.05,1,3)
      addprime=FALSE
    }
    arrows$SE<-NULL
    arrows$curve=0
    arrows$dd=0
    for(i in seq_along(curved.arrow)){
        arrows$curve[arrows$name==names(curved.arrow)[i]]=curved.arrow[[i]]
    }
    for(i in seq_along(segment.arrow)){
      arrows$dd[arrows$name==names(segment.arrow)[i]]=segment.arrow[[i]]
    }

    if(is.null(nodelabels)) {
      nodelabels=labels
    }



    # if(is.null(ylim)) {
    #    ylim=c(min(nodes$ypos)-0.15,max(nodes$ypos)+0.15)
    #    if(ylim[1]>0.2) ylim[1]=0.2
    #    if(ylim[2]<0.8) ylim[2]=0.8
    # }

    nodes=setPositionNodes(nodes,arrows,radx,rady,xmargin,ymargin,
                           parallel2=parallel2,parallel3=parallel3)
    for(i in seq_along(node.pos)){
      nodes$xpos[nodes$name==names(node.pos)[i]]=node.pos[[names(node.pos)[i]]][1]
      nodes$ypos[nodes$name==names(node.pos)[i]]=node.pos[[names(node.pos)[i]]][2]
    }

    if(showPos) {
       cat("\nnodes\n")
       print(nodes)
       cat("\n\narrows\n")
       print(arrows)
    }
    if(!drawCovar){

        covarnames=arrows$start[str_detect(arrows$name,"^f|^g")]
        if(length(covarnames)>0){
            arrows<-arrows[!(arrows$start %in% covarnames),]
            nodes <-nodes[!(nodes$name %in% covarnames),]
        }

    }
    if(is.null(ylim)) {
      ylim=c(min(nodes$ypos)-0.15,max(nodes$ypos)+0.15)
      if(ylim[1]>0.2) ylim[1]=0.2
      if(ylim[2]<0.8) ylim[2]=0.8
    }

    if(is.null(xlim)) {
      if(nodemode==4) {
        add=0.15
      } else {
        add=0.35
      }
      xlim=c(min(nodes$xpos)-add,max(nodes$xpos)+add)
     }

    openplotmat(xlim=xlim,ylim=ylim)


    for(i in 1:nrow(arrows)){
        temppos=arrows$labelpos[i]
        if(!is.null(arrow.pos[[arrows$name[i]]])) temppos=arrow.pos[[arrows$name[i]]]
        if(totalOnly){
        myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=temppos,arr.pos=NULL,lty=arrows$lty[i],addprime=addprime,
                 xspace=xspace,mode=2,xadj=0,curve=arrows$curve[i],dd=arrows$dd[i])
        } else{
        myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=temppos,arr.pos=NULL,lty=arrows$lty[i],addprime=addprime,
                 xspace=xspace,mode=2,curve=arrows$curve[i],dd=arrows$dd[i])
        }
    }

    nodes

    # cat("str(labels)\n")
    # str(labels)
    if(length(box.col)==1) box.col=rep(box.col,nrow(nodes))
    if(!is.null(palette)) {
      box.col=ztable::palette2colors(palette,reverse=reverse)
    }
    for(i in 1:nrow(nodes)){
        xpos=nodes$xpos[i]
        # xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace,mode=2)
        mid=c(xpos,nodes$ypos[i])

        label=nodes$name[i]
        if(nodemode==2) {
            if(!is.null(labels[[label]])) label=labels[[label]]
        } else if(nodemode==3){
            if(!is.null(labels[[label]])) label=paste0(labels[[label]],"(",label,")")
        } else if(nodemode==5){
            label=label2name(label,labels)
        }
        drawtext(mid,radx=radx,rady=rady,lab=label,latent=FALSE,box.col=box.col[i])
        if(nodemode==1){
        if(!is.null(nodelabels[[label]])) {
            if(is.null(yinterval)) yinterval=rady+ymargin
            if(is.null(xinterval)) xinterval=radx+xmargin
            if(label.pos==1){
              if(mid[2]<=rady+2*ymargin){
                newmid=mid-c(0,yinterval)
                adj=c(0.5,1.2)
              } else if(mid[2]>=0.9){
                newmid=mid+c(0,yinterval)
                adj=c(0.5,-0.2)
              } else if(mid[1]==0.5){
                newmid=mid+c(0,yinterval)
                adj=c(0.5,-0.5)
              } else if(mid[1]>0.85){
                newmid=mid+c(xinterval,0)
                adj=c(0,0.5)
              } else{
                newmid=mid-c(xinterval,0)
                adj=c(1,0.5)
              }
            } else if(label.pos==2){
              if(mid[2]<=0.5){
                newmid=mid-c(0,yinterval)
                adj=c(0.5,1.2)
              } else {
                newmid=mid+c(0,yinterval)
                adj=c(0.5,-0.2)
              }
            }
            textplain(mid=newmid,lab=nodelabels[[label]],adj=adj)
        }
        }
    }
    if(kmediator){
       ypos1=seq(tempypos[select+1],tempypos[select-1],length.out=8)
       for(i in 1:4){
          mid=c(0.5,ypos1[i+2])
          textplain(mid,lab=".")
       }
    }
    if(drawbox) rect(xlim[1],ylim[2],xlim[2],ylim[1])

}


#' Change label into name
#' @param label A string
#' @param labels A named list
#' @export
#' @examples
#' label="X:W:Z"
#' labels=list(X="dep",W="mod",Z="mod2")
#' label2name(label,labels)
label2name=function(label,labels){
     temp=unlist(strsplit(label,":"))
     res=c()
     for(i in seq_along(temp)){
         res=c(res,ifelse(is.null(labels[[temp[i]]]),temp[i],labels[[temp[i]]]))
     }
     paste0(res,collapse=":")
}


#' Set Position of nodes
#' @param nodes A data.frame of nodes
#' @param arrows A data.frame of arrows
#' @param radx horizontal radius of the box.
#' @param rady vertical radius of the box.
#' @param xmargin horizontal margin between nodes
#' @param ymargin vertical margin between nodes
#' @param xlim the x limits (min,max) of the plot
#' @param ylim the y limits (min,max) of the plot
#' @param parallel2 logical
#' @param parallel3 logical
setPositionNodes=function(nodes,arrows,radx=0.08,rady=0.06,xmargin=0.02,ymargin=0.02,
                          xlim=c(-0.3,1.35),ylim=c(-0.07,1.05),parallel2=FALSE,parallel3=FALSE){

   # radx=0.08;rady=0.06;xmargin=0.02;ymargin=0.02;xlim=c(-0.3,1.35);ylim=c(-0.07,1.05)
   nodes1<-nodes
   arrows1<-arrows
   # print(nodes1)
   #  print(arrows1)
   # cat("radx=",radx,",rady=",rady,",xmargin=",xmargin,
   #     ",ymargin=",ymargin,",xlim=",xlim,",ylim=",ylim,"\n")

   tempname=arrows1$name
   tempname2=unlist(stringr::str_extract_all(tempname,"^[a-z]"))
   maxname=attr(which.max(table(tempname2)),"names")
   mode=ifelse(maxname=="a",1,2)
   label=c()
   maxlabel=c()
   ymcount=0
   for(i in seq_along(nodes1$name)){
      temp=nodes1$name[i]
      temp
      if(temp=="Y"){
         label=c(label,"y")
         maxlabel=c(maxlabel,"y")
         ymcount=ymcount+1
      } else if(nodes1$name[i] %in% paste0("M",c("",1:9))){
        label=c(label,"m")
        maxlabel=c(maxlabel,"m")
        ymcount=ymcount+1
      } else{
         temp1=arrows1$name[arrows1$start==temp]
         label=c(label,paste0(temp1,collapse=","))
         if(mode==1) temp2=min(temp1)
         else temp2=max(temp1)
         maxlabel=c(maxlabel,max(temp2))
      }
   }
   label
   maxlabel
   nodes1$label=label
   nodes1$maxlabel=maxlabel
   nodes1$pos=substr(nodes1$maxlabel,1,1)
   nodes1$pos1=as.numeric(substr(nodes1$maxlabel,2,nchar(nodes1$maxlabel)))
   nodes1$pos1[is.na(nodes1$pos1)]=0
   nodes1$pos2=0
   nodes1$pos2[nodes1$pos %in% c("a","c")]=1
   # nodes1$pos2[nodes1$pos=="c"]=2
   nodes1$pos2[nodes1$pos=="b"]=3
   nodes1$pos2[nodes1$pos %in% c("d","g","f")]=4
   nodes1<-eval(parse(text="dplyr::arrange(nodes1,pos2,pos1)"))
   # print(nodes1)
   xinterval=2*radx+xmargin
   yinterval=2*rady+ymargin
   xinterval
   width=xlim[2]-xlim[1]
   height=ylim[2]-ylim[1]
   width
   height
   xinterval
   maxcol=(width-0.3)%/%xinterval
   maxrow=1%/%yinterval
   maxcol=max(7,maxcol)
   maxrow=max(7,maxrow)
   suma=length(nodes1$pos[nodes1$pos=="a"])
   sumb=length(nodes1$pos[nodes1$pos=="b"])
   sumc=length(nodes1$pos[nodes1$pos=="c"])
   sumd=length(nodes1$pos[nodes1$pos %in% c("d","g","f")])
   suma
   sumb
   sumc
   pos2=c()
   total=suma+sumb+sumc+sumd
   # cat("total=",total,"\n")
   if(total<=(maxrow+maxcol)){
      if(total <= (maxcol*2)) {
          if(total%%2==0){
             pos2=c(rep(1,total/2),1.5,rep(2,total/2-1))
          } else{
            pos2=c(rep(1,total%/%2),1.5,rep(2,total%/%2))
          }
      } else{
         pos2=c(rep(1,total-maxcol),1.5,rep(2,maxcol-1))
      }
   } else {
         pos2=c(rep(1,maxrow-2),1.5,rep(2,maxcol-1),rep(3,total-maxcol-maxrow+2))
   }

   pos2=c(rep(0,ymcount),pos2)
   # cat("total=",total,"\n")
   # cat("maxcol=",maxcol,"\n")
   # cat("maxrow=",maxrow,"\n")
   # cat("pos2=",pos2,"\n")
   nodes1$pos2=pos2
   nodes1$xpos1=nodes1$xpos
   nodes1$ypos1=nodes1$ypos
   nodes1
   nodes1$xpos[nodes1$pos2==1]=0
   nodes1$xpos[nodes1$pos2==1.5]=xinterval/2
   nodes1$xpos[nodes1$pos2==2]=seq(xinterval,by=xinterval,length.out=length(nodes1$xpos[nodes1$pos2==2]))
   startx=max(1,nodes1$xpos)
   nodes1$xpos[nodes1$pos2==3]=seq(startx,by=-xinterval,length.out=length(nodes1$xpos[nodes1$pos2==3]))
   if(length(nodes1$xpos[nodes1$pos2==3])>0){
      nodes1$xpos[nodes1$pos=="m"]=min(min(nodes1$xpos[nodes1$pos2==3])-xinterval,0.5)
   }
   if(parallel2 | parallel3){
      if(total<=maxrow) nodes1$xpos[nodes1$pos2>0]=0
   }
   starty=0

   if(parallel2 | parallel3){starty=-0.3}
   if(total<=4) {
     starty=0.5-yinterval*(total-1)
   } else if(total==5){
     starty=0.5-yinterval*(total-2)
   }
   nodes1$ypos[nodes1$pos2==2]=starty
   nodes1$ypos[nodes1$pos2==1.5]=starty+yinterval
   nodes1$ypos[nodes1$pos2==1]=seq(to=starty+yinterval*2,by=-yinterval,length.out=length(nodes1$ypos[nodes1$pos2==1]))
   nodes1$ypos[nodes1$pos2==3]=0.9
   if(total==1) {
     nodes1$ypos[nodes1$pos2>1]=starty
     nodes1$xpos[nodes1$pos2>1]=0
   }

   if(parallel2 | parallel3){
     if(total<=maxrow) nodes1$ypos[nodes1$pos2>0]=seq(0.9,by=-yinterval,length.out=length(nodes1$ypos[nodes1$pos2>0]))
   }

   # print(nodes1)
   nodes1
}

