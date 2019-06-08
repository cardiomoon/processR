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


#' Draw statistical diagram with an object of class lavaan
#' @param semfit AN object of class lavaan
#' @param labels list of variable names
#' @param nodelabels list of nodes names
#' @param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","name")
#' @param mode integer If 1, models with categorical X
#' @param nodemode integer If 1, separate node name and node label
#' @param xmargin horizontal margin between nodes
#' @param radx horizontal radius of the box.
#' @param ymargin vertical margin between nodes
#' @param xlim the x limits (min,max) of the plot
#' @param ylim the y limits (min,max) of the plot
#' @param box.col fill color of the box
#' @param rady vertical radius of the box.
#' @param maxypos maximal y position of X or W variables
#' @param minypos minimal y position of X or W variables
#' @param ypos  The x and y position of Y node. Default value is c(1,0.5)
#' @param mpos The x and y position of M node. Default value is c(0.5,0.9)
#' @param xinterval numeric. Horizontal intervals among labels for nodes and nodes
#' @param yinterval numeric. Vertical intervals among labels for nodes and nodes
#' @param xspace numeric. Horizontal distance bewteen nodes
#' @param label.pos Optional list of arrow label position
#' @param interactionFirst logical If true, place nodes with interaction first
#' @param totalOnly logical If true, draw total effect model only
#' @param digits integer indicating the number of decimal places
#' @importFrom dplyr arrange
#' @export
#' @examples
#' library(lavaan)
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
#' drawModel(semfit,labels=labels,nodelabels=nodelabels,whatLabel="name",xlim=c(-0.4,1.3),xinterval=0.26)
#' drawModel(semfit,labels=labels)
#' labels=list(X="cyl",M=c("am","wt","hp"),Y="mpg",W="vs")
#' moderator=list(name=c("vs"),site=list(c("a","b")))
#' model=multipleMediation(labels=labels,moderator=moderator,data=mtcars)
#' semfit=sem(model=model,data=mtcars)
#' drawModel(semfit,labels=labels)
drawModel=function(semfit,labels=NULL,nodelabels=NULL,whatLabel="name",mode=1,
                      nodemode=1,
                      xmargin=0.02,radx=NULL,
                      ymargin=0.02,xlim=c(-0.3,1.3),ylim=xlim,box.col="white",
                   rady=0.06,maxypos=0.6,minypos=0,ypos=c(1,0.5),mpos=c(0.5,0.9),
                   xinterval=NULL,yinterval=NULL,xspace=NULL,label.pos=list(),
                   interactionFirst=FALSE,totalOnly=FALSE,
                   digits=3){

    # nodelabels=NULL;whatLabel="name"
    # xmargin=0.01;radx=NULL;mode=2;nodemode=1
    # ymargin=0.02;xlim=c(-0.2,1.2);ylim=xlim
    # rady=0.04;maxypos=0.6;minypos=0;ypos=c(1,0.5);mpos=c(0.5,0.9)
    # xinterval=NULL;yinterval=NULL;xspace=NULL;label.pos=list()
    # digits=3
    # interactionFirst=TRUE;totalOnly=TRUE

    if(is.null(radx)) radx=ifelse(nodemode %in% c(1,4),0.09,0.12)
    res=parameterEstimates(semfit)
    res=res[res$op=="~",]
    res
    res=res[c(1,3,4,5,8)]
    colnames(res)=c("end","start","name","est","p")
    res
    res$start=changeLabelName(res$start,labels,add=FALSE)
    res$end=changeLabelName(res$end,labels,add=FALSE)
    df1=res
    df1
    if(totalOnly){
       df1=df1[df1$end=="Y",]
       df1=df1[df1$start!="M",]
    }


    name=unique(c(df1$start,df1$end))
    nodes=data.frame(name=name,stringsAsFactors = FALSE)
    nodes$no=4
    nodes$no[nodes$name=="Y"]=1
    nodes$no[str_detect(nodes$name,"^M[0-9]?$")]=2
    nodes$no[str_detect(nodes$name,"^X[0-9]?$")]=3
    count=length(nodes$no[nodes$no==4])

    nodes$no[(nodes$no==4)&(str_detect(nodes$name,":"))]=6
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
    icount=length(which(nodes$no>=5))
    icount
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
        nodes$xpos[nodes$no>=5]=seq(from=0.1,to=0.9,length.out=icount)
      }
      if(totalOnly) {
        nodes$xpos[nodes$no!=1]=0
      }
      nodes$ypos=1
      nodes$ypos[nodes$no==1]=ypos[2]
      nodes$ypos[nodes$no==2]=mpos[2]
      if(mcount>2){
          starty=mpos[2]-0.1
          nodes$ypos[nodes$no==2]=c(starty,rep(mpos[2],mcount-2),starty)
      }
      temp="dplyr::arrange(nodes,no)"
      nodes=eval(parse(text=temp))
      select1=which(nodes$no<=2)
      select=setdiff(which(nodes$no<5),select1)
      select
      nodes$ypos[select]=seq(to=2,by=-1,length.out = length(select))

    nodes
    # nodes$xpos1=adjustxpos(nodes$xpos,xmargin=xmargin,radx=radx)
    nodes$ypos=adjustypos(nodes$ypos,ymargin=ymargin,rady=rady,
                          maxypos=maxypos,minypos=minypos,totalOnly=totalOnly)

    arrows=df1
    arrows$labelpos=0.5
    arrows$arrpos=0.8
    arrows$no=1
    arrows$lty=1
    arrows$label1=arrows$name
    if(whatLabel=="name") {
        arrows$label=arrows$name
        addprime=ifelse(totalOnly,FALSE,TRUE)
    } else{
        arrows$label=round(arrows$est,digits)
        arrows$lty=ifelse(arrows$p<0.05,1,3)
        addprime=FALSE
    }

         # print(nodes)
         # print(arrows)
    if(is.null(nodelabels)) {
      nodelabels=labels
    }

    openplotmat(xlim=xlim,ylim=ylim)


    for(i in 1:nrow(arrows)){
        temppos=arrows$labelpos[i]
        if(!is.null(label.pos[[arrows$name[i]]])) temppos=label.pos[[arrows$name[i]]]
        if(totalOnly){
        myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=temppos,arr.pos=NULL,lty=arrows$lty[i],addprime=addprime,
                 xspace=xspace,mode=2,xadj=0)
        } else{
        myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=temppos,arr.pos=NULL,lty=arrows$lty[i],addprime=addprime,
                 xspace=xspace,mode=2)
        }
    }

    for(i in 1:nrow(nodes)){
        xpos=nodes$xpos[i]
        xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace,mode=2)
        mid=c(xpos,nodes$ypos[i])

        label=nodes$name[i]
        if(nodemode==2) {
            if(!is.null(labels[[label]])) label=labels[[label]]
        } else if(nodemode==3){
            if(!is.null(labels[[label]])) label=paste0(labels[[label]],"(",label,")")
        }
        drawtext(mid,radx=radx,rady=rady,lab=label,latent=FALSE,box.col=box.col)
        if(nodemode==1){
        if(!is.null(nodelabels[[label]])) {
            if(is.null(yinterval)) yinterval=2*rady+ymargin
            if(is.null(xinterval)) xinterval=2*radx
            if(mid[2]<=rady+ymargin){
                newmid=mid-c(0,yinterval)
            } else if(mid[2]>=0.9){
                newmid=mid+c(0,yinterval)
            } else if(mid[1]>0.85){
                newmid=mid+c(xinterval,0)
            } else{
                newmid=mid-c(xinterval,0)
            }
            textplain(mid=newmid,lab=nodelabels[[label]])
        }
        }
    }

}
