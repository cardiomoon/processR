#' Make data.frame from a list of vars
#' @param vars A list
#' @param mpos A numeric vector of length 2
#' @param df A data.frame
#' @export
#' @examples
#' vars=list(name=list(c("tenure","age")),site=list(c("a","b")))
#' vars2df(vars)
#' vars=list(name=list(c("milk","hair")),matrix=list(c(1,0,0,0,0,0,1,0,0,0)),pos=5)
#' vars2df(vars)
vars2df=function(vars,mpos=c(0.5,0.9),df=NULL){
    name<-label<-xpos<-ypos<-c()
    if(is.null(vars$pos)){
        vars[["pos"]]=1:length(vars$name)
    }
    vars
    count=length(vars$name)
    for(i in 1:length(vars$name)){
        name=c(name,vars$name[[i]])
        if(is.null(vars$label)){
           if(count==1) {
               label=c(label,c("W","Z"))
           } else{
               label=c(label,paste0(c("W","Z"),i))
           }
        } else{
           label=c(label,vars$label[[i]])
        }
        if(vars$pos[i]==1){
            xpos=c(xpos,0,-0.1)
            ypos=c(ypos,mpos[2]+0.05,(mpos[2]+0.05+0.5)/2)
        } else if(vars$pos[i]==2){
            xpos=c(xpos,1,1.1)
            ypos=c(ypos,mpos[2]+0.05,(mpos[2]+0.05+0.5)/2)
        } else if(vars$pos[i]==3){
            xpos=c(xpos,0.5,0)
            ypos=c(ypos,0.2,0.2)
        } else if(vars$pos[i]==4){
            xpos=c(xpos,0.5,1)
            ypos=c(ypos,0.2,0.2)
        } else if(vars$pos[i]==5){
          xpos=c(xpos,0.5,0.3)
          ypos=c(ypos,0.2,0.3)
        } else{
            xpos=c(xpos,0.5,0.35)
            ypos=c(ypos,1.0,0.9)
        }
    }
    df1=data.frame(name=name,label=label,xpos=xpos,ypos=ypos,stringsAsFactors = FALSE)
    if(is.null(df)) {
      df2<-df1
    } else{
      df2=anti_join(df1,df,by="name")
    }
    df2

}

#' Make data.frame from a list of moderator
#' @param moderator A list
#' @param mpos A numeric vector of length 2
#' @param vars A list
#' @param df A data.frame
#' @export
#' @examples
#' moderator=list(name=c("milk","hair"),matrix=list(c(1,1,0,1,0,0,0,0,0,0)
#'    ,c(0,0,0,0,0,0,0,1,0,0)))
#' moderator2df(moderator)
moderator2df=function(moderator,mpos=c(0.5,0.9),vars=NULL,df=NULL){
    name<-label<-xpos<-ypos<-c()
    moderator
    vars
    mpos
    if(is.null(moderator$pos)){
        if(is.null(vars)){
            moderator$pos=1:length(moderator$name)
        } else{
            if(is.null(vars$pos)){
                if(length(vars$name)==1){
                    moderator$pos=2:(length(moderator$name)+1)
                } else if(length(vars$name)==2){
                    moderator$pos=3:(length(moderator$name)+2)
                }
            } else{
                position=setdiff(1:3,vars$pos)
                position
                moderator$pos=position[1:length(moderator$name)]

            }
        }
    }
    moderator
    modname=moderator$name
    if(!is.null(vars)) modname=setdiff(modname,unlist(vars$name))
    count=length(modname)
    prefix=ifelse(is.null(vars),"W","V")

    count
    i=2
    for(i in seq_along(modname)){

        (name=c(name,modname[i]))
        if(is.null(moderator$label)){
        if(count==1) {
          label=c(label,prefix)
        } else{
          label=c(label,paste0(prefix,i))
        }
        } else{
          label=c(label,moderator$label[i])
        }
        (select=which(moderator$name==modname[i]))
        if(moderator$pos[select]==1){
            xpos=c(xpos,0.05)
            ypos=c(ypos,mpos[2]-0.05)
        } else if(moderator$pos[select]==2){
            xpos=c(xpos,0.95)
            ypos=c(ypos,mpos[2]-0.05)
        } else if(moderator$pos[select]==3){
          xpos=c(xpos,0.05)
          ypos=c(ypos,0.05)
        } else if(moderator$pos[select]==4){
          xpos=c(xpos,0.95)
          ypos=c(ypos,0.05)
        } else if(moderator$pos[select]==5){
          xpos=c(xpos,0.5)
          ypos=c(ypos,0.3)
        } else if(moderator$pos[select]==6){
          xpos=c(xpos,0.5)
          ypos=c(ypos,0.9)
        } else{
           if(!is.null(moderator$matrix)){

               xpos=c(xpos,0.5)
               ypos=c(ypos,1)

           } else{
             xpos=c(xpos,0.5)
             ypos=c(ypos,0.2)
           }
        }
    }
    df1=data.frame(name=name,label=label,xpos=xpos,ypos=ypos,stringsAsFactors = FALSE)
    if(is.null(df)) {
      df2<-df1
    } else if(count==0){
      df2<-df1
    } else{
      df2=anti_join(df1,df,by="name")
    }
    df2

}

#'Make data.frame with covariates
#'@param covar A list
#'@param df A data.frame
#'@importFrom dplyr anti_join
covar2df=function(covar=list(),df){

    start=length(intersect(covar$name,df$name))
    temp=setdiff(covar$name,df$name)
    count=length(temp)
    for(i in seq_along(temp)){
       xpos=seq(min(df$xpos)+0.05,by=0.1,length.out = count)
       ypos=seq(min(df$ypos)-0.1,by=-0.1,length.out = count)
    }
    if(is.null(covar$label)){
       label=paste0("C",(1+start):(count+start))
    } else{
       label=c()
       for(i in seq_along(temp)){
           label=c(label,covar$label[which(covar$name==temp[i])])
       }
    }
    df1=data.frame(name=temp,label=label,
                  xpos=xpos,ypos=ypos,stringsAsFactors = FALSE)
    df2=anti_join(df1,df,by="name")
    df2
}

#' Draw Concept Diagram
#' @param labels A list
#' @param nodelabels A list
#' @param vars A list of triple moderators
#' @param moderator A list of modeators
#' @param covar A list of covariates
#' @param nodemode integer If 1, separate node name and node label
#' @param xpos  The x and y position of X node. Default value is c(0,0.5)
#' @param mpos The x and y position of M node. Default value is c(0.5,0.9)
#' @param ypos  The x and y position of Y node. Default value is c(1,0.5)
#' @param minypos minimal y position of X or W variables
#' @param maxypos maximal y position of X or W variables
#' @param node.pos A optional list of node position
#' @param serial Logical. If TRUE, serial variables are added
#' @param parallel logical If true, draw parallel multiple mediation model
#' @param parallel2 logical If true, draw parallel2 multiple mediation model
#' @param parallel3 logical If true, draw parallel3 multiple mediation model
#' @param bmatrix integer specifying causal relations among mediators
#' @param curved.arrow Optional numeric vector specifying curvedarrow
#' @param segment.arrow Optional numeric vector specifying segmentarrow
#' @param radx horizontal radius of the box.
#' @param rady vertical radius of the box.
#' @param box.col fill color of the box
#' @param xmargin horizontal margin between nodes
#' @param ymargin vertical margin between nodes
#' @param showPos logical If true print node position
#' @param xinterval numeric. Horizontal intervals among labels for nodes and nodes
#' @param yinterval numeric. Vertical intervals among labels for nodes and nodes
#' @param label.pos Integer Position of nodelabels. Choices are one of 1:2
#' @export
#' @examples
#' labels=list(X="estress",M="affect",Y="withdraw")
#' vars=list(name=list(c("tenure","age")),site=list(c("a","b")))
#' moderator=list(name=c("age","sex"),site=list(c("c"),c("b","c")))
#' drawConcept(labels=labels)
#' drawConcept(labels=labels,vars=vars)
#' drawConcept(labels=labels,moderator=moderator)
#' drawConcept(labels=labels,vars=vars,moderator=moderator)
#' labels=list(X="X",M=c("M1","M2","M3"),Y="Y")
#' drawConcept(labels=labels,serial=TRUE)
#' drawConcept(labels=labels,parallel=TRUE,bmatrix=c(1,1,0,1,0,0,1,1,1,1))
#' labels=list(X="baby",M=c("wine","tent","sand"),Y="tile")
#' bmatrix=c(1,1,0,1,0,0,1,1,1,1)
#' drawConcept(labels=labels,parallel=TRUE,bmatrix=bmatrix)
#' moderator=list(name=c("milk","hair"),
#'   matrix=list(c(1,1,0,1,0,0,0,0,0,0),c(0,0,0,0,0,0,0,1,0,0)))
#' drawConcept(labels=labels,parallel=TRUE,bmatrix=bmatrix,moderator=moderator)
#' bmatrix=c(1,1,0,0,1,1,1,1,0,1)
#' moderator=list(name=c("milk","hair"),
#'             matrix=list(c(1,0,0,0,1,0,1,0,0,0),c(1,1,0,0,0,0,0,0,0,0)),
#'             pos=c(1,4))
#' node.pos=list(X=c(0,0.5),M1=c(0.3,0.9),M2=c(0.3,0.1),M3=c(0.7,0.9),
#' Y=c(1,0.5),W1=c(0.7,0.1),W2=c(0,0.9))
#' drawConcept(labels=labels,bmatrix=bmatrix,moderator=moderator,node.pos=node.pos)
#' labels=list(X="baby",M=c("wine","tent","sand"),Y="tile")
#' vars=list(name=list(c("milk","hair")),matrix=list(c(1,0,0,0,0,0,1,0,0,0)),pos=2)
#' bmatrix=c(1,1,0,1,0,0,1,1,1,1)
#' drawConcept(labels=labels,parallel=TRUE,bmatrix=bmatrix,vars=vars)
#' labels=list(X="X",M=c("M1","M2"),Y="Y")
#' vars=list(name=list(c("W","Z")),matrix=list(c(0,0,1,0,0,0)),pos=5)
#' bmatrix=c(1,1,1,1,1,1)
#' drawConcept(labels=labels,bmatrix=bmatrix,vars=vars)
#' labels=list(X="X",M="M",Y="Y")
#' vars=list(name=list(c("W","Z")),site=list(c("a","c")),arr.pos=list(c(0.7,0.3)))
#' moderator=list(name=c("V","Q"),site=list(c("b","c"),c("c")),
#'    pos=c(2,5),arr.pos=list(c(0.3,0.7),0.5))
#' drawConcept(labels=labels,vars=vars,moderator=moderator,nodemode=2)
drawConcept=function(labels,nodelabels=list(),vars=NULL,moderator=NULL,covar=NULL,nodemode=1,
                    xpos=c(0,0.5),mpos=c(0.5,0.9),ypos=c(1,0.5),minypos=0,maxypos=0.6,
                    node.pos=list(),serial=FALSE,parallel=FALSE,parallel2=FALSE,parallel3=FALSE,
                    bmatrix=NULL,curved.arrow=NULL,segment.arrow=NULL,
                    radx=0.06,rady=0.04,box.col="white",
                    xmargin=0.02,ymargin=0.02,showPos=FALSE,
                    xinterval=NULL,yinterval=NULL,label.pos=1) {

# xpos=c(0,0.5);mpos=c(0.5,0.9);ypos=c(1,0.5);minypos=0;maxypos=0.6
# node.pos=list()
# serial=FALSE; parallel=FALSE
# bmatrix=NULL
# radx=0.06
# rady=0.06
# box.col="white"
# xmargin=0.01
# ymargin=0.01
# labels=list(X="Illit",Y="LifeExp")
# bmatrix=NULL;vars=NULL;moderator=NULL
# labels=list(X="baby",M=c("wine","tent","sand"),Y="tile")
# bmatrix=c(1,1,0,1,0,0,1,1,1,1)
# moderator=list(name=c("milk","hair"),
#                matrix=list(c(1,1,0,1,0,0,0,0,0,0),c(0,0,0,0,0,0,0,1,0,0)))
# vars=NULL

vars
moderator

X<-M<-Y<-NULL
X=labels[["X"]]
xcount=length(X)
mcount=0
if(is.null(M)) {
    M=labels[["M"]]
    mcount=length(M)
}

Y=labels[["Y"]]

name=c(X,M,Y)
name
if(xcount==1) {
  label=c("X")
} else {
  label=paste0("X",1:xcount)
}
if(mcount==1) {
  label=c(label,"M")
} else if(mcount>1){
  label=c(label,paste0("M",1:mcount))
}
label=c(label,"Y")

if(xcount==1){
  xposition = xpos[1]
  yposition = xpos[2]
} else{
    xposition=seq(minypos,maxypos,length.out=xcount+1)[-1]
    yposition=rep(xpos[2],xcount)
}

xposition
yposition
if(mcount>1) {
    if(mcount==2){
        mympos=seq(from=0.2,to=0.8,length.out=mcount)
    } else{
        mympos=seq(from=0.05,to=0.95,length.out=mcount)
    }
    for(i in 1:mcount){
        labels[[paste0("M",i)]]=labels$M[i]
    }
} else if(mcount==1){
    mympos=mpos[1]
}

if(parallel) {
  xposition=c(xposition,rep(0.5,mcount))
} else if(parallel2){
   mrow=mcount%/%2+ifelse(mcount%%2==0,0,1)
   newpos=seq(0,1,length.out=(mrow+2))[2:(mrow+1)]
   newpos=rep(newpos,each=2)[1:mcount]
   xposition=c(xposition,newpos)
} else if(parallel2){
  mrow=mcount%/%2+ifelse(mcount%%2==0,0,1)
  newpos=seq(0,1,length.out=(mrow+2))[2:(mrow+1)]
  newpos=rep(newpos,each=2)[1:mcount]
  xposition=c(xposition,newpos)
} else if(parallel3){
  mrow=mcount%/%2+ifelse(mcount%%2==0,0,1)
  newpos=seq(0,1,length.out=(mrow+2))[2:(mrow+1)]
  newpos=rep(newpos,2)[1:mcount]
  xposition=c(xposition,newpos)
} else if(mcount>0){
  xposition=c(xposition,mympos)
}

if(mcount>1){
    if(parallel){
       starty=mpos[2]
       endy=minypos
       tempy=seq(starty,endy,length.out=mcount+1)
       yposition=c(yposition,tempy[-2])
    } else if(parallel2){
      tempy=rep(c(mpos[2],minypos),mcount/2+1)[1:mcount]
      yposition=c(yposition,tempy)
    } else if(parallel3){
      mrow=mcount%/%2+ifelse(mcount%%2==0,0,1)
      tempy=rep(c(mpos[2],minypos),each=mrow)[1:mcount]
      yposition=c(yposition,tempy)
    } else{
       starty=mpos[2]-0.12
       yposition=c(yposition,starty,rep(mpos[2],mcount-2),starty)
    }
} else if(mcount==1){
    yposition=c(yposition,mpos[2])
}

xposition=c(xposition,ypos[1])
yposition=c(yposition,ypos[2])

df=data.frame(name=name,label=label,xpos=xposition,ypos=yposition,stringsAsFactors = FALSE)
df
df1<-df2<-df3<-NULL
if(!is.null(vars)) {
  df1=vars2df(vars,mpos,df)
  df=rbind(df,df1)
}
if(!is.null(moderator)) {
  df2=moderator2df(moderator=moderator,mpos,vars=vars,df=df)
  df=rbind(df,df2)
}
if(!is.null(covar)) {
  df3=covar2df(covar=covar,df)
  df=rbind(df,df3)
}
if(showPos) print(df)


for(i in seq_along(node.pos)){
  df$xpos[df$label==names(node.pos)[i]]=node.pos[[names(node.pos)[i]]][1]
  df$ypos[df$label==names(node.pos)[i]]=node.pos[[names(node.pos)[i]]][2]
}

if(is.null(yinterval)) yinterval=rady+ymargin
if(is.null(xinterval)) xinterval=radx+xmargin

(xlim=c(min(df$xpos)-radx-2*xmargin-ifelse(label.pos==1,radx+xinterval,0),
        max(df$xpos)+radx+2*xmargin+ifelse(label.pos==1,radx+xinterval,0)))
(ylim=c(min(df$ypos)-2*rady-2*ymargin,max(df$ypos)+2*rady+2*ymargin))
if(ylim[1]>0.2) ylim[1]=0.2
if(ylim[2]<0.8) ylim[2]=0.8

name2pos=function(name,df,pos=0.5){
    if(length(name)==1) {
        pos=c(df$xpos[df$label==name],df$ypos[df$label==name])
    } else if(length(name)==2){
        xpos1=df$xpos[df$label==name[1]]
        xpos2=df$xpos[df$label==name[2]]
        ypos1=df$ypos[df$label==name[1]]
        ypos2=df$ypos[df$label==name[2]]
        xpos=xpos1+(xpos2-xpos1)*pos
        ypos=ypos1+(ypos2-ypos1)*pos
        pos=c(xpos,ypos)
    } else if(length(name)==3){
        xpos1=df$xpos[df$label==name[1]]
        xpos2=df$xpos[df$label==name[2]]
        xpos3=df$xpos[df$label==name[3]]
        ypos1=df$ypos[df$label==name[1]]
        ypos2=df$ypos[df$label==name[2]]
        ypos3=df$ypos[df$label==name[3]]

        xpos4=xpos2+(xpos3-xpos2)*pos
        ypos4=ypos2+(ypos3-ypos2)*pos

        xpos=xpos1+(xpos4-xpos1)*0.5
        ypos=ypos1+(ypos4-ypos1)*0.5

        pos=c(xpos,ypos)
    }
    pos
}

myarrow=function(start,end,df,arr.pos=0.5,mod.pos=0.5,curve=0,dd=0,...){
    # cat("start=",start,"\n")
    # cat("end=",end,"\n")
    from=name2pos(start,df=df)
    to=name2pos(end,df=df,pos=mod.pos)
    if(arr.pos==1) {
        res=from-to
        length=sqrt(res[1]^2+res[2]^2)
        arr.pos=1-(1-length)/10

    } else if(arr.pos==-1){
        res=from-to
        length=sqrt(res[1]^2+res[2]^2)

        res=abs(res)
        # cat("res=",res,"\n")
        ratio=max((res[1]-radx)/res[1],(res[2]-rady)/res[2])
        arr.pos=ratio-0.025
        if(from[1]>to[1]) arr.pos=arr.pos-0.03

        # cat("arr.pos=",arr.pos,"\n")
    }
    if(curve!=0){
      curvedarrow(from,to,arr.pos=arr.pos,lwd=1,arr.length=0.3,arr.type="triangle",curve=curve,...)
    } else if(dd!=0){
      segmentarrow(from,to,arr.pos=arr.pos,lwd=1,arr.length=0.3,arr.type="triangle",path="DHU",dd=dd,...)
    }
    else{
      straightarrow(from,to,arr.pos=arr.pos,lwd=1,arr.length=0.3,arr.type="triangle",...)
    }

}

openplotmat(xlim=xlim,ylim=ylim)

if(mcount==1){
  myarrow("X","M",df=df,-1)
  myarrow("M","Y",df=df,-1)
} else if(is.null(bmatrix)){
  for(i in seq_along(M)){
      myarrow("X",paste0("M",i),df=df,-1)
      myarrow(paste0("M",i),"Y",df=df,-1)
  }
  if(serial){
     res=combn(1:mcount,2)
     for(i in 1:ncol(res)){
        myarrow(paste0("M",res[1,i]),paste0("M",res[2,i]),df=df,-1)
     }
  }
} else{
   res=matrix2df(bmatrix)
   for(i in 1:nrow(res)){
      for(j in 1:ncol(res)){
          curve=0
          if(!is.null(curved.arrow)) {
              res1=matrix2df(curved.arrow)
              curve=as.numeric(res1[i,j])
          }
          dd=0
          if(!is.null(segment.arrow)) {
            res1=matrix2df(segment.arrow)
            dd=as.numeric(res1[i,j])
          }
          if(res[i,j]=="1"){
               myarrow(colnames(res)[j],rownames(res)[i],df=df,-1,curve=curve,dd=dd)
          }
      }
   }
}

if(is.null(bmatrix)) myarrow("X","Y",df=df,-1)

if(!is.null(vars)){
  count=length(vars$name)
for(i in 1:count){
    if(is.null(vars$matrix)){
    for(j in 1:length(vars$site[[i]])){
        vars$site[[i]][j]
        if(vars$site[[i]][j]=="a") {
            end=c("X","M")
        } else if(vars$site[[i]][j]=="b") {
            end=c("M","Y")
        } else {
            end=c("X","Y")
        }
        mod.pos=0.5
        if(!is.null(vars$arr.pos)) {
          if(length(vars$arr.pos)>=i){
            if(!is.na(vars$arr.pos[[i]][j])) mod.pos=vars$arr.pos[[i]][j]
          }
        }
        if(count==1){
           if(is.null(vars$label)){
              start1="W"
              start2="Z"
           } else{
              start1=vars$label[[1]][1]
              start2=vars$label[[1]][2]
           }

        } else{
          if(is.null(vars$label)){
            start1=paste0("W",i)
            start2=paste0("Z",i)
          } else{
            start1=vars$label[[i]][1]
            start2=vars$label[[i]][2]
          }
        }

        myarrow(start1,end,df=df,arr.pos=1,mod.pos=mod.pos)
        myarrow(start2,c(start1,end),df=df,arr.pos=1,mod.pos=mod.pos)

    }
    } else{
        res=matrix2df(vars$matrix[[i]])


        temp=df$label[df$name==moderator$name[i]]
        for(k in 1:nrow(res)){
          for(l in 1:ncol(res)){
            if(res[k,l]=="1"){
              end=c(rownames(res)[k],colnames(res)[l])
              mod.pos=0.5
              if(k==1) {
                matrixpos= l
              } else{
                matrixpos= sum(1:(k-1))+l
              }
              if(!is.null(vars$arr.pos)) {
                if(length(vars$arr.pos)>=i){
                  if(!is.na(vars$arr.pos[[i]][matrixpos])) mod.pos=vars$arr.pos[[i]][matrixpos]
                }
              }
              if(count==1){
                if(is.null(vars$label)){
                  start1="W"
                  start2="Z"
                } else{
                  start1=vars$label[[1]][1]
                  start2=vars$label[[1]][2]
                }

              } else{
                if(is.null(vars$label)){
                  start1=paste0("W",i)
                  start2=paste0("Z",i)
                } else{
                  start1=vars$label[[i]][1]
                  start2=vars$label[[i]][2]
                }
              }
              myarrow(start1,end,df=df,arr.pos=1,mod.pos=mod.pos)
              myarrow(start2,c(start1,end),df=df,arr.pos=1,mod.pos=mod.pos)
              # if(count==1){
              #   myarrow("W",end,df=df,arr.pos=1,mod.pos=1-mod.pos)
              #   myarrow("Z",c("W",end),df=df,arr.pos=1,mod.pos=1-mod.pos)
              # } else{
              #   myarrow(paste0("W",i),end,df=df,arr.pos=1,mod.pos=1-mod.pos)
              #   myarrow(paste0("Z",i),c(paste0("W",i),end),df=df,arr.pos=1,mod.pos=1-mod.pos)
              # }
            }
          }
        }
    }
}
}

if(!is.null(moderator)){
  count=length(moderator$name)
  count
  i=1
  moderator
  for(i in 1:count){
    if(is.null(moderator$matrix)){
    for(j in 1:length(moderator$site[[i]])){
      if(moderator$site[[i]][j]=="a") {
        end=c("X","M")
      } else if(moderator$site[[i]][j]=="b") {
        end=c("M","Y")
      } else {
        end=c("X","Y")
      }
      temp=df$label[df$name==moderator$name[i]]
      mod.pos=0.5
      if(!is.null(moderator$arr.pos)) {
         if(length(moderator$arr.pos)>=i){
             if(!is.na(moderator$arr.pos[[i]][j])) mod.pos=moderator$arr.pos[[i]][j]
         }
      }
      myarrow(temp,end,df=df,arr.pos=1,mod.pos=mod.pos)

    }
    } else{
        res=matrix2df(moderator$matrix[[i]])
        res
        temp=df$label[df$name==moderator$name[i]]
        temp
        for(k in 1:nrow(res)){
              for(l in 1:ncol(res)){
                 if(res[k,l]=="1"){
                    end=c(rownames(res)[k],colnames(res)[l])
                    mod.pos=0.5
                    target=ifelse(k==1,1,sum(1:(k-1))+l)
                    if(!is.null(moderator$arr.pos)) {
                      if(length(moderator$arr.pos)>=i){
                        if(!is.na(moderator$arr.pos[[i]][target])) mod.pos=moderator$arr.pos[[i]][target]
                      }
                      # mod.pos=moderator$arr.pos[[i]][target]
                    }
                    myarrow(temp,end,df=df,arr.pos=1,mod.pos=mod.pos)
                 }
              }
        }
    }
  }
}

if(!is.null(covar)){
  count=length(covar$name)
  count
  for(i in 1:count){
     for(j in 1:length(covar$site[[i]])){
        start=df$label[df$name==covar$name[i]]
        myarrow(start,covar$site[[i]][j],df=df,-1)
      }

  }
}

for(i in 1:nrow(df)){
    mid=c(df$xpos[i],df$ypos[i])

    if(nodemode==2){
        lab=df$name[i]
    } else if(nodemode==3){
        lab=paste0(df$name[i],"(",df$label[i],")")
    } else{
      lab=df$label[i]
    }
    textrect(mid,radx,rady,lab=lab,box.col=box.col)

    if(nodemode==1){
    # if(df$ypos[i]>=0.7){
    #         newypos=df$ypos[i]+rady+ymargin
    #         adj=c(0.5,-0.2)
    # } else{
    #         newypos=df$ypos[i]-rady-ymargin
    #         adj=c(0.5,1.2)
    # }

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

    # newmid=c(df$xpos[i],newypos)
    lab=df$name[i]
    if(!is.null(nodelabels[[label[i]]])) lab=nodelabels[[label[i]]]
    textplain(newmid,lab=lab,adj=adj)
    }
}

}


