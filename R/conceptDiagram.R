#' Draw arrow
#' @param from coordinates (x,y) of the point *from* which to draw arrow.
#' @param to coordinates (x,y) of the point *to* which to draw arrow.
#' @param lwd line width
#' @param adjust adjust position
#' @param label label
#' @param label.pos label position
#' @param arr.pos arrow position
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xadj numeric x adjustment
#'@param yadj numeric y adjustment
#' @param ... Further argument to be passed to straightarrow()
#' @export
#' @importFrom diagram textplain straightarrow
myarrow=function(from,to,lwd=1,adjust=1,label="",label.pos=0.5,arr.pos=NULL,radx=0.10,rady=0.06,xadj=NULL,yadj=NULL,...){
    if(!is.null(arr.pos)){
        if(arr.pos==0) arr.pos<-NULL
    }
    if(is.null(arr.pos)){
        # if(adjust){
        #     if(from[2]==to[2]) arr.pos=0.8
        #     else if(from[2]>to[2]) arr.pos=0.7
        #     else arr.pos=0.68
        # } else{
        #     distance=abs(to[2]-from[2])+abs(to[1]-from[1])
        #     if(distance<=0.23) {
        #         arr.pos=0.86
        #
        #     } else if(distance>0.5){
        #         arr.pos=0.95
        #     } else{
        #         arr.pos=0.94
        #     }
        # }
        # distance=abs(to[2]-from[2])+abs(to[1]-from[1])
      if(adjust==0){
         if(abs(to[1]-from[1])>abs(to[2]-from[2])){
             distance=abs(to[1]-from[1])
             arr.pos=(distance-0.01)/distance
         } else{
           distance=abs(to[2]-from[2])
           arr.pos=(distance-0.02)/distance
         }
      } else {
        distance1=abs(to[1]-from[1])
        ratio1=ifelse(distance1==0,0,(distance1-radx-0.016)/distance1)
        distance2=abs(to[2]-from[2])
        ratio2=ifelse(distance2==0,0,(distance2-rady-0.03)/distance2)
        arr.pos=max(ratio1,ratio2)
      }
      # else if(abs(to[2]-from[2])>=0.3){
      #      if(abs(to[1]-from[1])>=0.4){
      #        distance=abs(to[1]-from[1])
      #        arr.pos=(distance-radx-0.015)/distance
      #
      #      } else{
      #        distance=abs(to[2]-from[2])
      #        arr.pos=(distance-rady-0.03)/distance
      #      }
      #
      # } else {   #if(abs(to[1]-from[1])>abs(to[2]-from[2]))
      #   distance=abs(to[1]-from[1])
      #   arr.pos=(distance-radx-0.015)/distance
      # }

    }
    mid=from+label.pos*(to-from)*arr.pos

    xadj1=1
    yadj1=-0.3

    # cat("from=",from,"\n")
    # cat("to=",to,"\n")
    if(length(to)>1){
    if(from[2]>=to[2]) {
      xadj1=0
    }
    if(from[1]==to[1]) {
      xadj1=1.5
      yadj1=0
    }
    }

    if(is.null(xadj)) xadj=xadj1
    if(is.null(yadj)) yadj=yadj1
    straightarrow(from=from,to=to,lwd=lwd,arr.pos=arr.pos,arr.type="triangle",...)
    textplain(mid=mid,lab=label,adj=c(xadj,yadj))
}

#' Draw node
#' @param ... Further argument to be passed to textellipse() or textrect()
#' @param latent Logical
#' @importFrom diagram textellipse textrect
#' @export
drawtext=function(...,latent=TRUE){
    if(latent) textellipse(...)
    else textrect(...)
}

midPoint=function(from=0,to=1,length.out=2){
    res=seq(from,to,length.out = length.out+2)
    res[c(-1,-length(res))]
}

#'Make concept Diagram
#'@param X character Name of independent variable
#'@param M character Name of mediator variable
#'@param Y character Name of dependent variable
#'@param latent Logical. whether or not X,Y and Z are latent variables or not
#'@param xb Logical. if positive draw line between X and (Y+Z)
#'@param mc Logical. if positive draw line between M and (X+Y)
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xmargin horizontal margin of plot
#'@param yinterval vertical interval between box
#'@param moderator optional list of moderators
#'@param labels optional labels of X,Y and Z variables
#'@param covar covariate optional list of covariates
#'@importFrom diagram openplotmat
#'@examples
#'labels=list(X="Time Spent in\n Grad School", M="# of\n Publications", Y="# of Job Offers")
#'conceptDiagram2(xb=TRUE,labels=labels)
#'moderator=list(name="Z1",label="Time Spent\n with Alex",pos=3,
#'     site=list(c("a","b","c")),latent=FALSE)
#'conceptDiagram2(moderator=moderator,labels=labels)
#'moderator=list(name=c("Z1","Z2"),label=c("Time Spent\n with Alex","Z2label"),pos=c(3,3),
#'     site=list(c("a","b","c"),c("b","c")),latent=c(FALSE,FALSE))
#'conceptDiagram2(moderator=moderator,labels=labels,yinterval=0.4)
#'covar=list(name=c("C1","C2"),label=c("sex","tenure"),site=list(c("Y"),c("Y")))
#'conceptDiagram2(M=NULL,moderator=list(name="M",pos=4,site=list("c"),latent=FALSE),covar=covar)
#'conceptDiagram2(covar=covar)
#'@export
conceptDiagram2=function(X="X",M="M",Y="Y",latent=rep(FALSE,3),xb=FALSE,mc=FALSE,
                        radx=0.06,rady=0.06,xmargin=0.03,yinterval=NULL,
                        moderator=list(),labels=list(),covar=list()){

      # radx=0.12;rady=0.05;xmargin=0.03;yinterval=NULL
      # latent=rep(FALSE,3);xb=FALSE;mc=FALSE;labels=list()
      # labels
      #
      # X="X";M="M";Y="Y";latent=rep(FALSE,3);xb=FALSE
      # moderator
      # library(diagram)
      # covar

    if(is.null(yinterval)) yinterval=rady*7
    openplotmat()
    ystart=0.4
    if(length(covar$name)>2) ystart=0.5
    x=c(0+radx+xmargin,ystart)
    y=c(1-(radx+xmargin),ystart)
    m=c(0.5,ystart+yinterval)


    moderator

    select=which(moderator$pos==4)
    xpos=midPoint(0,1,length(select))
    select
    for(j in seq_along(select)){
        temp=c(xpos[j],0.4+yinterval-0.05)
        assign(paste0("z",select[j]),temp)
    }

    select=which(moderator$pos==3)
    xpos=midPoint(0,1,length(select))
    select
    for(j in seq_along(select)){
        temp=c(xpos[j],0.5-yinterval+0.05)
        assign(paste0("z",select[j]),temp)
    }

    select=which(moderator$pos==2)
    select
    if(length(select)==1){
        xpos=1-(radx+xmargin)
        ypos=0.5+yinterval
    } else{
       xpos=midPoint(0.5+2*radx,1-radx,length(select))
       ypos=midPoint(0.5+yinterval+2*rady,0.5+rady,length(select))
    }
    for(j in seq_along(select)){
        temp=c(xpos[j],ypos[j])
        assign(paste0("z",select[j]),temp)
    }
    select=which(moderator$pos==1)
    if(length(select)==1){
        xpos=radx+xmargin
        ypos=0.5+yinterval
    } else{
    xpos=midPoint(radx-xmargin,0.5-2*radx,length(select))
    ypos=midPoint(0.5+rady,0.5+yinterval+2*rady,length(select))
    }
    for(j in seq_along(select)){
        temp=c(xpos[j],ypos[j])
        assign(paste0("z",select[j]),temp)
    }

    startpos=list()
    sum=1
    for(i in seq_along(moderator$pos)){
        for(j in 1:length(moderator$site[[i]])){
            startpos[[sum]]=get(paste0("z",i))
            sum=sum+1
        }
    }
    startpos
    labels

    (xlab=ifelse(is.null(labels[[X]]),X,labels[[X]]))
    if(!is.null(M)) (mlab=ifelse(is.null(labels[[M]]),M,labels[[M]]))
    (ylab=ifelse(is.null(labels[[Y]]),Y,labels[[Y]]))



    if(!is.null(M)){
        # myarrow(from=x,to=y,label="c'")
        # myarrow(from=x,to=m,label="a")
        # myarrow(from=m,to=y,label="b")
        myarrow(from=x,to=y,radx=radx,rady=rady)
        myarrow(from=x,to=m,radx=radx,rady=rady)
        myarrow(from=m,to=y,radx=radx,rady=rady)
    } else{
        myarrow(from=x,to=y,label="",radx=radx,rady=rady)
    }
    if(xb) myarrow(from=x,to=0.5*(m+y),radx=radx,rady=rady,adjust=0)
    if(mc) myarrow(from=m,to=0.5*(x+y),radx=radx,rady=rady)

    endpos=moderator2pos(moderator,x,y,m)
    endpos

    for(i in seq_along(endpos)){
        myarrow(from=startpos[[i]],to=endpos[[i]],adjust=0,radx=radx,rady=rady)
    }

    if(length(covar$name)>0) drawCovar(covar,x,y,m,radx=radx,rady=rady)

    drawtext(x,radx=radx,rady=rady,lab=xlab,latent=latent[1])

    drawtext(y,radx=radx,rady=rady,lab=ylab,latent=latent[3])
    if(!is.null(M)) {

        drawtext(m,radx=radx,rady=rady,lab=mlab,latent=latent[2])
    }

    for(i in seq_along(moderator$pos)){
        z=eval(parse(text=paste0("z",i)))
        lab=ifelse(is.null(moderator$label[i]),moderator$name[i],moderator$label[i])
        drawtext(z,radx=radx,rady=rady,lab=lab,latent=moderator$latent[i])
    }
}


#'get position from moderator
#'@param moderator A list
#'@param x position of x
#'@param y position of y
#'@param m position of m
moderator2pos=function(moderator=list(),x,y,m){
    result=unlist(moderator$site)
    result
    count=rep(0,3)
    total=c(sum(result=="a"),sum(result=="b"),sum(result=="c"))
    pos=list()
    x
    y
    m
    (x+y)*2/3
    for(i in seq_along(result)){
       if(result[i]=="a"){
           count[1]=count[1]+1
           pos[[i]]=(m*count[1]+x*(total[1]+1-count[1]))/(total[1]+1)
       } else if(result[i]=="b"){
            count[2]=count[2]+1
            pos[[i]]=(y*count[2]+m*(total[2]+1-count[2]))/(total[2]+1)
       } else{
           count[3]=count[3]+1
           pos[[i]]=(y*count[3]+x*(total[3]+1-count[3]))/(total[3]+1)
        }
    }
    pos
}

#'Draw covariate
#'@param covar A list
#'@param x position of x
#'@param y position of y
#'@param m position of m
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param yinterval vertical interval between box
drawCovar=function(covar=list(),x,y,m,radx=0.10,rady=0.06,yinterval=0.02){

    count=length(covar$name)
    count
    covar$site[[1]]
    covar
    if(is.null(covar$latent)) {
        covar$latent=rep(FALSE,count)
    }
    pos=list()

    for(i in 1:count){
        pos[[i]]<-c(x[1]+(radx/2)*(i),x[2]-(rady*2+yinterval)*i)

        if("M" %in% covar$site[[i]]) myarrow(pos[[i]],m,radx=radx,rady=rady)
        if("Mi" %in% covar$site[[i]]) myarrow(pos[[i]],m,radx=radx,rady=rady)
        if("Y" %in% covar$site[[i]]) myarrow(pos[[i]],y,radx=radx,rady=rady)
    }
    for(i in 1:count){
        lab=ifelse(is.null(covar$label[i]),covar$name[i],covar$label[i])
        drawtext(pos[[i]],radx=radx,rady=rady,lab=lab,latent=covar$latent[i])
    }

}


#' Make a data.frame for conceptDiagram
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @importFrom stringr str_flatten str_detect str_extract_all str_replace str_extract
#'@export
fit2df2=function(fit){
    res=parameterEstimates(fit,standardized=TRUE)
    res
    ## latent variable
    res1=res[res$op=="~",]
    res1
    text<-group<-x<-y<-latent<-c()
    count=0
    res1
    for(i in 1:nrow(res1)){
        #i=1
        temp=res1$lhs[i]
        temp
        if(!(temp %in% text)){
            text=c(text,temp)
            # whether temp is a latent varible
            tempres=any(res1[res1$lhs==temp,]$op=="=~")
            latent<-c(latent,tempres)
            # group determination #
            (tempgroup=seekGroup(temp,res1,group))

            group=c(group,tempgroup)

        }
        temp=res1$rhs[i]
        if(!(temp %in% text)){
            text=c(text,temp)
            # whether temp is a latent varible
            tempres=any(res1[res1$lhs==temp,]$op=="=~")
            latent<-c(latent,tempres)
            # group determination #
            (tempgroup=seekGroup(temp,res1,group))

            group=c(group,tempgroup)

        }
    }
    group
    df=data.frame(text,latent,group,stringsAsFactors = FALSE)
    df
    res1
    label=c()
    to=c()
    for(i in seq_along(df$text)){
        temp=stringr::str_flatten(res1$label[res1$rhs==df$text[i]],",")
        label=c(label,ifelse(length(temp)>0,temp,""))
        temp=stringr::str_flatten(res1$lhs[res1$rhs==df$text[i]],",")
        to=c(to,ifelse(length(temp)>0,temp,""))
    }
    label
    df$label=label
    df$to=to
    df
    mod=df[stringr::str_detect(df$text,":"),]
    moderator=c()

    for(i in seq_along(mod$text)){
        moderator=c(moderator,unlist(stringr::str_split(mod$text[i],":"))[2])
    }
    moderator
    mod$text=unlist(stringr::str_split(mod$text[i],":"))[2]
    mod
    df<-df[!stringr::str_detect(df$text,":"),]
    df=rbind(df,mod)
    role=c()
    df
    for(i in seq_along(df$text)){
        if(df$text[i] %in% moderator) temp="Z"
        else if(stringr::str_detect(df$group[i],"M")) temp="M"
        else if(stringr::str_detect(df$group[i],"Y")) temp="Y"
        else temp="X"
        role=c(role,temp)
    }
    df$role1=role
    temp=stringr::str_replace(df$label,"dash","")
    df$site=unlist(lapply(stringr::str_extract_all(temp,"[a-c,]"),myflatten))
    df$pos=0
    df$pos[stringr::str_detect(df$site,"c")]=3
    df$pos[df$pos==0 & stringr::str_detect(df$site,"a")]=1
    df$pos[df$pos==0 & stringr::str_detect(df$site,"b")]=2
    df$pos[df$pos==0 & df$site==""]=3
    df
    for(i in seq_along(df$text)){
          if(i==1) {
              df1=df[1,]
          } else{
              if(!(df$text[i] %in% df1$text)){
              temp=df[df$text==df$text[i],]
              if(nrow(temp)==1) {
                  df1=rbind(df1,temp)
              } else{
                  df2=temp[1,]
                  df2$site=str_flatten(unique(unlist(str_split(temp$site,","))),",")
                  df2$pos=max(temp$pos)
                  df1=rbind(df1,df2)
              }
              }
          }
    }
    df1$group=str_extract(df1$group,"[a-zA-Z]")
    df1$role=ifelse(df1$role1==df1$group,
                    df1$role1,
                    paste0(df1$group,",",df1$role1))
    df1
    # for(i in seq_along(df1$role)){
    #      temp=str2vector(df1$role[i])
    #      temp
    #      if(length(temp)>1){
    #          if("X" %in% temp){
    #              df1$site[i]=str_setdiff(df1$site[i],c("a","c"))
    #          } else if("M" %in% temp){
    #              df1$site[i]=str_setdiff(df1$site[i],c("a","b"))
    #
    #          }
    #      }
    # }
    df1

}


#' Make character vector from string
#' @param string string
#' @export
str2vector=function(string="a,b,c"){
    unlist(str_split(string,","))
}


#'Remove matched pattern from string
#'@param string string
#'@param pattern pattern to look for
#'@export
str_setdiff=function(string="a,c",pattern="a"){

    temp=unlist(str_split(string,","))
    res=setdiff(temp,pattern)
    if(length(res)==0) {
        res=""
    } else{
        res=str_flatten(res,",")
    }
    res
}

# str_setdiff("b",c("a","b"))
#' flatten string
#' @param x character to flatten
#' @export
myflatten=function(x){
    result=stringr::str_flatten(x)
    if(length(result)==0) result=""
    result
}

#' Make conceptDiagram
#'@param fit An object of class lavaan. Result of sem function of package lavaan
#'@param labels labels
#'@importFrom stringr str_split
#'@export
conceptDiagram=function(fit,labels=NULL){
    df=fit2df2(fit)
    df
    (X=df$text[str_detect(df$role,"X")][1])
    (Y=df$text[str_detect(df$role,"Y")])
    (M=df$text[str_detect(df$role,"M")])
    if(length(M)==0) {
        M<-NULL
        latent=df$latent[c(which(str_detect(df$role,"X"))[1],NA,which(str_detect(df$role,"Y")))]
    } else{
        latent=df$latent[c(which(str_detect(df$role,"X")),which(str_detect(df$role,"M")),
                           which(str_detect(df$role,"Y")))]
    }
    xb=FALSE
    mc=FALSE
    df=df[str_detect(df$role,"Z"),]
    df
    label=df$text
    label
    if(length(label)>0) {
        if(!is.null(labels)){
           label=c()
           for(i in seq_along(df$text)){
              temp=labels[[df$text[i]]]
              label=c(label,temp)
           }
        }
    }
    label
    df
    X
    for(i in seq_along(df$text)){
       if(df$text[i]==X){
           xb=TRUE
           df=df[df$text[i]!=X,]
       } else if(!is.null(M)){
           if(df$text[i]==M){
           mc=TRUE
           df=df[df$text[i]!=M,]
           }
       }
    }
    if(nrow(df)>0){
    moderator=list(name=df$text,pos=df$pos,latent=df$latent,label=label)
    moderator$site=stringr::str_split(df$site,",")
    moderator
    for(i in seq_along(moderator$site)){
        if(length(moderator$site[[i]])==1){
           if(moderator$site[[i]]=="") moderator$site[[i]]<-"c"
        }
    }
    } else {
        moderator=NULL
    }

    X
    Y
    M
    latent
    moderator
    labels
    # X=X;M=M;Y=Y;latent=latent;moderator=moderator;labels=NULL
    if(is.null(M)){
        conceptDiagram2(X=X,M=NULL,Y=Y,latent=latent,moderator=moderator,labels=labels)
    } else {
       conceptDiagram2(X=X,M=M,Y=Y,latent=latent,moderator=moderator,labels=labels,xb=xb,mc=mc)
    }
}


#'Find group with variable name
#'
#'@param var A string to seek
#'@param  res A data.frame. Result of parameterEstimates function of package lavaan or subset.
seekGroup1=function(var,res){

    tempgroup<-""
    tofind<-var
    mode<-0

    res4<-res[res$op=='~',]

    (Left<-res4$lhs)
    (Right<-res4$rhs)


    if(any(Left==tofind)) {
        mode<-mode+1
        #print("mode<-mode+1")
    }
    if(any(Right==tofind)) {
        mode<-mode+2
        #print("mode<-mode+2")
    }
    mode
    if(mode==3) {
        tempgroup="M"
    } else if(mode==1) {
        tempgroup="Y"
    } else if(mode==2) tempgroup="X"
    tempgroup
    if(tempgroup==""){
        res5=res[res$op=='=~',]

        #if(tofind %in% res5$lhs) tempgroup="Y"
        if(tofind %in% res5$lhs) tempgroup="X"
    }

    #print(group)
    # print(mode)
    # print(tofind)
    # print(Left)
    # print(Right)
    # print(tempgroup)
    tempgroup

}


#'Find group with variable name
#'
#'@param var A string to seek
#'@param  res A data.frame. Result of parameterEstimates function of package lavaan or subset.
#'@param group A character vector
seekGroup2=function(var,res,group){

    res3=res[(res$rhs==var) & (res$op!="~~"),]
    temp=res3$lhs[1]
    if(is.na(temp)) result="X"
    else result=seekGroup1(temp,res)
    tempgroup=""
    if(result=="X") tempgroup="0"
    else if(substr(result,1,1)=="M") {
        # print("\n")
        # print(var)
        # print(temp)
        # print(result)
        # print(group)
        tempgroup="H"
    } else if(result=="Y") tempgroup="5"
    paste0(tempgroup,",",temp)
}

#' Count the group names start with "M"
#'@param group A string vectors
countM=function(group){
    result=0
    if(length(group)>0){
        for(i in 1:length(group)){
            if(substr(group[i],1,1)=="M") result=result+1
        }
    }
    result


}


#'Find group with variable name
#'
#'@param var A string to seek
#'@param  res A data.frame. Result of parameterEstimates function of package lavaan or subset.
#'@param group A string vector
#'@export
seekGroup=function(var,res,group){
    # res=res1;var=temp
    (result=seekGroup1(var,res))
    if(result=="M"){
        count=countM(group)
        result=paste0(result,count+1)
    }
    if(result=="") result=seekGroup2(var,res,group)
    result
}

