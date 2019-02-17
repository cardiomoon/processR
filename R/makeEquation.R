# X=c("x1","x2") # independent variables
# M=c("m1","m2","m3")   # mediation variables
# Y=c("Y1","Y2")   #dependent variables


#'Add line feed to string
#'
#'@param x A string
#'@param ... one or more R objects, to be converted to character vectors.
addLine=function(x,...){
    if(x=="") x<-paste0(...)
    else x<-paste0(x,"\n",...)
    x
}

#'Add `+` mark to string
#'
#'@param x A string
#'@param ... one or more R objects, to be converted to character vectors.
addPlus=function(x,...){
  if(x=="") x<-paste0(...)
  else x<-paste0(x,"+",...)
  x
}

#' Make mediation equations 1
#'
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param stage An integer indicating the order
#' @param start An integer
#' @param add2ndMediation whether or not make a 2nd mediation equation
makeEquation1=function(X,M,stage=1,start=0,add2ndMediation=TRUE){
  countX=length(X)
  countM=length(M)

  equation=""

  for(i in 1:countM){
    sub=""
    for(j in 1:countX){
      sub=addPlus(sub,letters[stage],start+(i-1)*countX+j,"*",X[j])
    }
    if(add2ndMediation &(countM>1)&(i>1)){
      sub=addPlus(sub,"d",i-1,"*",M[i-1])
    }
    temp=paste0(M[i],"~",sub)
    equation=addLine(equation,temp)
  }
  equation
}

#' Make mediation equations 2
#'
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param Y A character vectors indicating dependent variables
makeEquation2=function(X,M,Y){
  countX=length(X)
  countM=length(M)
  countY=length(Y)

  equation=""

  for(i in 1:countY){

    temp=makeEquation1(M,Y[i],stage=2,start=(i-1)*countM)
    temp
    sub=""
    for(j in 1:countX){
      sub=addPlus(sub,letters[3],(i-1)*countX+j,"*",X[j])
    }
    temp=addPlus(temp,sub)
    equation=addLine(equation,temp)
  }
  equation
}

#' Make mediation equations 3
#'
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param Y A character vectors indicating dependent variables
#' @param add2ndMediation whether or not make a 2nd mediation equation
makeEquation3=function(X,M,Y,add2ndMediation=TRUE){
  (countX=length(X))
  (countM=length(M))
  (countY=length(Y))

  equation=""
  ind=c()
  for(k in 1:countY){
    for(i in 1:countX) {
      for(j in 1:countM) {

        noA=(i-1)*countM+j
        no=noA+(k-1)*countX*countM
        b=((noA-1)%/%countX)+1+(k-1)*countM
       #temp=paste0("ind",no,":=a",noA,"*b",b)
       #equation=addLine(equation,temp)
        ind=c(ind,paste0("a",noA,"*b",b))
        temp=paste0("ind",length(ind),":=",ind[length(ind)])
        equation=addLine(equation,temp)
      }
    }
  }
  equation
  secondInd=c()
  if(add2ndMediation &(countM>1)){
    for(k in 1:countY){
      for(j in 2:countM){
        equationa=""
        for(i in 1:countX){
          start=(j-2)*countX
          tempa=paste0("a",start+i,"*d",j-1,"*b",j+(k-1)*countM)
          equationa=addPlus(equationa,tempa)
        }
        #temp=paste0("secondInd",(j-1)+(k-1)*(countM-1),":=",equationa)
        secondInd=c(secondInd,equationa)
        temp=paste0("secondInd",length(secondInd),":=",secondInd[length(secondInd)])
        equation=addLine(equation,temp)
      }
    }
  }
  equation
  thirdInd=c()
  if(add2ndMediation &(countM>2)){
    for(k in 1:countY){
      for(j in 3:countM){
        equationa=""
        for(i in 1:countX){
          start=(j-3)*countX
          tempa=paste0("a",start+i,"*d",j-2,"*d",j-1,"*b",j+(k-1)*countM)
          equationa=addPlus(equationa,tempa)
        }
        #temp=paste0("thirdInd",j-2,":=",equationa)
        #temp=paste0("thirdInd",(j-2)+(k-1)*(countM-2),":=",equationa)
        thirdInd=c(thirdInd,equationa)
        temp=paste0("thirdInd",length(thirdInd),":=",thirdInd[length(thirdInd)])
        equation=addLine(equation,temp)
      }
    }
  }
  equation
  ## total effect
  total=c()

  for(k in 1:countY){
       # direct effect
       direct=paste0("c",seq(1:countX)+(k-1)*countX)
       Effect=Reduce(addPlus,direct)
       # indirect effect
      if(countM>=1){
       start=1+(k-1)*length(ind)/countY
       end=start+length(ind)/countY-1
       indirectEffect=Reduce(addPlus,ind[start:end])
       Effect=addPlus(Effect,indirectEffect)
      }
       # secondIndirect
       if(countM>=2){
       start=1+(k-1)*length(secondInd)/countY
       end=start+length(secondInd)/countY-1
       secondIndEffect=Reduce(addPlus,secondInd[start:end])
       if(add2ndMediation) Effect=addPlus(Effect,secondIndEffect)
       }
       # thirdIndirect
       if(countM>=3){
       start=1+(k-1)*length(thirdInd)/countY
       end=start+length(thirdInd)/countY-1
       thirdIndEffect=Reduce(addPlus,thirdInd[start:end])
       if(add2ndMediation) Effect=addPlus(Effect,thirdIndEffect)

       }
       temp=paste0("total",k,":=",Effect)
       equation=addLine(equation,temp)

  }


  equation
}


#' Make mediation equations 3
#'
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param Y A character vectors indicating dependent variables
#' @param add2ndMediation whether or not make a 2nd mediation equation
#' @param covar Optional list of covariates
#' @export
#' @examples
#' X="X";M=c("M1","M2","M3");Y=c("Y1","Y2");add2ndMediation=TRUE
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M1","Y1"),"Y2","Y2"))
#' cat(makeEquation(X,M,Y,covar=covar))
makeEquation=function(X,M,Y,add2ndMediation=TRUE,covar=list()){
    # cat("X=",X,"\n")
    # cat("M=",M,"\n")
    # cat("Y=",Y,"\n")
    # cat("str(covar)")
    # str(covar)
    (countX=length(X))
    (countM=length(M))
    (countY=length(Y))
    if(countX*countM*countY==0) {
      equation=" # You need at least one dependent variable(s),\n#one mediation variable(s) and one independent variable(s)."
    } else{
      temp=makeEquation2(X,M,Y)
      temp
      temp=addCovarEquation(temp,covar,prefix="g")
      equation=paste0("# Mediation Effect\n",temp)
      temp=makeEquation1(X,M,add2ndMediation=add2ndMediation)
      temp=addCovarEquation(temp,covar,prefix="h")
      equation=addLine(equation,temp)
      equation=addLine(equation,makeEquation3(X,M,Y,add2ndMediation=add2ndMediation))
    }
  equation
}

#' Add covariates to equation
#' @param equation The equation
#' @param covar A list
#' @param prefix prefix
#' @importFrom stringr str_trim
#' @export
#' @examples
#' equation="M ~ X*W"
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' addCovarEquation(equation,covar=covar,prefix=NULL)
addCovarEquation=function(equation,covar=list(),prefix="h"){


  temp1=unlist(strsplit(equation,"\n"))
  temp1
  temp2=strsplit(temp1,"~")
  temp2
  result=list()
  start=1
  i=1
  for(i in 1:length(temp2)){
      var=temp2[[i]][1]
      var=str_trim(var,side="both")
      covar
      temp3=seekVar(covar=covar,var=var,prefix=prefix,start=start)
      temp3
      if(is.null(temp3)){
          result[[i]]=paste(var,"~",temp2[[i]][2])
      } else {
          temp4=paste(temp2[[i]][2],"+",paste(temp3,collapse=" + "))
          result[[i]]=paste(var,"~",temp4)
          start=start+length(temp3)
      }
  }
  paste(unlist(result),collapse="\n")
}

#' Seek var form covariates
#' @param covar A list of covariates
#' @param var A name of variable to look for
#' @param prefix A prefix
#' @param start A start number
#' @export
#' @examples
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' var="M"
#' seekVar(covar,var,prefix=NULL)
seekVar=function(covar=list(),var,prefix="h",start=1){
  temp=c()
  if(length(covar$name)>0){
    j=start
    for(i in 1:length(covar$name)){
      # if(!is.null(covar$label[i])) var<-covar$label[i]
      if(var %in% covar$site[[i]]){
        if(!is.null(prefix)) {
          temp=c(temp,paste0(prefix,j,"*",covar$name[i]))
        } else{
          temp=c(temp,covar$name[i])
        }
        j=j+1
      }
    }
  }
  temp
}

# X="age"
# M=c("policy","male")
# Y="interest"
# add2ndMediation=TRUE
# covar=list(name="educ",site=list(c("M1","M2","Y")))
# makeEquation(X,M,Y,covar=covar)
# makeEquation1(X,M)
# makeEquation3(X,M,Y)


# makeDf=function(X,M=NULL,Y,maxx=30,maxy=30){
#
#    countX=length(X)
#    mediation<-0
#    if(!is.null(M)) {
#       mediation<-1
#       countM=length(M)
#    }
#    countY=length(Y)
#
#    height=3
#    width=5
#
#    x<-y<-text<-group<-c()
#    for(i in 1:countX){
#       x<-c(x,ifelse(mediation==1,5,10))
#       starty=ifelse(countX>2,20,ifelse(countX==2,20,15))
#       stepy=ifelse(countX>1,(maxy-(maxy-starty)*2)/(countX-1),0)
#       y<-c(y,starty-stepy*(i-1))
#       text<-c(text,X[i])
#       group<-c(group,"X")
#    }
#    if(mediation==1) for(i in 1:countM){
#      x<-c(x,maxx*3/7)
#      starty=maxy-3
#      stepy=ifelse(countM>1,(maxy-(maxy-starty)*2)/(countM-1),0)
#      y<-c(y,starty-stepy*(i-1))
#      text<-c(text,M[i])
#      group<-c(group,"M")
#    }
#    for(i in 1:countY){
#      x<-c(x,ifelse(mediation==1,maxx-5,maxx-10))
#      starty=ifelse(countY>2,22,ifelse(countY==2,22,15))
#      stepy=ifelse(countY>1,(maxy-(maxy-starty)*2)/(countY-1),0)
#      y<-c(y,starty-stepy*(i-1))
#      text<-c(text,Y[i])
#      group<-c(group,"Y")
#    }
#    df=data.frame(x,y,text,group)
#    df
# }
#
#
# makeDf2=function(df){
#   dfX=df[df$group=="X",]
#   dfM=df[df$group=="M",]
#   dfY=df[df$group=="Y",]
#
#   countX=nrow(dfX)
#   countM=nrow(dfM)
#   countY=nrow(dfY)
#
#   height=3
#   width=5
#   x<-y<-xend<-yend<-group<-id<-c()
#   if(countM>=1){
#
#         for(j in 1:countY){
#           for(i in 1:countM){
#             x<-c(x,dfM$x[i]+width/2)
#             y=c(y,dfM$y[i])
#             xend=c(xend,dfY$x[j]-width/2)
#             yend=c(yend,dfY$y[j])
#             group<-c(group,1)
#             id=c(id,paste0("b",(j-1)*countM+i))
#             cat("M=",i,",Y=",j,",b=",(j-1)*countM+i,"\n")
#           }
#         }
#     for(i in 1:countM){
#         if(i<countM){
#
#           x<-c(x,dfM$x[i])
#           y=c(y,dfM$y[i]-height/2)
#           xend=c(xend,dfM$x[i+1])
#           yend=c(yend,dfM$y[i+1]+height/2)
#           group<-c(group,1)
#           id=c(id,paste0("d",i))
#         }
#     }
#
#   }
#
#     if(countM>=1) {
#        for(j in 1:countM){
#          for(i in 1:countX){
#            x<-c(x,dfX$x[i]+width/2)
#            y=c(y,dfX$y[i])
#            xend=c(xend,dfM$x[j]-width/2)
#            yend=c(yend,dfM$y[j])
#            group<-c(group,1)
#            id=c(id,paste0("a",(j-1)*countX+i))
#          }
#        }
#     }
#     for(k in 1:countY){
#       for(i in 1:countX){
#       x<-c(x,dfX$x[i]+width/2)
#       y=c(y,dfX$y[i])
#       xend=c(xend,dfY$x[k]-width/2)
#       yend=c(yend,dfY$y[k])
#       group<-c(group,2)
#       id=c(id,paste0("c",(k-1)*countX+i))
#       }
#     }
#
#   resultdf=data.frame(x,y,xend,yend,group,id)
#   resultdf
# }


# plotXMY=function(X,M,Y){
#   df=makeDf(X,M,Y)
#   df
#   df2=makeDf2(df)
#   df2
#
#   height=3
#   width=5
#   p<-ggplot(data=df,aes(x=x,y=y))+
#     xlim(0,30)+ylim(0,30)+
#     geom_rect(aes(fill=group),xmin=df$x-width/2,xmax=df$x+width/2,
#               ymin=df$y-height/2,ymax=df$y+height/2,color="black",alpha=0.5)+
#     geom_text(aes(label=text),size=7)
#
#   p<-p+geom_segment(data=df2,aes(x=x,y=y,xend=xend,yend=yend),
#                     arrow=arrow(angle=15,length=unit(0.2,"cm"),type="closed"))
#   p<-p+geom_label(data=df2,aes(x=(x+xend)/2,y=(y+yend)/2,label=id))
#   p<-p+theme_clean()+ theme(legend.position="none")
#   p
# }
#
