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
#' @param add2ndMediation whether or not make a secondmediation equation
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
#' @param add2ndMediation whether or not make a secondmediation equation
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
#' @param add2ndMediation whether or not make a secondmediation equation
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
#' @param grouplabels A list
#' @param multipleMediator logical
#' @importFrom stringr str_trim
#' @export
#' @examples
#' equation="M ~ X*W\nY ~ a1*M + C1*X"
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' grouplabels=list(C1="e")
#' addCovarEquation(equation,covar=covar)
#' equation="M1 ~ a11*X\nM2 ~ a12*M"
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M1","Y"),"M2","M2"))
#' addCovarEquation(equation,covar=covar,multipleMediator=TRUE)
#' addCovarEquation(equation,covar=covar)
addCovarEquation=function(equation,covar=list(),prefix="f",grouplabels=NULL,multipleMediator=FALSE){

       # prefix="f";grouplabels=NULL;multipleMediator=FALSE
  # prefix=NULL
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
      var
      covar
      if(multipleMediator){
         suffix=i
      } else{
         suffix<-NULL
      }
      temp3=seekVar(covar=covar,var=var,prefix=prefix,start=start,grouplabels=grouplabels,suffix=suffix)
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
#' @param grouplabels A list
#' @param suffix A suffix
#' @export
#' @examples
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' var="Y"
#' seekVar(covar,var,prefix="h")
seekVar=function(covar=list(),var,prefix="h",start=1,grouplabels=NULL,suffix=NULL){
  # prefix="h";start=1;grouplabels=NULL
  temp=c()
  if(length(covar$name)>0){
    j=start
    for(i in 1:length(covar$name)){
      if(var %in% covar$site[[i]]) temp=c(temp,covar$name[i])
    }
    res=c()
    for(i in seq_along(temp)){
        if(temp[i] %in% names(grouplabels)){
          count=attr(grouplabels[[temp[i]]],"length")
          temp1=paste0(grouplabels[[temp[i]]],1:count)
          res=c(res,temp1)
        } else{
          res=c(res,temp[i])
        }
    }

    temp=c()
    j=1
    for(i in seq_along(res)){
        if(!is.null(prefix)) {
          temp=c(temp,paste0(prefix,j,suffix,"*",res[i]))
        } else{
          temp=c(temp,res[i])
        }
        j=j+1
    }
  }
  temp
}


