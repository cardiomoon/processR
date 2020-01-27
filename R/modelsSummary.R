#' Change regression coefficient name
#' @param name string vector to change
#' @param dep names of dependent variable
#' @param labels optional list
#' @param constant name of constant
#' @param prefix name of prefix
#' @export
makeCoefLabel=function(name,dep,labels,constant,prefix){

  # name=c("(Intercept)","negtone","negexp","dysfunc","negtone:negexp")
  # name=c("(Intercept)","dysfunc")
  # dep="perform"
  # labels=list(X="dysfunc",M="negtone",W="negexp",Y="perform")
  # constant="iy"
  # prefix="c"
    # labels=list(X="protest",W="sexism",M="respappr",Y="liking")
    # dep="respappr"
    # name=c("D1","D2","sexism")
   # constant="iy"
   # prefix="c"
  # name=c("(Intercept)","cond","import")
  # dep="pmi"
  # labels=list(X="cond",M=c("import","pmi"),Y="reaction")
  #  name=c("X","M1","M2")
  #  dep="M3"
  # labels=list(X="X",M=c("M1","M2","M3"),Y="Y")
  # cat("name=",name,"\n")
   # name=c("M","X","M:W","W","X:W")
   # dep="Y"
   # labels=list(X="X",M="M",Y="Y")
   # constant="iy"
   # prefix="b"
   # name=c("baby","milk","baby:milk")
   # prefix="a"
   # dep="wine";constant="iy"
   # labels=list(X="baby",M=c("wine","tent","sand"),Y="tile",W="milk")



  result=c()
  dep=changeLabelName(dep,labels,add=FALSE)
  dep
   # cat("dep=",dep,"\n")
  j<-k<-l<-m<-1
  # if((substr(dep,1,1)=="M")&(nchar(dep)==2)){
  #     j=as.numeric(paste0(substr(dep,2,2),"1"))
  # }

  temp=changeLabelName(name,labels,add=FALSE)
  # cat("temp=",temp,"\n")

  for(i in seq_along(temp)){
  if(stringr::str_detect(temp[i],"D[1-9]")){
      temp[i]=stringr::str_replace(temp[i],"D","X")
   }
  }

  for(i in seq_along(temp)){
    if(dep=="Y"){
        if(temp[i]=="(Intercept)"){
           result=c(result,"iY")
        } else if(str_detect(temp[i],"^X[1-9]?")){
           result=c(result,paste0("c",j))
           j=j+1
        } else if(str_detect(temp[i],"^C[1-9]?")){
          if(nchar(temp[i])==1){
            result=c(result,paste0("g"))
          } else{
            result=c(result,paste0("g",substr(temp[i],2,2)))
          }
        } else if(str_detect(temp[i],"^(W|V|Z)[1-9]?")){
            if(paste0("X:",temp[i]) %in% temp){
              result=c(result,paste0("c",j))
              j=j+1
            } else if(any(str_detect(temp,paste0("X[1-9]?:")))){
                result=c(result,paste0("c",j))
                j=j+1
            } else{
              result=c(result,paste0("b",l))
              l=l+1
            }
        } else if(str_detect(temp[i],":X[1-9]?")){
          result=c(result,paste0("c",j))
          j=j+1
        } else{
          result=c(result,paste0("b",l))
          l=l+1
        }
    } else if(dep=="M"){
      if(temp[i]=="(Intercept)"){
        result=c(result,"iM")
      } else if(str_detect(temp[i],"^C[1-9]?")){
        if(nchar(temp[i])==1){
          result=c(result,"f")
        } else{
           result=c(result,paste0("f",substr(temp[i],2,2)))
        }
      } else{
        result=c(result,paste0("a",j))
        j=j+1
      }
    } else{    ##  "M1","M2",...
      if(temp[i]=="(Intercept)"){
        result=c(result,paste0("i",dep))
      } else if(str_detect(temp[i],"^C[1-9]?")){
          result=c(result,paste0("f",k,substr(dep,2,2)))
          k=k+1
      } else if(str_detect(temp[i],"^M[1:9]?")){
          if(nchar(temp[i])==2){  # M1
            result=c(result,paste0("d",substr(dep,2,2),substr(temp[i],2,2)))
          } else{  ## M1:W
            result=c(result,paste0("d",substr(dep,2,2),substr(temp[i],2,2),l))
            l<-l+1
          }
      } else if(str_detect(temp[i],"^(W|V|Z)[1-9]?")){
        if(paste0("X:",temp[i]) %in% temp){
          result=c(result,paste0("a",j,substr(dep,2,2)))
          j=j+1
        } else{
          tempvar=temp[str_detect(temp,"^M[1-9]:")][1]
          result=c(result,paste0("d",substr(dep,2,2),substr(tempvar,2,2),l))
          # result=c(result,paste0(prefix,l))
          l<-l+1
          # result=c(result,paste0("b",l))
          # l=l+1
        }
      } else{
          result=c(result,paste0("a",substr(dep,2,2)))
         # if(length(temp)==1){
         #   result=c(result,paste0("a",substr(dep,2,2)))
         # } else{
         #   result=c(result,paste0("a",j,substr(dep,2,2)))
         #   j=j+1
         # }
      }
    }
    # if(temp[i]=="(Intercept)") {
    #    if(dep=="M1") {
    #      result=c(result,"iM1")
    #    } else if(dep=="M2"){
    #      result=c(result,"iM2")
    #    } else {
    #      result=c(result,constant)
    #    }
    # } else if(temp[i]=="M") {
    #   result=c(result,paste0("b",l))
    #   l=l+1
    # } else if(substr(temp[i],1,1)=="C"){
    #   if(dep=="Y") {
    #     result=c(result,paste0("g",k))
    #   } else{
    #     result=c(result,paste0("f",k))
    #
    #   }
    #   k<-k+1
    # } else if(temp[i]=="X"){
    #   if(dep=="Y") {
    #      result=c(result,paste0(ifelse(any(str_detect(temp,"^M")),"c'","c"),j))
    #
    #   } else if(str_detect(dep,"^M")){
    #      result=c(result,paste0("a",j))
    #   }  else{
    #     result=c(result,paste0(prefix,j))
    #
    #   }
    #   j<-j+1
    # } else if(temp[i]=="W"){
    #   if(("X:W" %in% temp)|("W:X" %in% temp)|("D1:W" %in% temp)|("W:D1" %in% temp)){
    #     if(dep=="Y") {
    #       result=c(result,paste0(ifelse(any(str_detect(temp,"^M")),"c'","c"),j))
    #
    #     } else{
    #       result=c(result,paste0(prefix,j))
    #
    #     }
    #     j<-j+1
    #   } else if(dep %in% c(paste0("M",2:9))) {
    #     tempvar=temp[str_detect(temp,"^M[1-9]:")][1]
    #     temp1=paste0("d",substr(dep,2,2),substr(tempvar,2,2))
    #     if(m>1) temp1=paste0(temp1,m)
    #     result=c(result,temp1)
    #     m<-m+1
    #   } else{
    #     # result=c(result,paste0("b",l))
    #     # l<-l+1
    #     if(any(str_detect(temp,"^M[1-9]:"))){
    #       # tempvar=temp[str_detect(temp,"^M[1-9]:")][1]
    #       # cat("tempvar=",tempvar,"\n")
    #       # result=c(result,paste0(prefix,substr(tempvar,2,2),l))
    #       result=c(result,paste0(prefix,l))
    #       l<-l+1
    #     } else{
    #       result=c(result,paste0(prefix,j))
    #       l<-l+1
    #     }
    #
    #   }
    #
    # } else if(temp[i] %in% c("X:W","W:X")){
    #   if(dep=="Y") {
    #     result=c(result,paste0(ifelse(any(str_detect(temp,"^M")),"c'","c"),j))
    #
    #   } else{
    #     result=c(result,paste0(prefix,j))
    #
    #   }
    #   j<-j+1
    #
    #
    # } else if(str_detect(temp[i],"^D[0-9]")){
    #   if(dep=="Y") {
    #     result=c(result,paste0(ifelse("M" %in% temp,"c'","c"),j))
    #
    #   } else{
    #     result=c(result,paste0(prefix,j))
    #
    #   }
    #   j<-j+1
    #
    #
    # } else if(str_detect(temp[i],"^M[1-9]:?")){
    #
    #   if(dep %in% c(paste0("M",2:9))) {
    #      temp1=paste0("d",substr(dep,2,2),substr(temp[i],2,2))
    #      if(m>1) temp1=paste0(temp1,m)
    #      result=c(result,temp1)
    #      m<-m+1
    #   } else{
    #     # result=c(result,paste0("b",substr(temp[i],2,2),l))
    #     result=c(result,paste0("b",l))
    #     l=l+1
    #   }
    # } else {  #if(temp[i]=="M:W")
    #    result=c(result,paste0("b",l))
    #    l=l+1
    #
    # }
  }

  if(any(str_detect(result,"b"))){
      result=str_replace(result,"c","c'")
  }
  result
  if(!("c2" %in% result)) result[result=="c1"]="c"
  if(!("c'2" %in% result)) result[result=="c'1"]="c'"
  if(!("b2" %in% result)) result[result=="b1"]="b"
  if(!dep %in% c("M1","M2","M3")){
    if(!("a2" %in% result)) result[result=="a1"]="a"
  }
  if(!("f2" %in% result)) result[result=="f1"]="f"
  if(!("g2" %in% result)) result[result=="g1"]="g"
  if("W:X" %in% temp) {
     result[result=="c'1"]="c'4"
     result[result=="c'2"]="c'1"
     result[result=="c'4"]="c'2"
  }
  # cat("result=",result,"\n")
  result

}


#' Make Summary for Model Coefficients
#' @param fit A list of objects of class lm
#' @param labels optional list
#' @param vars optional list
#' @param moderator optional list
#' @param covar optional list
#' @param serial logical
#' @param data optional data.frame
#' @param prefix A character
#' @param constant A string vector
#' @param autoPrefix logical automatic numbering of prefix
#' @importFrom dplyr full_join
#' @importFrom purrr reduce
#' @importFrom stringr "%>%"
#' @importFrom tidyselect starts_with
#' @export
#' @return A data.frame
#' @examples
#' fit1=lm(mpg~wt,data=mtcars)
#' fit2=lm(mpg~wt*hp*am,data=mtcars)
#' fit=list(fit1,fit2)
#' labels=list(Y="mpg",X="wt",W="hp",Z="am")
#' modelsSummary(fit,labels=labels)
#' labels=list(Y="withdraw",M="affect",X="estress")
#' covar=list(name=c("ese","sex","age"),site=list(c("M","Y"),c("M","Y"),c("M","Y")))
#' modelsSummary(labels=labels,covar=covar,data=estress)
#' labels=list(X="dysfunc",M="negtone",W="negexp",Y="perform")
#' moderator=list(name="negexp",site=list(c("a","b","c")))
#' eq=tripleEquation(labels=labels,moderator=moderator,data=teams,mode=1)
#' fit=eq2fit(eq,data=teams)
#' modelsSummary(fit,labels=labels)
#' labels=list(X="cond",M="pmi",Y="reaction")
#' modelsSummary(labels=labels,data=pmi)
modelsSummary=function(fit=NULL,labels=NULL,vars=NULL,moderator=NULL,covar=NULL,serial=FALSE,data=NULL,prefix="b",constant="iy",autoPrefix=TRUE){

      # prefix="b";constant="iy";autoPrefix=TRUE

    if(!is.null(covar)){
      for(i in seq_along(covar$name)){
        labels[[paste0("C",i)]]=covar$name[i]
      }
    }
  if(length(vars)>0){
    if(is.null(vars$label)){
      if(length(vars$name)==1){
        labels[["W"]]=vars$name[[1]][1]
        labels[["Z"]]=vars$name[[1]][2]
      } else{

        for(i in seq_along(vars$name)){
          labels[[paste0("W",i)]]=vars$name[[i]][1]
          labels[[paste0("Z",i)]]=vars$name[[i]][2]
        }

      }
    } else{
      if(length(vars$name)==1){
        labels[[vars$label[[1]][1]]]=vars$name[[1]][1]
        labels[[vars$label[[1]][2]]]=vars$name[[1]][2]
      } else{

        for(i in seq_along(vars$name)){
          labels[[vars$label[[i]][1]]]=vars$name[[i]][1]
          labels[[vars$label[[i]][2]]]=vars$name[[i]][2]
        }

      }
    }
  }

  if(length(moderator)>0){
    if(is.null(moderator$label)){
      prefix=ifelse(length(vars)==0,"W","V")
      if(length(moderator$name)==1){
        labels[[prefix]]=moderator$name
      } else{
        for(i in 1:length(moderator$name)){
          labels[[paste0(prefix,i)]]=moderator$name[i]
        }
      }
    } else{
      for(i in 1:length(moderator$label)){
        labels[[moderator$label[i]]]=moderator$name[i]
      }
    }

  }

    if(is.null(fit)){
        #eq=tripleEquation(labels=labels,vars=vars,moderator=moderator,covar=covar,data=data,mode=1)
        eq=multipleMediation(labels=labels,vars=vars,moderator=moderator,covar=covar,serial=serial,data=data,mode=1)
        fit=eq2fit(eq,data=data)
    }
    if("lm" %in%  class(fit)) fit=list(fit)
    count=length(fit)

    df<-coef<-list()
    modelNames=c()
    if(length(constant)==1) constant=rep(constant,count)
    if(length(prefix)==1) prefix=rep(prefix,count)

    for(i in 1 :count){

        df[[i]]=data.frame(summary(fit[[i]])$coef)
        colnames(df[[i]])=paste0(c("coef","se","t","p"),i)
        df[[i]][["name1"]]=rownames(df[[i]])
        if(autoPrefix &(!is.null(labels))) {
          temp=names(fit[[i]]$model)[1]
          temp1=changeLabelName(temp,labels,add=FALSE)

          if(temp1=="M") {
             constant[i]="im"
             prefix[i]="a"
          } else if(temp1=="Y"){
            constant[i]="iy"
            prefix[i]="c"
            temp2=changeLabelName(rownames(df[[i]]),labels,add=FALSE)

            if("M" %in% temp2){
               prefix[i]="c'"
            }
          }
        }
        df

        if(autoPrefix &(!is.null(labels))) {
          df[[i]][[paste0("label",i)]]=makeCoefLabel(rownames(df[[i]]),dep=names(fit[[i]]$model)[1],labels,constant[i],prefix[i])
        } else{
        df[[i]][[paste0("label",i)]]=c(constant[i],paste0(prefix[i],1:(nrow(df[[i]])-1)))
        }

        colnames(df[[i]])[5]="name1"
        coef[[i]]=getInfo(fit[[i]])[1:5]
        df[[i]]<-df[[i]] %>% select(starts_with("label"),everything())
        modelNames=c(modelNames,names(fit[[i]]$model)[1])
    }
    if(!is.null(labels)) modelNames=changeLabelName(modelNames,labels,add=TRUE)
    df
    if(count==1){
        mydf=df[[1]]
    } else{
    mydf<-reduce(df,full_join,by="name1")
    }
    mydf
    mydf<-mydf %>% select("name1",everything())
    rownames(mydf)=mydf[["name1"]]
    mydf<-mydf[-1]

    mydf[]=lapply(mydf,myformat)
    mydf<-mydf[c(2:nrow(mydf),1),]
    rownames(mydf)[nrow(mydf)]="Constant"
    mydf
    for(i in 1:count){
        mydf[[5*i]]=pformat(mydf[[5*i]])
    }
    finalNames=rownames(mydf)
    df2=data.frame(coef,stringsAsFactors = FALSE)
    colnames(df2)=paste0("coef",1:ncol(df2))
    finalNames=c(finalNames,c("Observations","R2","Adjusted R2","Residual SE","F statistic"))

    res=full_join(mydf,df2,by=paste0("coef",1:count))
    res[is.na(res)]=""
    res
    if(!is.null(labels)) finalNames=changeLabelName(finalNames,labels,add=TRUE)
    rownames(res)=finalNames
    res
    class(res)=c("modelSummary","data.frame")
    attr(res,"modelNames")=modelNames
    res
}


#'S3 method print for object modelSummary
#'@param x Object of class modelSummary
#'@param ... additional arguments to pass to print.modelSummary
#'@importFrom stringr str_pad
#'@export
print.modelSummary=function(x,...){
    count=ncol(x)/5
    colwidth=37
    left=25
    total=left+colwidth*count+1
    right=colwidth*count+1

    cat(paste(rep("=",total),collapse = ""),"\n")
    cat(paste0(centerPrint("",left),centerPrint("Consequent",right)),"\n")
    cat(paste0(centerPrint("",left),paste(rep("-",right),collapse = "")),"\n")
    names=attr(x,"modelNames")
    cat(paste0(centerPrint("",left)))
    for(i in 1:count){cat(centerPrint(names[i],colwidth))}
    cat("\n")
    cat(paste0(centerPrint("",left)))
    for(i in 1:count) cat(paste0(rep("-",colwidth),collapse = "")," ")
    cat("\n")
    cat(paste0(centerPrint("Antecedent",left)))
    for(i in 1:count) cat(paste0(centerPrint("",5),centerPrint("Coef",8),centerPrint("SE",8),centerPrint("t",8),centerPrint("p",8)," "))
    cat("\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in 1:(nrow(x)-5)){
        cat(centerPrint(rownames(x)[i],left))
        for(j in 1:count){
            for(k in 1:5){
                if(k==1){
                    cat(str_pad(x[i,(j-1)*5+k],3,"left")," ")
                } else{
                    cat(str_pad(x[i,(j-1)*5+k],6,"left")," ")
                }
                cat(ifelse((j==count)&(k==5),"\n",""))
            }
        }
    }
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in (nrow(x)-4):nrow(x)){
        cat(centerPrint(rownames(x)[i],left))
        for(j in 1:count){
            cat(centerPrint(x[i,(j-1)*5+2],colwidth))
            cat(ifelse(j==count,"\n",""))
        }

    }
    cat(paste(rep("=",total),collapse = ""),"\n")
}


#' Print a string in center
#' @param string A string
#' @param width A numeric
#' @export
centerPrint=function(string,width){
    str_pad(string,width,side="both")
}


#'Make number subscript
#'@param ft An object of class flextable
#'@param label string vector
#'@importFrom officer fp_text
#'@importFrom flextable display
#'@importFrom stringr str_extract
numberSubscript=function(ft,label){
  for(i in seq_along(label)){
    temp=paste0("display(ft, col_key = '",label[i],
                "', pattern = '{{A}}{{a}}',
                  formatters = list(A ~ stringr::str_extract(",label[i],",'[^0-9yYmM]*'),
                                    a~ stringr::str_extract(",label[i],",'[0-9yYmM].*')),
                  fprops = list(A=fp_text(italic=TRUE),a=fp_text(vertical.align='subscript',italic=TRUE)),
                  part='body')")
    ft<-eval(parse(text=temp))
  }
  ft
}

#' Make Summary Table for Model Coefficients
#' @param x An object of class modelSummary
#' @param vanilla A logical
#' @param ... further arguments to be passed to modelsSummary()
#' @importFrom officer fp_border
#' @importFrom flextable flextable merge_h_range align hline_top hline add_header
#' @importFrom flextable bold fontsize width italic set_header_labels add_header_row
#' @importFrom flextable theme_zebra vline_left
#' @importFrom stats pf
#' @importFrom dplyr select
#' @importFrom tidyselect everything
#' @importFrom stats setNames
#' @export
#' @return A flextable
#' @examples
#' \donttest{
#' fit1=lm(mpg~wt,data=mtcars)
#' fit2=lm(mpg~wt*hp,data=mtcars)
#' fit3=lm(mpg~wt*hp*am,data=mtcars)
#' labels=list(X="wt",W="hp",Y="mpg",Z="am")
#' x=modelsSummary(fit1,labels=labels)
#' modelsSummaryTable(x)
#' modelsSummary(list(fit1,fit2),labels=labels)
#' modelsSummaryTable(list(fit1,fit2),labels=labels,vanilla=FALSE)
#' x=modelsSummary(list(fit1,fit2,fit3),labels=labels)
#' modelsSummaryTable(x)
#' modelsSummaryTable(labels=labels,data=pmi)
#'}
modelsSummaryTable=function(x=NULL,vanilla=TRUE,...){

      # vanilla=TRUE
      # require(tidyverse)
      # require(flextable)
      # require(officer)
      # x=modelsSummary(list(fit1,fit2))
    if(is.null(x)) {
       x=modelsSummary(...)
    }
    if(!("modelSummary" %in% class(x))) {
        x=modelsSummary(x,...)
    }
    x[x=="im"]="iM"
    x[x=="iy"]="iY"
    modelNames=attr(x,"modelNames")
    modelNames

    result=x
    count=ncol(x)/5
    count
    result[["name1"]]=rownames(result)
    if(vanilla){
        if(count>1){
            for(i in 2:count) result[[paste0("s",(i-1))]]=""
        }
    }
    result<-result %>% select("name1",everything())
    rowcount=nrow(result)

    if(vanilla) {
    col_keys=c("name1",names(result)[2:6])
    if(count>1){
        for(i in 1:(count-1)) {
        col_keys=c(col_keys,paste0("s",i),names(result)[(i*5+2):(i*5+6)])
        }
    }
    } else{
        col_keys=names(result)
    }

    ft<-flextable(result,col_keys=col_keys)
    ft
    hlabel=c("Antecedent","","Coef","SE","t","p")
    if(count>1){
        for(i in 2:count){
             if(vanilla) { hlabel=c(hlabel,"","","Coef","SE","t","p") }
             else { hlabel=c(hlabel,"","Coef","SE","t","p") }
        }
    }
    hlabel<-setNames(hlabel,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    ft<-ft %>% set_header_labels(values=hlabel)

    colcount=5+ifelse(vanilla,1,0)
    ft
    for(i in 1:count){
        ft<- ft %>% merge_h_range(i=(rowcount-4):rowcount,
                                 j1=colcount*(i-1)+3,j2=colcount*(i-1)+6)
    }
    ft<- ft %>% align(align="center",part="all") %>%
         hline_top(part="header",border=fp_border(color="white",width=0))
    ft
    for(i in 1:count){
       ft <- ft %>% hline_top(j=((i-1)*colcount+2):(i*colcount),
                              part="header",border=fp_border(color="black",width=1))
    }
    big_border=fp_border(color="black",width=2)

    hlabel=c("","",modelNames[1],rep("",3))
    if(count>1){
    for(i in 2:count){
        if(vanilla) {hlabel=c(hlabel,"","",modelNames[i],rep("",3))}
        else {hlabel=c(hlabel,"",modelNames[i],rep("",3))}
    }
    }
    hlabel<-setNames(hlabel,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    length(hlabel)
    length(col_keys)
    count
    colcount
    ft <- add_header_row(ft,values=hlabel,top=TRUE,
                         colwidths=rep(1,count*colcount+ifelse(vanilla,0,1)))
    ft <- ft %>%
        hline_top(j=2:(count*colcount++ifelse(vanilla,0,1)),part="header",border=fp_border(color="black",width=1))
    ft
    for(i in 1:count){
        ft<-ft %>% hline(i=1,j=((i-1)*colcount+3):((i-1)*colcount+6),
                         part="header",border=fp_border(color="black",width=1))
    }
    for(i in 1:count){
        ft <- ft %>% merge_h_range (i=1,j1=(i-1)*colcount+3,j2=(i-1)*colcount+6,
                                    part="header")
    }
    ft
    hlabel=list(name1="",coef1="Consequent")
    ft<-ft %>%
        add_header_row(top=TRUE,values=hlabel,
                       colwidths=c(1,count*colcount+ifelse(vanilla,0,1)-1)) %>%
        hline_top(part="header",border=big_border) %>%
        hline(i=1,j=2:(count*colcount+ifelse(vanilla,0,1)),part="header",
              border=fp_border(color="black",width=1)) %>%
        merge_h_range(i=1,j1=2,j2=count*colcount+ifelse(vanilla,0,1),part="header") %>%
        align(align="center",part="header") %>%
        align(align="right",part="body") %>%
        bold(part="header") %>%
        align(i=(nrow(x)-4):nrow(x),align="center",part="body") %>%
        fontsize(part="all",size=12) %>%
        hline(i=rowcount-5,border=fp_border(color="gray"),part="body")
    ft

    if(count>1){
        if(vanilla) {
        for(i in 1:(count-1)){
            ft<-ft %>% width(j=i*6+1,width=0.001)
        }
        for(i in 1:count){
            ft<-ft %>%  width(j=(i-1)*6+2,width=0.4)
        }
        } else{
        for(i in 1:count){
            ft<-ft %>% width(j=(i-1)*5+2,width=0.4)
        }
        }
        for(i in 1:(count)){
            ft<-ft %>% italic(i=3,j=c(((i-1)*colcount+3):((i-1)*colcount+5)),
                              italic=TRUE,part="header")
        }
    } else{
        ft<-ft %>% width(j=2,width=0.4)
    }
    ft
    if(!vanilla){
     ft <-ft %>%
            theme_zebra(even_body="#EFEFEF",odd_body="transparent",
                        even_header ="#5B7778",odd_header="#5B7778") %>%
            fontsize(size=12,part="all") %>%
            align(align="center",part="header") %>%
            align(j=1,align="center",part="body") %>%
            align(j=2:(1+colcount*count),align="right",part="body") %>%
            align(i=(nrow(x)-4):nrow(x),align="center",part="body") %>%
            color(color="white",part="header") %>%
            hline(j=2:(1+colcount*count),border=fp_border(color="black",width=1),
                   part="header") %>%
            vline(j=1:(1+colcount*count),border=fp_border(color="black",width=1),
                  part="header") %>%
            vline_left(border=fp_border(color="black",width=1),
                       part="header") %>%
            hline(j=1:(1+colcount*count),border=fp_border(color="#EDBD3E",width=1),
                  part="body") %>%
            vline(j=1:(1+colcount*count),border=fp_border(color="#EDBD3E",width=1),
                  part="body") %>%
            vline_left(border=fp_border(color="#EDBD3E",width=1),
                       part="body")


    }
    ft
    count=ncol(x)/5
    label=paste0("label",1:count)
    ft<-ft %>% numberSubscript(label=label) %>%
      align(align="center",j=label,part="body")
    ft

}

#'Format a numeric vector
#'@param x A numeric vector
#'@param digits integer indicating the number of decimal places
#'@export
myformat=function(x,digits=3){
    if(is.numeric(x)){
    fmt=paste0("%.0",digits,"f")
    x=sprintf(fmt,x)
    }
    x[x=="NA"]<-""
    x
}

#' Make p value format
#'@param x A numeric vector
#'@export
pformat=function(x){
    temp=substr(x,2,nchar(x))
    temp[temp==".000"]="<.001"
    temp
}

#'Get information of a model
#' @param fit object of class lm
#' @param digits integer indicating the number of decimal places
#' @export
#' @examples
#' fit=lm(mpg~wt*hp,data=mtcars)
#' getInfo(fit)
getInfo=function(fit,digits=3){
    fmt=paste0("%.0",digits,"f")
    r1=nrow(fit$model)
    x<-summary(fit)
    r2=sprintf(fmt,x$r.squared)
    r3=sprintf(fmt,x$adj.r.squared)
    r4=paste0(sprintf(fmt,x$sigma)," ( df = ",round(x$df[2]),")")
    f=paste0("F(",round(x$fstatistic[2]),",",round(x$fstatistic[3]),") = ",
             sprintf(fmt,x$fstatistic[1]))
    p=sprintf(paste0("%0.",digits,"f"),pf(x$fstatistic[1L],
                     x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE))
    p
    p=substr(p,2,digits+2)
    if(p==".000") p="< .001"
    else p=paste0("= ",p)
    f=paste0(f,", p ",p)
    MSE=sprintf(paste0("%0.",digits,"f"),sum(fit$residuals^2)/(r1-x$fstatistic[2]-1))
    result=c(r1,r2,r3,r4,f,MSE)
    result
}

