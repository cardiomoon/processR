#' Make Summary for Model Coefficients
#' @param fit A list of objects of class lm
#' @param labels optional list
#' @param prefix A character
#' @param constant A string vector
#' @param fitlabels Optional. labels of models
#' @param autoPrefix logical
#' @importFrom dplyr full_join
#' @importFrom purrr reduce
#' @importFrom stringr "%>%"
#' @export
#' @return A data.frame
#' @examples
#' fit1=lm(mpg~wt,data=mtcars)
#' fit2=lm(mpg~wt*hp*vs+am,data=mtcars)
#' labels=list(Y="mpg",X="wt",W="hp",Z="vs")
#' fit=list(fit1,fit2)
#' modelsSummary2(fit,labels=labels)
#' modelsSummary2(fit,labels=labels,prefix=c("c","b"),autoPrefix=FALSE)
#' modelsSummary2(fit1)
modelsSummary2=function(fit,labels=NULL,prefix="b",constant="iy",fitlabels=NULL,autoPrefix=TRUE){

      # labels=NULL;prefix="b";fitlabels=NULL;constant="iy";autoPrefix=TRUE
    if("lm" %in%  class(fit)) fit=list(fit)
    count=length(fit)

    df<-coef<-list()
    modelNames=c()
    rowcount=c()
    r2=c()
    mse=c()
    fstat=c()
    fitnames=c()
    if(length(constant)==1) constant=rep(constant,count)
    if(length(prefix)==1) prefix=rep(prefix,count)
    for(i in 1 :count){

        df[[i]]=data.frame(getCoef(fit[[i]]))
        colnames(df[[i]])=c("coef","se","t","p")
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
        if(autoPrefix &(!is.null(labels))) {
          df[[i]][["name"]]=makeCoefLabel(rownames(data.frame(summary(fit[[i]])$coef)),
                                      dep=names(fit[[i]]$model)[1],labels,constant[i],prefix[i])
        } else{
          df[[i]][["name"]]=c(constant[i],paste0(prefix[i],1:(nrow(df[[i]])-1)))
        }
        colnames(df[[i]])[5]="name1"
        coef[[i]]=getInfo(fit[[i]])
        modelNames=c(modelNames,names(fit[[i]]$model)[1])
        rowcount=c(rowcount,nrow(df[[i]]))
        r2=c(r2,coef[[i]][2])
        mse=c(mse,coef[[i]][6])
        fstat=c(fstat,coef[[i]][5])
        temp=paste("Model",i)
        if(!is.null(fitlabels[i])) temp=paste0(temp,": ",fitlabels[i])
        fitnames=c(fitnames,temp)
    }
    if(!is.null(labels)) modelNames=changeLabelName(modelNames,labels,add=TRUE)

    df



    mydf=reduce(df,rbind)
    mydf
    mydf<-mydf %>% select("name1","name",everything())

    mydf[]=lapply(mydf,myformat)

    mydf$name1[mydf$name1=="(Intercept)"]="Constant"
    mydf

    mydf[[6]]=pformat(mydf[[6]])

    finalNames=mydf[["name1"]]
    finalNames
    # changeLabelName(finalNames,labels,add=TRUE)
    if(!is.null(labels)) finalNames=changeLabelName(finalNames,labels,add=TRUE)
    finalNames
    mydf[["name1"]]=finalNames
    mydf
    class(mydf)=c("modelSummary2","data.frame")
    attr(mydf,"fitnames")=fitnames
    attr(mydf,"r2")=r2
    attr(mydf,"mse")=mse
    attr(mydf,"fstat")=fstat
    attr(mydf,"count")=rowcount
    mydf
}

#'Get coef summary table
#'@param fit An object of class lm
#'@examples
#'fit=lm(mpg~hp*wt+am,data=mtcars)
#'getCoef(fit)
#'@export
getCoef=function(fit){
  df=summary(fit)$coef
  rnames=rownames(df)
  vars2=rnames[str_detect(rnames,":")]
  vars1=unique(unlist(strsplit(vars2,":")))
  vars1=rnames[rnames %in% vars1]
  vars3=setdiff(rnames[-1],union(vars1,vars2))
  vars=c(rnames[1],vars1,vars2,vars3)
  df[vars,]
}

#'S3 method print for object modelSummary2
#'@param x Object of class modelSummary
#'@param ... additional arguments to pass to print.modelSummary
#'@export
print.modelSummary2=function(x,...){

    width=c(20,5,10,10,10,10)
    width[1]=max(20,max(nchar(x[["name1"]]))+2)
    total=sum(width)

    cat(paste(rep("=",total),collapse = ""),"\n")
    cat(paste0(centerPrint("",width[1]),centerPrint("",width[2]),
        centerPrint("coef",width[3]+1),centerPrint("SE",width[4]+1),
        centerPrint("t",width[5]+1),centerPrint("p",width[6]+1)),"\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    count=attr(x,"count")
    modelcount=length(attr(x,"r2"))
    no=1
    for(i in seq_along(count)){
        if(modelcount!=1){
          cat(attr(x,"fitnames")[i],"\n")
          cat("R2 =",attr(x,"r2")[i],", MSE =",attr(x,"mse")[i],"\n")
          cat(attr(x,"fstat")[i],"\n")
        }
        for(j in 1:count[i]){
            cat(paste0(rightPrint(x$name1[no],width[1]),rightPrint(x$name[no],width[2]),
                       rightPrint(x$coef[no],width[3]),rightPrint(x$se[no],width[4]),
                       rightPrint(x$t[no],width[5]),rightPrint(x$p[no],width[6])),"\n")
            no=no+1
        }
        if(i!=length(count)) cat(paste(rep("-",total),collapse = ""),"\n")
    }
    if(modelcount==1){
      # cat(attr(x,"fitnames")[i],"\n")
      cat("\n",rightPrint(paste0("R2 = ",attr(x,"r2")[i],", MSE = ",attr(x,"mse")[i]),total-1),"\n")
      cat(rightPrint(attr(x,"fstat")[i],total),"\n")
    }

    cat(paste(rep("=",total),collapse = ""),"\n")
}

#' Make Summary Table 2 for Model Coefficients
#' @param x An object of class modelSummary2
#' @param vanilla A logical
#' @param mode An Integer One of 1:2
#' @param ... further arguments to be passed to modelsSummary2()
#' @export
#' @importFrom flextable as_grouped_data as_flextable
modelsSummary2Table=function(x,vanilla=TRUE,mode=1,...){


    if(!("modelSummary2" %in% class(x))) {
        x=modelsSummary2(x,...)
    }
  x[x=="im"]="iM"
  x[x=="iy"]="iY"
    res=x
    count=attr(res,"count")
    Model=c()
    for(i in seq_along(count)){
        temp=rep(i,count[i])
        Model=c(Model,temp)
    }

    (Modelnames=str_replace(attr(res,"fitnames"),"Model",""))


    res$Model=paste0(Modelnames[Model],"\n","R2 = ",attr(res,"r2")[Model],
                     ", MSE = ",attr(res,"mse")[Model])
    if(mode==2) {
      res$Model=paste0(res$Model,"\n",attr(res,"fstat")[Model])
    }


    gcount=cumsum(count)+c(1:length(count))
    gcount=gcount[-length(gcount)]
    modelcount=length(attr(res,"r2"))

    if(modelcount>1){
    ft<- res %>% as_grouped_data(groups="Model") %>% as_flextable() %>%
        set_header_labels(name1 = "", name="", coef = "Coef", se="SE",
                          t="t",p="p" ) %>%
        italic(i=1,part="header") %>%
        align(i=1,align="center",part="header") %>%
        hline(i=gcount,border=fp_border(color="gray", width = 1))
    } else{
      ft<- res[-length(res)] %>% rrtable::df2flextable(vanilla=vanilla) %>%
        set_header_labels(name1 = "", name="", coef = "Coef", se="SE",
                          t="t",p="p" )
    }
    if(!vanilla) {
        gstart=c(1,cumsum(count)+1+1:length(count))
        gstart=gstart[-length(gstart)]

        ft<-ft %>% theme_zebra(even_body="#EFEFEF",odd_body="transparent",
                even_header ="#5B7778",odd_header="#5B7778") %>%
            color(color="white",part="header") %>%
            vline(border=fp_border(color="black",width=1),
                  part="header") %>%
            vline_left(border=fp_border(color="black",width=1),
                       part="header") %>%
            vline(border=fp_border(color="#EDBD3E",width=1),
                  part="body") %>%
            vline_left(border=fp_border(color="#EDBD3E",width=1),
                       part="body") %>%
            hline(border=fp_border(color="#EDBD3E",width=1),
                part="body") %>%
            align(i=1,align="center",part="header") %>%
            align(i=gstart,align="left",part="body")

    }
    if(modelcount==1){
      temp=paste0("R2 = ",attr(res,"r2"),", MSE =",attr(res,"mse"),
                  "\n",attr(res,"fstat"))
      ft<-ft %>%
        add_footer_lines(values=temp) %>%
        align(i=1,align="right",part="footer") %>%
        italic(i=1,part="header") %>%
        align(i=1,align="center",part="header")

    }
    ft
    ft<-ft %>% numberSubscript(label="name") %>%
      align(align="center",j="name",part="body")
    ft
}
