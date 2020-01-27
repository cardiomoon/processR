#' Summarize the moderated mediation
#' @param semfit An object of class lavaan
#' @param mod name of moderator
#' @param values Optional. Numeric vector
#' @param boot.ci.type Type of bootstrapping interval. Choices are c("norm","basic","perc",bca.simple")
#' @param add.range logical Wheter or not add range
#' @importFrom lavaan parameterEstimates
#' @export
#' @return A data.frame and an object of class modmedSummary
#' @examples
#' require(lavaan)
#' labels=list(X="frame",M="justify",Y="donate",W="skeptic")
#' moderator=list(name="skeptic",site=list(c("a","c")))
#' model=tripleEquation(labels=labels,moderator=moderator)
#' cat(model)
#' \donttest{
#' semfit=sem(model,data=disaster,se="boot",bootstrap=100)
#' modmedSummary(semfit)
#' conditionalEffectPlot(semfit,data=disaster)
#' labels=list(X="dysfunc",M="negtone",Y="perform",W="negexp")
#' moderator=list(name="negexp",site=list("b"))
#' model=tripleEquation(labels=labels,moderator=moderator,data=teams,rangemode=2)
#' cat(model)
#' semfit=sem(model,data=teams,se="boot",bootstrap=100)
#' summary(semfit)
#' modmedSummary(semfit)
#' conditionalEffectPlot(semfit,data=teams)
#' }
modmedSummary=function(semfit,mod=NULL,values=NULL,boot.ci.type="perc",add.range=TRUE){

                 # boot.ci.type="perc";mod=NULL;values=NULL;add.range=TRUE

    res=parameterEstimates(semfit,boot.ci.type = boot.ci.type,
                           level = .95, ci = TRUE,
                           standardized = FALSE)
    res=res[res$label!="",]
    res

    if(is.null(mod)){
        mod=res$lhs[str_detect(res$label,"mean")][1]
        mod
    }
    if(is.null(values)){
      # values1=res$est[res$label==paste0(mod,".mean")]+c(0,-1,1)*sqrt(res$est[res$label==paste0(mod,".var")])
      values1=extractRange(res,mod=mod)
      values1=unique(values1)
      values1
    } else{
        values1=values
    }
    values1
    # select=c("indirect","indirect.below","indirect.above")
    selected=which(str_detect(res$lhs,"indirect"))
    selected
    if(length(values1)==2){
       selected3=selected[str_detect(res$lhs[selected],"above|below")]
    } else{
       selected3=selected
    }
    selected3
    indirect=res$est[selected3]
    lower=res$ci.lower[selected3]
    upper=res$ci.upper[selected3]
    indirectp=res$pvalue[selected3]

    selected1=which(str_detect(res$lhs,"direct"))
    selected2=setdiff(selected1,selected)
    selected2
    if(length(values1)==2){
      selected4=selected2[str_detect(res$lhs[selected2],"above|below")]
    } else{
      selected4=selected2
    }
    selected4
    direct=res$est[selected4]
    lowerd=res$ci.lower[selected4]
    upperd=res$ci.upper[selected4]
    #
    # se=res$se[selected]
    directp=res$p[selected4]

    indirect
    lower
    direct
    values=as.numeric(values1)
    if(length(values)==2){
        values=rep(values,length(indirect)/length(values))
    }
    df=data.frame(values=as.numeric(values1),indirect,lower,upper,indirectp,direct,lowerd,upperd,directp)
    df
    nrow(df)

    if(nrow(df)%%3==0){
    select=c(2,1,3)
    count=nrow(df)/3
    select2=c()
    for(i in 1:count) {
       select2=c(select2,select+(i-1)*3)
    }
    select2
    df=df[select2,]
    }
    df[]=round(df,3)
    attr(df,"mod")=mod
    df


    if(is.null(values1)) {
        selected=which(str_detect(res$lhs,"indirect"))
        selected1=selected[!str_detect(res$lhs[selected],"\\.a") & !str_detect(res$lhs[selected],"\\.b")]
        indirect=res$rhs[selected1]
        indirect=str_replace_all(indirect,paste0(mod,".mean"),"W")
        selected1=which(str_detect(res$lhs,"direct"))
        selected2=setdiff(selected1,selected)
        selected3=selected2[!str_detect(res$lhs[selected2],"\\.a") & !str_detect(res$lhs[selected2],"\\.b")]
        selected3
        direct=res$rhs[selected3]
        direct=str_replace_all(direct,paste0(mod,".mean"),"W")
    } else{
      selected=which(str_detect(res$lhs,"indirect"))
      selected1=selected[!str_detect(res$lhs[selected],"\\.a") & !str_detect(res$lhs[selected],"\\.b")]
      indirect=res$rhs[selected1]
      indirect
      if(str_detect(indirect,paste0(mod,".mean"))) {
        indirect=str_replace_all(indirect,paste0(mod,".mean"),"W")
      } else{
        temp=as.character(values1[1])
        indirect=str_replace_all(indirect,temp,"W")
      }
        selected1=which(str_detect(res$lhs,"direct"))
        selected2=setdiff(selected1,selected)
        selected3=selected2[!str_detect(res$lhs[selected2],"\\.a") & !str_detect(res$lhs[selected2],"\\.b")]
        selected3
        direct=res$rhs[selected3]
        direct
        if(str_detect(direct,paste0(mod,".mean"))) {
          direct=str_replace_all(direct,paste0(mod,".mean"),"W")
        } else{
          temp=as.character(values1[1])
          direct=str_replace_all(direct,temp,"W")
        }

    }

    if(nrow(df)%%3==0){
    if(count>1){
        df$label=paste0("D",rep(1:count,each=3))
        if(add.range==FALSE){
            values=df$values[1:3]
            df1=data.frame(W=values)
            for(i in 1:count){
                temp=df$indirect[((i-1)*3+1):((i-1)*3+3)]
                df1=cbind(df1,temp)
            }
            for(i in 1:count){
              temp=df$direct[((i-1)*3+1):((i-1)*3+3)]
              df1=cbind(df1,temp)
            }
            colnames(df1)=c("W",paste0("indirectD",1:count),paste0("directD",1:count))
            df=df1
        }
    }
    } else{
       count=nrow(df)/length(values1)
       indirect=indirect[1:count]
       direct=direct[1:count]
       df$label=rep(paste0("M",1:count),each=length(values1))

    }

    attr(df,"indirect")=indirect
    attr(df,"direct")=direct
    attr(df,"boot.ci.type")=boot.ci.type
    if((count>1) &(add.range==FALSE)){
      class(df)=c("modmedSummary2","data.frame")
    } else{
       class(df)=c("modmedSummary","data.frame")
    }
    df
}

#'Extract range from a data.frame
#'@param res A data.frame
#'@param mod Name of moderator
#'@param what string
extractRange=function(res,mod,what="indirect"){

  select=str_detect(res$lhs,what)
  res1=res[select,c(1,3)]
  temp=res1$rhs
  temp
  result=extractNumber(temp)
  result
  if(length(result)>0) {
    values=result
  } else{
    values=res$est[res$label==paste0(mod,".mean")]+c(0,-1,1)*sqrt(res$est[res$label==paste0(mod,".var")])
  }
  values=unique(values)
  values
}


#' extract number from string
#' @param x a string
#' @export
extractNumber=function(x){
  result=c()
  for(i in seq_along(x)){
      temp=x[i]
      # temp=str_replace_all(temp,"\\(|\\)","")
      # temp=unlist(str_split(temp,"\\+|\\*|-"))
      # temp
      # select=which(str_detect(temp,"^[0-9|\\.].*"))
      #
      temp=str_replace_all(temp,"\\(|\\)","")
      temp=unlist(str_split(temp,"\\+|\\*"))
      select=which(str_detect(temp,"^[0-9|\\.|-].*"))
      if(length(select)>0) {
          result=c(result,temp[select])
      }
  }
  result
}

#' Print a string in right alignment
#' @param string A string
#' @param width A numeric
#' @export
rightPrint=function(string,width){
    str_pad(string,width,side="left")
}


#'S3 method print for an object of class modmedSummary
#'@param x An object of class modmedSummary
#'@param ... additional arguments to pass to print.modmedSummary
#'@export
print.modmedSummary=function(x,...){
    count=nrow(x)
    x[]=lapply(x,myformat)

    x[[5]]=pformat(x[[5]])
    x[[9]]=pformat(x[[9]])

    mod=paste0(attr(x,"mod"),"(W)")
    indirect=paste0(attr(x,"indirect"),collapse="/")
    direct=paste0(attr(x,"direct"),collapse="/")
    left=max(nchar(mod)+2,8)
    addlabel=FALSE
    if(nrow(x)>3) addlabel=TRUE
    total=73+left+ifelse(addlabel,7,0)

    cat("\nInference for the Conditional Direct and Indirect Effects - boot.ci.type:",attr(x,"boot.ci.type"),"\n")
    cat(paste(rep("=",total),collapse = ""),"\n")
    cat(centerPrint("",left),ifelse(addlabel,"     ",""),
        centerPrint("Indirect Effect",35),centerPrint("Direct Effect",35),"\n")
    cat(centerPrint("",left),ifelse(addlabel,"     ",""),
        centerPrint(indirect,35),centerPrint(direct,35),"\n")
    cat(centerPrint("",left),ifelse(addlabel,"     ",""),
        paste(rep("-",35),collapse = ""),paste(rep("-",36),collapse = ""),"\n")

    templabel="  X  "
    if(sum(str_detect(x$label,"M1"))>0) templabel="  M  "
    cat(centerPrint(mod,left),ifelse(addlabel,templabel,""),centerPrint("estimate",8),centerPrint("95% Bootstrap CI",18),centerPrint("p",8))
    cat(centerPrint("estimate",11),centerPrint("95% Bootstrap CI",18),centerPrint("p",8),"\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in 1:count){
        cat(rightPrint(x[i,1],left-1),"")
        if(addlabel) cat(rightPrint(x[i,"label"],6))
        cat(rightPrint(x[i,2],8))
        cat(paste0(rightPrint(x[i,3],8)," to ",rightPrint(x[i,4],6)))
        cat(rightPrint(x[i,5],8))
        cat(rightPrint(x[i,6],13))
        cat(paste0(rightPrint(x[i,7],8)," to ",rightPrint(x[i,8],6)))
        cat(rightPrint(x[i,9],8),"\n")
    }
    cat(paste(rep("=",total),collapse = ""),"\n")
}


#' Make a table summarizing the moderated mediation
#' @param x An object of class modmedSummary or class lavaan
#' @param vanilla A logical
#' @param showP logical
#' @param ... Further arguments to be passed to modmedSummary
#' @importFrom flextable bg vline add_footer_lines
#' @export
modmedSummaryTable=function(x,vanilla=TRUE,showP=FALSE,...){

    if("lavaan" %in% class(x)){
       x=modmedSummary(x,...)
    }
    count=nrow(x)
    addlabel=FALSE
    if(count>3) {
      addlabel=TRUE
      mode=1
      if(sum(str_detect(x$label,"M1"))>0) mode<-2
    }
    x[]=lapply(x,myformat)
    x[[5]]=pformat(x[[5]])
    x[[9]]=pformat(x[[9]])

    x1=x

    if(vanilla){
    x1$s=""
    x1$ci=paste0("(",x1$lower," to ",x1$upper,")")
    x1$ci2=paste0("(",x1$lowerd," to ",x1$upperd,")")

    if(addlabel){
        x1=x1[c(1,10,2,12,5,11,6,13,9)]
    } else{
        x1=x1[c(1:2,11,5,10,6,12,9)]
    }
    class(x1)="data.frame"
    x1
    if(!showP) {
        if(addlabel) x1<-x1[-c(5,9)]
        else x1<-x1[-c(4,8)]
    }
    ft=rrtable::df2flextable(x1,vanilla=TRUE,digits=3)
    ft
    if(showP){
    if(addlabel){
      temp=ifelse(mode==1,"X","M")
      hlabel=c(paste0(attr(x,"mod"),"(W)"),temp,"estimate","95% Bootstrap CI","p","","estimate","95% Bootstrap CI","p")
    } else{
    hlabel=c(paste0(attr(x,"mod"),"(W)"),"estimate","95% Bootstrap CI","p","","estimate","95% Bootstrap CI","p")
    }
    } else{
      if(addlabel){
        temp=ifelse(mode==1,"X","M")
        hlabel=c(paste0(attr(x,"mod"),"(W)"),temp,"estimate","95% Bootstrap CI","","estimate","95% Bootstrap CI")
      } else{
        hlabel=c(paste0(attr(x,"mod"),"(W)"),"estimate","95% Bootstrap CI","","estimate","95% Bootstrap CI")
      }
    }
    hlabel
    col_keys=colnames(x1)
    hlabel<-setNames(hlabel,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    ft<-ft %>% set_header_labels(values=hlabel)
    ft
    if(showP){
    if(addlabel){
      ft<-ft %>% width(j=c(4,8),width=1.6) %>% width(j=6,width=0.1)
    } else{
      ft<-ft %>% width(j=c(3,7),width=1.6) %>% width(j=5,width=0.1)
    }
    } else{
      if(addlabel){
        ft<-ft %>% width(j=c(4,7),width=1.6) %>% width(j=5,width=0.1)
      } else{
        ft<-ft %>% width(j=c(3,6),width=1.6) %>% width(j=4,width=0.1)
      }
    }
    big_border=fp_border(color="black",width=2)
    noP=ifelse(showP,0,1)
    if(addlabel){
      temp=ifelse(mode==1,"D","M")
      indirect= paste0(temp,1:length(attr(x,"indirect")),":",attr(x,"indirect"))
      indirect=paste0(indirect,collapse="\n")
      direct= paste0(temp,1:length(attr(x,"direct")),":",attr(x,"direct"))
      direct=paste0(direct,collapse="\n")
      hlabel=c("","",paste0("Indirect Effect\n",indirect),"",
                  paste0("Direct Effect\n",direct))
      hlabel
      ft
      ft<- ft %>%
        hline_top(part="header",border=fp_border(color="black",width=0)) %>%
        add_header_row(top=TRUE,values=hlabel,colwidths=c(1,1,3-noP,1,3-noP)) %>%
        hline_top(part="header",border=big_border) %>%
        hline(i=1,j=3:(5-noP),part="header",border=fp_border(color="black",width=1))%>%
        hline(i=1,j=(7-noP):(9-noP*2),part="header",border=fp_border(color="black",width=1)) %>%
        merge_h_range (i=1,j1=3,j2=5-noP,part="header") %>%
        merge_h_range (i=1,j1=(7-noP),j2=(9-noP*2),part="header") %>%
        align(align="center",part="all") %>%
        align(align="right",part="body") %>%
        align(j=2,align="right",part="body") %>%
        fontsize(size=12,part="header") %>%
        bold(part="header") %>% width(j=1,width=1)
        if(showP) ft<-ft %>%italic(i=2,j=c(5,9),italic=TRUE,part="header")

    } else{

      hlabel=list(values="",
                indirect=paste0("Indirect Effect\n",attr(x,"indirect")),s="",
                direct=paste0("Direct Effect\n",attr(x,"direct")))
      ft<-ft %>%
        hline_top(part="header",border=fp_border(color="black",width=0)) %>%
        add_header_row(top=TRUE,values=hlabel,colwidths=c(1,3-noP,1,3-noP)) %>%
        hline_top(part="header",border=big_border) %>%
        hline(i=1,j=2:(4-noP),part="header",border=fp_border(color="black",width=1)) %>%
        hline(i=1,j=(6-noP):(8-noP*2),part="header",border=fp_border(color="black",width=1)) %>%
        merge_h_range (i=1,j1=2,j2=(4-noP),part="header") %>%
        merge_h_range (i=1,j1=(6-noP),j2=(8-noP*2),part="header") %>%
        align(align="center",part="all") %>%
        align(align="right",part="body") %>%
        fontsize(size=12,part="header") %>%
        bold(part="header") %>%
        width(j=1,width=1)
        if(showP) ft<-ft %>% italic(i=2,j=c(4,8),italic=TRUE,part="header")
      ft

    }

    } else{
        x1$ci=paste0("(",x1$lower," to ",x1$upper,")")
        x1$ci2=paste0("(",x1$lowerd," to ",x1$upperd,")")

        if(addlabel){
          x1=x1[c(1,10,2,11,5,6,12,9)]
        } else{
          x1=x1[c(1,2,10,5,6,11,9)]
        }

        ft=rrtable::df2flextable(x1,vanilla=FALSE,digits=3)

        ft
        if(addlabel){
          temp=ifelse(mode==1,"X","M")
          hlabel=c(paste0(attr(x,"mod"),"(W)"),temp,"estimate","95% Bootstrap CI","p","estimate","95% Bootstrap CI","p")
        } else{
        hlabel=c(paste0(attr(x,"mod"),"(W)"),"estimate","95% Bootstrap CI","p","estimate","95% Bootstrap CI","p")
        }
        hlabel
        col_keys=colnames(x1)
        hlabel<-setNames(hlabel,col_keys)
        hlabel=as.list(hlabel)
        hlabel
        ft<-ft %>% set_header_labels(values=hlabel)
        ft
        if(addlabel){
          ft<-ft %>% width(j=c(4,7),width=1.6) %>% width(j=6,width=0.1)
        } else{
        ft<-ft %>% width(j=c(3,6),width=1.6) %>% width(j=5,width=0.1)
        }
        big_border=fp_border(color="black",width=2)

        if(addlabel){
          indirect= paste0("D",1:length(attr(x,"indirect")),":",attr(x,"indirect"))
          indirect=paste0(indirect,collapse="/")
          direct= paste0("D",1:length(attr(x,"direct")),":",attr(x,"direct"))
          direct=paste0(direct,collapse="/")
          temp=ifelse(mode==1,"X","M")
          hlabel=c(paste0(attr(x,"mod"),"(W)"),temp,paste0("Indirect Effect\n",indirect),
                   paste0("Direct Effect\n",direct))
          hlabel

          ft<- ft %>%
            hline_top(part="header",border=fp_border(color="black",width=0)) %>%
            add_header_row(top=TRUE,values=hlabel,colwidths=c(1,1,3,3)) %>%
            hline_top(part="header",border=big_border) %>%
            hline(i=1,j=3:5,part="header",border=fp_border(color="black",width=1))%>%
            hline(i=1,j=6:8,part="header",border=fp_border(color="black",width=1))
          ft<-ft  %>%
            merge_h_range (i=1,j1=3,j2=5,part="header") %>%
            merge_h_range (i=1,j1=6,j2=8,part="header") %>%
            align(align="center",part="all") %>%
            align(align="right",part="body") %>%
            fontsize(size=12,part="header") %>%
            bold(part="header") %>%
            italic(i=2,j=c(5,8),italic=TRUE,part="header")

          ft<-ft %>% color(i=1,j=1:8,color="white",part="header") %>%
            bg(i=1,j=1:8,bg="#5B7778",part="header") %>%
            merge_at(i=1:2,j=1,part="header") %>%
            merge_at(i=1:2,j=2,part="header")
          ft<-ft %>% vline(i=1:2,border=fp_border(color="white"),part="header") %>%
            hline(i=1:2,border=fp_border(color="white"),part="header") %>%
            width(j=1,width=1)

        } else{
        hlabel=list(values=paste0(attr(x,"mod"),"(W)"),
                    indirect=paste0("Indirect Effect\n",attr(x,"indirect")),
                    direct=paste0("Direct Effect\n",attr(x,"direct")))
        ft<- ft %>%
            hline_top(part="header",border=fp_border(color="black",width=0)) %>%
            add_header_row(top=TRUE,values=hlabel,colwidths=c(1,3,3)) %>%
            hline_top(part="header",border=big_border) %>%
            hline(i=1,j=2:4,part="header",border=fp_border(color="black",width=1))%>%
            hline(i=1,j=5:7,part="header",border=fp_border(color="black",width=1))
        ft<-ft  %>%
            merge_h_range (i=1,j1=2,j2=4,part="header") %>%
            merge_h_range (i=1,j1=5,j2=7,part="header") %>%
            align(align="center",part="all") %>%
            align(align="right",part="body") %>%
            fontsize(size=12,part="header") %>%
            bold(part="header") %>%
            italic(i=2,j=c(4,7),italic=TRUE,part="header")
        ft<-ft %>% color(i=1,j=1:7,color="white",part="header") %>%
            bg(i=1,j=1:7,bg="#5B7778",part="header") %>%
            merge_at(i=1:2,j=1,part="header")
        ft<-ft %>% vline(i=1:2,border=fp_border(color="white"),part="header") %>%
            hline(i=1:2,border=fp_border(color="white"),part="header") %>%
            width(j=1,width=1)
        }
    }

    ft<-ft %>% add_footer_lines(paste0("boot.ci.type = ",attr(x,"boot.ci.type") )) %>%
        align(align="right",part="footer")
    ft
}

#' Summarize the mediation effects
#' @param semfit An object of class lavaan
#' @param boot.ci.type Type of bootstrapping interval. Choices are c("norm","basic","perc",bca.simple","all")
#' @param effects Names of effects to be summarized
#' @importFrom purrr map_df
#' @export
#' @return A data.frame and an object of class medSummary
#' @examples
#' library(lavaan)
#' labels=list(X="cond",M="pmi",Y="reaction")
#' model=tripleEquation(labels=labels)
#' \donttest{
#' semfit=sem(model=model,data=pmi, se="boot", bootstrap=100)
#' medSummary(semfit)
#' medSummary(semfit,boot.ci.type="all")
#' }
medSummary=function(semfit,boot.ci.type="perc",effects=c("indirect","direct")){
  if(boot.ci.type!="all"){
    res=parameterEstimates(semfit,boot.ci.type = boot.ci.type,
                           level = .95, ci = TRUE,
                           standardized = FALSE)
    res
    select=which(str_detect(res$label,"CE|direct|total|prop|ind|Contrast"))
    res=res[select,c(1,3,5,9,10,8)]
    names(res)[1:2]=c("Effect","equation")
    attr(res,"boot.ci.type")=boot.ci.type
    attr(res,"se")=semfit@Options$se
    class(res)=c("medSummary","data.frame")
    res
  } else{
        # effects=c("indirect","direct","total")
    count=length(effects)
    type=c("norm","basic","perc","bca.simple")
    result=list()
    for(i in 1:4){
      res=parameterEstimates(semfit,boot.ci.type = type[i],
                             level = .95, ci = TRUE,
                             standardized = FALSE)

      res=res[res$label %in% effects,c(1,3,5,9,10,8)]
      res$type=type[i]
      result[[i]]=res
    }
    df1=purrr::map_df(result,rbind)
    df1
    equation=df1$rhs[1:count]
    equation
    df=list()
    for(i in seq_along(effects)){
        df[[i]]=df1[df1$lhs==effects[i],]
    }
    df
    df=lapply(1:count,function(i){
        df[[i]][3:6]})
    df
    df2=purrr::reduce(df,cbind)
    df2
    temp=c("est","lower","upper","p")
    colnames(df2)=paste0(rep(temp,count),".",rep(effects,each=4))
    df2$type=type
    df2 <- df2 %>% select(type,everything())
    attr(df2,"effects")<-effects
    attr(df2,"equations")<-equation
    attr(df2,"se")=semfit@Options$se
    class(df2)=c("medSummary2","data.frame")
    #str(df2)
    df2
  }
}

#'S3 method print for an object of class medSummary
#'@param x An object of class medSummary
#'@param ... additional arguments to pass to print.medSummary
#'@export
print.medSummary=function(x,...){
    count=nrow(x)
    x[]=lapply(x,myformat)

    x[[6]]=pformat(x[[6]])
    tempnames=c("Effect","Equation","est","95% Bootstrap CI","p")
    if(attr(x,"se")=="standard") tempnames[4]="95% CI"

    widthEffect=max(nchar(x$Effect))+2
    widthEq=max(nchar(x$equation))+2

    width=c(widthEffect,widthEq,8,19,8)

    total=sum(width)
    cat(centerPrint("Summary of Mediation Effects",total),"\n")
    cat(paste(rep("=",total),collapse = ""),"\n")
    cat("  ")
    for(i in seq_along(tempnames)){
        cat(centerPrint(tempnames[i],width[i]))
    }
    cat("\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in 1:nrow(x)){
        cat(centerPrint(x[i,1],width[1]))
        cat(centerPrint(x[i,2],width[2]))
        cat(rightPrint(x[i,3],width[3]))
        cat(rightPrint(paste0("(",x[i,4]," to ",x[i,5],")"),width[4]))
        cat(rightPrint(x[i,6],width[5]))
        cat("\n")
    }
    cat(paste(rep("=",total),collapse = ""),"\n")
    if(attr(x,"se")!="standard") cat(rightPrint(paste0("boot.ci.type: ",attr(x,"boot.ci.type")),total))
    cat("\n")
}

#'S3 method print for an object of class medSummary2
#'@param x An object of class medSummary
#'@param ... additional arguments to pass to print.medSummary
#'@export
print.medSummary2=function(x,...){
  count=(ncol(x)-1)/4
  count
  df=x
  df[]=lapply(df,myformat)

  for(i in 1:count){
     df[[1+i*4]]=pformat(df[[1+i*4]])
  }
  for(i in 1:count){
    df[[paste0("ci",i)]]=paste0("(",df[[3+(i-1)*4]]," to ",df[[4+(i-1)*4]],")")
  }
  df
  select=c(1)
  for(i in 1:count){
    select=c(select,2+(i-1)*4,count*4+1+i,5+(i-1)*4)
  }
  df=df[select]
  df
  if(attr(x,"se")=="standard") temp=rep(c("estimate","95% CI","p"),count)
  else temp=rep(c("estimate","95% Bootstrap CI","p"),count)
  colnames(df)=c("type",temp)
  width=c(12,rep(c(8,22,8),count))
  colwidth=38
  total=sum(width)
  cat(centerPrint("Summary of Mediation Effects",total),"\n")
  cat(paste(rep("=",total),collapse = ""),"\n")
  cat(centerPrint("",12))
  for(i in 1:count){
     cat(centerPrint(attr(x,"effects")[i],colwidth))
  }
  cat("\n")
  cat(centerPrint("",11))
  for(i in 1:count){
    cat(centerPrint(attr(x,"equations")[i],colwidth))
  }
  cat("\n",centerPrint("",12))
  for(i in 1:count) cat(paste(rep("-",colwidth),collapse = ""))
  cat("\n  ")
  for(i in seq_along(colnames(df))){
    cat(centerPrint(colnames(df)[i],width[i]))
  }
  cat("\n")
  cat(paste(rep("-",total),collapse = ""),"\n")
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)) {
       cat(centerPrint(df[i,j],width[j]))
    }
    cat("\n")
  }
  cat(paste(rep("=",total),collapse = ""),"\n")
  cat("\n")
}

#' Make a table summarizing the mediation effects
#' @param x An object of class medSummary or medSummary2 or lavaan
#' @param vanilla A logical
#' @param ... Further arguments to be passed to medSummary
#' @export
medSummaryTable=function(x,vanilla=TRUE,...){
   if("lavaan" %in% class(x)){
      x=medSummary(x,...)
   }
   if("medSummary2" %in% class(x)){
     medSummaryTable2(x,vanilla=vanilla)
   } else{
     medSummaryTable1(x,vanilla=vanilla)
   }
}

#' Make a table summarizing the mediation effects
#' @param x An object of class medSummary
#' @param vanilla A logical
#' @param showP A logical
#' @importFrom flextable autofit
#' @export
medSummaryTable1=function(x,vanilla=TRUE,showP=FALSE){
   df=x
   df[]=lapply(df,myformat)
   df[[6]]=pformat(df[[6]])
   df$ci=paste0("(",df$ci.lower," to ",df$ci.upper,")")
   df<-df %>% select(c(1,2,3,7,6))
   colnames(df)[2:5]=c("Equation","estimate","95% Bootstrap CI","p")
   if(attr(x,"se")=="standard") colnames(df)[4]="95% CI"
   if(!showP){
       df=df[-5]
   }
   table=df2flextable(df,vanilla=vanilla)
   table<-table %>% width(j=4,width=2) %>%
     align(j=c(1,2,4),align="center",part="body") %>%
     fontsize(size=12,part="header") %>%
     bold(part="header")
   if(showP)  table<-table %>% italic(i=1,j=c(5),italic=TRUE,part="header")
   if(attr(x,"se")!="standard") {
     table <- table %>%
       add_footer_lines(paste0("boot.ci.type = ",attr(x,"boot.ci.type") )) %>%
       align(align="right",part="footer")
   }
   table %>%
     autofit()

}

#' Make a table summarizing the mediation effects
#' @param x An object of class medSummary2
#' @param vanilla A logical
#' @importFrom flextable autofit
#' @export
medSummaryTable2=function(x,vanilla=TRUE){

  count=(ncol(x)-1)/4
  count
  df=x
  class(df)="data.frame"
  df[]=lapply(df,myformat)

  for(i in 1:count){
    df[[1+i*4]]=pformat(df[[1+i*4]])
  }
  for(i in 1:count){
    df[[paste0("ci",i)]]=paste0("(",df[[3+(i-1)*4]]," to ",df[[4+(i-1)*4]],")")
  }
  if(vanilla){
    for(i in 1:count){ df[[paste0("s",i)]]=""}
    select=c(1)
    for(i in 1:count){
      select=c(select,2+(i-1)*4,count*4+1+i,5+(i-1)*4)
      if(i<count) select=c(select,which(colnames(df)==paste0("s",i)))
    }
    df=df[select]
    df
    if(attr(x,"se")=="standard") temp=rep(c("estimate","95% CI","p",""),count)
    else temp=rep(c("estimate","95% Bootstrap CI","p",""),count)

    temp=c("type",temp[-length(temp)])
    temp
    table=rrtable::df2flextable(df,vanilla=vanilla)
    table
    col_keys=colnames(df)
    hlabel<-setNames(temp,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    table<-table %>% set_header_labels(values=hlabel)

    hlabel=list(type="",
                est.indirect=paste0("Indirect Effect\n",attr(x,"equations")[1]),
                s1="",
                est.direct=paste0("Direct Effect\n",attr(x,"equations")[2]))
    big_border=fp_border(color="black",width=2)

    table<-table %>%
      hline_top(part="header",border=fp_border(color="black",width=0)) %>%
      add_header_row(top=TRUE,values=hlabel,colwidths=c(1,3,1,3)) %>%
      hline_top(part="header",border=big_border) %>%
      hline(i=1,j=6,part="header",border=fp_border(color="black",width=1))%>%
      hline(i=1,j=2,part="header",border=fp_border(color="black",width=1)) %>%
      width(j=c(5),width=0.01)


    table<-table %>%
      align(j=c(1),align="center",part="body") %>%
      align(align="center",part="header") %>%
      fontsize(size=12,part="header") %>%
      bold(part="header") %>%
      italic(i=2,j=c(4,7),italic=TRUE,part="header") %>%
      width(j=c(3,7),width=2) %>%
      align(j=c(3,7),align="center",part="all")
  } else{
    # vanilla=FALSE
    select=c(1)
    for(i in 1:count){
      select=c(select,2+(i-1)*4,count*4+1+i,5+(i-1)*4)
    }
    df=df[select]
    df
    temp=rep(c("estimate","95% Bootstrap CI","p"),count)
    if(attr(x,"se")=="standard") temp[2]="95% CI"
    temp =c("type",temp)
    table=rrtable::df2flextable(df,vanilla=vanilla)
    table
    col_keys=colnames(df)
    hlabel<-setNames(temp,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    table<-table %>% set_header_labels(values=hlabel)
    table
    hlabel=list(type="type",
                est.indirect=paste0("Indirect Effect\n",attr(x,"equations")[1]),
                est.direct=paste0("Direct Effect\n",attr(x,"equations")[2]))
    big_border=fp_border(color="black",width=2)

    table<-table %>%
      add_header_row(top=TRUE,values=hlabel,colwidths=c(1,3,3))


    table<-table %>%
      align(j=c(1),align="center",part="body") %>%
      align(align="center",part="header") %>%
      fontsize(size=12,part="header") %>%
      bold(part="header") %>%
      italic(i=2,j=c(4,7),italic=TRUE,part="header") %>%
      width(j=c(3,6),width=2)

    table<-table %>% color(i=1,j=1:7,color="white",part="header") %>%
      bg(i=1,j=1:7,bg="#5B7778",part="header") %>%
      merge_at(i=1:2,j=1,part="header")
    table
    table<-table %>% vline(i=1:2,border=fp_border(color="white"),part="header") %>%
      hline(i=1:2,border=fp_border(color="white"),part="header") %>%
      width(j=1,width=1) %>%
      align(j=c(3,6),align="center",part="all")
  }
  table
}


#'S3 method print for an object of class modmedSummary2
#'@param x An object of class modmedSummary2
#'@param ... additional arguments to pass to print.modmedSummary2
#'@export
print.modmedSummary2=function(x,...){

  if("lavaan" %in% class(x)){
    x=modmedSummary(x,add.range=FALSE,...)
  }
     count=ncol(x)
     indirect=attr(x,"indirect")
     direct=attr(x,"direct")
     cnames=c(indirect,direct)
     add=str_replace(unlist(strsplit(indirect[1],"\\)\\*\\("))[2],"\\)","")
     add
     cnames2=c(paste0("\u03B8","D",1:((count-1)/2),"->","Y"),
               paste0("\u03B8","D",1:((count-1)/2),"->","M*(",add,")"))
     width=max(nchar(cnames),nchar(cnames2))
     total=5+(count-1)*(width+1)
     cat(paste(rep("=",total),collapse = ""),"\n")
     cat(centerPrint("",5),centerPrint("Indirect Effect",width*2),centerPrint("Direct Effect",width*2),"\n")
     cat("     ",paste(rep("-",width*2),collapse = "")," ",paste(rep("-",width*2),collapse = ""),"\n")
     cat(centerPrint("W",5),centerPrint(cnames2,width),"\n")
     cat("     ",rep(paste(rep("-",width),collapse = ""),4),"\n")
     cat(centerPrint("",5),centerPrint(cnames,width),"\n")
     cat(paste(rep("-",total),collapse = ""),"\n")
     for(i in 1:nrow(x)){
         cat(rightPrint(sprintf("%0.3f",x[i,1]),5))
         for(j in 2:count){
            cat(rightPrint(sprintf("%0.3f",x[i,j]),width))
         }
         cat("\n")
     }
     cat(paste(rep("-",total),collapse = ""),"\n")

}

#' make table summarizing moderated mediation effect
#' @param x An object of class lavaan ot modmedSummary2
#' @param vanilla logical.
#' @param ... Further arguments to be passed to modmedSummary
modmedSummary2Table=function(x,vanilla=TRUE,...){

  if("lavaan" %in% class(x)){
    x=modmedSummary(x,add.range=FALSE,...)
  }
  count=ncol(x)
  dcount=(count-1)/2
  indirect=attr(x,"indirect")
  direct=attr(x,"direct")

  if(vanilla){
  x$s=""
  x=x[c(1:3,6,4:5)]
  cnames=c("W",indirect,"",direct)
  add=str_replace(unlist(strsplit(indirect[1],"\\)\\*\\("))[2],"\\)","")
  cnames2=c("W",paste0("\u03B8","D",1:dcount,"->","Y"),"",
            paste0("\u03B8","D",1:dcount,"->","M*(",add,")"))
  cnames3=c("W","Indirect Effect","","Direct Effect")
  } else{
    cnames=c("W",indirect,direct)
    add=str_replace(unlist(strsplit(indirect[1],"\\)\\*\\("))[2],"\\)","")
    cnames2=c("W",paste0("\u03B8","D",1:dcount,"->","Y"),
              paste0("\u03B8","D",1:dcount,"->","M*(",add,")"))
    cnames3=c("W","Indirect Effect","Direct Effect")
  }

  ft=df2flextable(x,vanilla=vanilla,digits=3)
  ft
  col_keys=colnames(x)
  hlabel=cnames2
  hlabel
  hlabel<-setNames(hlabel,col_keys)
  hlabel=as.list(hlabel)
  hlabel
  ft<-ft %>% set_header_labels(values=hlabel)
  ft
  if(vanilla){
  ft<- ft %>% hline_top(part="header",border=fp_border(color="black",width=0)) %>%
    add_header_row(top=TRUE,values=cnames,colwidths = rep(1,(count+1))) %>%
    add_header_row(top=TRUE,values=cnames3,colwidths = c(1,dcount,1,dcount)) %>%
    width(j=c(2,3,5,6),width=1.3) %>%
    width(j=4,width=0.01) %>%
    align(align="center",part="header") %>%
    merge_at(i=1:3,j=1,part="header") %>%
    bold(part="header") %>%
    fontsize(size=12,part="header") %>%
    hline_top(part="header",border=fp_border(color="black",width=2)) %>%
    hline(i=1,j=2:3,part="header",border=fp_border(color="black",width=1))%>%
    hline(i=1,j=5:6,part="header",border=fp_border(color="black",width=1)) %>%
    hline(i=2,j=2,part="header",border=fp_border(color="black",width=1))%>%
    hline(i=2,j=3,part="header",border=fp_border(color="black",width=1))%>%
    hline(i=2,j=5,part="header",border=fp_border(color="black",width=1))%>%
    hline(i=2,j=6,part="header",border=fp_border(color="black",width=1))
  } else{
    ft<-ft %>%
      add_header_row(top=TRUE,values=cnames,colwidths = rep(1,count)) %>%
      add_header_row(top=TRUE,values=cnames3,colwidths = c(1,dcount,dcount)) %>%
      width(j=c(2:5),width=1.3) %>%
      align(align="center",part="header") %>%
      merge_at(i=1:3,j=1,part="header") %>%
      bold(part="header") %>%
      fontsize(size=12,part="all") %>%
      color(color="white",part="header") %>%
      bg(bg="#5B7778",part="header") %>%
      vline(border=fp_border(color="white"),part="header") %>%
      hline(border=fp_border(color="white"),part="header")


  }

  ft

}


