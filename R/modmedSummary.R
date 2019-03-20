#' Summarize the moderated mediation
#' @param fit An object of class lavaan
#' @param mod name of moderator
#' @param values Optional. Numeric vector
#' @param boot.ci.type Type of bootstrapping interval. Choices are c("norm","basic","perc",bca.simple")
#' @importFrom lavaan parameterEstimates
#' @export
#' @return A data.frame and an object of class modmedSummary
modmedSummary=function(fit,mod="skeptic",values=NULL,boot.ci.type="bca.simple"){

     # boot.ci.type="bca.simple";mod="skeptic";values=NULL
      # fit=semfit;mod="HBP";values=NULL;boot.ci.type="bca.simple"
    res=parameterEstimates(fit,boot.ci.type = boot.ci.type,
                           level = .95, ci = TRUE,
                           standardized = FALSE)
    res=res[res$label!="",]
    res
    if(is.null(values)){
      values1=res$est[res$label==paste0(mod,".mean")]+c(0,-1,1)*sqrt(res$est[res$label==paste0(mod,".var")])
      values1
    } else{
        values1=values
    }
    select=c("indirect","indirect.below","indirect.above")
    indirect=res$est[which(res$lhs %in% select)]
    lower=res$ci.lower[which(res$lhs %in% select)]
    upper=res$ci.upper[which(res$lhs %in% select)]
    indirectp=res$pvalue[which(res$lhs %in% select)]
    select=c("direct","direct.below","direct.above")
    direct=res$est[which(res$lhs %in% select)]
    lowerd=res$ci.lower[which(res$lhs %in% select)]
    upperd=res$ci.upper[which(res$lhs %in% select)]
    #
    # se=res$se[which(res$lhs %in% select)]
    directp=res$p[which(res$lhs %in% select)]

    df=data.frame(values=values1,indirect,lower,upper,indirectp,direct,lowerd,upperd,directp)
    df=df[c(2,1,3),]
    df[]=round(df,3)
    attr(df,"mod")=mod

    if(is.null(values)) {
        indirect=res$rhs[res$lhs=="indirect"]
        indirect=str_replace(indirect,paste0(mod,".mean"),"W")
        direct=res$rhs[res$lhs=="direct"]
        direct=str_replace(direct,paste0(mod,".mean"),"W")
    } else{
        indirect=res$rhs[res$lhs=="indirect"]
        indirect=str_replace(indirect,paste0(values[1]),"W")
        direct=res$rhs[res$lhs=="direct"]
        direct=str_replace(direct,paste0(values[1]),"W")
    }

    attr(df,"indirect")=indirect
    attr(df,"direct")=direct
    attr(df,"boot.ci.type")=boot.ci.type
    class(df)=c("modmedSummary","data.frame")
    df
}

#' Print a string in center
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
    indirect=attr(x,"indirect")
    direct=attr(x,"direct")
    left=max(nchar(mod)+2,8)
    total=73+left

    cat("\nInference for the Conditional Direct and Indirect Effects - boot.ci.type:",attr(x,"boot.ci.type"),"\n")
    cat(paste(rep("=",total),collapse = ""),"\n")
    cat(centerPrint("",left),centerPrint("Indirect Effect",35),centerPrint("Direct Effect",35),"\n")
    cat(centerPrint("",left),centerPrint(indirect,35),centerPrint(direct,35),"\n")
    cat(centerPrint("",left),paste(rep("-",35),collapse = ""),paste(rep("-",36),collapse = ""),"\n")

    cat(centerPrint(mod,left),centerPrint("estimate",8),centerPrint("95% Bootstrap CI",18),centerPrint("p",8))
    cat(centerPrint("estimate",11),centerPrint("95% Bootstrap CI",18),centerPrint("p",8),"\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in 1:count){
        cat(rightPrint(x[i,1],left-1),"")
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
#' @param x An object of class modmedSummary
#' @param vanilla A logical
#' @importFrom flextable bg vline add_footer_lines
#' @export
modmedSummaryTable=function(x,vanilla=TRUE){
    count=nrow(x)
    x[]=lapply(x,myformat)
    x[[5]]=pformat(x[[5]])
    x[[9]]=pformat(x[[9]])

    x1=x

    if(vanilla){
    x1$s=""
    x1$ci=paste0("(",x1$lower," to ",x1$upper,")")
    x1$ci2=paste0("(",x1$lowerd," to ",x1$upperd,")")

    x1=x1[c(1:2,11,5,10,6,12,9)]

    ft=rrtable::df2flextable(x1,vanilla=vanilla,digits=3)
    ft
    hlabel=c(paste0(attr(x,"mod"),"(W)"),"estimate","95% Bootstrap CI","p","","estimate","95% Bootstrap CI","p")
    hlabel
    col_keys=colnames(x1)
    hlabel<-setNames(hlabel,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    ft<-ft %>% set_header_labels(values=hlabel)
    ft
    ft<-ft %>% width(j=c(3,7),width=1.6) %>% width(j=5,width=0.1)
    big_border=fp_border(color="black",width=2)
    hlabel=list(values="",
                indirect=paste0("Indirect Effect\n",attr(x,"indirect")),s="",
                direct=paste0("Direct Effect\n",attr(x,"direct")))
    ft<- ft %>%
        hline_top(part="header",border=fp_border(color="black",width=0)) %>%
        add_header_row(top=TRUE,values=hlabel,colwidths=c(1,3,1,3)) %>%
        hline_top(part="header",border=big_border) %>%
        hline(i=1,j=2:4,part="header",border=fp_border(color="black",width=1))%>%
        hline(i=1,j=6:8,part="header",border=fp_border(color="black",width=1)) %>%
        merge_h_range (i=1,j1=2,j2=4,part="header") %>%
        merge_h_range (i=1,j1=6,j2=8,part="header") %>%
        align(align="center",part="all") %>%
        align(align="right",part="body") %>%
        fontsize(size=12,part="header") %>%
        bold(part="header") %>%
        italic(i=2,j=c(4,8),italic=TRUE,part="header") %>%
        width(j=1,width=1)
    } else{
        x1$ci=paste0("(",x1$lower," to ",x1$upper,")")
        x1$ci2=paste0("(",x1$lowerd," to ",x1$upperd,")")

        x1=x1[c(1:2,10,5,6,11,9)]
        ft=rrtable::df2flextable(x1,vanilla=vanilla,digits=3)

        ft
        hlabel=c(paste0(attr(x,"mod"),"(W)"),"estimate","95% Bootstrap CI","p","estimate","95% Bootstrap CI","p")
        hlabel
        col_keys=colnames(x1)
        hlabel<-setNames(hlabel,col_keys)
        hlabel=as.list(hlabel)
        hlabel
        ft<-ft %>% set_header_labels(values=hlabel)
        ft
        ft<-ft %>% width(j=c(3,6),width=1.6) %>% width(j=5,width=0.1)
        big_border=fp_border(color="black",width=2)
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

    ft<-ft %>% add_footer_lines(paste0("boot.ci.type = ",attr(x,"boot.ci.type") )) %>%
        align(align="right",part="footer")
    ft
}

