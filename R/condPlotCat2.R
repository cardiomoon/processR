#' Draw direct and indirect effect plot
#' @param labels list of variable labels
#' @param data data.frame
#' @param semfit An object of class lavaan
#' @param catlabels labels for direct/indirect effects
#' @param digits Integer indicating the number of decimal places
#' @param add.point logical. Whether or not add points to the plot
#' @param ... further argumnets to be passed to predict3d::add_lines()
#' @importFrom predict3d add_lines
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(lavaan)
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' moderator=list(name="sexism",site=list(c("a","c")))
#' data1=addCatVars(protest,"protest",mode=3)
#' catlabels=c("Indirect: Protest\n   vs. No Protest","Indirect: Collective\n       vs. Individual","Direct: Protest\n vs. No Protest","Direct: Collective\n      vs. Individual")
#' model=catMediation(X="protest",M="respappr",Y="liking",moderator=moderator,data=data1,maxylev=6,rangemode = 2)
#' semfit=sem(model=model,data=data1)
#' condPlotCat2(labels=labels,data=data1,semfit=semfit,catlabels=catlabels,xpos=c(0.7,0.3,0.3,0.7),add.point=TRUE)
condPlotCat2=function(labels=NULL,data=NULL,semfit,catlabels=NULL,digits=3,
                     add.point=FALSE,...){

     # labels=labels;data=data1;semfit=semfit;catlabels=catlabels;add.point=TRUE
     # digits=3

    X=labels$X
    Y=labels$Y
    W=labels$W


    myeval=function(equation){
        sapply(equation,function(x){eval(parse(text=x))})
    }

    df=parameterEstimates(semfit)
    df=df[df$label!="",]
    df
    for(i in 1:nrow(df)) assign(df$label[i],df$est[i])
    df=df[str_detect(df$label,"direct"),3:5]
    df1<-df %>% filter(!str_detect(df$label,"\\.a")&!str_detect(df$label,"\\.b"))
    df1
    median=quantile(data[[W]],probs=0.5,type=6)
    df1$rhs=str_replace_all(df1$rhs,as.character(median),"W")
    df1$eq=unfold(df1$rhs)
    df1$intercept=unfold(df1$rhs,mode=0)
    df1$intercept1=myeval(df1$intercept)
    df1$slope=unfold(df1$rhs,mode=1)
    df1$slope1=myeval(df1$slope)
    df1$quadratic=unfold(df1$rhs,mode=2)
    df1$quadratic1=myeval(df1$quadratic)
    df1$eq1=round(df1$intercept1,digits)
    df1$eq1=ifelse(df1$slope1==0,df1$eq1,
                   paste0(df1$eq1,ifelse(df1$slope1>0,"+","-"),
                          abs(round(df1$slope1,digits)),"*W"))
    df1$eq1=ifelse(df1$quadratic1==0,df1$eq1,
                   paste0(df1$eq1,ifelse(df1$quadratic1>0,"+","-"),
                          abs(round(df1$quadratic1,digits)),"*W^2"))
    df1
    df2=df1[c("slope1","intercept1","eq1","label")]
    names(df2)=c("slope","intercept","eq","label")
    df2$label=paste0(df2$label," == ",df2$eq)
    df2
    x=quantile(data[[W]],probs=c(0.16,0.5,0.84),type=6)
    df3=data.frame(x=rep(x,each=4))
    df3$no=rep(1:4,3)
    df3$slope=rep(df2$slope,3)
    df3$intercept=rep(df2$intercept,3)
    df3$y=df3$x*df3$slope+df3$intercept
    df3
    if(is.null(catlabels)) {
        df3$label=rep(df1$label,3)
    } else{
        df3$label=rep(catlabels,3)
    }
    df3$label1=reorder(df3$label,df3$no)
    df3$label2=round(df3$y,digits)
    p=ggplot(data=data,aes_string(x=W))

    p<-add_lines(p,df2,size=1,parse=TRUE,...)

    if(add.point){
        p<-p +geom_point(data=df3,aes_string(x="x",y="y",color="label1"))+
            geom_text_repel(data=df3,aes_string(x="x",y="y",color="label1",
                                         label="label2"),box.padding = 1)
    }
    p<-p+ theme(legend.position="top")+
        theme(legend.title=element_blank())+
        labs(y=paste0("Relative Conditional Effect on ",Y))
    p

}

#' Separate equation
#' @param equation string. Equations to separate
#' @export
#' @examples
#' equation="( a1 + b1 * W )"
#' separateEq(equation)
separateEq=function(equation){
    equation %>% str_replace_all(" ","") %>%
        str_replace_all("\\(","") %>%
        str_replace_all("\\)","") %>%
        str_split("\\+") %>%
        unlist()
}

#' Make products of equations
#' @param equation1 The first equation
#' @param equation2 Thw second equation
#' @export
#' @examples
#' equation1=c("a1+b1*W")
#' equation2=c("a2+b2*W")
#' productEq(equation1,equation2)
productEq=function(equation1,equation2){
    result=c()
    eq1<- separateEq(equation1)
    eq2<- separateEq(equation2)

    for(i in seq_along(eq1)){
        for(j in seq_along(eq2)){
            result=c(result,paste(eq1[i],eq2[j],sep="*"))
        }
    }
    result
}

#' Unfold equations
#' @param string Character vectors with equation
#' @param var name of variable
#' @param mode integer. Default value is -1. If 0, get intercept, If 1, get slope
#' @importFrom stringr str_count
#' @export
#' @examples
#' string=c("(a1+b1*W)*(a2+b2*W)*(a3+b3*W)","a1+b1*W")
#' unfold(string)
unfold=function(string,var="W",mode=-1) {

    result=c()
    for(i in seq_along(string)){
        equations<- string[i] %>% strsplit("\\)\\*\\(") %>%
            unlist() %>%
            str_replace_all("\\(","") %>%
            str_replace_all("\\)","")
        equations
        if(length(equations)==1){
           temp=unlist(strsplit(equations,"\\+"))
        } else{
           temp=reduce(equations,productEq)
        }
        temp

        maxOrder=max(str_count(temp,var))
        eq=list()
        for(i in 1:(maxOrder+1)){
            eq[[i]]<-paste0(temp[str_count(temp,var)==(i-1)],collapse="+") %>%
                       str_replace_all(paste0("\\*",var),"")
        }
        eq

        eq1=lapply(eq,function(x){
            ifelse(str_detect(x,"\\+"),paste0("(",x,")"),x)
        })

        eq2<- lapply(1:length(eq1),function(i){
            if(i==1) {
                eq1[[i]]
            } else if(i==2){
                paste0(eq1[[i]],"*",var)
            } else{
                paste0(eq1[[i]],"*",var,"^",(i-1))
            }
        })
        res<-eq2 %>% unlist() %>% paste(collapse="+")
        if(mode==-1) {
            result=c(result,res)
        } else {
            if(length(eq)>=(mode+1)){
               result=c(result,eq[[mode+1]])
            } else{
                result=c(result,0)
            }
        }
    }
    result
}


