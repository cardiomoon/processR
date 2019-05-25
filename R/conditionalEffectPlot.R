#'Make conditional effect plot
#'@param semfit An object of class lavaan
#'@param values Optional value
#'@param data A data.frame
#'@param mod Name of moderator variable
#'@param color charactor vector line color
#'@param lty numeric line type
#'@param linesize numeric linesize
#'@importFrom ggplot2 stat_function guides xlab ylab ggplot aes_string coord_fixed
#'@importFrom ggplot2 geom_text aes theme_bw
#'@importFrom predict3d theme_bw2
#'@export
conditionalEffectPlot=function(semfit,values=NULL,data,
                               mod=NULL,color=c("black","red"),lty=c(1,3),linesize=1){

    # values=NULL;mod=NULL;color=c("black","red");lty=c(1,3);linesize=1

    res=parameterEstimates(semfit)
    res=res[res$label!="",]
    res
    if(is.null(mod)){
        mod=res$lhs[str_detect(res$label,"mean")][1]
        mod
    }
    x=modmedSummary(semfit,mod=mod,values=values)
    indirect=attr(x,"indirect")
    direct=attr(x,"direct")
    indirect
    direct
    for(i in 1:nrow(res)){
        indirect=str_replace(indirect,res$label[i],as.character(res$est[i],3))
        direct=str_replace(direct,res$label[i],as.character(res$est[i],3))
    }
    fun1=function(W) eval(parse(text=direct))
    fun2=function(W) eval(parse(text=indirect))
    p<-ggplot(data=data,aes_string(x=mod))+
        stat_function(fun=fun1,color=color[1],lty=lty[1],size=linesize)+
        stat_function(fun=fun2,color=color[2],lty=lty[2],size=linesize)
    p
    res1=fun2eq(fun1)
    res2=fun2eq(fun2)
    slope=c(res1$slope,res2$slope)
    intercept=c(res1$intercept,res2$intercept)
    name=c("Direct Effect","Indirect Effect")
    df=data.frame(name,slope,intercept,stringsAsFactors = FALSE)
    df
    info=getAspectRatio(p)
    info
    ratio=info$ratio

    df$slope2=df$slope*ratio
    df$radian=atan(df$slope2)
    df$angle=df$radian*180/pi
    df
    # str(df)
    df$label=paste0("Y = ",sprintf("%0.3f",df$intercept),
                    ifelse(df$slope==0,"",ifelse(df$slope>0," + "," - ")),
                    ifelse(df$slope==0,"",paste0(sprintf("%0.3f",abs(df$slope)),"*W")))
    p
    x=info$xmin+(info$xmax-info$xmin)/3*(1:2)
    x
    y1=unlist(lapply(1:2,function(i){fun1(x[i])}))
    y2=unlist(lapply(1:2,function(i){fun2(x[i])}))
    select=which.max(abs(y1-y2))
    df$x=x[select]
    y=c(fun1(x[select]),fun2(x[select]))
    df$y=y
    df
    p+coord_fixed(ratio=ratio)+
        geom_text(data=df,aes_string(x="x",y="y",label="name",angle="angle"),color=color,vjust=-1.0,fontface="italic")+
        geom_text(data=df,aes_string(x="x",y="y",label="label",angle="angle"),color=color,fontface="italic",vjust=1.5)+
        guides(color=FALSE)+
        xlab(paste0(mod,"(W)"))+ylab("Conditional Effects")+
        theme_bw2()
}

#'Make equation from function
#'@param fun A function
#'@export
fun2eq=function(fun){
    intercept=fun(0)
    intercept
    y1=fun(1)
    slope=(y1-intercept)
    slope
    result=list(slope=slope,intercept=intercept)
    result
}

#'Get aspect information of a ggplot
#'@param p A ggplot object
#'@importFrom ggplot2 layer_scales
#'@export
getAspectRatio=function(p){
    xmin=layer_scales(p)$x$range$range[1]
    xmax=layer_scales(p)$x$range$range[2]
    ymin=layer_scales(p)$y$range$range[1]
    ymax=layer_scales(p)$y$range$range[2]

    ratio=(xmax-xmin)/(ymax-ymin)
    list(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,ratio=ratio)
}
