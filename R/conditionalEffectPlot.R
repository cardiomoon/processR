#'Make conditional effect plot
#'@param fit An object of class lavaan
#'@param values Optional value
#'@param data A data.frame
#'@param mod Name of moderator variable
#'@importFrom ggplot2 stat_function guides xlab ylab ggplot aes_string coord_fixed
#'@importFrom ggplot2 geom_text aes theme_bw
#'@export
conditionalEffectPlot=function(fit,values=NULL,data,mod="skeptic"){
    res=parameterEstimates(fit)
    res=res[res$label!="",]
    res
    x=modmedSummary(fit,mod=mod,values=values)
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
        stat_function(fun=fun1,color="black",size=1)+
        stat_function(fun=fun2,color="red",lty=3,size=1)
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
    df$label=paste0("Y = ",sprintf("%0.3f",df$intercept),ifelse(df$slope>0," + "," - "),
                    sprintf("%0.3f",abs(df$slope)),"*W")
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
        geom_text(data=df,aes_string(x="x",y="y",label="name",angle="angle"),color=c("black","red"),vjust=-1.0,fontface="italic")+
        geom_text(data=df,aes_string(x="x",y="y",label="label",angle="angle"),color=c("black","red"),fontface="italic",vjust=1.5)+
        guides(color=FALSE)+
        xlab(paste0(mod,"(W)"))+ylab("Conditional Effects")+
        theme_bw()
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

#'Get aspect information og a ggplot
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
