#' append something to file
#' @param ... Further argument passed to the cat()
#' @param file name of file
mycat=function(...,file="report.Rmd"){
    base::cat(...,file=file, append=TRUE)
}

#' make powerpoint presentation
#' @param data A data.frame with title and code
#' @param preprocessing string preprocessing
#' @param filename character name of output file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from
#' @param rmdRemove A logical
#' @importFrom rmarkdown render powerpoint_presentation
#' @export
makePPTx=function(data,preprocessing="",filename="report.pptx",rawDataName=NULL,
                  rawDataFile="rawData.RDS",rmdRemove=TRUE) {

    file.create("report.Rmd")
    tempReport <-  "report.Rmd"

    title="Conditional Process Analysis"
    subtitle="prepared by processR package"

    mycat("---\ntitle: '",title,"'\n")

    if(subtitle!=""){
        mycat("subtitle: '",subtitle,"'\n")
    }
    mycat("date: '`r Sys.time()`'\n")
    mycat("output: powerpoint_presentation\n---\n")

    mycat("```{r setup, include=FALSE}\n")
    mycat("knitr::opts_chunk$set(echo = FALSE,comment=NA,message=FALSE,
          warning=FALSE,
          fig.width=7,fig.height=5, dpi=300)\n")
    mycat("```\n")

    mycat("```{r}\n")
    mycat("require(processR)\n")
    mycat("require(predict3d)\n")
    mycat("require(lavaan)\n")
    mycat("require(flextable)\n")
    mycat("require(ggplot2)\n")
    mycat("require(tidyverse)\n")
    mycat("```\n\n")

    if(!is.null(rawDataName)){
        mycat("```{r}\n")
        mycat("# Read Raw Data\n")
        temp=paste0("rawData=readRDS('",rawDataFile,"')\n")
        mycat(temp)
        temp=paste0("assign('",rawDataName,"',rawData)\n")
        mycat(temp)
        mycat("```\n\n")
    }


    if(preprocessing!="") {
        mycat("```{r}\n")
        mycat(preprocessing,'\n')
        mycat("```\n\n")
    }

    count=nrow(data)
    for(i in 1:count){
        # if(data$title[i]=="") mycat("\n\n----\n\n")
        # else mycat("\n\n#### ",data$title[i],"\n")
        mycat("```{r}\n")
        mycat(data$code[i],"\n")
        mycat("```\n\n\n")

    }

    rmarkdown::render('report.Rmd', output_file="report.pptx",
                      rmarkdown::powerpoint_presentation())
    if(rmdRemove) file.remove("report.Rmd")

}



#' Make powerpoint presentation from R file
#' @param file source file name
#' @param filename destination file name
#' @param keyword A string vector
#' @param rmdRemove A logical
#' @importFrom readr read_file
#' @importFrom stringr str_detect
#' @export
r2pptx=function(file,filename="report.pptx",
                keyword=c("Concept","Diagram","Model","Plot","plot","Table","summary"),
                rmdRemove=TRUE){

    code<-c()
    count=0

    text=readr::read_file(file)
    text=unlist(strsplit(text,"\n"))

    tempcode=""
    pattern=paste0(keyword,collapse="|")
    pattern
    temp=""
    for( i in seq_along(text)){
        if(temp=="") {
            temp=text[i]
        } else{
            temp=paste0(temp,"\n",text[i])
        }
        if(str_detect(text[i],"^#")) next
        if(str_detect(text[i],"%>%$")) next
        if(str_detect(temp,pattern)) {
            code=c(code,temp)
            temp=""
        }
    }
    code
    df=data.frame(code,stringsAsFactors = FALSE)
    makePPTx(df,filename=filename,rmdRemove=rmdRemove)
}




