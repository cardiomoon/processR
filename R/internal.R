#' Variance Inflation Factors
#' Calculates variance-inflation and generalized variance-inflation factors for linear, generalized linear, and other models.
#'
#' @param mod an object that responds to coef, vcov, and model.matrix, such as an lm or glm object.
#' @importFrom stats coef coefficients cov2cor model.matrix vcov
#' @return A vector of vifs, or a matrix containing one row for each term in the model, and columns for the GVIF, df.
vif=function (mod)
{
    if (any(is.na(coef(mod))))
        stop("there are aliased coefficients in the model")
    v <- vcov(mod)
    assign <- attr(model.matrix(mod), "assign")
    if (names(coefficients(mod)[1]) == "(Intercept)") {
        v <- v[-1, -1]
        assign <- assign[-1]
    }
    else warning("No intercept: vifs may not be sensible.")
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2)
        stop("model contains fewer than 2 terms")
    R <- cov2cor(v)
    detR <- det(R)
    result <- matrix(0, n.terms, 3)
    rownames(result) <- terms
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
    for (term in 1:n.terms) {
        subs <- which(assign == term)
        result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs,
                                                                           -subs]))/detR
        result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1))
        result <- result[, 1]
    else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    result
}


#' Correlation and Fitting linear model function for function "mycor"
#'
#' @param y numeric vectors of data values
#' @param x numeric vectors of data values
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used.
#' @importFrom stats cor.test lm
#' @return mylm returns a list of following components
#'
#'     \describe{
#'        \item{out}{a list of class "htest" from \code{\link{cor.test}}
#'           between the last paired samples in a data.frame.}
#'        \item{result}{a numeric vector of length 4, consist of r and p values
#'              from \code{\link{cor.test}},slope and intercept values from
#'              \code{\link{lm}} between numeric vector y and x}
#'     }
mylm=function(y,x,digits=3){
    # performing cor.test
    out1=cor.test(y,x)
    my.r.value= round(out1$estimate,digits)
    my.p.value= round(out1$p.value,digits)
    # performing lm to get slope and intercept
    out=lm(y~x)
    result=c(my.r.value,my.p.value,
             round(out$coef[2],max(2,digits-1)),
             round(out$coef[1],max(2,digits-1)))

    # Return list consist of output of cor.test
    # as weel as r, p, slope, intercept
    list(out=out1,result=result)
}


#'Perform correlation and linear regression for a data.frame
#'@param x A data.frame
#'@param digits	integer indicating the number of decimal places
mycor=function (x, digits = 3) {
                select <- (lapply(x, function(x) is.numeric(x)) == TRUE)
                num_data = x[select]
                y <- names(num_data)
                ncol = length(num_data)
                r.value <- matrix(0, ncol, ncol)
                colnames(r.value) <- rownames(r.value) <- y
                p.value <- slope <- intercept <- r.value
                for (i in 1:length(y)) {
                    for (j in 1:length(y)) {
                        out = mylm(num_data[[j]], num_data[[i]], digits = digits)
                        r.value[i, j] = out$result[1]
                        p.value[i, j] = out$result[2]
                        slope[j, i] = out$result[3]
                        intercept[j, i] = out$result[4]
                    }
                }
                result <- list(df = x, select = select, out = out$out, r = r.value,
                               p = p.value, slope = slope, intercept = intercept)
                result
}

#'Draw a heatmap of correlation test
#'@param data A data.frame
#'@param what if 1, correlation, if 2, partial correlation, if 3, semi-partial correlation
#'@param label if 0, no label(default), if 1, use r value as label, if 2, use r value with significant mark as label
#'@param colors colors for low, mid and high correlation values
#'@param title if true, add title to the heatmap
#'@param mode 1 or 2
#'@param digits The number of decimal place'
#'@param yreverse If true, reverse y axis
#'@param xangle x-axis text angle
#'@param  yangle y-axis text angle
#'@param use.label Logical whether or not use label in case of labelled data
#'@importFrom stats na.omit
#'@importFrom ggplot2 coord_equal geom_tile ggtitle scale_fill_gradient2 scale_y_discrete "%+replace%"
ggCor=function (data, what = 1, label = 0, colors = NULL, title = TRUE,
          mode = 2, digits = 2,  yreverse = TRUE,
          xangle = 45, yangle = 0, use.label = FALSE)
{
    data = as.data.frame(data)
    select = sapply(data, is.numeric)
    data = data[select]
    if (what > 1) {
        data = na.omit(data)
    }
    data
    if (what == 1) {
        result = mycor(data, digits = digits)
        method = result$out$method
        Lab = paste("Correlation Coeffients by", method)
    }
    if (is.null(colors))
        colors = c("#6D9EC1", "white", "#E46726")
    cor_mat <- result$r
    p_mat <- result$p
    diag(cor_mat) <- NA
    diag(p_mat) <- NA
    if (mode == 2)
        cor_mat[upper.tri(cor_mat)] = NA
    var1 <- rep(row.names(cor_mat), ncol(cor_mat))
    var2 <- rep(colnames(cor_mat), each = nrow(cor_mat))
    cor <- as.numeric(cor_mat)
    cor_mat <- data.frame(var1 = var1, var2 = var2, cor = cor,
                          stringsAsFactors = FALSE)
    pval = as.numeric(p_mat)
    cor_mat$label = ifelse(is.na(cor_mat$cor), "", sprintf(paste0("%0.",
                                                                  digits, "f"), cor_mat$cor))
    if (label == 2)
        cor_mat$label = paste0(cor_mat$label, ifelse(is.na(pval),
                                                     "", ifelse(pval < 0.001, "***", ifelse(pval < 0.01,
                                                                                            "**", ifelse(pval < 0.05, "*", "")))))
    else if (label == 3)
        cor_mat$label = paste0(cor_mat$label, "\n", p2chr(pval))
    cor_mat$p = ifelse(is.na(pval), "", ifelse(pval < 0.001,
                                               "< 0.001", sprintf(" = %0.3f", pval)))
    cor_mat[["tooltip"]] <- sprintf("<i>%s</i> vs <i>%s</i>:</br><i>r</i> = %s</br><i>p</i> %s",
                                    var1, var2, cor_mat$label, cor_mat$p)
    if (mode == 2)
        cor_mat = na.omit(cor_mat)
    if (mode == 1)
        p <- ggplot(data = cor_mat, aes_string(x = "var1", y = "var2",
                                               tooltip = "tooltip"))
    else if (mode == 2)
        p <- ggplot(data = cor_mat, aes_string(x = "var2", y = "var1",
                                               tooltip = "tooltip"))
    p <- p + geom_tile(aes(fill = cor), colour = "grey50") +
        scale_fill_gradient2(low = colors[1], mid = colors[2],
                             high = colors[3], limits = c(-1, 1)) + coord_equal() +
        xlab("") + ylab("")
    if (title) {
        p <- p + ggtitle(Lab)
    }
    if (label > 0)
        p <- p + geom_text(aes(label = label))
    if (mode == 2) {
        mynames = rownames(result$r)
        p <- p + scale_x_discrete(limits = mynames[-length(mynames)])
        if (yreverse)
            p <- p + scale_y_discrete(limits = rev(mynames[-1]))
        else p <- p + scale_y_discrete(limits = mynames[-1])
        p <- p + theme_clean2(xangle = xangle, yangle = yangle)
        p <- p + theme(legend.position = c(0.8, 0.8)) + labs(fill = "r value")
    }
    p
}


#'Clean theme for ggCor
#'@param base_size base font size
#'@param xangle x-axis text angle
#'@param yangle y-axis text angle
#'@importFrom ggplot2 theme_grey
theme_clean2=function (base_size = 12, xangle = 45, yangle = 0)
{
    theme_grey(base_size) %+replace% theme(panel.background = element_blank(),
                                           panel.grid = element_blank(), axis.title = element_blank(),
                                           axis.text.x = element_text(angle = xangle), axis.text.y = element_text(angle = yangle),
                                           axis.ticks.length = unit(0, "cm"), complete = TRUE)
}

#'Convert p values to character
#'@param x	A vector
p2chr=function (x)
{
    ifelse(is.na(x), "", ifelse(x < 0.001, "(<.001)", paste0("(",
                                                             substr(sprintf("%.3f", x), 2, 5), ")")))
}
