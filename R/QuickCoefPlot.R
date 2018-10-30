#' QuickCoefPlot
#'
#' QuickCoefPlot (aka qcp) is an easy interface for linear regression coefficient plots in R. This includes the option to request robust and clustered standard errors, automatic labeling, and easy selection of coefficients to plot.
#' Written by Sondre U. Solstad (ssolstad@princeton.edu) - Please cite my github: github.com/sondreus/QuickCoefPlot.
#'
#' @param model Fitted "lm" object.
#' @param iv.vars.names (Optional) Vector of desired independent variable names in table output (e.g. c("GDP per capita", "Population")). Defaults to values in "iv.vars" if none provided.
#' @param plot.title (Optional) Specifies the title of the coefficient plot. Defaults to no title.
#' @param xlim (Optional) Vector of limits on x-axis of plot. If none supplied, this is automatically selected by ggplot.
#' @param include.only (Optional) Vector of coefficients by number to keep in the plot (e.g. c(1, 2, 6)). If none specified, defaults to all.
#' @param robust.se (Optional) If TRUE, returns robust standard errors calculated using a sandwich estimator from the "sandwich" package. Defaults to TRUE (i.e. robust rather than normal standard errors).
#' @param cluster (Optional) Name of variable by which cluster-robust standard errors should be computed using the cluster.vcov command of the multiwayvcov package. If this variable is not in the model, a data frame common to both the model variables and the clustering variable must be supplied.
#' @param cluster.vars.names (Optional) Desired name or label of clustering variable to be reported in table output (e.g. "Country" yields a note on the bottom of the table reading "Country-Clustered Standard Errors in Parenthesis"). If cluster specified but no "cluster.vars.names" provided, "Cluster-Robust Standard Errors in Parenthesis" is reported.
#' @param boot.se (Optional) If TRUE, calculates confidence interval based on bootstrap simulations. By default based on 100 such simulations, see boot.b below.
#' @param boot.b (Optional) Integer specifying the number of bootstrap simulation to run if boot.se is set to TRUE. Defaults to 100.
#' @param boot.plot.est (Optional) If TRUE, plots the bootstrap estimates semi-transparently on the coefficient plot. Defaults to FALSE.
#' @param plot.lines (Optional) If FALSE, does not plot confidence intervals on the coefficient plot. Defaults to TRUE.
#' @param legend.on (Optional) If TRUE turns on the plot legend. Defaults to FALSE.
#' @param colors.off (Optional) If TRUE turns off color scheme (sky-blue if p > 0.1, dark blue if p < 0.1, black if p < 0.05). Defaults to FALSE.
#' @param plot.margin (Optional) Vector of plot margins in centimeters. Defaults to (1, 1, 1, 1).
#' @param text.size (Optional) Text size for plot elements.
#' @param hide.summary.lines (Optional) Vector of summary lines to hide in plot output. If none supplied, defaults to none.
#' @param add.summary.lines (Optional) Vector of summary lines to add to plot. If none supplied, defaults to none.
#' @param horserace (Optional) If TRUE Produces a table comparing t-statistics instead of a coefficient plot.
#' @keywords lm coefplot robust.se robust cluster LS reg horse-race tstat regression glm
#' @export QuickCoefPlot qcp
#' @aliases qcp
#' @examples
#'
#' QuickCoefPlot(lm(x ~y, data = mydata))
#' qcp(lm(x ~ y, data = mydata, boot.se = TRUE, boot.plot.est = TRUE, legend.on = TRUE))
#'
#' Please see: github.com/sondreus/QuickCoefPlot

QuickCoefPlot <- qcp <- function(model, iv.vars.names, plot.title, include.only, robust.se, cluster, cluster.vars.names, boot.se, boot.b, boot.plot.est, plot.lines, plot.margin = c(0.5, 0.5, 0.5, 0.5), legend.on, colors.off, text.size, add.summary.lines, hide.summary.lines, xlim, horserace){
  require(lmtest)
  require(sandwich)


# Checking if Zelig object, if so,  transforming into fitted model object:

  # Setting default
  zelig <- FALSE

if(length(grep("Zelig", class(model)[1])) > 0){
        zelig <- TRUE
        zelig.model <- model$name
        if(!missing(cluster)){
          if(length(cluster) != 0){
            zelig.data <- model$data
          }
        }

        model <- from_zelig_model(model)
}

  # Setting variable names:
  if(missing(iv.vars.names)){
    iv.vars.names <- rownames(coeftest(model))[2:dim(coeftest(model))[1]]
  } else if(length(iv.vars.names) < length(rownames(coeftest(model))[2:dim(coeftest(model))[1]])){
    iv.vars.names[1:length(iv.vars.names)] <- iv.vars.names

    iv.vars.names[(length(iv.vars.names)+1):length(rownames(coeftest(model))[2:dim(coeftest(model))[1]])] <- rownames(coeftest(model))[(length(iv.vars.names)+2):dim(coeftest(model))[1]]
  } else if(length(iv.vars.names) > length(rownames(coeftest(model))[2:dim(coeftest(model))[1]])){
    iv.vars.names <- iv.vars.names[1:length(rownames(coeftest(model))[2:dim(coeftest(model))[1]])]
  }



  # Setting robust SE as default
  robust <- TRUE

  # Checking if non-robust standard errors requested
  if(!missing(robust.se)){
    if(robust.se == FALSE){
      robust <- FALSE
    }
  }

  # Checking if boot-strapped standard errors requested

  # Setting default
  boot <- FALSE

  if(!missing(boot.se)){
    if(boot.se == TRUE){
      boot <- TRUE
      robust <- FALSE

      # Setting B (default is 100)
      boot.b <- ifelse(missing(boot.b), 100, boot.b)
      }
    }


  # Switching if horse-race regression selected

  # Setting default:
  t.stat <- FALSE

  if(!missing(horserace)){
    if(horserace == TRUE){
      t.stat <- TRUE

      # (bootstrapping is not supported for horserace output)
      boot <- FALSE
    }
  }


  cluster.se <- FALSE
  # Checking if cluster standard errors requested
  if(!missing(cluster)){
    if(!is.null(cluster)){
      cluster.se <- TRUE

      library(multiwayvcov, quietly = TRUE)

      # Setting cluster name:
      if(!missing(cluster.vars.names)){
        cluster.vars.names <- cluster.vars.names[1]
      } else {
      cluster.vars.names <- cluster[1]
      }
    }
  }


  if(class(model)[1] == "lm"){
    model.summary.stat <- paste0(", Adjusted R-Squared = ", round(summary(model)$adj.r.squared, 3))
    model.type <- "OLS"
    model.description <- "OLS"
  } else { if(class(model)[1] == "glm"){
    model.summary.stat <- paste0(", AIC = ", round(model$aic, 3))

    model.type <- "GLM"
    model.description <- "GLM"
  } else { if(class(model)[2] == "glm"){

      model.type <- "GLM"
      model.description <- class(model)[1]

      model.summary.stat <- paste0(", AIC = ", round(model$aic, 3))

    } else {
    return(print("Did not recognize model type - at present, QuickCoefPlot can only work with lm, glm and zelig objects (and the latter two only experimentally)"))}
    }
  }

  # If robust standard errors requested, then:
if(robust == TRUE & cluster.se == FALSE & boot == FALSE){
  lines <- c(paste0(model.description, " Coefficient Estimates with 90 % and 95 % C.I.s based on robust S.E. "), paste0("\n (N = ",nobs(model), model.summary.stat, ")"))


  # Checking if clustered standard errors requested
  } else if(cluster.se == TRUE & boot == FALSE) {

    lines <- c(paste0(model.description, " Coefficient Estimates with 90 % and 95 % C.I.s based on ", ifelse(missing(cluster.vars.names), "Clustered S.E. ", paste0(cluster.vars.names, "-clustered S.E. "))), paste0("\n (N = ",nobs(model), model.summary.stat, ")"))


  # Checking if boot-strapped standard errors requested
  } else if(boot == TRUE){
    lines <- c(paste0(model.description, " Coefficient Estimates with 90 % and 95 % C.I.s based on ", boot.b, " Bootstrap Simulations"), paste0("\n (N = ",nobs(model), model.summary.stat, ")"))

   } else {
  # If neither then:
  lines <- c(paste0(model.description, " Coefficient Estimates with 90 % and 95 % C.I.s based on normal S.E. "), paste0("\n (N = ",nobs(model), model.summary.stat, ")"))

   }

  if(!missing(add.summary.lines)){
    lines <- c(lines, "\n", paste0(add.summary.lines, collapse = "\n"))
  }


  # Setting plot title to default if not specified.
  if(missing(plot.title)){
    plot.title <- ""
  }

  if(t.stat == FALSE){

    xlab <- paste(lines, collapse = " ")

    if(!missing(hide.summary.lines)){
      if(!is.null(hide.summary.lines)){
        xlab <- paste(lines[-hide.summary.lines], collapse = " ")
      }
    }

    # If horse-race not selected, extract standard error:
    extract <- 2
  } else {

    # If horse-race selected, extract t-value
    extract <- 3
    }

  if(boot == TRUE){

    # Extracting model data
    if(zelig == TRUE){
      if(length(model.frame(model)[1,1]) == 2){
    model.data <- cbind.data.frame(model.frame(model)[, 1][, 1], model.matrix(model)[, -1])
      } else {
      model.data <- cbind.data.frame(model.frame(model)[, 1], model.matrix(model)[, -1])
      }
    } else {
    model.data <- cbind.data.frame(model.frame(model)[, 1], model.matrix(model)[, -1])
    }

    n.vars <- dim(model.data)[2]
    colnames(model.data) <- paste0("var", 1:n.vars)

    se <- matrix(ncol = n.vars - 1, nrow = boot.b)

    # Fitting regressions and extracting coefficients
    se <-  t(sapply(1:boot.b, FUN = function(x) {

    if(model.type == "OLS"){
    boot.sample <- coeftest(lm(as.formula(paste0("var1 ~ ", paste0("var", 2:n.vars, collapse = " + "))), data = model.data[sample(1:nrow(model.data), nrow(model.data), replace = TRUE), ]))[-1, ifelse(extract == 2, 1, extract)]
    }

    if(model.type == "GLM"){

    if(zelig == TRUE){
    boot.sample <- coeftest(from_zelig_model(zelig(as.formula(paste0("var1 ~ ", paste0("var", 2:n.vars, collapse = " + "))), data = model.data[sample(1:nrow(model.data), nrow(model.data), replace = TRUE), ], model = zelig.model, cite = FALSE)))[-1, ifelse(extract == 2, 1, extract)]
    } else {
    boot.sample <- coeftest(glm(as.formula(paste0("var1 ~ ", paste0("var", 2:n.vars, collapse = " + "))), data = model.data[sample(1:nrow(model.data), nrow(model.data), replace = TRUE), ], family = model$family))[-1, ifelse(extract == 2, 1, extract)]
    }
    }

    if(length(boot.sample) == (n.vars -1)){
      return(boot.sample)

      # If some coefficients are missing, impute NA for these
    } else {

    container <- data.frame(matrix(ncol= n.vars -1))
    colnames(container) <- colnames(model.data)[-1]
    boot.sample <- data.frame(t(boot.sample))
    boot.sample <- merge(container, boot.sample, all = TRUE)[ 1, paste0("var", 2:n.vars)]
    return(setNames(as.numeric(boot.sample), colnames(boot.sample)))
    }
            }))

    # Sort coefficients in increasing order
    se <- apply(se,2, FUN = function(x){sort(x ,decreasing=F, na.last=TRUE)})

  } else {

if(robust == TRUE & cluster.se == FALSE){

      # Calculating robust standard errors
    se <- as.vector(as.numeric(coeftest(model, vcov = sandwich)[2:dim(coeftest(model))[1], extract]))


    } else if(cluster.se == TRUE){

      # Calculating clustered standard errors
      library(multiwayvcov, quietly = FALSE)

      if(zelig == TRUE){

       vcov.cluster <- tryCatch(cluster.vcov(model, zelig.data[complete.cases(all.vars(formula(model))), cluster]), error = function(x) {NULL})

      } else {
        vcov.cluster <- tryCatch(cluster.vcov(model, as.formula(paste("~ ", paste(cluster, collapse = "+")))), error = function(x) {NULL})
      }

        if(length(vcov.cluster) == 0){
          return("Error: Model and clustering variables(s) must both be in accessible common data frame")
        }
    se <- as.vector(as.numeric(coeftest(model, vcov. = vcov.cluster)[2:dim(coeftest(model))[1], extract]))

        } else {

      # Calculating normal standard errors
    se <- as.vector(coef(summary(model))[2:dim(coeftest(model))[1], extract])
    }


  }

estimate <- as.vector(as.numeric(model$coefficients[2:dim(coeftest(model))[1]]))


if(t.stat == FALSE){

    # If horse-race not selected:

  if(boot == FALSE){
    ci95 <- qnorm(1 - 0.05 / 2)
    ci90 <- qnorm(1 - 0.1 / 2)

    coef.plot <- cbind.data.frame(estimate,
                                  estimate + se*ci95,
                                  estimate - se*ci95,
                                  estimate - se*ci90,
                                  estimate + se*ci90)
  } else {


coef.plot <- cbind.data.frame(estimate,
                             se[round(boot.b*0.975), ],
                             se[max(1, round(boot.b*0.025)), ],
                             se[max(1, round(boot.b*0.050)), ],
                             se[round(boot.b*0.950), ])

# If we have missing coefficients, we need to account for the lower number in our confidence intervals:

if(length(colnames(se)[colSums(is.na(se)) > 0]) != 0){
  colnames(se) <- iv.vars.names
  missing.coefs <- colnames(se)[colSums(is.na(se)) > 0]

  for (i in missing.coefs){
  message(paste0("% Note: the variable '", i, "' has no observations in", " ", (summary(se[, i])[7])*100/boot.b, " percent of bootstrap samples", "\n"))

  coef.plot[match(i, colnames(se)), 2:5] <- c(se[round((boot.b-summary(se[, i])[7])*0.975)], se[max(1, round((boot.b-summary(se[, i])[7])*0.025))], se[max(1, round((boot.b-summary(se[, i])[7])*0.050))], se[round((boot.b-summary(se[, i])[7])*0.950)])
  }

}

}

colnames(coef.plot) <- c("est", "top", "bot", "lower", "upper")
coef.plot$vars <- factor(iv.vars.names, levels = iv.vars.names[length(iv.vars.names):1])

ylab <- ""
col <- "black"

## Checking if legend requested

# Setting default
legend.position <- "none"
if(!missing(legend.on)){
  if(legend.on == TRUE){
    legend.position <- "bottom"
  }
}

## Checking if only some variables to be reported
if(!missing(include.only)){
  if(!is.null(include.only)){
    coef.plot <- coef.plot[include.only, ]
  }
}

coef.plot$col <- "p>0.1, "
coef.plot$col <- ifelse(coef.plot$est > 0 & coef.plot$lower > 0, "p<0.1, ", ifelse(coef.plot$est < 0 & coef.plot$upper < 0, "p<0.1, ", coef.plot$col))
coef.plot$col <- ifelse(coef.plot$est > 0 & coef.plot$bot > 0, "p<0.05, ", ifelse(coef.plot$est < 0 & coef.plot$top < 0, "p<0.05, ", coef.plot$col))

if(!missing(colors.off)){
  if(colors.off == TRUE){
    coef.plot$col <- "p<0.05, "

    # Also turns off color-indicating legend
    legend.position <- "none"
  }
}


# Checking if estimates to be plotted:


if(!missing(boot.plot.est) & boot == TRUE){
  if(boot.plot.est == TRUE){

    se <- as.data.frame(se)

    if(!missing(include.only)){
      if(!is.null(include.only)){
        se <- se[, include.only]
        iv.vars.names <- iv.vars.names[include.only]


        }
    }


    if(NCOL(se) == 1){
      se.long <- cbind.data.frame(b.est = se, vars= iv.vars.names)
      se.long <- merge(se.long, coef.plot[, c("vars", "col", "est")], by="vars")
    } else {

    colnames(se) <- factor(iv.vars.names, levels = iv.vars.names[length(iv.vars.names):1])

    rownames(se) <- 1:nrow(se)
    se.long <- reshape(se,
                       varying = colnames(se),
                       v.names = "b.est",
                       timevar = "vars",
                       times = colnames(se),
                       direction = "long")
    rownames(se.long) <- 1:nrow(se.long)

    coef.plot$order <- 1:nrow(coef.plot)
    se.long <- merge(se.long, coef.plot[, c("vars", "col", "est", "order")], by = "vars")

    # Ensuring ordering respected:
    se.long <- se.long[order(se.long$order), ]
    se.long$order <- NULL

    se.long$vars <- as.character(se.long$vars)
    se.long$vars <- factor(se.long$vars, levels=rev(unique(se.long$vars)))
      }
    }
}


    require(ggplot2)
    p <- ggplot(coef.plot, aes(x=vars, y=est))+coord_fixed(ratio=1)

      # Plotting boot estimates if requested
    if(!missing(boot.plot.est)){
      if(boot.plot.est == TRUE){
        se.long$dist_from_est <- abs(abs(se.long$b.est)-abs(se.long$est))
       p <- p + geom_jitter(data = se.long, aes(x= vars, y = b.est, color=col), alpha = max(min(10/boot.b, 0.2), 0.21), width =       0.5*pnorm(-se.long$dist_from_est/(NROW(coef.plot)+4), 0, 0.5*sd(se.long$dist_from_est, na.rm=TRUE)))
           }
    }

    # Checking if requested no lines (i.e. just estimates)
    if(!missing(plot.lines)){

      # Do not plot lines, else:

      if(plot.lines != FALSE){
      p <- p +geom_linerange(aes(color=col, ymin=lower, ymax=upper),size=1.2)+ geom_linerange(aes(x=vars, ymin=bot, ymax=top, color = col))

      }
      } else {

      # Plot lines
      p <- p +geom_linerange(aes(color=col, ymin=lower, ymax=upper),size=1.2)+ geom_linerange(aes(x=vars, ymin=bot, ymax=top, color = col))

    }


    p <- p+geom_point(aes(color=col),size=3)+
      coord_flip() +
      theme_bw() + ggtitle(plot.title) +
      theme(legend.position=legend.position, legend.title = element_blank()) + scale_size_continuous() +
      ylab(xlab) +
      xlab(ylab)  + geom_hline(yintercept = 0, linetype=2)+scale_colour_manual(values = c("p>0.1, " = "skyblue", "p<0.1, " = "#0072B2", "p<0.05, " = "black"))

    # Adding plot margin
    if(!missing(plot.margin)){
      p <- p + theme(plot.margin = unit(plot.margin, "cm"))
    } else {
      p <- p+ theme(plot.margin = unit(c(1,1,1,1), "cm"))
    }

    # Adding custom text size
    if(!missing(text.size)){
    p <- p+ theme(text = element_text(size=text.size), axis.text=element_text(size=text.size),axis.title=element_text(size=text.size), title = element_text(size=text.size))
    }

    if(!missing(xlim)){
      p <- p+ylim(min = xlim[1], max = xlim[2])
    }
    suppressWarnings(return(print(p)))}

  # If horserace selected
else {

    lines[3] <- lines[2]
    lines[2] <- "\n Negative/Positive effects in blue/red. \n"
    lines[1] <- ("t-values of OLS Coefficient Estimates based on robust S. E. ")
    xlab <- paste(lines, collapse = " ")

    if(!missing(hide.summary.lines)){
      if(!is.null(hide.summary.lines)){
        xlab <- paste(lines[-hide.summary.lines])
      }
    }

      t.test <- se
      estimate <- ifelse(estimate > 0, "red", "blue")

    coef.plot <- cbind.data.frame(estimate, t.test)
    colnames(coef.plot) <- c("est", "t")
    coef.plot$vars <- factor(iv.vars.names, levels = iv.vars.names[length(iv.vars.names):1])

    ylab <- ""
    col <- "black"

    if(!missing(include.only)){
      if(!is.null(include.only)){
        coef.plot <- coef.plot[include.only, ]
      }
    }

    require(ggplot2)
    p <- ggplot(coef.plot, aes(x=vars, y=t, col=est))+coord_fixed(ratio=1)
    p <- p +geom_point(size=3)+
      coord_flip() +
      theme_bw() + ggtitle(plot.title) +
      theme(legend.position="none") + scale_size_continuous() +
      ylab(xlab) + scale_color_manual(values = c("blue", "red"))+theme(plot.margin = unit(c(0,1,0.2,0), "inch"))+
      xlab(ylab)  # aspect.ratio = 0.8)

    # Adding plot margin
    if(!missing(plot.margin)){
      p <- p + theme(plot.margin = unit(plot.margin, "cm"))
    } else {
      p <- p+ theme(plot.margin = unit(c(1,5,1,1), "cm"))
    }

    # Adding custom text size
    if(!missing(text.size)){
      p <- p+ theme(text = element_text(size=text.size), axis.text=element_text(size=text.size),axis.title=element_text(size=text.size), title = element_text(size=text.size))
    }

    if(!missing(xlim)){
      p <- p+ylim(min = xlim[1], max = xlim[2])
    }
    return(p)

  }

}

