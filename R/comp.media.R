#' Title
#'
#' @param ...                   data/variables to work with
#' @param by					          default value: NULL
#' @param decimals				      default value: 2
#' @param DEBUG					        default value: FALSE
#' @param show.vars				      default value: TRUE
#' @param show.by				        default value: TRUE
#' @param show.groups			      default value: TRUE
#' @param show.n.valid			    default value: TRUE
#' @param show.n.missing		    default value: TRUE
#' @param show.min				      default value: TRUE
#' @param show.max				      default value: TRUE
#' @param show.mean				      default value: TRUE
#' @param show.sd				        default value: TRUE
#' @param show.median			      default value: TRUE
#' @param show.IRQ				      default value: TRUE
#' @param show.p.norm			      default value: TRUE
#' @param show.p.norm.exact		  default value: FALSE
#' @param show.nor.test			    default value: TRUE
#' @param show.is.normal		    default value: TRUE
#' @param show.interpretation	  default value: TRUE
#' @param lang					        default value: "es"
#' @param show.global			      default value: TRUE
#'
#' @return
#' @export
#'
#' @examples
comp.media <- function(...,by=NULL,decimals=2,DEBUG=FALSE,show.vars=TRUE,
                  show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                  show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                  show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                  show.IRQ=TRUE,show.p.norm=TRUE,show.p.norm.exact=FALSE,
                  show.nor.test=TRUE,show.is.normal=TRUE,
                  show.interpretation=TRUE,lang="es",show.global=TRUE, show.desc = TRUE,
                  show.p.exact = FALSE, sig.p = 0.05, sig.p.2 = 0.01, small.p = 0.001){

  library("dplyr", quietly = TRUE)
  library("nortest", quietly = TRUE) #for lillie.test
  library("car", quietly = TRUE)     #for leveneTest

  FORMA.DATOS <- feR:::forma.datos(..., by = by, DEBUG = FALSE, DEBUG.CALL = FALSE)
  HAS.BY <- attr(FORMA.DATOS, "HAS.BY")
  DATOS <- attr(FORMA.DATOS, "DATA")
  desc.media <- feR::media(..., by=by,
                             decimals=decimals,
                             DEBUG=DEBUG,
                             show.vars=show.vars,
                             show.by=show.by,
                             show.groups=show.groups,
                             show.n.valid=show.n.valid,
                             show.n.missing=show.n.missing,
                             show.min=show.min,
                             show.max=show.max,
                             show.mean=show.mean,
                             show.sd=show.sd,
                             show.median=show.median,
                             show.IRQ=show.IRQ,
                             show.p.norm=show.p.norm,
                             show.p.norm.exact=show.p.norm.exact,
                             show.nor.test=show.nor.test,
                             show.is.normal=show.is.normal,
                             show.interpretation=show.interpretation,
                             lang=lang,
                             show.global=show.global)
  desc.groups <- attr(desc.media, "RESULT.GROUP")
  if(missing(small.p)) small.p <- 10^((decimals + 1)*-1)
  if (HAS.BY) {
    BY.DATA <- attr(FORMA.DATOS, "BY.DATA")
    if (DEBUG) {
      cat("\n[comp.media] BY.DATA:\n")
      print(BY.DATA)
      cat("\n")
    }
    for(var in names(DATOS)) {
      if (DEBUG) cat("\n[comp.media] VAR:",var, "\n")
      for(by.var in names(BY.DATA)) {
        if (DEBUG) cat("\n[comp.media] BY.VAR:",by.var, "\n")
        by.values <- as.factor(BY.DATA %>% pull(by.var))
        var.values <- DATOS %>% pull(var)
        total.levels <- length(levels(by.values))
        normal.levels <- all(desc.groups$is.normal[(desc.groups$var == var) & (desc.groups$by == by.var)])


        if(normal.levels) {
          if (total.levels == 2) {
            comp.m <- t.test(var.values ~ by.values)
            if(show.p.exact) comp.p <- comp.m$p.value
            else comp.p <- ifelse(comp.m$p.value < small.p, paste0("<",small.p), round(comp.m$p.value, digits = decimals + 1))
            result.comp <- data.frame(var = var, by = by.var,
                                      test = "Welch t-test",
                                      stat = comp.m$statistic, p.value = comp.p)

          }
          else if (total.levels > 2) {
            comp.m <- aov(var.values ~ by.values)
            comp.m.summary <- summary(comp.m)
            comp.m.p.value <- comp.m.summary[[1]]$`Pr(>F)`[1]
            comp.m.stat <- round(comp.m.summary[[1]]$`F value`[1], digits = decimals)

            if(show.p.exact) {
              comp.p <- comp.m.p.value
            } else {
              comp.p <- ifelse(comp.m.p.value < small.p, paste0("<",small.p), round(comp.m.p.value, digits = decimals + 1))
            }
            var.test.p <- car::leveneTest(var.values, group = by.values)$`Pr(>F)`[1]
            homocedasticity.p <- ifelse(var.test.p < small.p, paste0("<",small.p), round(var.test.p, digits = decimals + 1))
            homocedasticity <- ifelse(var.test.p < sig.p, FALSE, TRUE)
            result.comp <- data.frame(var = var, by = by.var,
                                      homocedasticity = homocedasticity,
                                      test = "ANOVA",
                                      stat = comp.m.stat,
                                      p.value = comp.p)


            if(homocedasticity == F){
              if (exists("result.post.hoc")) result.post.hoc <- NULL
              result.post.hoc <- as.data.frame(TukeyHSD(comp.m)[[1]])
              empty.fill <- rep(" ", nrow(result.post.hoc)-1)
              result.post.hoc$var <- c(var, empty.fill)
              result.post.hoc$by <-  c(by.var, empty.fill)
              result.post.hoc$group.pairs <- rownames(result.post.hoc)
              result.post.hoc$diff <- round(result.post.hoc$diff, digits = decimals)
              result.post.hoc$lwr <- round(result.post.hoc$lwr, digits = decimals)
              result.post.hoc$upr <- round(result.post.hoc$upr, digits = decimals)
              result.post.hoc$p.value <- ifelse(result.post.hoc$`p adj` < small.p, paste("<", small.p), round(result.post.hoc$`p adj`, digits = decimals+1))
              result.post.hoc$p.symbols[result.post.hoc$`p adj`>= sig.p] <- "-"
              result.post.hoc$p.symbols[result.post.hoc$`p adj`< sig.p] <- "*"
              result.post.hoc$p.symbols[result.post.hoc$`p adj`< sig.p.2] <- "**"
              result.post.hoc$p.symbols[result.post.hoc$`p adj`< small.p] <- "***"
              result.post.hoc$post.hoc <- c("Tukey HSD", empty.fill)
              result.post.hoc <- result.post.hoc[,c("var","by","group.pairs","diff","lwr","upr","p.value","p.symbols","post.hoc")]
              rownames(result.post.hoc) <- NULL


            } else {
              # if (exists("result.post.hoc")) result.post.hoc <- NULL
              gh <- PMCMRplus::gamesHowellTest(comp.m)
              for(x in dimnames(gh$p.value)[[1]]){
                for(y in dimnames(gh$p.value)[[2]]){
                  if(x==y) next
                  temp.group <- paste(x,"-",y)
                  temp.p.value <- gh$p.value[x,y]
                  temp.stat.value <- gh$statistic[x,y]


                  temp.res <- data.frame(stat.value = temp.stat.value, p.value = temp.p.value)
                  rownames(temp.res) <- temp.group
                  if(!exists("result.post.hoc")) result.post.hoc <- temp.res
                  else result.post.hoc <- rbind(temp.res, result.post.hoc)
                }
              }
              empty.fill <- rep(" ", nrow(result.post.hoc)-1)
              result.post.hoc$p.symbols[result.post.hoc$p.value >= sig.p] <- "-"
              result.post.hoc$p.symbols[result.post.hoc$p.value < sig.p] <- "*"
              result.post.hoc$p.symbols[result.post.hoc$p.value < sig.p.2] <- "**"
              result.post.hoc$p.symbols[result.post.hoc$p.value < small.p] <- "***"
              result.post.hoc$group.pairs <- rownames(result.post.hoc)
              result.post.hoc$p.value <- ifelse(result.post.hoc$p.value < small.p, paste("<", small.p), round(result.post.hoc$p.value, digits = decimals+1))
              result.post.hoc$post.hoc <- c("Games-Howell", empty.fill )
              result.post.hoc$var <- c(var, empty.fill)
              result.post.hoc$by <-  c(by.var, empty.fill)
              result.post.hoc <- result.post.hoc[,c("var","by","group.pairs","p.value","p.symbols","post.hoc")]
              rownames(result.post.hoc) <- NULL
            }





          }
          else if (total.levels < 2) {
            cat("\n[comp.media] Not enough levels in factor variable: ", by.var)
          }
        } else {
          if (total.levels == 2) {
            comp.m <- wilcox.test(var.values ~ by.values)
            if(show.p.exact) comp.p <- comp.m$p.value
            else comp.p <- ifelse(comp.m$p.value < small.p, paste0("<",small.p), round(comp.m$p.value, digits = decimals + 1))
            result.comp <- data.frame(var = var, by = by.var,
                                      test = "Wilcoxon-Mann-Whitney",
                                      stat = comp.m$statistic, p.value = comp.p)
          }
          else if (total.levels > 2) {
            comp.m <- kruskal.test(var.values ~ by.values)
            if(show.p.exact) comp.p <- comp.m$p.value
            else comp.p <- ifelse(comp.m$p.value < small.p, paste0("<",small.p), round(comp.m$p.value, digits = decimals + 1))
            result.comp <- data.frame(var = var, by = by.var,
                                      test = "Kruskal-Wallis",
                                      stat = comp.m$statistic, p.value = comp.p)
          }
          else if (total.levels < 2) {
            cat("\n[comp.media] nnNot enough levels in factor variable: ", by.var)
          }
        }
      }
    }
  }
  if(exists("result.comp")) {
    result.comp.final <- result.comp
    class(result.comp.final) <- append("feR.comp.media", class(result.comp.final))
    attr(result.comp.final, "RESULT.COMP") <- result.comp
    if(exists("result.post.hoc")) attr(result.comp.final, "RESULT.POST_HOC") <- result.post.hoc
    attr(result.comp.final, "SHOW.INTERPRETATION") <- show.desc
    attr(result.comp.final, "INTERPRETATION") <- "Not Available"
    attr(result.comp.final, "SHOW.DESC") <- show.desc
    if (show.desc) {
      if (exists("desc.media")) attr(result.comp.final, "RESULT.DESC") <- desc.media
      else attr(result.comp.final, "RESULT.DESC") <- "Not Available"
    }
    return(result.comp.final)
  }
  # return(desc.media)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.feR.comp.media <- function(x) {
  res.desc <- attr(x,"RESULT.DESC")
  res.comp <- attr(x,"RESULT.COMP")
  res.post.hoc <- attr(x, "RESULT.POST_HOC")
  show.desc <- attr(x,"SHOW.INTERPRETATION")
  show.interpretation <- attr(x,"SHOW.INTERPRETATION")
  interp <- attr(x, "INTERPRETATION")

  if (show.interpretation) cat("\n",interp,"\n")
  if (show.desc & !is.null(res.desc)) print(res.desc)
  if (exists("res.comp") & !is.null(res.comp)) print(knitr::kable(res.comp))
  if (exists("res.post.hoc") & !is.null(res.post.hoc)) print(knitr::kable(res.post.hoc))
}

