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
                  show.p.exact = FALSE){

  library("dplyr", quietly = TRUE)
  library("nortest", quietly = TRUE)

  FORMA.DATOS <- forma.datos(..., by = by, DEBUG = FALSE, DEBUG.CALL = FALSE)
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
  small.p <- 10^((decimals + 1)*-1)
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
        # for(by.level in levels(by.values)){
        #   var.values <- DATOS %>% pull(var)
        #   var.values <- var.values[by.values == by.level]
        #   temp.mean <- .feR.media(var.values, var, decimals = decimals)
        #   if (is.data.frame(temp.mean)){
        #     res <- cbind(var = var, by = by.var, group = by.level, temp.mean)
        #     if (!exists("result.group")) result.group <- res
        #     else result.group <- rbind(result.group, res)
        #   }
        # }
        total.levels <- length(levels(by.values))
        print("LEVELS")
        if(all(desc.groups$is.normal[(desc.groups$var == var) & (desc.groups$by == by.var)])) {
          if (total.levels == 2) {
            print("DOS")
          }
          else if (total.levels > 2) {
            print("MAS DE DOS")
          }
          else if (total.levels < 2) {
            cat("\n[comp.media] Not enough levels in factor variable: ", by.var)
          }
        } else {
          if (total.levels == 2) {
            print("nnDOS")
            comp.m <- wilcox.test(var.values ~ by.values)
            if(show.p.exact) comp.p <- comp.m$p.value
            else comp.p <- ifelse(comp.m$p.value < small.p, paste0("<",small.p), round(comp.m$p.value, digits = decimals + 1))
            result.comp <- data.frame(var = var, by = by.var,
                                      test = "Wilcoxon-Mann-Whitney",
                                      stat = comp.m$statistic, p.value = comp.p)
          }
          else if (total.levels > 2) {
            print("nnMAS DE DOS")
          }
          else if (total.levels < 2) {
            cat("\n[comp.media] nnNot enough levels in factor variable: ", by.var)
          }
        }
      }
    }
  }
  # return(desc.media)
}
