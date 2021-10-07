#' comp.media
#'
#' @param ...
#' @param by                    default value: NULL
#' @param decimals              default value: 2
#' @param DEBUG                 default value: FALSE
#' @param DEBUG.FORMA           default value: FALSE
#' @param DEBUG.CALL            default value: FALSE
#' @param show.vars             default value: TRUE
#' @param show.by               default value: TRUE
#' @param show.groups           default value: TRUE
#' @param show.n.valid          default value: TRUE
#' @param show.n.missing        default value: TRUE
#' @param show.min              default value: TRUE
#' @param show.max              default value: TRUE
#' @param show.mean             default value: TRUE
#' @param show.sd               default value: TRUE
#' @param show.median           default value: TRUE
#' @param show.IRQ              default value: TRUE
#' @param show.se               default value: TRUE
#' @param show.ci.upper         default value: TRUE
#' @param show.ci.lower         default value: TRUE
#' @param show.p.norm           default value: TRUE
#' @param show.p.norm.exact     default value: FALSE
#' @param show.nor.test         default value: TRUE
#' @param show.is.normal        default value: TRUE
#' @param show.interpretation   default value: FALSE
#' @param lang                  default value: "es"
#' @param show.global           default value: TRUE
#' @param p.sig                 default value: 0.05
#' @param p.sig.small           default value: 0.01
#' @param p.sig.very.small      default value: 0.001
#' @param ci                    default value: 0.95
#' @param show.post.hoc         default value: TRUE
#' @param show.desc             default value: TRUE
#' @param paired                default value: TRU
#'
#'
#' @return
#' @export
#'
#' @examples
comp.media <- function(...,by=NULL,decimals=2,
                       DEBUG=FALSE, DEBUG.FORMA=FALSE, DEBUG.CALL=FALSE,
                       show.vars=TRUE,show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                       show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                       show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                       show.IRQ=TRUE, show.se = TRUE, show.ci.upper = TRUE, show.ci.lower = TRUE,
                       show.p.norm=TRUE,show.p.norm.exact=FALSE,
                       show.nor.test=TRUE,show.is.normal=TRUE,
                       show.interpretation=FALSE,lang="es",show.global=TRUE,
                       p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                       ci = 0.95,
                       show.post.hoc = TRUE,
                       show.desc=TRUE, paired = T) {


  medias(...,
         by=by,
         decimals=decimals,
         DEBUG=DEBUG,
         DEBUG.FORMA=DEBUG.FORMA,
         DEBUG.CALL=DEBUG.CALL,
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
         show.se=show.se,
         show.ci.upper=show.ci.upper,
         show.ci.lower=show.ci.lower,
         show.p.norm=show.p.norm,
         show.p.norm.exact=show.p.norm.exact,
         show.nor.test=show.nor.test,
         show.is.normal=show.is.normal,
         show.interpretation=show.interpretation,
         lang=lang,
         show.global=show.global,
         p.sig=p.sig,
         p.sig.small=p.sig.small,
         p.sig.very.small=p.sig.very.small,
         ci=ci,
         comp=TRUE,
         show.post.hoc=show.post.hoc,
         show.desc=show.desc,
         paired=paired)
}

#' medias
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
#' @param show.se 				      default value: FALSE
#' @param show.ci.upper		      default value: FALSE
#' @param show.ci.lower		      default value: FALSE
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
medias <- function(...,by=NULL,decimals=2,
                   DEBUG=FALSE, DEBUG.FORMA=FALSE, DEBUG.CALL=FALSE,
                   show.vars=TRUE,show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                   show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                   show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                   show.IRQ=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                   show.p.norm=TRUE,show.p.norm.exact=FALSE,
                   show.nor.test=TRUE,show.is.normal=TRUE,
                   show.interpretation=FALSE,lang="es",show.global=TRUE,
                   p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                   comp=FALSE,
                   show.post.hoc = TRUE,
                   show.desc=TRUE, paired = T){

  library("dplyr", quietly = TRUE)
  library("nortest", quietly = TRUE)
  library("PMCMRplus", quietly = TRUE)

  FORMA.DATOS <- forma.datos(..., by = by, DEBUG = DEBUG.FORMA, DEBUG.CALL = DEBUG.CALL)
  HAS.BY <- attr(FORMA.DATOS, "HAS.BY")
  DATOS <- attr(FORMA.DATOS, "DATA")
  if (DEBUG) {
    cat("\n[media] FORMA.DATOS:\n")
    print(FORMA.DATOS)
    cat("\n")
  }
  # print(FORMA.DATOS)DATOS
  if (!HAS.BY | show.global == TRUE) {
    for(var in names(DATOS)) {
      if (DEBUG) cat("\n[media] VAR:",var,"\n")
      if(is.na(var)) {
        print("Variable name is empty")
        return(NA)
      }
      var.values <- DATOS %>% pull(var)
      temp.mean <- .feR.media(var.values, var,decimals = decimals)
      if(length(temp.mean) < 2) if (is.na(temp.mean)) return(NA)
      if (is.data.frame(temp.mean)){
        if (!HAS.BY) res.global <- cbind(var = var, temp.mean)
        else res.global <- cbind(var = var, by = "-", group = "-", temp.mean)
        if (!exists("result.global")) result.global <- res.global
        else result.global <- rbind(result.global, res.global)
      }
    }
  }
  if (HAS.BY) {

    BY.DATA <- attr(FORMA.DATOS, "BY.DATA")
    if (DEBUG) {
      cat("\n[media] BY.DATA:\n")
      print(BY.DATA)
      cat("\n")
    }
    for(var in names(DATOS)) {
      if (DEBUG) cat("\n[media] VAR:",var, "\n")
      for(by.var in names(BY.DATA)) {
        if (DEBUG) cat("\n[media] BY.VAR:",by.var, "\n")
        by.values <- as.factor(BY.DATA %>% pull(by.var))
        var.values <- DATOS %>% pull(var)

        for(by.level in levels(by.values)){
          var.values.groups <- var.values[by.values == by.level]
          temp.mean <- .feR.media(var.values.groups, var, decimals = decimals,
                                  p.sig = p.sig, p.sig.small = p.sig.small,
                                  p.sig.very.small = p.sig.very.small, ci = ci)
          if (is.data.frame(temp.mean)){
            res <- cbind(var = var, by = by.var, group = by.level, temp.mean)
            if (!exists("temp.result.group")) temp.result.group <- res
            else temp.result.group <- rbind(temp.result.group, res)
          }
        }

        if (is.na(temp.result.group)) return(NA)

        if(comp == TRUE) {
          is.normal = all(temp.result.group$is.normal)
          temp.comp <- .feR.comp.media.unpaired(var.values, var, by.values, by.var, decimals = decimals,
                                                p.sig = p.sig, p.sig.small = p.sig.small, p.sig.very.small = p.sig.very.small, is.normal = is.normal)
          if(!exists("result.comp")) result.comp <- temp.comp
          else result.comp <- rbind(result.comp, temp.comp)
          rm(list="temp.comp")
        }

        if (!exists("result.group")) result.group <- temp.result.group
        else result.group <- rbind(result.group, temp.result.group)
        rm(list="temp.result.group")
      }
    }
  }


  if (exists("result.global")) result.temp <- result.global
  if (exists("result.temp")){
    if (HAS.BY & exists("result.group")) result.temp <- rbind(result.temp, result.group)
  } else if (HAS.BY & exists("result.group")) result.temp <- result.group


  if (!exists("result.temp")) return(NA)
  else {
    if(!show.vars) result.temp$var <- NULL
    if(!show.by) result.temp$by <- NULL
    if(!show.groups) result.temp$groups <- NULL
    if(!show.n.valid) result.temp$n.valid <- NULL
    if(!show.n.missing) result.temp$n.missing <- NULL
    if(!show.min) result.temp$min <- NULL
    if(!show.max) result.temp$max <- NULL
    if(!show.mean) result.temp$mean <- NULL
    if(!show.sd) result.temp$sd <- NULL
    if(!show.median) result.temp$median <- NULL
    if(!show.IRQ) result.temp$IRQ <- NULL
    if(!show.se) result.temp$se <- NULL
    if(!show.ci.upper) result.temp$ci.upper <- NULL
    if(!show.ci.lower) result.temp$ci.lower <- NULL
    if(!show.p.norm) result.temp$p.norm <- NULL
    if(!show.p.norm.exact) result.temp$p.norm.exact <- NULL
    if(!show.nor.test) result.temp$nor.test <- NULL
    if(!show.is.normal) result.temp$is.normal <- NULL


    if(exists("result.global")) attr(result.temp, "RESULT.GLOBAL") <- result.global
    if(exists("result.group")) attr(result.temp, "RESULT.GROUP") <- result.group
    if(show.interpretation) attr(result.temp, "INTERPRETATION") <- .help.desc.media(result.temp, lang = lang, code = "HELP")

    class(result.temp) <- append("feR.media",class(result.temp))


    if(comp) {
      if(exists("result.comp")) {
        result.final = result.comp
        # attr(result.final,"RESULT.POST.HOC") attr(result.comp,"RESULT.POST.HOC")
      }
      else {
        result.final <- "No comparison result has been found"
      }
      attr(result.final, "SHOW.POST.HOC") <- show.post.hoc
      attr(result.final, "SHOW.DESCRIPTIVES") <- show.desc
      attr(result.final, "RESULT.DESCRIPTIVES") <- result.temp
      attr(result.final, "INTERPRETATION") <- "No interpretation yet"
      class(result.final) <- append("feR.comp.media", class(result.comp))

    } else {
      result.final <- result.temp
    }
    attr(result.final, "SHOW.INTERPRETATION") <- show.interpretation
    return(result.final)
  }
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.feR.media <- function(x) {
  res.global <- attr(x,"RESULT.GLOBAL")
  res.grupo <- attr(x, "RESULT.GROUP")
  show.interpretation <- attr(x,"SHOW.INTERPRETATION")
  interp <- attr(x, "INTERPRETATION")


  if (is.null(show.interpretation)) show.interpretation = FALSE
  if (show.interpretation) cat("\n",interp,"\n")
  if (exists("res.global") & !is.null(res.global)) print(knitr::kable(res.global))
  if (exists("res.grupo") & !is.null(res.grupo)) print(knitr::kable(res.grupo))

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
  show.desc <- attr(x,"SHOW.DESCRIPTIVES")
  if(show.desc) res.desc <- attr(x,"RESULT.DESCRIPTIVES")

  res.comp <- attr(x, "RESULT.COMP")
  show.post.hoc <- attr(x, "SHOW.POST.HOC")
  show.interpretation <- attr(x,"SHOW.INTERPRETATION")
  interp <- attr(x, "INTERPRETATION")
  res.post.hoc <- attr(x, "RESULT.POST.HOC")

  if (show.interpretation) cat("\n",interp,"\n")
  if (show.desc) {
    attr(res.desc, "SHOW.INTERPRETATION") <- FALSE
    print(res.desc)
  }

  print(knitr::kable(x, row.names = FALSE))
  if (show.post.hoc & !is.null(res.post.hoc)) print(knitr::kable(res.post.hoc, row.names = FALSE))
}

.help.desc.media <- function(x, lang="es", code = ""){
  if (lang == "es") {
    if (code == "HELP") {
      text = "
      \nLas funcion devuelve una tabla con las siguientes columnas:
      \n
      \n* n.valid: cuantas observaciones teneis (valores no perdidos)
      \n* n.missing: cuantas respuestas perdidas teneis
      \n* min: valor mínimo
      \n* max: valor máximo
      \n* mean: media
      \n* sd: desviación estandar
      \n* median: mediana
      \n* IQR: rango intercuartílico
      \n* p.normal: valor p de la prueba de normalidad
      \n* nor.test: prueba usada para la normalidad
      \n  - SW: Shapiro-Wilks
      \n  - Lille (KS): Kolmogorov-Smirnov con la corrección de Lilliefors
      \n* p.normal.exact: valor p de la prueba de normalidad exacto (sin redondeo)
      \n* is.normal:
      \n  - TRUE -> signfica que la variable **ES** normal
      \n  - FALSE-> significa que la variable **NO** es normal
      \n
      \nSi la variable ES normal la describimos como:
      \nLa edad media de los participantes era de XX años con una desviación estandar (DE) de xxx.
      \n
      \nSi la variable NO es normal la describimos como:
      \nLa mediana de edad de los participantes era XXX años y su rango intercuartílico es igual a xxxx
      \n"
    }
  }
  else {
    if (code == "HELP") {
      text ="
      \nThe mean function returns a data.frame with these columns:
      \n
      \n* n.valid: number of valid (not missing) observations
      \n* n.missing: number of missing observations
      \n* min: minimun
      \n* max: maximun
      \n* mean: mean value
      \n* sd: standard deviation
      \n* median: median
      \n* IQR: interquanntile range
      \n* p.normal: p value for normality test
      \n* nor.test: test used to check normality
      \n  - SW: Shapiro-Wilks
      \n  - Lille (KS): Lilliefor's correction of Kolmogorov-Smirnov
      \n* p.normal.exact: p value for normality test (without rounding)
      \n* is.normal:
      \n  - TRUE -> the variable **IS** normal (follows a normal distribution)
      \n  - FALSE-> the variable is **NOT** normal (doesn't follow a normal distribution)
      \n
      \nThe the variable follows a normal distribution we can describe it as:
      \nThe mean age for the participant was XXX years with a stardard deviation (SD) of xxx.
      \n
      \nIf the varible doesn't follow a normal distribution, we use median:
      \nThe median age for the participants was XXX years, with an interquanntile range of XXXX.
      \n"
    }
  }
  if (exists("text")) return(text)
}


.feR.comp.media.paired <- function(x.values, x.name, by.values, by.name, decimals = 2, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, is.normal = TRUE) {

  if(!is.numeric(by.values)) by.values <- as.numeric(by.values)

  if(is.normal){

  } else {

  }

}

.feR.comp.media.unpaired <- function(x.values, x.name, by.values, by.name, decimals = 2, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, is.normal = TRUE) {

  if (!is.factor(by.values)) by.values <- factor(by.values)
  total.levels <- length(levels(by.values))
  # cat("\n LEVELS BY -->", total.levels)


  if(is.normal) {
    if (total.levels == 2) {
      comp.m <- t.test(x.values ~ by.values)
      comp.m.p.value <- round(comp.m$p.value, digits = decimals)
      if (comp.m.p.value == p.sig ) comp.m.p.value <- round(comp.m$p.value, digits = decimals+1)

      comp.p <- ifelse(comp.m$p.value < p.sig.very.small, paste0("<",p.sig.very.small), comp.m.p.value)
      result.comp <- data.frame(var = x.name, by = by.name,
                                test = "Welch t-test",
                                stat = comp.m$statistic,
                                p.value = comp.p,
                                p.exact = comp.m.p.value)

    }
    else if (total.levels > 2) {
      comp.m <- aov(x.values ~ by.values)
      comp.m.summary <- summary(comp.m)
      comp.m.p.value <- comp.m.summary[[1]]$`Pr(>F)`[1]
      comp.m.stat <- round(comp.m.summary[[1]]$`F value`[1], digits = decimals)


      comp.m.p.value <- round(comp.m.p.value, digits = decimals)
      if (comp.m.p.value == p.sig ) comp.m.p.value <- round(comp.m.p.value, digits = decimals+1)

      comp.p <- ifelse(comp.m.p.value < p.sig.very.small, paste0("<",p.sig.very.small), comp.m.p.value)

      var.test.p <- car::leveneTest(x.values, group = by.values)$`Pr(>F)`[1]

      var.test.p.value <- round(var.test.p, digits = decimals)
      if (var.test.p.value == p.sig ) var.test.p.value <- round(var.test.p, digits = decimals+1)

      homocedasticity.p <- ifelse(var.test.p < p.sig.very.small, paste0("<",p.sig.very.small), var.test.p.value)
      homocedasticity <- ifelse(var.test.p < p.sig, FALSE, TRUE)
      result.comp <- data.frame(var = x.name, by = by.name,
                                homocedasticity = homocedasticity,
                                test = "ANOVA",
                                stat = comp.m.stat,
                                p.value = comp.p,
                                p.exact = comp.m.p.value)


      if(homocedasticity == F){
        if (exists("result.post.hoc")) result.post.hoc <- NULL
        result.post.hoc <- as.data.frame(TukeyHSD(comp.m)[[1]])
        empty.fill <- rep(" ", nrow(result.post.hoc)-1)
        result.post.hoc$var <- c(x.name, empty.fill)
        result.post.hoc$by <-  c(by.name, empty.fill)
        result.post.hoc$group.pairs <- rownames(result.post.hoc)
        result.post.hoc$diff <- round(result.post.hoc$diff, digits = decimals)
        result.post.hoc$lwr <- round(result.post.hoc$lwr, digits = decimals)
        result.post.hoc$upr <- round(result.post.hoc$upr, digits = decimals)

        post.hoc.p.value <- round(result.post.hoc$`p adj`, digits = decimals)
        if (post.hoc.p.value == p.sig) post.hoc.p.value <- round(result.post.hoc$`p adj`, digits = decimals+1)

        result.post.hoc$p.value <- ifelse(result.post.hoc$`p adj` < p.sig.very.small, paste("<", p.sig.very.small), post.hoc.p.value)
        result.post.hoc$p.exact <- post.hoc.p.value
        result.post.hoc$p.symbols[post.hoc.p.value>= p.sig] <- ""
        result.post.hoc$p.symbols[post.hoc.p.value< p.sig] <- "*"
        result.post.hoc$p.symbols[post.hoc.p.value< p.sig.small] <- "**"
        result.post.hoc$p.symbols[post.hoc.p.value< p.sig.very.small] <- "***"
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

            temp.p.value <- round(temp.p.value, digits = decimals)
            if (temp.p.value == p.sig) temp.p.value <- round(temp.p.value, digits = decimals+1)


            temp.res <- data.frame(stat.value = temp.stat.value, p.value = temp.p.value)
            rownames(temp.res) <- temp.group
            if(!exists("result.post.hoc")) result.post.hoc <- temp.res
            else result.post.hoc <- rbind(temp.res, result.post.hoc)
          }
        }


        empty.fill <- rep(" ", nrow(result.post.hoc)-1)
        result.post.hoc$p.exact = result.post.hoc$p.value

        result.post.hoc$p.symbols[result.post.hoc$p.value >= p.sig] <- "-"
        result.post.hoc$p.symbols[result.post.hoc$p.value < p.sig] <- "*"
        result.post.hoc$p.symbols[result.post.hoc$p.value < p.sig.small] <- "**"
        result.post.hoc$p.symbols[result.post.hoc$p.value < p.sig.very.small] <- "***"
        result.post.hoc$group.pairs <- rownames(result.post.hoc)

        result.post.hoc$p.value <- ifelse(result.post.hoc$p.value < p.sig.very.small, paste("<", p.sig.very.small), result.post.hoc$p.value)
        result.post.hoc$post.hoc <- c("Games-Howell", empty.fill )
        result.post.hoc$var <- c(x.name, empty.fill)
        result.post.hoc$by <-  c(by.name, empty.fill)
        result.post.hoc <- result.post.hoc[,c("var","by","group.pairs","p.value","p.symbols","post.hoc")]
        rownames(result.post.hoc) <- NULL
      }
    }
    else if (total.levels < 2) {
      cat("\n[.feR.comp.mean] Not enough levels in factor variable: ", by.name)
    }
  } else {
    if (total.levels == 2) {
      comp.m <- wilcox.test(x.values ~ by.values)
      comp.m.p.value <- round(comp.m$p.value, digits = decimals)
      if (comp.m.p.value == p.sig ) comp.m.p.value <- round(comp.m$p.value, digits = decimals+1)


      comp.p <- ifelse(comp.m$p.value < p.sig.very.small, paste0("<",p.sig.very.small), comp.m.p.value)
      result.comp <- data.frame(var = x.name, by = by.name,
                                group.pairs = paste(levels(by.values)[1],levels(by.values)[2], sep = " - "),
                                test = "Wilcoxon-Mann-Whitney",
                                stat = comp.m$statistic,
                                p.value = comp.p,
                                p.exact = comp.m.p.value)
    }
    else if (total.levels > 2) {
      comp.m <- kruskal.test(x.values ~ by.values)
      comp.m.p.value <- round(comp.m$p.value, digits = decimals)
      if (comp.m.p.value == p.sig ) comp.m.p.value <- round(comp.m$p.value, digits = decimals+1)

      comp.p <- ifelse(comp.m$p.value < p.sig.very.small, paste0("<",p.sig.very.small), comp.m.p.value)
      result.comp <- data.frame(var = x.name, by = by.name,
                                test = "Kruskal-Wallis",
                                stat = comp.m$statistic,
                                p.value = comp.p,
                                p.exact = comp.m.p.value)


      gh <- PMCMRplus::kwAllPairsConoverTest(x.values ~ by.values)
      # gh <- kwAllPairsDunnTest(formula, data= data)
      for(x in dimnames(gh$p.value)[[1]]){
        for(y in dimnames(gh$p.value)[[2]]){
          if(x==y) next
          temp.group <- paste(x,"-",y)
          temp.p.value <- gh$p.value[x,y]
          temp.stat.value <- round(gh$statistic[x,y], digits = decimals)

          temp.res <- data.frame(stat.value = temp.stat.value, p.value = temp.p.value)
          rownames(temp.res) <- temp.group
          if(!exists("result.post.hoc")) result.post.hoc <- temp.res
          else result.post.hoc <- rbind(result.post.hoc, temp.res)
        }
      }
      empty.fill <- rep(" ", nrow(result.post.hoc)-1)
      result.post.hoc$p.exact = result.post.hoc$p.value

      result.post.hoc$p.symbols[result.post.hoc$p.value >= p.sig] <- ""
      result.post.hoc$p.symbols[result.post.hoc$p.value < p.sig] <- "*"
      result.post.hoc$p.symbols[result.post.hoc$p.value < p.sig.small] <- "**"
      result.post.hoc$p.symbols[result.post.hoc$p.value < p.sig.very.small] <- "***"
      result.post.hoc$group.pairs <- rownames(result.post.hoc)

      result.post.hoc$p.value <- ifelse(result.post.hoc$p.value < p.sig.very.small, paste("<", p.sig.very.small), result.post.hoc$p.value)
      result.post.hoc$post.hoc <- c("Conover test", empty.fill )
      result.post.hoc$var <- c(x.name, empty.fill)
      result.post.hoc$by <-  c(by.name, empty.fill)
      result.post.hoc <- result.post.hoc[,c("var","by","group.pairs","p.value","p.symbols","post.hoc")]
      rownames(result.post.hoc) <- NULL


    }
    else if (total.levels < 2) {
      cat("\n[.feR.comp.mean] Not enough levels in factor variable: ", by.name)
    }
  }
  # result.comp <- attr(result.comp, "COMP") <- result.comp
  if (exists("result.post.hoc")) attr(result.comp,"RESULT.POST.HOC") <- result.post.hoc
  return(result.comp)
}


.feR.media <- function(x, x.name, decimals = 2, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = .95) {
  # require("nortest")

  if (!is.numeric(x)) {
    warning(paste0("[.feR.media] ERROR - Variable ",x.name," is not numeric"), call. = FALSE)
    return(NA)
  }

  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::.normal(x, n.valid=n.valid)

  if (n.valid > 0) {
    min = min(x, na.rm = TRUE)
    max = max(x, na.rm = TRUE)
    mean = round(mean(x, na.rm = TRUE), digits = decimals)
    sd = round(sd(x, na.rm = TRUE), digits = decimals)
    median = median(x, na.rm = TRUE)
    IQR = IQR(x, na.rm = TRUE)
    se <- sd(x, na.rm = TRUE)/sqrt(n.valid)
    alpha_2 <- ci+((1-ci)/2)
    if(x.normal$is.normal){
      error <- qnorm(alpha_2)*se
    } else {
      error <- qt(alpha_2, df=n.valid -1)*se
    }
    ci.upper <- mean + error
    ci.lower <- mean - error


  } else {
    min = NA
    max = NA
    mean = NA
    sd = NA
    median = NA
    IQR = NA
    se = NA
    ci.upper = NA
    ci.lower = NA
  }





  result <- data.frame("n.valid" = n.valid,
                       "n.missing" = n.missing,
                       "min" = min,
                       "max" = max,
                       "mean" = mean,
                       "sd" = sd,
                       "median" = median,
                       "IQR" = IQR,
                       "se" = se,
                       "ci.upper" = ci.upper,
                       "ci.lower" = ci.lower,
                       "p.norm" = x.normal$p.value,
                       "p.norm.exact" = x.normal$p.exact.value,
                       "nor.test" = x.normal$test,
                       "is.normal" =  x.normal$is.normal)
  class(result) <- append("udaic",class(result))

  return(result)
}
