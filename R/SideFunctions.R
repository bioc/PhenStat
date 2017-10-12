.onAttach <- function(lib, pkg) {
  packageStartupMessage(
    paste0(
      '\n >=========================================================================<',
      '\n This version of PhenStat includes *FEWER* functions than the previous ones ',
      '\n You *still* can use the previous functions by using `:::`. For example :   ',
      '\n PhenStat:::boxplotSexGenotype   or   PhenStat:::FisherExactTest            ',
      '\n *** Want to know what is new in this version? run PhenStat:::WhatIsNew()   ',
      '\n >=========================================================================<'
    ),
    domain = NULL,
    appendLF = TRUE
  )
}


depMessage = function() {
  cat('\n *** this function would be depricated in the new version of PhenStat.\n')

}
havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <-
    "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}


MakeUniqueString <- function(n = 1, lenght = 12)
{
  set.seed(Sys.getpid() + as.numeric(Sys.time()))
  randomString <- c(1:n)   # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, LETTERS, 0:9),
                                    lenght, replace = TRUE),
                             collapse = "")
  }
  return(randomString)
}

b2f = function(x)
  gsub("\\\\", "/", x)

s2s = function(x)
  gsub("[][!#$%()*,.:;<=>@^_`|~.{}]", "_", x)




checkWeights <- function(w,
                         n,
                         threshold = 10 ^ -18,
                         date = NULL) {
  lw = length(w)
  if (!is.null(w)) {
    if (lw != n || !is.numeric(w))
      stop("'weights' must be a NUMERIC vector of the SAME size as n=", lw)

    r      = 1:n
    date = as.character(date)
    zw     = which(abs(w) > threshold)
    #### need at least 2 observations in a group
    MorTh1 = names(table(date[zw]))[which(table(date[zw]) > 1)]
    zw     = zw[date[zw] %in% MorTh1] # no singleDay-singleData

    if (length(zw) > 0) {
      r  = r[zw]
      w  = (1 / w[zw])
    } else{
      cat('\n ** Model weights are ignored!
          \n *** All weights are close enough to zero! \n')
      w = NULL
      r = 1:n
    }
  } else{
    r = 1:n
  }
  return(list(w = w, wInd = r))
}



##### For PhenStat
######### grid search
PhengridSearch = function(PhenStatTestDataObject,
                          t,
                          x                 ,
                          y                 ,
                          m = mean(y)       ,
                          l = 1             ,
                          k = 1             ,
                          plot = TRUE       ,
                          mutInd = NULL     ,
                          threshold = 10 ^ -16,
                          ...) {
  lk = length(k)
  ll = length(l)
  n  = length(y)
  m  = unique(m)

  lmodel = list()
  wmat   = matrix(0, ncol = n + 1   , nrow = ll * lk) # +1 for index
  rmat   = matrix(0, ncol = 7       , nrow = ll * lk)
  colnames(rmat)          = colnames(rmat, do.NULL = FALSE)
  colnames(rmat)[1:7]     = c('Ind', 'Obs.in.Interval', 'AIC', 'BIC', 'esd', 'k', 'l')

  counter = 1
  for (lp in l) {
    for (kp in k) {
      weight = SmoothWin::expWeight(
        t = t,
        k = kp,
        l = lp,
        m = m,
        plot = FALSE
      )

      inn = abs(weight) > threshold

      if (plot) {
        plot(
          t,
          y,
          col = inn + 1,
          pch = inn + 1,
          main = paste(
            'l=',
            round(lp, 3),
            ', k=',
            round(kp, 3),
            ', #=',
            sum(inn),
            sep = ''
          ),
          xlab = 'Time',
          ylab = 'Response',
          ...
        )
        if (!is.null(mutInd))
          points(t[mutInd], y[mutInd], col = 3, pch = 19)
        lines(t,
              SmoothWin::scale21(weight) * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) + min(y, na.rm = TRUE),
              lty = 3)
        abline(v = m)
      }



      lmm = testDataset(
        phenList = PhenStatTestDataObject@phenList,
        depVariable = PhenStatTestDataObject@depVariable,
        equation = PhenStatTestDataObject@equation,
        outputMessages = FALSE,
        pThreshold = PhenStatTestDataObject@pThreshold,
        method = PhenStatTestDataObject@method,
        modelWeight = weight / sum(weight)
      )

      lmodel[[counter]]   = lmm
      rmat  [counter, ]   = c(
        counter ,
        sum(inn),
        AIC(lmm@analysisResults$model.output),
        BIC(lmm@analysisResults$model.output),
        sd(resid(lmm@analysisResults$model.output), na.rm = TRUE),
        kp,
        lp
      )
      wmat[counter, ]    = c(counter, weight / sum(weight))
      cat('\r', counter, '|', lk * ll)
      counter           = counter  + 1
    }
  }
  return(list(
    weights = wmat,
    output = as.data.frame(rmat),
    models = lmodel
  ))
}
