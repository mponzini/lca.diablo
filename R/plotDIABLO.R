#' @export

plotDIABLO <- function(object,
                        ncomp = 1,
                        legend = TRUE,
                        legend.ncol,
                        col.per.group = NULL,
                        ...)
{
  if (!is.null(list(...)$x))
    .stop("use of 'x' has been deprecated. Use 'object' instead.")

  Y <- object$Y
  ## col.per.group
  col.per.group <- .change_if_null(col.per.group, default =  color.mixo(1:nlevels(Y)))
  col.per.group <- .get.cols.and.group(col.per.group = col.per.group, group = Y, object = object, n_ind = length(object$names$sample))
  col.per.group <- col.per.group$col.per.group

  #need to reorder variates and loadings to put 'Y' in last
  opar = par()[! names(par()) %in% c("cin", "cra", "csi", "cxy", "din", "page")]

  indY=object$indY
  object$variates=c(object$variates[-indY],object$variates[indY])
  object$loadings=c(object$loadings[-indY],object$loadings[indY])

  VarX = do.call(cbind, lapply(object$variates, function(i) i[, ncomp]))
  datNames = colnames(VarX)

  if(ncol(VarX)<=2)
    stop("This function is only available when there are more than 3 blocks") # so 2 blocks + the outcome Y

  # check input parameters

  if (length(ncomp) != 1 | ncomp > min(object$ncomp))
    stop(paste0("'ncomp' must be a numeric value lower than ", min(object$ncomp),", which is min(object$ncomp)"))
  # end check parameters

  if(missing(legend.ncol))
    legend.ncol = min(5, nlevels(Y))

  numberOfCols = ncol(VarX)-1
  numberOfRows = numberOfCols #- 1

  mat = matrix(0, nrow = numberOfRows, ncol = numberOfRows)
  for(i in 1:nrow(mat))
  {
    for(j in 1:ncol(mat))
      mat[i,j] = paste(i,j, sep="_")
  }
  plotType = list(cor=mat[lower.tri(mat)], scatter=mat[upper.tri(mat)],
                  lab=diag(mat))#,
  #bar=paste(1:(numberOfRows-1), numberOfCols, sep="_"),
  #stackedbar=paste(numberOfRows, numberOfCols, sep="_"))

  par(mfrow = c(numberOfRows+1, numberOfCols), mar = rep.int(1/2, 4), oma = c(2,2,2,2))
  layout(matrix(c(1:(numberOfCols)^2, rep((numberOfCols)^2+1,numberOfCols)),numberOfRows+1,numberOfCols, byrow=TRUE),
         heights = c(rep(1,numberOfRows), 0.25 * floor(nlevels(Y)/legend.ncol)))
  for(i in 1:numberOfRows)
  {
    for(j in 1:numberOfCols)
    {
      ptype = unlist(lapply(plotType, function(x)
      {
        intersect(paste(i,j,sep="_"), x)
      }))
      splotMatPlot2(x=VarX[, j], y=VarX[, i], datNames, Y, ptype, col.per.group)

      if(i == 1 & j %in% seq(2, numberOfRows, 1))
        Axis(side = 3, x=VarX[, i])

      if(j == numberOfRows & i %in% seq(1, numberOfRows-1, 1))
        Axis(side = 4, x=VarX[, i])
    }
  }
  #add legend
  plot(1:3,1:3,type="n",axes=FALSE,xlab="",ylab="")
  if(legend)
    legend("center",legend=levels(Y), col = col.per.group, pch = 19, ncol = legend.ncol, cex = 1.5)

  par(opar)
}

#' @rdname plotDIABLO
#' @method plot sgccda
#' @export
plot.sgccda <- function(x, ...) plotDIABLO(object = x, ...)

splotMatPlot2 = function(x, y, datNames, Y, ptype, col.per.group)
{
  if(names(ptype) == "cor")
  {
    plot(1, type = "n", axes = FALSE)
    r = round(cor(x, y), 2)
    r.char = format(abs(r), nsmall = 2)
    text(1, 1, labels=r, cex = 0.6/strwidth(r.char)*abs(r))
    box()
  }
  if(names(ptype) == "scatter")
    panel.ellipses(x=x, y=y, Y = Y, col.per.group = col.per.group)

  if(names(ptype) == "lab")
  {
    plot(1, type = "n", axes = FALSE)
    ind = as.numeric(unlist(lapply(strsplit(ptype, "_"), unique)))
    text(x=1, y=1, labels=datNames[ind], cex = 2)
    box()
  }
  # if(FALSE)
  # {
  #     if(names(ptype) == "bar")
  #     {
  #         Y2 = factor(as.character(Y), levels = groupOrder)
  #         boxplot(x ~ Y2, horizontal=TRUE, axes = FALSE, ylim = c(min(x)-3, max(x)),
  #                 col= col.per.group[groupOrder])
  #         axis(4, at=1:nlevels(Y2), labels=levels(Y2))
  #     }
  #     if(names(ptype) == "stackedbar")
  #     {
  #         Y2 = factor(as.character(Y), levels = groupOrder)
  #         bars = table(Y2)
  #         barplot(bars, col= color.mixo(match(levels(Y2), levels(Y))),
  #                 axes = FALSE)
  #         axis(4, at=seq(0,max(bars),length.out=5), labels=seq(0,max(bars),length.out=5))
  #     }
  # }
}

panel.ellipses = function(x, y, Y = Y, pch = par("pch"), col.lm = "red", axes = FALSE, col.per.group, ...)
{
  ind.gp = matrice = cdg = variance = list()
  for(i in 1:nlevels(Y))
    ind.gp[[i]] = which(as.numeric(Y)==i)

  matrice = lapply(ind.gp, function(z){matrix(c(x[z], y[z]), ncol = 2)})
  cdg = lapply(matrice, colMeans)
  variance = lapply(matrice, var)

  library(ellipse)
  coord.ellipse = lapply(1:nlevels(Y), function(x){ellipse(variance[[x]], centre = cdg[[x]], ellipse.level = 0.95)})
  max.ellipse = sapply(coord.ellipse, function(x){apply(x, 2, max)})
  min.ellipse = sapply(coord.ellipse, function(x){apply(x, 2, min)})
  ind.names = names(Y)
  cex = 0.5
  plot(x, y, xlab = "X.label", ylab = "Y.label", col = col.per.group[Y], pch=20, axes=axes,
       xlim = c(min(x, min.ellipse[1, ]), max(x, max.ellipse[1, ])), ylim = c(min(y, min.ellipse[2, ]), max(y, max.ellipse[2, ])))
  #text(x, y, ind.names, col = col, cex = cex)
  box()
  for (z in 1:nlevels(Y))
    points(coord.ellipse[[z]], type = "l", col = col.per.group[z])

}

.are.colors <- function(cols) {
  #TODO use this to deduplicate code in internals
  ## check cols, if all valid colors, return cols, else, throw error
  all.cols <- sapply(cols, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
  if (any(!all.cols))
  {
    stop('invalid colour character/vector.\n', call. = FALSE)
  }
  cols
}

.get.colors <- function(col, n_col=0) {
  #TODO use this to deduplicate code in internals
  ## ensure 'col' is valid color(s) and of length 'col.length', repeat it if
  ## necessary
  ## return a vector of lenfth col.length, or throw condition
  if (is.null(col))
  {
    col <- 'black'
  }
  col <- .are.colors(col)
  if (length(col) == 1)
  {
    col <- rep(col, n_col)
  }
  else if (length(col == n_col))
  {
    col <- col
  }
  else
  {
    stop("'col' must be character of length 1 or ", paste0(n_col))
  }
  col
}

.get.character.vector <- function(arg, vec) {
  #TODO use this to deduplicate code in internals
  ## if 'arg' is TRUE, return vec, if FALSE, return NULL, if character, ensure
  ## it's of the same length as vec
  ## return a character of the same length as vec
  stopifnot(is.character(vec))
  fail.msg <- paste0(sQuote(deparse(substitute(arg))),
                     " must be either logical or a character vector of length ", length(vec))

  if (isTRUE(arg))
  {
    res <- vec
  }
  else if (isFALSE(arg))
  {
    res <- NULL
  }
  else if (is.character(arg))
  {
    if (length(arg) != length(vec))
    {
      stop(fail.msg, call. = FALSE)
    }
    else
    {
      res <- arg
    }
  } else
  {
    res <- tryCatch(as.character(arg))
    if (length(res) != length(vec))
    {
      stop(fail.msg, call. = FALSE)
    }

  }
  res
}

#' @noRd
#' @examples
#' .get.group(group = NULL, object = NULL, n_ind = 10)
#' .get.group(group = letters[1:10], object = NULL, n_ind = 10)
#' .get.group(group = NULL, object = structure(list(Y = 10:19), class='DA'), n_ind = 10)
#' \dontrun{
#' .get.group(group = letters[1:11], object = NULL, n_ind = 10)
#' .get.group(group = NULL, object = structure(list(Y = 10:19), class='DA'), n_ind = 10)
#' }
.get.group <- function(group, object=NULL, n_ind=0) {
  #TODO use this to deduplicate code in internals
  ## check group, if and if it's not NULL return a factor
  ## if object is of class 'DA', return object$Y which should be factor
  out <- group
  ## if no group given
  if (is.null(out))
  {
    if (is (object, 'DA'))
    {
      out <- object$Y
      if (n_ind != length(out))
      {
        stop("Unexpected error. Please submit an issue with reproducible example to\n",
             "https://github.com/mixOmicsTeam/mixOmics/issues")
      }
    }
    else
    {
      ## for consistency of plot so group is always an aes
      out <- factor(rep(1, n_ind))
    }

  }
  ## group is factor and does not drop NAs
  if (!is.factor(out))
  {
    out[is.na(out)] <- "missing group (NA)"
    out <- as.factor(out)
  }
  ## group length is valid
  if (length(out) != n_ind)
  {
    stop("'group' must be a factor of length ", n_ind)
  }

  return(out)
}

#'@noRd
#'@examples
#' .get.cols.and.group(n_ind = 3)
#' .get.cols.and.group(col = 'blue', n_ind = 3)
#' .get.cols.and.group(group = c(2,2,1))
#' .get.cols.and.group(col.per.group = c('1'='red', '2'='blue'), group = c(2,2,1))
.get.cols.and.group <- function(
    col.per.group=NULL,
    group=NULL,
    col=NULL,
    object=NULL,
    n_ind=ifelse(is.null(group), 0, length(group))
) {
  #TODO use this to deduplicate code in internals
  ## check group, if and if it's not NULL return a factor
  ## if object is of class 'DA', return object$Y which should be factor

  ## group factor
  group <- .get.group(group, object, n_ind)
  ## create col.per.group
  if (is.null(col.per.group))
  {
    if (nlevels(group) == 1)
    {
      col.per.group <- ifelse(is.null(col), color.mixo(1), .are.colors(col))
    } else if (nlevels(group) < 10)
    {
      col.per.group <- color.mixo(seq_len(nlevels(group)))
    } else if (nlevels(group) >= 10)
    {
      col.per.group <- color.jet(nlevels(group))
    }

    names(col.per.group) <- levels(group)
  }
  ## valid col.per.group
  if (!all(names(col.per.group) %in% levels(group)))
  {
    stop("'col.per.group' should be a named vector whose names match group (",
         levels(group), ")")
  }
  ## return group and col.per.group
  return(list(
    group = group,
    col.per.group = col.per.group
  ))

}
.get.ind.colors <- function(group=NULL, col=NULL, col.per.group=NULL, n_ind=0) {
  #TODO use this to deduplicate code in internals
  ## consider all valid combinations and return a vector of colors
  ## if group != NULL, return a vector whose names are colors and values the
  ## group levels for ggplot
  ## ------------- group == NULL
  if (is.null(group))
  {
    col <- .get.colors(col = col, n_col = n_ind)
  }
  else
  {
    col.per.group <-
      .change_if_null(col.per.group,
                      color.mixo(seq_len(1+nlevels(group))[-3] ## bc 3rd one is grey
                      )
      )
    names(col.per.group) <- levels(group)

    if (!all(names(col.per.group) %in% levels(group)))
    {
      stop("'col.per.group' should be a named vector whose names match group (",
           levels(group), ")")
    }
    col <- group
    names(col) <- col.per.group[group]
  }
  return(col)
}

#' @noRd
#' @examples
#' .get.pch(pch = NULL, pch.levels = NULL)
#' .get.pch(pch = 6, pch.levels = NULL)
#' .get.pch(pch = mtcars$cyl, pch.levels = NULL)
.get.pch <- function(pch = NULL,
                     pch.levels = NULL,
                     pch.legend = TRUE,
                     n_ind = length(pch))
{
  ## NULL -> 16
  if (is.null(pch))
  {
    pch <- 16
  }

  if (length(pch) == 1)
  {
    names(pch.levels) <- pch.levels <-  pch
    pch <- rep(pch, n_ind)
  }

  pch <- factor(pch)

  if (is.null(pch.levels))
  {
    all.pch.levels <- 0:24
    main.pch.levels <- c(16, 15, 17, 3, 4)
    ref.pch.levels <- c(main.pch.levels, all.pch.levels[-which(all.pch.levels %in% main.pch.levels)])

    pch.levels <- ref.pch.levels[seq_len(nlevels(pch))]
    names(pch.levels) <- levels(pch)

  }
  else
  {
    if ( length(setdiff(names(pch.levels),  levels(pch)) ) > 0 |
         length(setdiff(levels(pch),  names(pch.levels)) ) > 0 )
    {
      stop("'pch.levels' must be a vector of pch values with names: ",
           levels(pch), call. = FALSE)
    }
  }
  pch.legend <- pch.legend & ifelse(length(pch.levels) == 1, FALSE, TRUE)
  return(list(pch = pch,
              pch.levels = pch.levels,
              pch.legend = pch.legend))
}

.check.cutoff <- function(cutoff = 0,
                          style = 'ggplot2') {
  ncomp.vis <- ifelse(style != '3d', yes = 2, no = 3)
  cutoff <- .change_if_null(cutoff, 0)
  ## check that cutoff value is valid
  ## it could be of length 2 when different cutoff for different components
  ## are needed (useful when most of selected variables tend to contribute
  ## to one component only so we can tailor the cutoff)
  err <- FALSE
  if (length(cutoff) == 1)
  {
    if (! (is.numeric(cutoff) | (cutoff > 1) | (cutoff < 0)) )
    {
      err <- TRUE
    } else
    {
      cutoff <- rep(cutoff, ncomp.vis)
    }
  }
  else if (length(cutoff) == ncomp.vis)
  {
    if (! (all(is.numeric(cutoff) | (cutoff > 1) | (cutoff < 0)) ))
      err <- TRUE
  }
  else
  {
    err <- TRUE
  }

  if (isTRUE(err))
  {
    stop("cutoff must be a numeric of length 1 (or 'ncomp') between 0 and 1", call. = FALSE)
  }
  return(cutoff)
}


.change_if_null <- function(arg, default) {
  return(
    if ( missing(arg) || is.null(arg) )
      default
    else
      arg
  )
}
