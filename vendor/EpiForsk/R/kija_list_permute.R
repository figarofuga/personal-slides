#' Permute the levels of a nested list
#'
#' `ListPermute()` permutes the nesting structure of a nested list. e.g.
#' \eqn{x[["a"]][["b"]][["c"]]} is equivalent to \eqn{ListPermute(x, c(3, 1, 2))[["c"]][["a"]][["b"]]}.
#'
#' @param x A nested list. By default, the list is assumed to be rectangular. If `enforce_rectangular` = FALSE, the
#' length of each level in the list will be based on the first element from the level above.
#' @param order A numeric vector, containing a permutation of the levels of `x`. If `depth` is user specified, the
#' length of `order` must equal depth.
#' @param depth A natural number or NULL (default). If NULL, the depth of the list is inferred by following the first
#' element of the list at each level until the next element is not a list. If a natural number, a number smaller than
#' the actual depth of `x` will result in permutation of the lists at the level of `depth`, and a number higher than
#' the actual depth of `x` will result in extra levels added to the output at the locations specified by `order`.
#' @param enforce_rectangular logical, should the function exit with an error in case `x` is not rectangular?
#'
#' @returns A list, with the components of `x` permuted according to `order`.
#'
#' @author
#' KIJA
#'
#' @examples
#' list_builder <- function(len, nm = NULL, index = NULL, len_orig = len) {
#'   out <- vector("list", len[1])
#'   if (!is.null(nm)) names(out) <- nm[[1]]
#'   if (length(len) > 1) {
#'     return(
#'       lapply(
#'         if (is.null(nm)) seq_along(out) else structure(seq_along(out), names = nm[[1]]),
#'         \(i) {
#'           list_builder(
#'           len = len[-1],
#'           nm = if (is.null(nm)) NULL else nm[-1],
#'           index = c(index, i),
#'           len_orig = len_orig
#'           )
#'         }
#'       )
#'     )
#'   } else {
#'     for (i in seq_along(out)) {
#'       index2 <- c(index, i)
#'       out[[i]] <- as.numeric(
#'       (index2 - c(rep(1, length(len_orig) - 1), 0)) %*%
#'       c(rev(cumprod(rev(len_orig[-1]))), 1)
#'       )
#'
#'     }
#'     return(out)
#'   }
#' }
#'
#' l1 <- list_builder(c(3, 2))
#' ListPermute(l1, c(2,1))
#'
#' l2 <- list_builder(c(3, 2), list(letters[24:26], letters[14:13]))
#' ListPermute(l2, c(2,1))
#'
#' l3 <- list_builder(
#' c(4, 3, 2),
#' list(letters[1:4], letters[24:26], letters[14:13])
#' )
#' ListPermute(l3, c(2,1,3))
#'
#' l4 <- list_builder(
#' c(4, 3, 2, 3),
#' list(letters[1:4], letters[24:26], letters[14:13], letters[16:18])
#' )
#' ListPermute(l4, c(4, 1, 2, 3))
#'
#' l5 <- list_builder(
#' c(4, 3, 2, 3), list(letters[1:4], letters[24:26], letters[14:13], letters[16:18])
#' )
#' l5a <- l5
#' for (i in 3:4) {
#'   for (j in 1:3) {
#'     for (k in 1:2) {
#'       l5a[[i]][[j]][[k]] <- l5[[i]][[j]][[k]][[1]]
#'     }
#'   }
#' }
#' l5b <- l5
#' for (i in 1:2) {
#'   for (j in 1:3) {
#'     for (k in 1:2) {
#'       l5b[[i]][[j]][[k]] <- l5[[i]][[j]][[k]][[1]]
#'     }
#'   }
#' }
#' ListPermute(l5, c(3, 1, 2), depth = 3L)
#' ListPermute(l5a, c(4, 1, 2, 3), depth = 4L, enforce_rectangular = FALSE)
#' ListPermute(l5b, c(4, 1, 2, 3), depth = 4L, enforce_rectangular = FALSE)
#'
#' l6 <- list()
#' ListPermute(l6, vector("numeric"))
#'
#' l7 <- list(c(1, 2))
#' ListPermute(l7, 1)
#'
#' @export

ListPermute <- function(x, order, depth = NULL, enforce_rectangular = TRUE) {
  # Input checks
  if (!is.list(x)) stop("'x' must be a list")
  if (length(x) == 0) return(x)
  if (!(is.numeric(order) && all(sort(order) == seq_along(order)))) {
    stop("'order' must be a numeric vector containing a permutation of ", seq_along(order))
  }
  if (!(is.null(depth) || (is.numeric(depth) && length(depth) == 1 && depth > 0 && trunc(depth) == depth))) {
    stop("'depth' must be either NULL or a natural number")
  }
  if (is.null(depth) && !enforce_rectangular) {
    stop("The depth of 'x' to permute must be specified when 'x' is not rectangular")
  }
  if (is.null(depth)) {
    # Get depth of x
    y <- x
    depth <- 0L
    while (is.list(y)) {
      depth <- depth + 1
      y <- y[[1]]
    }
  }
  if (length(order) != depth) stop("length(order) must be equal to the depth of 'x'")

  # Get length and names at each level of x
  tmp <- x
  len <- length(tmp)
  nm <- list(names(tmp))
  for (i in seq_len(depth)[-1]) {
    tmp <- tmp[[1]]
    len <- c(len, length(tmp))
    nm <- c(nm, list(names(tmp)))
  }

  # recursive function to rebuild the list with the correct permuted structure
  list_rebuilder <- function(
    len2,
    nm2,
    index = vector("numeric")
  ) {
    out <- vector("list", len2[1])
    names(out) <- nm2[[1]]
    if (length(len2) > 1) {# recursive step
      return(
        lapply(
          structure(seq_along(out), names = nm2[[1]]),
          \(i) {
            list_rebuilder(len2 = len2[-1], nm2 = nm2[-1], index = c(index, i))
          }
        )
      )
    } else {# base step
      for (i in seq_along(out)) {
        index2 <- c(index, i)
        index2[order] <- index2[seq_len(depth)]
        tryCatch(
          eval(str2lang(sub("i", i, paste0("out[[i]] <- x", paste0("[[", index2, "]]", collapse = ""))))),
          error = \(e) {
            if (enforce_rectangular) stop("Input x must be a rectangular list")
          }
        )
      }
      return(out)
    }
  }

  # construct the output
  len2 <- len
  nm2 <- nm

  len2[seq_len(depth)] <- len2[order]
  nm2[seq_len(depth)] <- nm2[order]

  list_rebuilder(
    len2,
    nm2,
    vector("numeric")
  )
}
