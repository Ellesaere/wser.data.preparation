moveMeDataTable <-function(data, tomove, where = "first", ba = NULL) {
  suppressWarnings(nums <- as.numeric(tomove))
  nums[is.na(nums)] <- suppressWarnings(sapply(tomove[is.na(as.numeric(tomove))], 
                              function(x) which(names(data) == x)))
  nums <- unlist(nums, use.names=FALSE)
  tomove <- (names(data)[nums])  
  temp <- setdiff(names(data), tomove)
  tomove <- unique(tomove)

  x <- switch(
    where,
    first = setcolorder(data,c(tomove, temp)),
    last = setcolorder(data,c(temp, tomove)),
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)-1))
      setcolorder(data,order)
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)))
      setcolorder(data,order)
    })
  x
}