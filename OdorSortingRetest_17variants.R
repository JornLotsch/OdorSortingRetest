########## Libraries ##########

library(parallel)
library(pbmcapply)

nProc <- detectCores() - 1

########## Helper functions ##########

scale01 <- function(x, minX, maxX) {
  x_new <- (x - minX) / (maxX - minX)
  return(x_new)
}

ManhattanDistSorting <- function(x, reverse = FALSE) {
  correct_order <- 1:5
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  err <- sum(abs(x - correct_order))
  return(err)
}

EuclidDistSorting <- function(x, reverse = FALSE) {
  correct_order <- 1:5
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  d <- dist(rbind(x, correct_order))
  return(d)
}

SimpleErrorCount <- function(x, reverse = FALSE) {
  correct_order <- 1:5
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  err <- length(which(x - correct_order != 0))
  return(err)
}

ManhattanDistSorting2 <- function(x, reverse = FALSE) {
  correct_order <- rep(1:5, 2)
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  err <- sum(abs(x - correct_order))
  return(err)
}

EuclidDistSorting2 <- function(x, reverse = FALSE) {
  correct_order <- rep(1:5, 2)
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  d <- dist(rbind(x, correct_order))
  return(d)
}

SimpleErrorCount2 <- function(x, reverse = FALSE) {
  correct_order <- rep(1:5, 2)
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  err <- length(which(x - correct_order != 0))
  return(err)
}

ManhattanDistSortingFlat <- function(x, reverse = FALSE) {
  correct_order <- c(1:5, 1:5)
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  err <- sum(abs(x - correct_order))
  return(err)
}

EuclidDistSortingFlat <- function(x, reverse = FALSE) {
  correct_order <- c(1:5, 1:5)
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  d <- dist(rbind(x, correct_order))
  return(d)
}

SimpleErrorCountFlat <- function(x, reverse = FALSE) {
  correct_order <- c(1:5, 1:5)
  if (reverse == TRUE) {
    correct_order <- rev(correct_order)
  }
  err <- length(which(x - correct_order != 0))
  return(err)
}

########## Calculate score ##########


PerformSortingTest <- function(Data, ListOfScoreTypes = c("Add_EuclidCorrectOrder")) {
  SortingResults <- pbmcapply::pbmclapply(ListOfScoreTypes, function(ActualScoreType) {
    if (dim(Data)[2] == 10) {
      ## Define Maximum for scalings

      Max_1_Manhattan <- 12
      Max_1_Euclid <- dist(rbind(1:5, 5:1))
      Max_1_SimpleCount <- 5
      Max_1_Manhattan_penalized <- Max_1_Manhattan + Max_1_SimpleCount
      Max_1_Euclid_penalized <- Max_1_Euclid + Max_1_SimpleCount
      Max_Manhattan_Flat <- 24
      Max_Euclid_Flat <- 8.944272
      Max_Manhattan_Flat_penalized <- Max_Manhattan_Flat + 2 * Max_1_SimpleCount
      Max_Euclid_Flat_penalized <- Max_Euclid_Flat + 2 * Max_1_SimpleCount

      ## Compute test scores
      set.seed(42)
      switch(ActualScoreType,
        Mult_ManhattanCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_1 <- 1 - Score_1 / Max_1_Manhattan
          ActualData <- Data[, 6:10]
          Score_2 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_2 <- 1 - Score_2 / Max_1_Manhattan
          Score <- Score_1 * Score_2
        },
        Mult_EuclidCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_1 <- 1 - Score_1 / Max_1_Euclid
          ActualData <- Data[, 6:10]
          Score_2 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_2 <- 1 - Score_2 / Max_1_Euclid
          Score <- Score_1 * Score_2
        },
        Mult_SlopeCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(
            ActualData, 1,
            function(x) lm(unlist(as.list(as.data.frame(t(x)))) ~ c(1:5))$coef[[2]]
          )
          Score_1 <- scale01(Score_1, -1, 1)
          ActualData <- Data[, 6:10]
          Score_2 <- apply(
            ActualData, 1,
            function(x) lm(unlist(as.list(as.data.frame(t(x)))) ~ c(1:5))$coef[[2]]
          )
          Score_2 <- scale01(Score_2, -1, 1)
          Score <- Score_1 * Score_2
        },
        Mult_SimpleErrorCountCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_1 <- 1 - Score_1 / Max_1_SimpleCount
          ActualData <- Data[, 6:10]
          Score_2 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_2 <- 1 - Score_2 / Max_1_SimpleCount
          Score <- Score_1 * Score_2
        },
        Mult_ManhattanCorrectOrder_penalized = {
          ActualData <- Data[, 1:5]
          Score_11 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_p1 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_1 <- Score_11 + Score_p1
          Score_1 <- 1 - Score_1 / Max_1_Manhattan_penalized
          ActualData <- Data[, 6:10]
          Score_12 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_p2 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_2 <- Score_12 + Score_p2
          Score_2 <- 1 - Score_2 / Max_1_Manhattan_penalized
          Score <- abs(Score_1) * abs(Score_2)
        },
        Mult_EuclidCorrectOrder_penalized = {
          ActualData <- Data[, 1:5]
          Score_11 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_p1 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_1 <- Score_11 + Score_p1
          Score_1 <- 1 - Score_1 / Max_1_Euclid_penalized
          ActualData <- Data[, 6:10]
          Score_12 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_p2 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_2 <- Score_12 + Score_p2
          Score_2 <- 1 - Score_2 / Max_1_Euclid_penalized
          Score <- abs(Score_1) * abs(Score_2)
        },
        Add_ManhattanCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_1 <- 1 - Score_1 / Max_1_Manhattan
          ActualData <- Data[, 6:10]
          Score_2 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_2 <- 1 - Score_2 / Max_1_Manhattan
          Score <- rowMeans(cbind(Score_1, Score_2))
        },
        Add_EuclidCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_1 <- 1 - Score_1 / Max_1_Euclid
          ActualData <- Data[, 6:10]
          Score_2 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_2 <- 1 - Score_2 / Max_1_Euclid
          Score <- rowMeans(cbind(Score_1, Score_2))
        },
        Add_SlopeCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(
            ActualData, 1,
            function(x) lm(unlist(as.list(as.data.frame(t(x)))) ~ c(1:5))$coef[[2]]
          )
          Score_1 <- scale01(Score_1, -1, 1)
          ActualData <- Data[, 6:10]
          Score_2 <- apply(
            ActualData, 1,
            function(x) lm(unlist(as.list(as.data.frame(t(x)))) ~ c(1:5))$coef[[2]]
          )
          Score_2 <- scale01(Score_2, -1, 1)
          Score <- rowMeans(cbind(Score_1, Score_2))
        },
        Add_ManhattanCorrectOrder_penalized = {
          ActualData <- Data[, 1:5]
          Score_11 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_p1 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_1 <- Score_11 + Score_p1
          Score_1 <- 1 - Score_1 / Max_1_Manhattan_penalized
          ActualData <- Data[, 6:10]
          Score_12 <- apply(ActualData, 1, function(x) ManhattanDistSorting(x))
          Score_p2 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_2 <- Score_12 + Score_p2
          Score_2 <- 1 - Score_2 / Max_1_Manhattan_penalized
          Score <- rowMeans(cbind(abs(Score_1), abs(Score_2)))
        },
        Add_EuclidCorrectOrder_penalized = {
          ActualData <- Data[, 1:5]
          Score_11 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_p1 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_1 <- Score_11 + Score_p1
          Score_1 <- 1 - Score_1 / Max_1_Euclid_penalized
          ActualData <- Data[, 6:10]
          Score_12 <- apply(ActualData, 1, function(x) EuclidDistSorting(x))
          Score_p2 <- apply(ActualData, 1, function(x) SimpleErrorCount(x))
          Score_2 <- Score_12 + Score_p2
          Score_2 <- 1 - Score_2 / Max_1_Euclid_penalized
          Score <- rowMeans(cbind(abs(Score_1), abs(Score_2)))
        },
        ManhattanFlatCorrectOrder = {
          ActualData <- Data[, 1:10]
          Score <- apply(ActualData, 1, function(x) ManhattanDistSortingFlat(x))
          Score <- 1 - Score / Max_Manhattan_Flat
        },
        EuclidFlatCorrectOrder = {
          ActualData <- Data[, 1:10]
          Score <- apply(ActualData, 1, function(x) EuclidDistSortingFlat(x))
          Score <- 1 - Score / Max_Euclid_Flat
        },
        ManhattanFlatCorrectOrder_penalized = {
          ActualData <- Data[, 1:10]
          Score_1 <- apply(ActualData, 1, function(x) ManhattanDistSortingFlat(x))
          Score_p <- apply(ActualData, 1, function(x) SimpleErrorCountFlat(x))
          Score <- Score_1 + Score_p
          Score <- 1 - Score / Max_Manhattan_Flat_penalized
        },
        EuclidFlatCorrectOrder_penalized = {
          ActualData <- Data[, 1:10]
          Score_1 <- apply(ActualData, 1, function(x) EuclidDistSortingFlat(x))
          Score_p <- apply(ActualData, 1, function(x) SimpleErrorCountFlat(x))
          Score <- Score_1 + Score_p
          Score <- 1 - Score / Max_Euclid_Flat_penalized
        },
        Mult_TauCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(ActualData, 1, function(x) cor.test(x, 1:5, method = "kendall")$estimate)
          Score_1 <- scale01(Score_1, -1, 1)
          ActualData <- Data[, 6:10]
          Score_2 <- apply(ActualData, 1, function(x) cor.test(x, 1:5, method = "kendall")$estimate)
          Score_2 <- scale01(Score_2, -1, 1)
          Score <- Score_1 * Score_2
        },
        Add_TauCorrectOrder = {
          ActualData <- Data[, 1:5]
          Score_1 <- apply(ActualData, 1, function(x) cor.test(x, 1:5, method = "kendall")$estimate)
          Score_1 <- scale01(Score_1, -1, 1)
          ActualData <- Data[, 6:10]
          Score_2 <- apply(ActualData, 1, function(x) cor.test(x, 1:5, method = "kendall")$estimate)
          Score_2 <- scale01(Score_2, -1, 1)
          Score <- rowMeans(cbind(Score_1, Score_2))
        },
        Score = -99
      )
    }

    return(list(Score = Score))
  }, mc.cores = nProc)

  names(SortingResults) <- ListOfScoreTypes

  return(SortingResults)
}
