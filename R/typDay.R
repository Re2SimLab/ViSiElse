#' Simulated time data of the actions performed in a typical day
#'
#' This dataset shows the actions usually performed during a typical day. 
#' The simulated dataset of 100 subjects correspond to the timestamps (in min)
#' of each action of the day, from midnight to midnight. Each value is the time
#' elapse between the beginning of the day (midnight) and the execution of the
#' action.
#'
#' @docType data
#'
#' @usage data(typDay)
#'
#' @format A data frame with 100 rows and 15 variables:
#' \describe{
#'   \item{id}{Midwife students ID.}
#'   \item{start_sleep}{Time (in min) when the subject is sleeping. All subjects are set to 0 (0:00 or midnight) as the dataset shows a day from midnight to midnight}
#'   \item{stop_sleep}{Time (in min) when the subject stops to sleep.}
#'   \item{wake_up}{Time (in min) when the subject wakes up (same values as stop_sleep).}
#'   \item{shower}{Time (in min) when the subject takes a shower.}
#'   \item{breakfast}{Time (in min) when the subject eats breakfast.}
#'   \item{start_work}{Time (in min) when the subject starts working.}
#'   \item{start_lunch}{Time (in min) when the subject starts to eat lunch.}
#'   \item{stop_lunch}{Time (in min) when the subject finishes his lunch.}
#'   \item{stop_work}{Time (in min) when the subject stops to work.}
#'   \item{pickup_kids}{Time (in min) when the subject picks up his kids.}
#'   \item{start_cook}{Time (in min) when the subject starts cooking.}
#'   \item{stop_cook}{Time (in min) when the subject stops cooking.}
#'   \item{go_sleep}{Time (in min) when the subject goes to sleep.}
#'   \item{first_coffee}{Time (in min) when the subject drinks his first coffee of the day.}
#' }
#'
#' @keywords datasets
#'
#' @references Garnier EM, Fouret N, Descoins M (2019) ViSiElse: An innovative 
#' R-package to visualize raw behavioral data over time.
#'  PeerJ Preprints 10.7287/peerj.preprints.27665v2
#' ([PeerJ](https://doi.org/10.7287/peerj.preprints.27665v2))
#'
#' @examples
#' data(typDay)
#' head(typDay)
"typDay"
