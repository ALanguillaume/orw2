
#' Documentation to come...
#'
#' @importFrom crayon red green
#' @export

orw2 <- function(){
  d = read.csv2(file = "./data/NL_onregelmatige_werkwoorden.csv")
  d[c(1, 3)]
  h = colnames(d)
  a = "init"
  while(a != "quit"){

    l = d[sample(1:nrow(d), 1), c(1:3)]
    l = as.character(unlist(l))
    names(l) = h

    disp = l
    disp[2:length(disp)] = gsub("[a-z]", "#", x = disp[2:length(disp)])

    names(disp) = h

    print(disp)

    a = readline(prompt = "answer:")

    if(a != "quit"){
      a = unlist(strsplit(a, " "))
      test <- a == l[2:length(l)]
      correction <- l[2:length(l)]

      if(test == TRUE){
        cat("Answer: ", crayon::green(test), "\n")
      } else if(test == FALSE){
        cat("Answer: ", crayon::red(test), "\n")
      } else {
        stop("something went wrong with the correctiong process")
      }

      cat("Correction: ", correction, "\n")
      cat(paste(rep("-", 80), collapse = ""), "\n")
    } else {
      print("Tot ziens")
    }
  }
}

# To do if enter is pressed bug qrg length zero
# differentiate TRUE / FALSE color
# exception zullen "-"
# avoid repeats of correct answers
