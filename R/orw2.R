
#' Documentation to come...
#'
#' @importFrom crayon red green
#' @export

orw2 <- function(){

  # Read verb list
  d = read.csv2(file = "./data/NL_onregelmatige_werkwoorden.csv")
  # Define header
  header = colnames(d)

  answer = "init"
  while(answer != "quit"){

    # Sample one verb
    l = d[sample(1:nrow(d), 1), c(1:3)]
    l = as.character(unlist(l))
    names(l) = header

    # Question text to display with hashed words
    disp = l
    disp[2:length(disp)] = gsub("[a-z]", "#", x = disp[2:length(disp)])
    names(disp) = header

    # Display question
    print(disp)

    # Take input from the user
    answer = readline(prompt = "Answer:")
    while(answer == "init" || answer == "") {
      answer = readline(prompt = "Answer:")
    }

    # Process answer
    answer = unlist(strsplit(answer, " "))
    test <- answer == l[2:length(l)]
    correction <- l[2:length(l)]

    if(length(answer) != 1 && answer != "quit"){
      cat("Result: ", ifelse(test, crayon::green(test), crayon::red(test)), "\n")
      cat("Correction: ", correction, "\n")
      cat(paste(rep("-", 80), collapse = ""), "\n")

    }
  }
  print("Tot ziens")
}


# exception zullen "-"
# avoid repeats of correct answers
