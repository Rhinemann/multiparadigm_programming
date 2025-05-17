# Default alphabet generator function
default_alphabet_generator <- function() {
    while (TRUE) {
        input <- readline("Enter alphabet size: ")
        alphabet_size <- as.integer(input)

        if (alphabet_size <= 1) {
            cat("Number is too small, try again\n")
        } else if (alphabet_size < 26L) {
            break
        } else {
            cat("Number is too large, try again\n")
        }
    }

    upper_limit <- alphabet_size + 64
    alphabet <- unlist(strsplit(rawToChar(as.raw(65:upper_limit)), NULL))
    return(list(size = alphabet_size, alphabet = alphabet))
}

# Custom alphabet input function
custom_alphabet_generator <- function() {
    while (TRUE) {
        input <- readline("Enter alphabet size: ")
        alphabet_size <- as.integer(input)

        if (alphabet_size > 1) {
            break
        } else {
            cat("Number is too small, try again\n")
        }
    }

    cat("Input the alphabet letters: ")
    alphabet <- scan(what = "", quiet = TRUE, nmax = alphabet_size)
    return(list(size = alphabet_size, alphabet = alphabet))
}

# Alphabet input type handler
alphabet_input_handler <- function() {
    while (TRUE) {
        input <- readline("Use default alphabet? [Y/n]: ")

        if (input == "" | input == "Y" | input == "y") {
            return(default_alphabet_generator())
        } else if (input == "N" | input == "n") {
            return(custom_alphabet_generator())
        } else {
            cat("Wrong input, try again\n")
        }
    }

    return(input_data)
}

# Interval builder function
build_intervals <- function(series, alphabet_size) {
    min_val <- min(series)
    max_val <- max(series)
    step <- (max_val - min_val) / alphabet_size
    intervals <- seq(min_val, max_val, by = step)

    if (length(intervals) == alphabet_size) {
        intervals <- c(intervals, max_val)
    }
    return(intervals)
}

# Linguistic sequence builder function
build_linguistic_sequence <- function(series, intervals, alphabet_data) {
    indices <- findInterval(series, intervals, rightmost.closed = TRUE)
    indices[indices == 0] <- 1
    indices[indices > alphabet_data$size] <- alphabet_data$size
    return(alphabet_data$alphabet[indices])
}

# Precedence matrix builder function
build_precedence_matrix <- function(linguistic_sequence, alphabet_data) {
    matrix <- matrix(0, nrow = alphabet_data$size, ncol = alphabet_data$size, dimnames = list(alphabet_data$alphabet, alphabet_data$alphabet))

    for (i in 1:(length(linguistic_sequence) - 1)) {
        from <- linguistic_sequence[i]
        to <- linguistic_sequence[i + 1]
        matrix[from, to] <- matrix[from, to] + 1
    }
    return(matrix)
}

# Main function
main <- function() {
    alphabet_data <- alphabet_input_handler()

    numbers <- scan(file = "data.txt", what = numeric(), quiet = TRUE)
    sorted_numbers <- sort(numbers)


    intervals <- build_intervals(sorted_numbers, alphabet_data$size)
    linguistic_sequence <- build_linguistic_sequence(numbers, intervals, alphabet_data)
    precedence_matrix <- build_precedence_matrix(linguistic_sequence, alphabet_data)

    cat("\nЛінгвістичний ряд: ")
    cat(linguistic_sequence, sep = "")
    cat("\n")
    cat("Матриця передування:\n")
    print(precedence_matrix)
}

main()
