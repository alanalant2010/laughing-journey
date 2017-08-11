library(shiny)
folder = "V:/Science Group/DQMM/Alan/PSG Project Final/PSCs (TXTs)"      
  file_list = list.files(path=folder, pattern="*.txt")
  all_files = list()
  file_list_names = file_list

  for (i in 1:length(file_list)) {
    file_list[i] = paste0(folder, "/", file_list[i])
    all_files[[length(all_files)+1]] = readLines(file_list[i])
    names(all_files)[i] = file_list_names[i]
  }

shinyServer(function(input, output) {

  output$result <- renderDataTable({
    separated_search = strsplit(input$search, " ")[[1]]
    logic = rep(FALSE, length(separated_search))

    files_with_search = list()

    for (i in 1:length(all_files)) {
      for (j in all_files[i]) {
        for (k in j) {
          for (word in 1:length(separated_search)){
            if (grepl(separated_search[word], k, ignore.case = TRUE)) {
              logic[word] = TRUE
            }
          }
        }
        final_logic = TRUE
        for (compare in logic) {
          if (compare == FALSE) {
            final_logic = FALSE
          }
        }

        if (final_logic == TRUE){
          files_with_search[[length(files_with_search)+1]] = file_list_names[i]
        }
        logic = rep(FALSE, length(separated_search))
        final_logic = FALSE
      }
    }

    files_with_search = unlist(files_with_search)
    result_df = data.frame(Files = character(0), RLD_code = character(0), stringsAsFactors = FALSE)

    for (i in 1:length(files_with_search)) {
      six_digit = FALSE
      RLD = list()
      character_or_not = rep(FALSE, length(files_with_search[i]))
      characters = strsplit(files_with_search[i], "")[1]
      characters = unlist(characters)

      for (j in 1:length(characters)) {
        character_or_not[j] = suppressWarnings(as.numeric(characters[j]))
      }

      for (k in 1:(length(character_or_not)-5)) {
        if (!is.na(character_or_not[k])) {
          if (!is.na(character_or_not[k+1])) {
            if (!is.na(character_or_not[k+2])) {
              if (!is.na(character_or_not[k+3])) {
                if (!is.na(character_or_not[k+4])) {
                  if (!is.na(character_or_not[k+5])) {
                    RLD[[length(RLD)+1]] = character_or_not[k:(k+5)]
                    six_digit = TRUE
                  }
                  if (six_digit == FALSE) {
                    RLD[[length(RLD)+1]] = character_or_not[k:(k+4)]
                  }
                }
              }
            }
          }
        }
      }

      RLD_string = ""

      for (p in 1:length(RLD)) {
        for (j in RLD[[p]]) {
          RLD_string = paste0(RLD_string, j)
        }
        if ((length(RLD) > 1) & (p != length(RLD))) {
          RLD_string = paste0(RLD_string, ", ")
        }
      }

      file_df = data.frame(Files = files_with_search[i], RLD_code = RLD_string)

      result_df = rbind(result_df, file_df)

    }

    for (i in 1:length(files_with_search)) {
      result_df$Link[i] <- toString(tags$a(href=files_with_search[i], target='blank', "File"))
    }
    return(result_df)

  }, escape = FALSE)
})