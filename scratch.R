
outcome <- bernDraws(piParam = input$piParam, nTrials = input$nTrials) 
draws <- data.frame(drawVal = ifelse(outcome, "Success", "Failure") %>%  factor(levels = c("Success", "Failure")))
summaryDraws <- draws %>%  count(drawVal, .drop = FALSE) 