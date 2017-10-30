shinyServer(function(input, output) {
  library(ggplot2)
  library(plotly)
  library(xtable)
  library(scales)
  library(DT)
  library(shinydashboard)
  options(DT.fillContainer = FALSE)
  options(DT.autoHideNavigation = FALSE)
  mortgage <-
    function(loan_amount,
             interest,
             freq,
             compounding,
             number,
             grace,
             type) {
      compounding = as.numeric(compounding)
      options(scipen = 999)
      gr <- (interest * 100) * loan_amount * grace / (100 * 12)
      loan_amount <- loan_amount + gr
      number <- as.numeric(number) - as.numeric(grace)
      # difference with effective annual rate is the installment payment frequency, EAR assumes that the number
      # of installment per annum is equal to the compounding frequency, but installment no is 12 and compounding freq
      # is 4
      Int.Per_Payment <-
        (1 + interest / compounding) ^ (compounding / 12) -
        1
      installment_size <-
        pmt(Int.Per_Payment, number,-loan_amount, 0, type = type)
      inst <- rep(installment_size, number)
      Installment_No. <- 1:number
      interest_am <- c()
      principal_am <- c()
      beginning <- c()
      end <- c()
      for (i in 1:number) {
        if (i == 1) {
          beginning[i] <- loan_amount
          interest_am[i] <- loan_amount * Int.Per_Payment
          principal_am[i] <- installment_size - interest_am[i]
          end[i] <- loan_amount - principal_am[i]
        }
        else {
          beginning[i] <- end[i - 1]
          interest_am[i] <- beginning[i] * Int.Per_Payment
          principal_am[i] <- installment_size - interest_am[i]
          end[i] <- beginning[i] - principal_am[i]
        }
      }
      d <-
        round(
          data.frame(
            Installment_No.,
            Beginning_Balance = beginning,
            Installment_Size = inst,
            Principal_Deducted = principal_am,
            Interest_Charged = interest_am,
            Ending_Balance = end
          ),
          2
        )
      d2 <- d
      
      d2[1:nrow(d2), (2:ncol(d2))]<-sapply(d2[1:nrow(d2), (2:ncol(d2))], function(x) format(x, nsmall=2, big.mark=","))
      
      mm <- t(as.matrix(d[, 4:5]))
      par(mfrow = c(2, 1))
      s1 <- sum(d[, 4])
      s2 <- sum(d[, 5])
      l1 <- round(s1 / (s1 + s2) * 100, 2)
      l2 <- round(s2 / (s1 + s2) * 100, 2)
      Installment_Number = 1:number
      Relative_Installment_Size = c(d[, 4])
      pl <-
        plot_ly(
          x = Installment_Number,
          y = Relative_Installment_Size,
          name = "Principal Amount",
          type = "bar"
        )
      pl <-
        add_trace(
          pl,
          x = 1:number,
          y = c(d[, 5]),
          name = "Interest Amount",
          type = "bar"
        )
      pl <- layout(pl, barmode = "stack")
      ds <-
        data.frame(
          labels = c("Principal Amount", "Interest Amount"),
          values = c(sum(d[, 4]), sum(d[, 5]))
        )
      pll <-
        plot_ly(
          ds,
          labels =  ~ labels,
          values =  ~ values,
          type = "pie"
        ) %>%
        layout(
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          )
        )
      II <-
        data.frame(
          Total_Instalment_No = number,
          Interest_Rate_Per_Period = paste(round(Int.Per_Payment * 100, 2), "%", sep =
                                             ""),
          Installment_Size = format(installment_size, nsmall=2, big.mark=","),
          Grace_Period_Interest = format(gr,nsmall=2,big.mark=","),
          Total_Interest_Charged = format(sum(d[, 5]),nsmall=2, big.mark=","))
      list(
        amort = (d2),
        pl = pl,
        pll = pll,
        Installment_Size = comma_format()(II)
      )
    }
  output$A <-
    renderTable({
      mortgage(
        input$loan_amount,
        input$interest / (100),
        input$freq,
        input$compounding,
        input$number,
        input$grace,
        input$type
      )$Installment_Size
    }, include.rownames = F, digits = 2, striped = T, hover = T, bordered =
      T, align = "c")
  output$P <-
    DT::renderDataTable({
      mortgage(
        input$loan_amount,
        input$interest / (100),
        input$freq,
        input$compounding,
        input$number,
        input$grace,
        input$type
      )$amort
    }, options = list(
      pageLength = 25,
      columnDefs = list(
        list(className = c('dt-center'), targets = c(0)),
        list(className = c('dt-right'), targets = c(1:5))
      ),
      ordering = F
    ), rownames = FALSE)
  output$M <- renderPlotly({
    mortgage(
      input$loan_amount,
      input$interest / (100),
      input$freq,
      input$compounding,
      input$number,
      input$grace,
      input$type
    )$pl
  })
  output$NM <- renderPlotly({
    mortgage(
      input$loan_amount,
      input$interest / (100),
      input$freq,
      input$compounding,
      input$number,
      input$grace,
      input$type
    )$pll
  })
  output$downloadData <- downloadHandler(filename = reactive({
    paste(input$loan_amount,
          input$interest,
          input$number,
          ".xlsx",
          sep = "-")
  }),
  content <- function(file) {
    openxlsx::write.xlsx(
      mortgage(
        input$loan_amount,
        input$interest / (100),
        input$freq,
        input$compounding,
        input$number,
        input$grace,
        input$type
      )$amort,
      file,
      row.names = F,
      asTable = T
    )
  })
})