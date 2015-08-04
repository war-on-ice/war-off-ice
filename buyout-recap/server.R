## buyout-recap server
## acthomas, 8-20-14
library(shiny)
#load("season1314.RData")

source("global.R")

na.to.blank <- function(x) {y <- as.character(x); y[is.na(y)] <- ""; y[y=="NA"] <- ""; y}

shinyServer(function(input, output, session){
  
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); print(g1); g1})
    
    onload.woiid <- reactive (if (!is.null(GETterms()$woiid)) gsub("%27","'",gsub("0"," ",GETterms()$woiid)) else "crosbsi87")
    onload.name1 <- reactive (if (!is.null(GETterms()$name1)) gsub("%27","'",gsub("0"," ",GETterms()$name1)) else
                              roster.unique$firstlast[match(onload.woiid(), roster.unique$woi.id)])
    
    output$onload.name1 <-
        renderUI (textInput (inputId="nameSearch1", label="Filter Players", value = onload.name1()))

    onload.contract <- reactive(if (!is.null(GETterms()$contractid)) GETterms()$contractid else NULL)
    
    
    searchResult1 <- reactive({
        ru.sub <- roster.unique 
        pl.list <- ru.sub$firstlast[order(ru.sub$last)]
        pl.list <- pl.list[nchar(pl.list) > 3]
        pl.list[grep(tolower(input$nameSearch1), tolower(pl.list))]
    })
    output$nameSelect1 <- renderUI(selectInput ("player1", "", searchResult1()))

    contract.choices <- reactive ({
        firstyear <- contracts.complete %>% filter (woiid == this.woi.id()) %>% group_by (Contract.ID) %>%
            summarize (FirstSeason = first(Year)-1) %>% ungroup %>% arrange(FirstSeason)
        ##print(firstyear); print(firstyear$FirstSeason)
        firstyear
    })
    pick.contract <- reactive ({
        print (onload.contract())
        print (contract.choices()$Contract.ID)
        thispick <- match(onload.contract(), contract.choices()$Contract.ID)
        message ("This pick ", thispick)
        output <- if (is.null(onload.contract())) NULL else {
            if (is.na(thispick)) NULL else contract.choices()$FirstSeason[thispick]
        }
    })
    output$contractSelect <- renderUI(selectInput ("contract", "Contract Beginning",
                                                   choices = contract.choices()$FirstSeason
                                                   , selected = pick.contract()
                                                   ))
    this.contractID <- reactive({
        message ("Contract year: ", input$contract)
        contract.choices()$Contract.ID[match(input$contract, contract.choices()$FirstSeason)]
    })
    
    outdest <- reactive(paste0 ("http://war-on-ice.com/buyout.html?",
                                "woiid=", this.woi.id(),
                                "&contractid=", this.contractID()))

    this.id <- reactive(match(input$player1, roster.unique$firstlast))
    this.woi.id <- reactive(roster.unique$woi.id[this.id()])
    
    offseason.dates <- reactive ({
        message (this.woi.id())
        message (this.contractID())
        Years <- filter (contracts.complete, woiid == this.woi.id(), Contract.ID == this.contractID())$Year
        Years[-length(Years)]
    })
    output$DateSelect <- renderUI(selectInput('actionDate', 'Off-Season Action Date',
                                              choices=offseason.dates(),
                                              selected = if (2016 %in% offseason.dates()) 2016 else NULL))

    
    output$shareDestination <- renderText({ 
        input$sharePage
        outdest()
    })

    
    
        
    oid <- reactive({
        id <- match(input$player1, roster.unique$firstlast)
        list(roster.unique$firstlast[id], roster.unique$DOB[id],
             roster.unique$Shoots[id], roster.unique$Height[id],
             roster.unique$Weight[id], roster.unique$pos[id])
    })
    player.position <- reactive({
        ppos <- unlist(strsplit(oid()[[6]],""))
        c("Center", "Right Wing", "Left Wing", "Defense", "Goaltender")[match(ppos, c("C","R","L","D","G"))]
    })
    
    output$player.info <- renderUI(list(
        ## h4(oid()[[1]]),
        h6(paste("Position:", paste(player.position(), collapse=", "))),
        h6(paste("Date of Birth:", oid()[[2]])),
        h6(paste("Shoots:", oid()[[3]])),
        h6(paste("Height:", oid()[[4]])),
        h6(paste("Weight:", oid()[[5]]))
        ))
    

    player.contracts <-
        reactive ({
            message ("WOI ID: ",this.woi.id())
            contract.table <- 
                filter (contracts, woiid == this.woi.id()) %>%
                    mutate (Name = roster.unique$firstlast[match(this.woi.id(), woiid)]) %>%
                        arrange (Year) ## %>%
                           ## group_by (Contract.ID) %>% ## mutate (AAV = mean(TotalComp)) %>%
                              ##  select (-Contract.ID)

            contract.table$AAV [is.na(contract.table$Contract.Length)] <- NA
            contract.table
        })
    
    player.age <- reactive({
        yeardiff(paste0(input$actionDate,"-06-15"), roster.unique$DOB[this.id()])
    })
    ##player.age.at.contract.start <- reactive(ageAtEnd(current.contract()$Year[1]-1, roster.unique$DOB[this.id()]))
    player.age.at.contract.start <-
        reactive(ageAsOfJun30(paste0(current.contract()$Year[1]-1,current.contract()$Year[1]),
                              roster.unique$DOB[this.id()]))
    
    output$thisDisplay <- renderUI(h6(paste(if (contract.35plus()) "35+ Contract\n" else "", "Age on Decision Date:", player.age())))

    current.contract <- reactive ({
        out <- player.contracts() %>%
            select (Contract.ID, Year, AAV, TotalComp, NHL.Salary, Signing.Bonus) ##%>% rename (AAV = aAAV)

        ##print(input$actionDate)
        ##print(match(input$actionDate, out$Year))

        filter (out, Contract.ID == this.contractID())
    })
    
    contract.35plus <- reactive (player.age.at.contract.start() >= 35 && nrow(current.contract()) >= 2)
   
    buyout.pieces <- reactive ({

        ## Buyout eligibility
        ## print(current.contract())
        
        out <- current.contract()
        lastyear <- as.numeric(input$actionDate)
        message ("Buyout player age is ", player.age(), " given ", paste0(input$actionDate,"-06-15"), " ", roster.unique$DOB[this.id()])
        
        buyout.fraction <- (1 + 1*(player.age() >= 26)) / 3
        na.zero <- function(value) {value[is.na(value)] <- 0; value}
        
        years.remaining <- sum(out$Year > lastyear)
        if (years.remaining > 0) {
            total <- sum(anac(out$NHL.Salary[out$Year > lastyear]))
            new.data <- data.frame (Year = max(out$Year) + 1:years.remaining)
            integrated <- rbind_list(out, new.data) %>%
                mutate (Buyout = total*buyout.fraction/2*(Year > lastyear)/years.remaining,
                        New.AAV = na.zero(AAV) - na.zero(NHL.Salary) + Buyout)
            if (contract.35plus()) integrated$New.AAV <- integrated$AAV
            integrated$New.AAV[as.numeric(integrated$Year) <= lastyear] <- NA
        } else {
            total <- 0
            integrated <- out %>% mutate (Buyout = 0, New.AAV = AAV)
        }
        
        list (integrated=integrated,
              total.salary=total,
              buyout.fraction=buyout.fraction,
              years.remaining=years.remaining)
        
    })

    ## sprintf("%.3f", TotalComp/1E6,3)

    buyout.data <- reactive (
        buyout.pieces()$integrated %>%
        mutate (AAV = currform(AAV),
                TotalComp = currform(TotalComp),
                NHL.Salary = currform(NHL.Salary),
                Signing.Bonus = currform(Signing.Bonus),
                Buyout = currform(Buyout),
                New.AAV = currform(New.AAV)
                ) %>% select (-Contract.ID) %>% mutate (Year = paste (Year-1,substr(Year,3,4),sep="-"))
        )
    output$salaryTwo = renderDataTable({
        buyout.data()
    }, options = list(searching = FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=30)
        )
    fn.buy <- reactive(paste0("buyout-war-on-ice-", this.woi.id(),".csv"))
    output$downloadBuyout <- downloadHandler (
        filename = fn.buy(),
        content = function (file) write.csv (buyout.data() %>% mutate(Name=input$player1, woi.id=this.woi.id()) %>% as.data.frame, file)
        )

    recapture.pieces <- reactive ({

        ## Buyout eligibility
        out <- current.contract()
        lastyear <- as.numeric(input$actionDate)
        message(player.age())
        buyout.fraction <- (1 + 1*(player.age() >= 26)) / 3
        na.zero <- function(value) {value[is.na(value)] <- 0; value}
        
        years.remaining <- sum(out$Year > lastyear)
        years.on <- which(out$Year <= lastyear)
        if (years.remaining > 0) {

            cap.diff <- sum(out$TotalComp[years.on] - out$AAV[years.on])
            message ("Total cap recapture penalty: ", cap.diff)
            cap.diff <- max (0, cap.diff)
            out <- mutate(out, Penalty = cap.diff/years.remaining*(Year > lastyear))                        
            
        } else {
            out <- out %>% mutate (Penalty = 0)
        }

        list (integrated=out)
        
##        out
    })


    recapture.eligible <- reactive(nrow(current.contract()) >= 8) ## && current.contract()$Year[1] <= 2011)

    recap.data <- reactive({
        mainout <- if (!contract.35plus())
            recapture.pieces()$integrated %>% ungroup %>% 
                mutate (AAV = currform(AAV),
                        TotalComp = currform(TotalComp),
                        NHL.Salary = currform(NHL.Salary),
                        Signing.Bonus = currform(Signing.Bonus),
                        Penalty = currform(Penalty)
                        ) %>% 
                            select (Year, AAV, TotalComp, NHL.Salary, Signing.Bonus, Penalty) %>%
                                mutate (Year = paste (Year-1,substr(Year,3,4),sep="-"))    else
        current.contract() %>%
            mutate (AAV = currform(AAV),
                    TotalComp = currform(TotalComp),
                    NHL.Salary = currform(NHL.Salary),
                    Signing.Bonus = currform(Signing.Bonus)) %>% ungroup %>% 
                        select (Year, AAV, TotalComp, NHL.Salary, Signing.Bonus, Penalty) %>% mutate (Year = paste (Year-1,substr(Year,3,4),sep="-"))
        ## buyout.pieces() #%>% group_by (Contract.ID) %>% mutate (AAV = mean(TotalComp))

        if (!recapture.eligible() | this.woi.id() == "luongro79") mainout <- current.contract()[-(1:nrow(current.contract())),]
        mainout
    })
    output$salaryRecapture = renderDataTable({
        recap.data()
    }, options = list(searching = FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=30)
        )
    output$downloadRecap <- downloadHandler (
        filename = paste0("recapture-war-on-ice-", this.woi.id(), ".csv"),
        content = function (file) write.csv (recap.data() %>% as.data.frame, file)
        )


    output$description = renderUI ({
            if (input$tabset == "Buyout Calculator") list (
                    h6(paste0("At age ",player.age()," player's buyout is set at ",(1 + 1*(player.age() >= 26)),"/3 total remaining salary due.")),
                    h6(paste0("A buyout would eliminate the remaining ",
                              buyout.pieces()$years.remaining," from the contract, at a total value of $",
                              sprintf("%.3f", buyout.pieces()$total*buyout.pieces()$buyout.fraction/1E6, 3),
                              "MM in buyout payments, plus $",
                              sprintf("%.3f", sum(buyout.pieces()$integrated$Signing.Bonus[buyout.pieces()$integrated$Buyout > 0], na.rm=TRUE)/1E6, 3), "MM in bonuses."))
                    
                    ##                total.salary=total, buyout.fraction=buyout.fraction, years.remaining=years.remaining
                    ) else
            if (input$tabset == "Cap Recapture Penalties")  if(this.woi.id() == "luongro79") list(h6("Luongo's split contract gets its own page for cap recapture.")) else {
                if (recapture.eligible()) {
                    if (!contract.35plus()) list (
                        h6 (if (sum(recapture.pieces()$integrated$Penalty) <= 0) "No cap-recapture penalties will be applied if player retires at this point in time." else paste0("The total cap-recapture penalty to apply to team's AAV would be $", sprintf("%.3f", sum(recapture.pieces()$integrated$Penalty)/1e6, 3), "MM."))
                        ) else list (h6("Players on 35+ contracts are charged the full cap hit after retirement."))
                } else list (h6("This contract is not subject to recapture penalties."))
            }   

    })



    

  
})
