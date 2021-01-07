#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(httr)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(padr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    user <- reactive({
        validate(
            need(input$token != "", "Please provide your FB Token")
        )
        url <- "https://graph.facebook.com/v9.0/me"
        payload <- list(
            access_token = input$token,
            fields = "id,name"
        )
        r <- GET(url, query=payload)
        content(r, "parsed")
    })
    
    ad_accounts <- reactive({
        user_id <- user()$id
        
        url <- paste0("https://graph.facebook.com/v9.0/", user_id, "/adaccounts")
        payload <- list(
            access_token = input$token,
            fields = "id,name,currency"
        )
        r <- GET(url, query=payload)
        temp <- content(r, "parsed")
        df <- temp$data %>% 
            as.data.frame() %>% 
            t() %>% 
            as.data.frame() %>% 
            mutate(
                rnames = rownames(.),
                attr = str_extract(rnames, "[a-z]*"),
                no = str_extract(rnames, "\\d+")
            ) %>% 
            pivot_wider(-rnames, names_from = attr, values_from = V1) %>% 
            select(-no)
        
        while(!is.null(temp$paging$`next`)) {
            url <- temp$paging$`next`
            r <- GET(url, query=payload)
            temp <- content(r, "parsed")
            df_temp <- temp$data %>% 
                as.data.frame() %>% 
                t() %>% 
                as.data.frame() %>% 
                mutate(
                    rnames = rownames(.),
                    attr = str_extract(rnames, "[a-z]*"),
                    no = str_extract(rnames, "\\d+")
                ) %>% 
                pivot_wider(-rnames, names_from = attr, values_from = V1) %>% 
                select(-no)
            df <- rbind(df, df_temp)
        }
        df
    })
    
    campaigns <- reactive({
        act_id <- ad_accounts() %>% 
            filter(name == input$adaccounts) %>% 
            pull(id)
        
        url <- paste0("https://graph.facebook.com/v9.0/",
                      act_id,
                      "/campaigns")
        payload <- list(
            access_token = input$token,
            fields = "name,status,daily_budget,start_time"
        )
        r <- GET(url, query=payload)
        temp <- content(r, "parsed")
        df <- temp$data %>% 
            as.data.frame() %>% 
            t() %>% 
            as.data.frame() %>% 
            mutate(
                rnames = rownames(.),
                attr = str_extract(rnames, "[a-z_]*"),
                no = str_extract(rnames, "\\d+")
            ) %>% 
            pivot_wider(-rnames, names_from = attr, values_from = V1) %>% 
            select(-no) %>% 
            mutate(
                start_time = ymd_hms(start_time)
            )
        
        while(!is.null(temp$paging$`next`)) {
            url <- temp$paging$`next`
            r <- GET(url, query=payload)
            temp <- content(r, "parsed")
            df_temp <- temp$data %>% 
                as.data.frame() %>% 
                t() %>% 
                as.data.frame() %>% 
                mutate(
                    rnames = rownames(.),
                    attr = str_extract(rnames, "[a-z_]*"),
                    no = str_extract(rnames, "\\d+")
                ) %>% 
                pivot_wider(-rnames, names_from = attr, values_from = V1) %>% 
                select(-no) %>% 
                mutate(
                    start_time = ymd_hms(start_time)
                )
            df <- bind_rows(df, df_temp)
        }
        df
    })
    
    account_insights <- reactive({
        account_df <- ad_accounts()
        act_id <- account_df %>% pull(id)
        df <- data.frame()
        for(i in 1:length(act_id)){
            url <- paste0("https://graph.facebook.com/v9.0/",
                          act_id[i],
                          "/insights")
            payload <- list(access_token = input$token,
                            fields = "purchase_roas,objective,spend",
                            date_preset = "this_month",
                            time_increment = 1,
                            limit = 31)
            r <- GET(url, query=payload)
            temp <- content(r, "parsed")
            
            if(length(temp$data)>0){
                df_temp <- temp$data %>% 
                    as.data.frame() %>% 
                    t() %>% 
                    as.data.frame() %>% 
                    mutate(
                        fields = rownames(.),
                        no = str_extract(fields, "\\d+"),
                        var = str_remove(str_extract(fields, "\\D+"), "\\.$")
                    ) %>% 
                    select(no, V1, var) %>% 
                    pivot_wider(no, names_from='var', values_from='V1')
                if(!"purchase_roas.value" %in% colnames(df_temp)){
                    df_temp$purchase_roas.value <- NA
                }
                df_temp <- df_temp %>% 
                    mutate(
                        date = ymd(date_start),
                        spend = as.numeric(spend)
                    ) %>% 
                    select(no, purchase_roas.value, spend, date) %>% 
                    mutate_if(is.character, as.numeric) %>% 
                    mutate(
                        revenue = purchase_roas.value * spend
                    ) %>% 
                    select(purchase_roas.value, spend, date, revenue)
                df_temp$id <- act_id[i]
                df <- bind_rows(df, df_temp)
            }
        }
        df
    })
    
    output$select_account <- renderUI({
        df <- account_insights()
        df_act <- ad_accounts()
        active_accounts <- df %>% 
            pull(id) %>% 
            unique()
        account_name <- df_act %>%
            filter(id %in% active_accounts) %>%
            pull(name)
        selectInput("adaccounts",
                    label = "Select Ad Account",
                    choices = account_name,
                    selected = F)
    })
    
    output$user <- renderUI({
        h4(paste("Hi,", user()$name))
    })
    
    output$`active-month` <- renderUI({
        h5(paste("Active Accounts", month(Sys.Date(), label=T, abbr = F), year(Sys.Date())))
    })
    
    output$`pacing-table` <- renderDataTable({
        df <- account_insights()
        df_act <- ad_accounts()
        df_agg <- df %>%
            group_by(id) %>% 
            summarise(
                active_days = length(date),
                spend = sum(spend),
                start_date = min(date),
                revenue = sum(revenue, na.rm = T)
            )
        
        df_agg %>% 
            left_join(df_act) %>% 
            select(name, active_days, start_date, currency, spend, revenue) %>% 
            mutate(
                roas = revenue/spend
            ) %>% 
            arrange(desc(roas))
    })
    
    output$`active-campaigns` <- renderDataTable({
        campaigns() %>% 
            select(-id) %>% 
            arrange(desc(start_time))
    })
    
    output$`pacing-plot` <- renderPlot({
        max_month <- as.Date(str_extract(ceiling_date(Sys.Date(), unit = "months")-1, "[0-9\\-]*"))
        date_budget <- data.frame(date = seq(floor_date(Sys.Date(), unit = "months"), max_month, "day"),
                                  default_budget = input$budget / day(max_month))
        
        act_id <- ad_accounts()
        selected_id <- act_id %>% 
            filter(name == input$adaccounts) %>% 
            pull(id)
        
        account_ins <- account_insights() %>% 
            filter(id == selected_id)
        total_spent <- account_ins %>% 
            pull(spend) %>% 
            sum()
        
        rem_date <- day(max_month) - day(Sys.Date())
        
        rem_budget <- input$budget - total_spent
        budget_adj <- rem_budget / rem_date
        rem_date <- date_budget %>%
            filter(!date %in% account_ins$date) %>% 
            pull(date)
        
        budget_df <- data.frame(
            date = rem_date,
            adjustment = budget_adj
        )
        
        account_ins %>% 
            right_join(date_budget) %>% 
            left_join(budget_df) %>% 
            mutate(
                cumulative_spending = cumsum(spend),
                cumulative_budget = cumsum(default_budget),
                projected_spending = replace_na(spend, budget_adj),
                cumulative_adj = cumsum(projected_spending)
            ) %>% 
            ggplot(aes(x=date, y=cumulative_spending)) +
            geom_area(fill = "#ed4b09") +
            geom_line(aes(y = cumulative_budget), 
                      color = "#81cfe3", size = 0.8, lty = 2, group = 1) +
            geom_line(aes(y = cumulative_adj), group = 1, size = 0.8, lty = 4, group = 1, color = "tomato3") +
            theme_minimal() +
            labs(
                title = "Cumulative Spending"
            )
    
    })
    
    output$`pacing-opt` <- renderUI({
        account_ins <- account_insights()
        max_date <- account_ins %>% 
            pull(date) %>% 
            max() %>% 
            day()
        max_month <- ceiling_date(Sys.Date(), unit = "months") - 1
        budget_adj <- account_ins %>% 
            filter(id == input$adaccounts) %>% 
            pull(spend) %>% 
            sum()
        remaining_days <- day(max_month) - max_date
        verticalLayout(
            valueBox(subtitle = "Remaining Day", 
                     value = remaining_days, 
                     width = 12, color = "blue"),
            valueBox(subtitle = "Recommendation Adjustment", 
                     value = round((input$budget - budget_adj)/remaining_days,2),
                     width = 12, color = "blue")
        )
        
    })

})
