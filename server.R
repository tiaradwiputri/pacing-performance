#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(shiny)
library(DT)
library(httr)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)
library(scales)
library(glue)
library(googlesheets4)

gs4_auth(
    cache = ".secrets",
    email = "tiara@leaf.fm"
)

extract_main_metrics <- function(x,y){
    x %>% 
        filter(action_type == y) %>% 
        pull(value)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    user <- reactive({
        shiny::validate(
            need(input$token != "", "Please input your Facebook token")
        )
        url <- paste0("https://graph.facebook.com/v9.0/me")
        # token to be changed with input$token
        payload <- list(access_token = input$token,
                        fields = "id,name")
        r <- GET(url, query=payload)
        content(r, "parsed")
    })
    
    ad_accounts <- reactive({
        user_id <- user()$id
        
        url <- paste0("https://graph.facebook.com/v9.0/", user_id, "/adaccounts")
        # token to be changed with input$token
        payload <- list(
            access_token = token,
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
    
    client_metrics <- reactive({
        read_sheet("11KkNlg4bMvkGDUhNrktubodPtpykHnLv9zWhgwEzP8I")
    })
    
    account_insights <- reactive({
        df_act <- ad_accounts()
        metrics <- client_metrics()
        df_act <- df_act %>% 
            left_join(metrics, by = c("name" = "Clients"))
        act_id <- df_act %>% pull(id)
        df <- data.frame()
        
        #iterate all ad accounts
        for(i in 1:length(act_id)){
            url <- paste0("https://graph.facebook.com/v9.0/",
                          act_id[i],
                          "/insights")
            payload <- list(access_token = token,
                            fields = "account_id,spend,purchase_roas,unique_actions,actions",
                            date_preset = "this_month",
                            time_increment = 1,
                            limit = 31)
            r <- GET(url, query=payload)
            temp <- content(r, "text", encoding = "UTF-8")
            temp <- fromJSON(temp)
            if(length(temp$data) > 0){
                if("purchase_roas" %in% colnames(temp$data)){
                    temp$data <- temp$data %>% 
                        unnest(purchase_roas) %>% 
                        rename(purchase_roas = value) %>% 
                        select(-action_type)
                } else {
                    temp$data$purchase_roas <- NA
                }
                df_temp <- temp$data %>% 
                    mutate(
                        purchase = map(unique_actions, . %>% filter(action_type == "omni_purchase") %>% pull(value)),
                        purchase = as.integer(purchase),
                        lpv = map(unique_actions, . %>% filter(action_type == "landing_page_view") %>% pull(value)),
                        lpv = as.integer(lpv),
                        click = map(unique_actions, . %>% filter(action_type == "link_click") %>% pull(value)),
                        click = as.integer(click),
                        purchase_roas = as.numeric(purchase_roas),
                        spend = as.numeric(spend)
                    ) %>%  
                    mutate_if(is.numeric, list(~replace_na(., 0)))
                df <- bind_rows(df, df_temp)
            }
        }
        
        df %>%
            select(account_id, date_start, spend, purchase_roas, unique_actions, actions) %>% 
            mutate(
                date_start = ymd(date_start),
                revenue = purchase_roas * spend,
                account_id = paste0("act_", account_id)
            ) %>% 
            left_join(df_act_m, by=c("account_id" = "id")) %>% 
            replace_na(list(`Action Name` = 'link_click')) %>% 
            mutate(
                actions_metric = `Action Name`,
                result = case_when(is.na(Unique) ~ map2(actions, actions_metric, extract_main_metrics),
                                   TRUE ~ map2(unique_actions, actions_metric, extract_main_metrics)),
                result = as.integer(result),
                revenue = purchase_roas * spend,
                purchase = map(unique_actions, . %>% filter(action_type == "omni_purchase") %>% pull(value)),
                purchase = as.integer(purchase),
                lpv = map(unique_actions, . %>% filter(action_type == "landing_page_view") %>% pull(value)),
                lpv = as.integer(lpv),
                click = map(unique_actions, . %>% filter(action_type == "link_click") %>% pull(value)),
                click = as.integer(click)
            ) %>% 
            group_by(account_id) %>% 
            mutate(
                active_days = length(date_start)
            ) %>%  
            select(account_id, name, date_start,active_days, currency, spend, revenue, `Main Metrics`, result, purchase, lpv, click)
    })
    
    campaigns <- reactive({
        df_act <- ad_accounts() %>% 
            filter(name == input$`account-campaign`) %>% 
            pull(id)
        
        url <- paste0("https://graph.facebook.com/v9.0/",df_act,"/campaigns")
        
        payload <- list(
            access_token = token,
            fields = "account_id,name,daily_budget,lifetime_budget"
        )
        r <- GET(url, query=payload)
        temp <- content(r, "text", encoding = "UTF-8")
        paging <- content(r, "parsed")
        
        df <- fromJSON(temp)$data
        
        while(!is.null(paging$paging$`next`)){
            url <- paging$paging$`next`
            r <- GET(url, query=payload)
            temp <- content(r, "text", encoding = "UTF-8")
            paging <- content(r, "parsed")
            df_temp <- fromJSON(temp)$data
            df <- bind_rows(df, df_temp)
        }
        
        df %>% 
            rename(campaign_name = name) %>% 
            pivot_longer(cols=c(daily_budget,lifetime_budget), values_to="budget") %>% 
            na.omit()
    })
    
    campaign_insight <- reactive({
        df_campaign <- campaigns()
        campaigns_id <- df_campaign %>% pull(id)
        
        df <- data.frame()
        
        for(i in 1:length(campaigns_id)){
            url <- paste0("https://graph.facebook.com/v9.0/",
                          campaigns_id[i],
                          "/insights")
            # to be changed with input$token
            payload <- list(access_token = input$token,
                            fields = "campaign_id,objective,spend,unique_actions,actions,purchase_roas,conversions",
                            date_preset = "this_month",
                            time_increment = 1,
                            limit = 31)
            r <- GET(url, query=payload)
            temp <- content(r, "text", encoding = "UTF-8")
            temp <- fromJSON(temp)
            if(length(temp$data) > 0){
                if(is.null(temp$data$purchase_roas)){
                    temp$data$purchase_roas <- NA
                }
                df_temp <- temp$data %>% 
                    unnest(purchase_roas) %>%
                    select(-date_stop) %>% 
                    mutate(date_start = ymd(date_start))
                df <- bind_rows(df, df_temp)
            }
        }
        
        df_r <-  df %>% 
            select(-purchase_roas) %>% 
            rename(purchase_roas = value) %>% 
            select(-action_type) %>% 
            mutate(
                conversions = replace_na(conversions, list(data.frame(action_type = NA, value = NA))),
                result = case_when(objective == "LINK_CLICKS" ~ map(actions, (. %>% 
                                                                                  filter(action_type == "link_click") %>% 
                                                                                  pull(value))),
                                   objective == "CONVERSIONS" ~ map(conversions, (. %>% pull(value))),
                                   objective == "PRODUCT_CATALOG_SALES" ~ map(conversions, (. %>% pull(value))),
                                   TRUE ~ list(NA)),
                result = as.character(result)
            )
        
        df_r
    })
    
    pacing_df <- reactive({
        shiny::validate(
            need(input$adaccounts != "", "Please select Ad Account(s)")
        )
        
        df_insight <- account_insights()
        
        max_month <- as.Date(str_extract(ceiling_date(Sys.Date(), unit = "months")-1, "[0-9\\-]*"))
        last_date <- max(df_insight$date_start)
        rem_date <- day(max_month) - day(Sys.Date())
        
        
        # for shiny input
        budget <- c()
        for(i in 1:length(input$adaccounts)){
            budget <- c(budget, input[[paste0("budget_",i)]])
        }
        
        budget_df <- data.frame(accounts = input$adaccounts, budget = budget) %>% 
            mutate(
                daily_budget = budget / day(max_month)
            ) %>% 
            left_join(df_act, by = c("accounts" = "name"))
        
        # date holder
        date_b <- data.frame(date = seq(floor_date(Sys.Date(), unit = "months"), max_month, "day"))
        
        selected_insight <- df_insight %>% 
            filter(name %in% budget_df$accounts) %>% 
            right_join(date_b, by=c("date_start" = "date")) %>% 
            ungroup() %>% 
            select(account_id, date_start,spend, active_days) %>% 
            complete(account_id, date_start) %>% 
            drop_na(account_id)
        
        account_budget <- selected_insight %>% 
            group_by(account_id) %>% 
            summarise(
                total_spend = sum(spend, na.rm = T)
            ) %>%
            left_join(budget_df,by = c("account_id" = "id")) %>% 
            mutate(
                remaining = budget - total_spend,
                daily_adj = remaining / rem_date
            )
        
        selected_insight %>% 
            # add account information
            left_join(account_budget) %>% 
            select(account_id, accounts, currency, date_start, spend, budget, daily_budget, remaining, daily_adj, active_days, total_spend) %>% 
            # create cumsum information per id
            group_by(account_id) %>% 
            mutate(
                spend_cum = cumsum(replace_na(spend,0)),
                daily_adj = ifelse(date_start > Sys.Date(), daily_adj, spend),
                spend_cum = ifelse(date_start > Sys.Date(), NA, spend_cum),
                daily_budget_cum = cumsum(daily_budget),
                daily_adj = replace_na(daily_adj, 0),
                daily_adj_cum = cumsum(daily_adj),
                daily_adj = ifelse(daily_adj == 0, lead(daily_adj), daily_adj),
                daily_adj_cum = ifelse(date_start >= (Sys.Date()), daily_adj_cum, NA),
                # create currency as name in facet
                name = paste0(accounts, " (", currency, ")")
            ) %>% 
            group_by(accounts) %>% 
            fill(active_days, .direction = "downup")
    })
    
    output$select_account <- renderUI({
        df <- account_insights()
        account_name <- df %>% 
           pull(name)
        selectInput("adaccounts",
                    label = "Select Ad Account",
                    choices = account_name,
                    selected = F, multiple = T)
    })
    
    output$user <- renderUI({
        h4(paste("Hi,", user()$name))
    })
    
    output$`active-month` <- renderUI({
        h5(paste("Active Accounts", month(Sys.Date(), label=T, abbr = F), year(Sys.Date())))
    })
    
    output$`account-perf` <- renderDataTable({
        acct_ins <- account_insights()
        acct_ins %>% 
            group_by(name) %>% 
            summarise(
                currency = first(currency),
                date_start = first(date_start),
                spend = sum(spend, na.rm = T),
                revenue = sum(revenue, na.rm = T),
                purchase = sum(purchase, na.rm = T),
                lpv = sum(lpv, na.rm = T),
                click = sum(click, na.rm = T),
                active_days = first(active_days),
                main_metrics = first(`Main Metrics`),
                results = sum(result, na.rm = T)
            ) %>% 
            mutate(
                roas = revenue/spend,
                CPR = spend/results
            ) %>% 
            arrange(-roas) %>% 
            mutate(
                spend = comma(accuracy = 0.01, spend),
                roas = comma(accuracy = 0.01, roas),
                CPR = comma(accuracy = 0.01, CPR)
            ) %>% 
            select(name, active_days, date_start, currency, spend, roas, CPR, main_metrics)
    })
    
    output$`select-metrics` <- renderUI({
        selectInput("metrics", 
                    label = "Select metrics:", 
                    choices = c("Purchase", "LPV", "Click"))
    })
    
    output$`res-spend` <- renderPlotly({
        df <- account_insights()
        df_act <- ad_accounts()
        
        df_plot <- account_insight_res %>% 
                group_by(account_id) %>% 
                summarise(
                    date_start = first(date_start),
                    spend = sum(spend, na.rm = T),
                    revenue = sum(revenue, na.rm = T),
                    purchase = sum(purchase, na.rm = T),
                    lpv = sum(lpv, na.rm = T),
                    click = sum(click, na.rm = T),
                    active_days = first(active_days)
                ) %>% 
                mutate(
                    roas = revenue/spend,
                    account_id = paste0("act_",account_id)
                ) %>% 
                left_join(df_act, by = c("account_id" = "id")) %>% 
                mutate(
                    y = case_when(input$metrics == "Purchase" ~ purchase,
                                  input$metrics == "LPV" ~ lpv,
                                  input$metrics == "Click" ~ click)
                ) %>% 
                select(name, currency, spend, y) %>% 
                filter(y != 0)
        (df_plot %>% 
                ggplot(aes(x=spend, y=y)) +
                geom_point(color = "#ed4b09", alpha = 0.6, size = 2.5,
                           aes(text = glue("Ad Account: {name}
                   Purchase: {comma(y, accuracy=1)}
                   Spend: {comma(spend, accuracy=0.01)}"))) +
                facet_wrap(~currency) +
                scale_y_continuous(labels = comma) +
                scale_x_continuous(labels = comma) +
                labs(title = "Account Performance Overview", x = "Spend", y = input$metrics)+
                theme_minimal() +
                theme(axis.text = element_text(size = 6))) %>% 
            ggplotly(tooltip = "text")
    })
    
    output$budget_input <- renderUI({
        shiny::validate(
            need(!is.null(input$adaccounts), "Please select at least one ad account")
        )
        out <- list()
        for(i in 1:length(input$adaccounts)){
            out[[i]] <- numericInput(paste0("budget_",i), 
                                     label = paste0("Specify budget for ",input$adaccounts[i]),
                                     value = 0)
        }
        out
    })
    
    output$`pacing-plot` <- renderPlotly({
        df <- pacing_df()
        (df %>% 
            ggplot(aes(x=date_start, y=spend_cum)) +
            geom_area(fill="#ed4b09", 
            aes(text = glue("Total spend: {total_spend %>% comma(accuracy=0.01)}
                             Remaining: {remaining %>% comma(accuracy=0.01)}
                             Active days: {active_days %>% comma(accuracy=1)}"))) +
            geom_line(aes(y=daily_budget_cum, 
                          text = glue("Daily Budget: {daily_budget %>% comma(accuracy=0.01)}")), 
                      lty=5, color="tomato3") +
            geom_line(aes(y=budget, 
                          text = glue("Monthly Budget: {budget %>% comma(accuracy=0.01)}")), 
                      lty=3, color="slategray", size = 1) +
            geom_line(aes(y=daily_adj_cum, 
                          text = glue("Recommended Adjustment: {daily_adj %>% comma(accuracy=0.01)}"))
                      , lty=3, color="skyblue", size = 1) +
            facet_wrap(~name) +
            labs(title = "Account Pacing", x="Date", y="Cumulative Spending") +
            theme_minimal() +
            theme(strip.background = element_rect(fill = "lightgrey", color = NA))) %>% 
            ggplotly(tooltip = "text")
    })
    
    output$`select-account` <- renderUI({
        shiny::validate(
            need(length(input$adaccounts) > 0, "Please select an Ad Account")
        )
        selectInput("account-campaign", 
                    label = "Select Ad Account", 
                    choices = input$adaccounts,
                    selected = input$adaccounts[1])
        
    })
    
    observeEvent(input$`campaign-fetch`, {{
        output$`campaign-table` <- renderDataTable({
            df_r <- campaign_insight()
            df_r %>% 
                select(campaign_id, objective, date_start, result, spend, purchase_roas) %>% 
                left_join(df_campaign, by=c("campaign_id" = "id")) %>% 
                select(account_id, campaign_id, name, objective, effective_status, budget_type, value, spend, result, purchase_roas) %>% 
                mutate(
                    purchase_roas = as.numeric(purchase_roas),
                    spend = as.numeric(spend),
                    revenue = purchase_roas * spend,
                    result = as.numeric(result)
                ) %>% 
                group_by(campaign_id) %>% 
                summarise(
                    name = first(name),
                    objective = first(objective),
                    effetive_status = last(effective_status),
                    budget_type = last(budget_type),
                    value = last(value),
                    spend = sum(spend, na.rm = T),
                    result = sum(result, na.rm = T),
                    revenue = sum(revenue, na.rm = T)
                ) %>% 
                mutate(
                    purchase_roas = revenue/spend,
                    value = as.numeric(value),
                    value = comma(value, accuracy = 1),
                    spend = comma(spend, accuracy = 0.01),
                    result = comma(result, accuracy = 1),
                    purchase_roas = comma(purchase_roas, accuracy = 0.01)
                ) %>% 
                select(-campaign_id, -revenue)
        })
    }})
})
