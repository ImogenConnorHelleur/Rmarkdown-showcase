################################################################################
#function to get dental data into the right format for slide 4
get_delivery_data_calendar <- function(calendar_data = UDA_calendar_data,  
                                       scheduled_data = UDA_scheduled_data,
                                       remove_prototypes = T,
                                       UDAorUOA = "UDA",
                                       regional_lines = F,
                                       STP_lines = F,
                                       cat_lines = F, 
                                       renameColumns = F){
  
  if(UDAorUOA == "UDA"){
    #join in contracted UDAs from scheduled data
    contracted_UDAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UDA)
    
    data <- calendar_data %>%
      left_join(contracted_UDAs, by = c("month", "contract_number"))
  }else{
    contracted_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA, annual_contracted_UDA)
    
    data <- calendar_data %>%
      left_join(contracted_UOAs, by = c("month", "contract_number"))
  }
  
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 0)
  }
  
  if(regional_lines){
    data <- data %>%
      group_by(month, region_name)
  }else if(STP_lines){
    data <- data %>%
      group_by(month, commissioner_name)
  }else if(cat_lines){
    data <- data %>%
      group_by(month, category_sub_type)
  }else{
    data <- data %>%
      group_by(month)
  }
  
  data <- data %>%
    summarise(monthly_UDA_UOAs_delivered = ifelse(UDAorUOA == "UDA",
                                                  sum(UDA_total, na.rm = T),
                                                  sum(UOA_total, na.rm = T)),
              annual_contracted_UDA_UOA = ifelse(UDAorUOA == "UDA",
                                                 sum(annual_contracted_UDA, na.rm = T),
                                                 sum(annual_contracted_UOA, na.rm = T))) %>%
    mutate(scaled_monthly_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 12) %>%
    mutate(scaled_perc_UDA_UOA_delivered = monthly_UDA_UOAs_delivered * 12 * 100 / annual_contracted_UDA_UOA) %>%
    mutate(month = as.Date(month))
  
  
  #for PCOR and SOF table
  if(renameColumns){
    
    #add a region column to the data
    region_STP_lookup <- calendar_data %>%
      select(commissioner_name, region_name) %>%
      distinct()
    
    data <- left_join(data, region_STP_lookup, by = c("commissioner_name"))
    
    data <- data %>%
      select(calendar_month = month,
             commissioner_name,
             region_name,
             monthly_UDAs_delivered = monthly_UDA_UOAs_delivered,
             scaled_monthly_UDAs_delivered = scaled_monthly_UDA_UOAs_delivered,
             annual_contracted_UDAs = annual_contracted_UDA_UOA,
             scaled_perc_UDAs_delivered = scaled_perc_UDA_UOA_delivered)
  }
  
  data
}