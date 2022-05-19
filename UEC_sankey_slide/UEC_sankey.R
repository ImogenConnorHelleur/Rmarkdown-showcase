################################################################################
#library(networkD3)
plot_UEC_CPCS_sankey <- function(data = sankey_data){
  
  data <- select(data, -group)
  
  links <- data
  
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique(),
    group=c("UEC", "UEC", "UEC", "UEC", "UEC", 
            "Pharmacy", "Pharmacy", 
            "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", 
            "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", 
            "Sign-post", "Escalation", 
            "Dropped", 
            "End", 
            "End", 
            "Sign-post", "Sign-post", 
            "Sign-post", 
            "Escalation", 
            "Escalation", 
            "Escalation",
            "Escalation")
  )
  
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  my_color <- 'd3.scaleOrdinal() .domain(["UEC", "Pharmacy", "Sign-post", "Escalation",
  "Dropped", "End"]) 
  .range(["blue","orange","red", "red", "grey", "green"])'
  
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE,
                     colourScale=my_color, NodeGroup="group")
  p
  
  
}

################################################################################
plot_UEC_CPCS_sankey_grouped <- function(data = sankey_grouped){
  
  links <- data
  
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique(),
    group=c("Escalation", "Sign-post", "Pharmacy", "UEC", 
            "Escalation", "Escalation", "Escalation", 
            "Escalation", 
            "Sign-post", "Sign-post", 
            "Sign-post", "End", 
            "End", "Dropped")
  )
  
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  my_color <- 'd3.scaleOrdinal() .domain(["UEC", "Pharmacy", "Sign-post", "Escalation",
  "Dropped", "End"])
  .range(["blue","orange","red", "red", "grey", "green"])'
  
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE,
                     colourScale=my_color, NodeGroup="group",
                     fontSize = 14
  )
  p
  
}
