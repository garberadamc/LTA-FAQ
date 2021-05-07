
# This code is adapted from the source code for the `plotLTA` function found in from the `MplusAutomation` package:
# Link to MplusAuotomation source code:
# https://github.com/michaelhallquist/MplusAutomation/blob/995d1ecfae3656524153456ce647f86fe8c1cf1e/R/mixtures.R

# mplus_model <- readModels(filefilter = "sci_attitude", quiet = TRUE)

plot_transitions_function <- function(model_name,color_pallete,facet_labels,timepoint_labels, class_labels){
  
  node_stroke = 2
  edges <- model_name$class_counts$transitionProbs
  
  all_classes <- unique(c(edges$from, edges$to))
  latent_variables <- unique(gsub("\\..+$", "", all_classes))
  edges$x <- as.numeric(factor(gsub("\\..+$", "", as.character(edges$from)), levels = latent_variables))
  edges$xend <-as.numeric(factor(gsub("\\..+$", "", as.character(edges$to)), levels = latent_variables))
  edges$y <- as.numeric(gsub("^.+\\.", "", as.character(edges$from)))
  edges$yend <- as.numeric(gsub("^.+\\.", "", as.character(edges$to)))
  
  edges <- edges %>% 
    mutate(trans_perc = round(probability*100,0)) %>% 
    mutate(y_mid = y + ((yend - y)/2)) 
  
  nodes <- rbind(edges[, c(1, 4, 6)], setNames(edges[, c(2, 5, 7)], names(edges)[c(1, 4, 6)]))
  nodes <- nodes[!duplicated(nodes),]
  names(nodes)[1] <- "nodeID"
  
  n_prop <- model_name$class_counts$modelEstimated %>% 
    mutate(percent = round(proportion*100,0))
  
  n_prop$proportion <- node_stroke * n_prop$proportion * inverse.rle(list(
    lengths = rle(n_prop$variable)$lengths,
    values = rle(n_prop$variable)$lengths))
  
  n_prop$nodeID <- paste(n_prop$variable, n_prop$class, sep = ".")
  nodes <- merge(nodes, n_prop, by = "nodeID") 
  nodesize <- max(max(sapply(nodes$nodeID, nchar)) * 4.5, 6)

  nodes2 <- nodes %>% rename(x1 = x, y1 = y) %>% 
    mutate(class = factor(class))
  
  pallete <- color_pallete
  
  p <- ggplot(NULL) +
    geom_segment(data = edges,
                 aes(x = x,y = y,xend = xend,yend = yend,size = probability,alpha = probability),
                 color = "black") +
    scale_alpha_continuous(range = c(.05,.5)) + 
    facet_wrap(~y, labeller = labeller( y = function(x) str_wrap(facet_labels,width = 40))) +
    scale_x_continuous(expand = c(.1, .1), breaks = c(1,2), labels = timepoint_labels) + 
    scale_y_reverse(expand = c(.15, .15), breaks = c(1,2,3,4), labels = class_labels) +
    geom_text_repel(data=edges, aes(x=xend, y=y_mid, label = glue("{trans_perc}%")), 
                    segment.colour = NA, nudge_x = -.5) + 
    geom_point(data = nodes2,
               shape = 21, size = nodesize, color = "black",
               aes_string(x = "x1", y = "y1", stroke = "proportion", fill = "class"), show.legend = FALSE) +
    scale_fill_manual("", values = rev(pallete)) +
    geom_text(data = nodes2, aes(x = x1, y = y1, label = paste0(percent,"%"))) +
    theme_minimal() + labs(y="",x="") + 
    theme(legend.position = "none", panel.grid.minor.x = element_blank()) 
  
  p 
  return(p) 
}

# plot_transitions_function(
#   model_name = mplus_model,
#   color_pallete = pnw_palette("Bay", n=4, type = "discrete"),
#   facet_labels =c(
#     `1` = "Transitions to 10th Grade from the Pro-Science w/Elevated Utility Class",
#     `2` = "Transitions to 10th Grade from the Ambivalent w/Elevated Utility Class",
#     `3` = "Transitions to 10th Grade from the Ambivalent w/Minimal Utility Class",
#     `4` = "Transitions to 10th Grade from the Anti-Science w/Minimal Utility Class"),
#   timepoint_labels = c('1' = "7th Grade", '2' = "10th Grade"),
#   class_labels = c(
#     "Pro-Science / Elev. Utility",
#     "Ambivalent / Elev. Utility",
#     "Ambivalent / Min. Utility",
#     "Anti-Science / Min. Utility")
# )
  
  
  

  
  
  