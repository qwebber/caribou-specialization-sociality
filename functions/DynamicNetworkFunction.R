

#df <- locs[season == "winter" & HERD == "MIDRIDGE" & Year == "2011"]
#idField <- "ANIMAL_ID"
## Dynamic network function to calculate networks for each season:
"DynamicNetwork" = function(df, idField){
 
  d <- data.table::dcast(df, formula = group ~ get(idField), fun.aggregate = length,
                         value.var = 'group')
  
  gbi_df <- data.matrix(d[, !'group', with=FALSE])
  
  rownames(gbi_df) <- d$group

  gbi.net_df <- get_network(gbi_df, data_format="GBI",association_index="SRI")

  gbi.grph_df<- graph.adjacency(gbi.net_df,mode="undirected",diag=FALSE,weighted=TRUE)
  
  return(list(strength_soc = graph.strength(gbi.grph_df),
              gDen = graph.density(gbi.grph_df),
              ID = names(degree(gbi.grph_df)),
              N = length(names(degree(gbi.grph_df)))))
}
