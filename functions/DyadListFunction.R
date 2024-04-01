




#df <- locs[season == "winter" & HERD == "MIDRIDGE" & Year == "2011"]
#idField <- "ANIMAL_ID"
## Dynamic network function to calculate networks for each season:
"DyadList" = function(df, idField){
  
  
  d <- data.table::dcast(df, formula = groupIG ~ get(idField), fun.aggregate = length,
                         value.var = 'groupIG')
  
  gbi_df <- data.matrix(d[, !'groupIG', with=FALSE])
  
  rownames(gbi_df) <- d$groupIG
  
  gbi.net_df <- get_network(gbi_df, data_format="GBI",association_index="SRI")
  
  gbi.net_df[lower.tri(gbi.net_df)] <- NA
  diag(gbi.net_df) <- NA
  
  dfDyads <- data.table(melt(gbi.net_df))
  dfDyads <- na.omit(dfDyads)
  
  return(data.table(ID1 = as.character(dfDyads$Var1),
              ID2 = as.character(dfDyads$Var2), 
              SRI = dfDyads$value))

}
