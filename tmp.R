na_count <- function(df){
  for(i in 1:ncol(df)){
    cat( colnames(df)[i], "/ NA count =", sum(is.na(df[,i])), "\n")
  }  
}


norm_func <- function(df, y){
  tmp_len1 <- length(df)
  tmp_len2 <- length(y)
  
  for(i in y){
    tmp_min = min(df[,i], na.rm = T)
    tmp_max = max(df[,i], na.rm = T)
    tmp_col = (df[,i] - tmp_min) / (tmp_max - tmp_min)
    df <- cbind(df, tmp_col)
  }
  
  colnames(df)[ (tmp_len1+1) : (tmp_len1+tmp_len2)] <- paste0( colnames(df)[y], "_norm")
  
  return(df)
}