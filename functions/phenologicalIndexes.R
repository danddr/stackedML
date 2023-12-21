#'function to compute pseudo-phenological indexes: onset, peak and offset of the seasons
#' @param df: data frame with the observed or predicted egg numbers
#' @param field.ID: character, observations field in the df, usually the sampling ID or sampling location 
#' @param value.field: character,response variable field in the df 
#' @param field.date: character, date field in the df 
#' @param field.strata: character, a grouping factor field in the df, usually a combination of model (predicted Vs observed) and partition (training, testing) 
#' @param field.partition: character, partition field in the df (Training, testing)
#' @param field.year: character, year field in the df 
#' @param onset.thresh: numeric, onset quantile threshold default \code{onset.thresh = 0.05 }
#' @param offset.thresh: numeric, offset quantile threshold default \code{offset.thresh = 0.95 }
#' @param method: character, either \code{method = "quantile" } or \code{method = "threshold" }
#' @param egg.thresh: numeric, egg threshold to define the start and end  of the season
#' @author Daniele Da Re, \email{daniele.dare@unitn.it}

phenologicalIndexes <- function(df = NULL, field.ID = NULL, value.field = NULL, field.date=NULL, field.strata=NULL, field.partition = NULL, field.year = NULL, onset.thresh = 0.05 , offset.thresh = 0.95, threshold=NULL, method = "quantile",  egg.thresh= NULL){
  
  if(method == "quantile") {
    
    tmp <- df %>% 
      group_by(across(all_of(c(field.ID, field.strata, field.year)))) %>% 
      mutate(cum_sum := round(cumsum(!!dplyr::sym(value.field)) / sum(!!dplyr::sym(value.field)), 3))
    
    #onset
    onset <- tmp %>% 
      dplyr::group_by(across(all_of(c(field.ID, field.strata, field.year)))) %>% 
      dplyr::filter(cum_sum >= onset.thresh) %>% 
      dplyr::filter(cum_sum == min(cum_sum)) %>% 
      dplyr::ungroup()
    # dplyr::select({{field.ID}},{{field.partition}}, {{field.date}}) 
    
    #offset
    offset<- tmp %>% 
      dplyr::group_by(across(all_of(c(field.ID, field.strata, field.year)))) %>% 
      dplyr::filter(cum_sum >= offset.thresh) %>% 
      dplyr::filter(cum_sum == min(cum_sum)) %>% 
      dplyr::ungroup()
    # dplyr::select({{field.ID}},{{field.partition}}, {{field.date}}) 
    
    
    #peak
    peak <- tmp  %>% 
      dplyr::group_by(across(all_of(c(field.ID, field.strata, field.year)))) %>% 
      dplyr::filter(value==max(value))%>% 
      dplyr::ungroup() 
    # dplyr::select({{field.ID}},{{field.partition}}, {{field.date}}) 
  }
  
  
  if( method == "threshold") {
   
    tmp <- df %>% 
      dplyr::group_by(across(all_of(c(field.ID, field.strata, field.year)))) %>% 
      dplyr::filter(!!as.symbol(value.field)  >= egg.thresh)

    #onset
    onset <- tmp %>%
      dplyr::filter(!!as.symbol(field.date) == min(!!as.symbol(field.date) )) %>% 
      dplyr::ungroup()
    # dplyr::select({{field.ID}},{{field.partition}}, {{field.date}}) 
    
    #offset
    offset<- tmp %>%
      dplyr::filter(!!as.symbol(field.date) == max(!!as.symbol(field.date) )) %>% 
      dplyr::ungroup()
    # dplyr::select({{field.ID}},{{field.partition}}, {{field.date}}) 
    
    #peak
    peak <- tmp  %>% 
      dplyr::group_by(across(all_of(c(field.ID, field.strata, field.year)))) %>% 
      dplyr::filter(value==max(value))%>% 
      dplyr::ungroup() 
    # dplyr::select({{field.ID}},{{field.partition}}, {{field.date}}) 
  }

  #save and export
  myOut<-list(Onset= onset, 
              Peak = peak, 
              Offset = offset)
  return(myOut)
}  