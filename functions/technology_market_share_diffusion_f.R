#' technology_market_share_diffusion_f
#' Function: Calculate prospective deployment of alternative technology from a specified model.
#' @export
technology_market_share_diffusion_f <- function(i_value,i_year,last_yr=NA,market_share_diffusion_mdl=NA,market_share_linear_diff_coef=NA,market_share_bass_innovation_coef=NA,market_share_bass_imitation_coef=NA,market_share_market_potential=NA){
  attribute_f("technology_market_share_diffusion_f")
  if (market_share_diffusion_mdl=="linear"){
    proj_values <- sapply(i_value+market_share_linear_diff_coef*0:(last_yr-i_year),function(x)ifelse(x<=market_share_market_potential,x,market_share_market_potential))
  } else if(market_share_diffusion_mdl=="bass"){
    ##Function: Bass diffusion
    bass_diffusion <- function(t,p,q,market_potential,i_value,i_year){
      market_potential*(1-p*(market_potential-i_value)/(i_value*q+p*market_potential)*exp(-(p+q)*(t-i_year)))/(1+q*(market_potential-i_value)/(i_value*q+p*market_potential)*exp(-(p+q)*(t-i_year)))
    }
    #In case imitation coef is null, constant market share
    if (market_share_bass_imitation_coef==0){
      proj_values <- rep(i_value,times=length(i_year:last_yr))
    } else {
      proj_values <- bass_diffusion(t=i_year:last_yr,p=market_share_bass_innovation_coef,q=market_share_bass_imitation_coef,market_potential=market_share_market_potential,i_value=i_value,i_year=i_year)
    }
  }
  return(proj_values)
}
