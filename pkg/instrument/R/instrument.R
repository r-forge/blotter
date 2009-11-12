###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, Joshua Ulrich and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: random_portfolios.R 1413 2009-11-10 22:50:39Z braverock $
#
###############################################################################

## we should probably assign instruments into a special namespace and create get* functions.  Jeff?

is.instrument <- function( x ) {
  x <- get(x,envir='instrument')
  inherits( x, "instrument" )
}

instrument<-function(primary_id , currency , multiplier , type=NULL , identifiers = NULL, ...){
  if(is.null(primary_id)) stop("you must specify a primary_id for the instrument")
  
  # not sure this is correct, maybe should store the primary_id for the currency instead.  Why doesn't R have pointers?
  if(!is.currency(currency)) stop("currency must be an object of type 'currency'") 

  if(!hasArg(identifiers)) identifiers = list()

  if(!is.numeric(multiplier) | length(multiplier) > 1) stop("multiplier must be a single number")
  
  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = type,
                         currency = currency,
                         multiplier = multiplier,
                         identifiers = identifiers
                        ),
                    class="instrument"
                  ), # end structure
        envir='instrument'
        )     
}

stock <- function(primary_id , currency , multiplier , type="stock" , identifiers = NULL, ...){
  stock_temp = instrument(primary_id , currency , multiplier , type="stock" , identifiers = identifiers, ...)
  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = "stock",
                         currency = currency,
                         multiplier = multiplier,
                         identifiers = identifiers
                        ),
                    class=c("stock","instrument")
                  ), # end structure
         envir='instrument'
  )
}

future <- function(primary_id , currency , multiplier , type="future" , identifiers = NULL, ..., underlying_id){
  future_temp = instrument(primary_id , currency , multiplier , type="future" , identifiers = identifiers, ...)

  if(is.null(underlying_id)) warning("underlying_id should only be NULL for cash-settled futures")

  if(!exists(underlying_id, envir='instrument')) warning("underlying_id not found") # assumes that we know where to look
  ## now structure and return
  assign(primary_id, structure( list(primary_id = future_temp$primary_id,
                         type = "future",
                         currency = future_temp$currency,
                         multiplier = future_temp$multiplier,
                         identifiers = future_temp$identifiers,
                         underlying_id = future_temp$underlying_id
                        ),
                    class=c("future","instrument")
                  ), # end structure
         envir='instrument'
  )
}

option <- function(primary_id , currency , multiplier , type="option" , identifiers = NULL, ..., underlying_id){
  option_temp = instrument(primary_id , currency , multiplier , type="option" , identifiers = identifiers, ...)

  if(is.null(underlying_id)) warning("underlying_id should only be NULL for cash-settled options")

  if(!exists(underlying_id, envir='instrument')) warning("underlying_id not found") # assumes that we know where to look
  ## now structure and return
  assign(primary_id, structure( list(primary_id = option_temp$primary_id,
                         type = "option",
                         currency = option_temp$currency,
                         multiplier = option_temp$multiplier,
                         identifiers = option_temp$identifiers,
                         underlying_id = option_temp$underlying_id
                        ),
                    class=c("option","instrument")
                  ), # end structure
         envir='instrument'
  )
}

currency <- function(primary_id , currency=NULL , multiplier=1 , type="currency" , identifiers = NULL, ...){
  currency_temp = instrument(primary_id , currency=primary_id , multiplier=1 , type="currency" , identifiers = identifiers, ...)
  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = "currency",
                         currency = primary_id,
                         multiplier = 1,
                         identifiers = identifiers
                        ),
                    class=c("currency","instrument")
                  ), # end structure
         envir='instrument'
  )
}

exchange_rate <- function (primary_id , currency , second_currency, type="exchange_rate" , identifiers = NULL, ...){
  exchange_rate_temp = instrument(primary_id , currency , multiplier=1 , type="exchange_rate" , identifiers = identifiers, ...)

  if(!exists(currency, envir='instrument')) warning("currency not found") # assumes that we know where to look
  if(!exists(second_currency, envir='instrument')) warning("second_currency not found") # assumes that we know where to look

  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = "option",
                         currency = currency,
                         second_currency = second_currency,
                         identifiers = identifiers
                        ),
                    class=c("exchange_rate","instrument")
                  ), # end structure
         envir='instrument'
  )
}

getInstrument <- function(x){
  get(x,envir='instrument')
}

