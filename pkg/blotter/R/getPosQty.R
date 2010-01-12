#' @export
getPosQty <- function(Portfolio, Symbol, Date)
{ # @author Peter Carl
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",name," not found, use initPortf() to create a new account"))
    
    # DESCRIPTION:
    # Gets the previous position 

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # Date: timestamp as of which to have the most recent position

    # Outputs
    # Numeric value of the most recent position.

    # FUNCTION
    PosQty = as.numeric(getPos(pname, Symbol, Date)[,"Pos.Qty"])
    return(PosQty)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################