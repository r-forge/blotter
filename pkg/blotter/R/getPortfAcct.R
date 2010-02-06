getPortfAcct <- function(Account,Portfolio, Dates=NULL) #should symbol subsets be supported too?  probably not.
{ # @author Brian Peterson
    acct<-try(get(paste("account",Account,sep='.'),envir=.blotter),silent=TRUE)
    if(inherits(acct,"try-error"))
        stop(paste("Account",Account,"not found, use initAcct() to create a new account."))
    pname=Portfolio
    Portfolio<-try(acct[[paste("portfolio",pname,sep='.')]],silent=TRUE)
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname,"not found, use initPortf() to create a new portfolio or initAcct() to place it in",Account))
    
    if(!is.null(Dates)){
        Portfolio<-Portfolio[Dates]
    }
    
    return(Portfolio)
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