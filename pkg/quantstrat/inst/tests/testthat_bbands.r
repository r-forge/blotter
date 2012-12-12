require(testthat)

################## bee.r  #########################

source('bbands_version_for_tests.r')

stratstat   = tradeStats(portfolio.st)

Txns      = stratstat$Num.Txns
Trades    = stratstat$Num.Trades
NetPL     = stratstat$Net.Trading.PL
LWinner   = stratstat$Largest.Winner
LLoser    = stratstat$Largest.Loser
MaxDD     = stratstat$Max.Drawdown

suppressWarnings(rm("order_book.bbands",pos=.strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratBBands","initDate","initEq",'start_t','end_t'))

######################## RUN TEST SUITE #######################

context('Consistent trade statistics for bbands.R')

test_that('Number of transactions is consistent', {

  expect_that(Txns, equals(51))
})

test_that('Number of the number of trades is consistent', {

  expect_that(Trades, equals(21))
})

test_that('Net Trading PL is consistent', {

  expect_that(NetPL, equals(46))
})

test_that('Largest Winner is consistent', {

  expect_that(LWinner, equals(24))
})

test_that('Largest Loser is consistent', {

  expect_that(LLoser, equals(-38))
})

test_that('Max Drawdown is consistent', {

  expect_that(MaxDD, equals(-84))
       
})


