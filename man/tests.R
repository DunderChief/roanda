source('R/roanda.R')
acct_buy=7590055
acct_sell=9957517
auth_id='ea8b265f2074f8b712ae96657f40b80a-53eb6318cb1d365c893924bfc7814607'
acct_type='fxpractice'


# Open some trades
marketOrder('EUR_USD', units=1000, side='sell', SL = 100, TP=100, slippage=4, 
            acct=acct_sell, auth_id='poop', acct_type='fxpractice')
marketOrder('EUR_USD', units=1000, side='sell', SL = 100, TP=100, slippage=4, 
            acct=acct_sell, auth_id=auth_id, acct_type='fxpractice')

# Past Orders
getPastOrders('EUR_USD', count=10, acct=acct_sell, auth_id=auth_id, acct_type=acct_type)

# Get open Trades
openTrades <- getOpenTrades('EUR_USD', acct=acct_buy, auth_id=auth_id, acct_type=acct_type)
openTrades
orderID <- as.vector(openTrades$id[1])
closeTrade(orderID = orderID, acct=acct_sell, auth_id=auth_id, acct_type=acct_type)

# Close the rest
closeAllTrades('EUR_USD', acct=acct_sell, auth_id=auth_id, acct_type=acct_type)

# Open some more trades
marketOrder('EUR_USD', units=1000, side='sell', SL = 100, TP=100, slippage=4, 
            acct=acct_sell, auth_id=auth_id, acct_type='fxpractice')

getPositions('EUR_USD', acct=acct_sell, auth_id=auth_id, acct_type=acct_type)


pairs <- c('AUD_CAD', 'EUR_USD', 'USD_JPY', 'NZD_USD', 'GBP_USD', 'EUR_CAD', 'USD_CAD', 'USD_CHF')
for(this.pair in pairs) {
  closeAllTrades('EUR_USD', acct=acct_sell, auth_id=auth_id, acct_type=acct_type)
  closeAllTrades('EUR_USD', acct=acct_buy, auth_id=auth_id, acct_type=acct_type)
}
