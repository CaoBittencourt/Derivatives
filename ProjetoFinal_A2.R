# 1. PACOTES --------------------------------------------------------------
pkg <- c(
  'gghighlight', 'ggthemes', 'viridis', 'patchwork', 'naniar', 'scales' #Visualização
  , 'GUIDE', 'derivmkts', 'fOptions', 'quantmod', 'BatchGetSymbols', 'GetBCBData' #Finanças
  # , 'simfinR' #, 'simfinapi' 
  , 'lubridate' #Manipulação de Datas
  , 'glue', 'readr', 'purrr', 'tidyverse') #Leitura e Manipulação de Dados

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# Conflito de pacotes
filter <- dplyr::filter 
rename <- dplyr::rename
mutate <- dplyr::mutate


# 2. DADOS ----------------------------------------------------------------
# Escopo (1 semestre)
DATA.inicial_1 <- Sys.Date() - months(6) - 22 #Para calcular a volatilidade anual sem gerar NA's na primeira data é necessário ter 22 dias a mais de dados no início (i.e. para calcular a rolling volatility de 22 dias)  
DATA.inicial_2 <- Sys.Date() - months(6)
DATA.final <- Sys.Date()


# Tickers
# BatchGetSymbols::GetIbovStocks() -> IBOV.stocks
BatchGetSymbols::GetSP500Stocks() -> SP500.stocks
# BatchGetSymbols::GetFTSE100Stocks() -> FTSE100.stocks

list(
  # 'IBOV' = IBOV.stocks$tickers
  # , 
  'SP500' = SP500.stocks$Tickers
  # , 'FTSE100' = FTSE100.stocks$tickers
) -> list.tickers 

list(
  # 'IBOV' = '.SA'
  # , 
  'SP500' = NULL
  # , 'FTSE100' = '.L'
) -> list.suffix

Map(
  function(stocks, suffix){
    
    paste0(stocks, suffix)
    
  }
  , stocks = list.tickers
  , suffix = list.suffix
) -> list.tickers


# Preços
lapply(
  list.tickers
  , function(stocks){
    
    BatchGetSymbols::BatchGetSymbols(
      tickers = stocks
      , first.date = DATA.inicial_1
      , last.date = DATA.final
    )$df.tickers %>% as_tibble(.)
    
  }) -> list.prices


# Taxa de juro livre de risco brasileira (SELIC)
# GetBCBData::gbcbd_get_series(
#   id = c('SELIC_ANO' = 1178)
#   # Código 1178 = SELIC anualizada (apenas 11, não anualizada). 
#   # Para mais séries, ver "http://www.bcb.gov.br/?sgs".
#   , first.date = DATA.inicial_2
#   , last.date = DATA.final
#   , format.data = 'wide'
# ) %>%  
#   as_tibble(.) %>% 
#   fill(
#     SELIC_ANO, 
#     .direction = 'downup'
#   ) %>%
#   mutate(
#     SELIC_ANO = SELIC_ANO/100
#   ) -> risk.free.BRA

# Taxa de juro livre de risco americana (US T-Bill 3m, ao ano)
quantmod::getSymbols(Symbols = 'DGS3MO', src = 'FRED')

DGS3MO %>% 
  as_tibble(rownames = 'ref.date') %>% 
  mutate(ref.date = as_date(ref.date)) %>% 
  filter(
    ref.date >= DATA.inicial_2
    , ref.date <= DATA.final
  ) %>% 
  fill(
    DGS3MO, 
    .direction = 'downup'
  ) -> risk.free.USA 

# Taxa de juro livre de risco inglesa (LIBOR 3m, ao ano)
# quantmod::getSymbols(Symbols = 'GBP3MTD156N', src = 'FRED')
# 
# GBP3MTD156N %>%
#   as_tibble(rownames = 'ref.date') %>% 
#   mutate(ref.date = as_date(ref.date)) %>% 
#   filter(
#     ref.date >= DATA.inicial_2
#     , ref.date <= DATA.final
#   ) %>% 
#   fill(
#     GBP3MTD156N, 
#     .direction = 'downup'
#   ) -> risk.free.GBR 

list(
  # 'IBOV' = risk.free.BRA
  # , 
  'SP500' = risk.free.USA
  # , 'FTSE100' = risk.free.GBR
) -> list.risk.free


lapply(
  list.risk.free
  , function(risk.free){
    
    colnames(risk.free) <- c('ref.date', 'risk.free.rate')
    
    return(risk.free)
    
  }) -> list.risk.free


# Volatilidade
# Muitos modelos para estimar a volatilidade
# Modelo mais simples (ingênuo): annualized moving standard deviation (trail 22 dias de sd anualizado)
# Outros modelos: https://www.valpo.edu/mathematics-statistics/files/2015/07/Estimating-the-Volatility-in-the-Black-Scholes-Formula.pdf

rollvol <- function(
  stock.returns
  , window = 22
  , annual = T
){
  
  rollapplyr(
    data = stock.returns
    , width = window
    , FUN = sd
    , na.rm = T
    , fill = NA
  ) -> vol
  
  if(annual){vol*sqrt(252) -> vol}
  
  return(vol)
  
}


# list.prices$SP500 %>%
#   arrange(ref.date) %>%
#   group_by(ticker) %>%
#   mutate(volatility = rollvol(ret.closing.prices)) -> list.prices$SP500


lapply(
  list.prices
  , function(stocks){
    
    stocks %>%
      arrange(ref.date) %>%
      group_by(ticker) %>%
      mutate(volatility = rollvol(ret.closing.prices))
    
  }) -> list.prices

# Dividend Yield (por simplicidade, supõem-se zero para todas as ações)
# getDividends vetorizada para incluir um vetor de tickers
Vectorize(
  quantmod::getDividends
  , vectorize.args = 'Symbol'
) -> vectorized.getDividends

vectorized.getDividends(
  Symbol = head(list.tickers$SP500)
  , from = DATA.inicial_2 - years(1)
  , to = DATA.inicial_2
  
) -> list.dividends.USA

# Data frame de dividendos
Map(
  function(stock, stock.ticker){
    
    colnames(stock) <- 'dividend'
    
    stock %>%
      as_tibble(rownames = 'div.date') %>%
      mutate(
        div.date = as_date(div.date)
        , ticker = stock.ticker
        , total.div.12m = sum(dividend) 
      )  
    
  }
  , stock = list.dividends.USA
  , stock.ticker = names(list.dividends.USA)
) %>% bind_rows(.) -> df.dividends.USA

# Adicionar dividendos ao data frame de preços e calcular dividend yield = total.dividendos(12m)/preço.ação
df.dividends.USA %>% 
  full_join(list.prices$SP500) %>% 
  mutate(div.yield = total.div.12m/price.close) -> list.prices$SP500


# Com lista de tickers
lapply(
  list.tickers
  , function(stocks){
    
    vectorized.getDividends(
      Symbol = head(list.tickers$SP500)
      , from = DATA.inicial_2 - years(1)
      , to = DATA.inicial_2
    )  
    
  }) -> list.dividends

lapply(
  list.dividends
  , function(stocks){
    Map(
      function(stock, stock.ticker){
        
        colnames(stock) <- 'dividend'
        
        stock %>%
          as_tibble(rownames = 'div.date') %>%
          mutate(
            div.date = as_date(div.date)
            , ticker = stock.ticker
            , total.div.12m = sum(dividend) 
          )  
      }
      , stock = stocks
      , stock.ticker = names(stocks)
    ) %>% bind_rows(.)
  }) -> list.dividends

Map(
  function(dividends, prices){
    
    dividends %>% 
      full_join(prices) %>% 
      mutate(div.yield = total.div.12m/price.close)
    
  }
  , dividends = list.dividends
  , prices = list.prices
) -> list.prices


# Opções
# Get Option Chain vetorizada (talvez não precise disso)
Vectorize(
  quantmod::getOptionChain
  , vectorize.args = 'Symbols'
) -> vectorized.getOptionChain

lapply(list.tickers, head) -> list.tickers.sample

lapply(
  list.tickers.sample
  , function(stocks){
    
    vectorized.getOptionChain(
      Symbols = stocks
      , Exp = (list.prices$SP500$ref.date + months(1))
      , src = 'yahoo'
    )
    
  }) -> list.options

Map(
  function(stocks, sample){
    
    stocks %>% 
      filter(
        ticker %in% sample 
      )
    
  }
  , stocks = list.prices
  , sample = list.tickers.sample
) -> list.prices


quantmod::getOptionChain(
  Symbols = list.tickers$SP500 %>% head(.)
  # , Exp = DATA.inicial_2 %>% year(.) %>% as.character(.)
  , Exp = (list.prices$SP500$ref.date + months(1))
  , src = 'yahoo'
) -> list.options.USA



list.prices$SP500 %>%
  filter(
    # ticker == 'AAPL'
    ticker %in% head(list.tickers$SP500)
    # , ref.date >= min(calls$LastTradeTime)
    # , ref.date <= max(calls$LastTradeTime)
  ) %>% 
  mutate(
    div.yield = 0 #Simplificação: supondo dividendos = 0 para todas
  ) -> list.prices.USA

list.options.USA %>% purrr::flatten(.) -> lalala

lapply(
  list.options
  , function(options){
    
    options %>% 
      purrr::flatten(.) -> options
    
    list(
      options[-which(options %>% names(.) %in% c('calls', 'puts'))] %>% flatten_df(.)
      , options[which(options %>% names(.) %in% c('calls', 'puts'))] %>% bind_rows(.)
    ) %>% bind_rows(.) -> options
    
    options %>% 
      as_tibble(rownames = 'contract') %>%
      dplyr::mutate(
        
        exp.date = parse_number(contract)
        , exp.date = as.character(exp.date)
        , exp.date = ymd(exp.date)
        
        , ticker = str_remove_all(contract, '[:digit:]')
        , opt.type = str_sub(ticker, start = -1)
        , ticker = str_sub(ticker, end = -2)
        
        , is.put.opt = ifelse(opt.type == 'P', yes = T, no = F)
        
        , ref.date = as_date(LastTradeTime)
        
        , time.to.exp = time_length(exp.date - ref.date, unit = 'days')
        , time.to.exp.yrs = time.to.exp/252
        
      ) %>% return(.)
    
  }) -> list.options


list.options$SP500 %>% View(.)

list(
  lalala[-which(lalala %>% names(.) %in% c('calls', 'puts'))] %>% flatten_df(.)
  , lalala[which(lalala %>% names(.) %in% c('calls', 'puts'))] %>% bind_rows(.)
) %>% bind_rows(.) -> lalala

lalala %>% 
  as_tibble(rownames = 'contract') %>%
  dplyr::mutate(
    
    exp.date = parse_number(contract)
    , exp.date = as.character(exp.date)
    , exp.date = ymd(exp.date)
    
    , ticker = str_remove_all(contract, '[:digit:]')
    , opt.type = str_sub(ticker, start = -1)
    , ticker = str_sub(ticker, end = -2)
    
    , is.put.opt = ifelse(opt.type == 'P', yes = T, no = F)
    
    , ref.date = as_date(LastTradeTime)
    
    , time.to.exp = time_length(exp.date - ref.date, unit = 'days')
    , time.to.exp.yrs = time.to.exp/252
    
  ) -> lalala

Map(
  function(options, prices, risk.free){
    
    options %>% 
      full_join(prices) %>%
      left_join(risk.free) %>%
      fill(
        risk.free.rate
        , .direction = 'downup'
      )
    
  }
  , options = list.options
  , prices = list.prices
  , risk.free = list.risk.free
  
) -> list.final


list.prices.USA %>% 
  full_join(lalala) %>% 
  left_join(list.risk.free$SP500) %>% 
  fill(
    DGS3MO
    , .direction = 'downup'
  ) -> lalala


# 3. MODELOS --------------------------------------------------------------
# TypeFlag = “c” for call or “p” for put
# 
# [pronto] S = current or starting stock price
# 
# [pronto] X = strike price. The function uses X instead of K.
# 
# [printo] Time = time to expiration in years
# 
# [pronto] r = risk free interest rate
# 
# [quase] sigma = volatility of the stock

# Modelo Binomial vetorizado
Vectorize(
  derivmkts::binomopt
  , vectorize.args = 
    c(
      'putopt'#call = F, put = T
      ,'s' #stock price
      , 'k'  #strike price
      , 'tt' #time to expiration
      , 'r' #risk free rate
      , 'v' #volatility
      , 'd' #dividend yield
    )
) -> vectorized.binomopt

# Modelo Black-Scholes (preço da call)
Vectorize(
  derivmkts::bscall
  , vectorize.args = 
    c(
      's' #stock price
      , 'k'  #strike price
      , 'tt' #time to expiration
      , 'r' #risk free rate
      , 'v' #volatility
      , 'd' #dividend yield
    )
) -> vectorized.bscall

# Modelo Black-Scholes (preço da put)
Vectorize(
  derivmkts::bsput
  , vectorize.args = 
    c(
      's' #stock price
      , 'k'  #strike price
      , 'tt' #time to expiration
      , 'r' #risk free rate
      , 'v' #volatility
      , 'd' #dividend yield
    )
) -> vectorized.bsput


lapply(
  list.final
  , function(options){
    
    options %>%
      mutate(
        opt.price.binom.ame = vectorized.binomopt(
          american = T
          , putopt = is.put.opt
          , s = price.close #stock price
          , k = Strike #strike price
          , tt = time.to.exp.yrs #time to expiration
          , r = risk.free.rate #risk free rate
          , v = volatility #volatility
          , d = div.yield #dividend yield
          , nstep = 10
        )
        
        , opt.price.binom.eur = vectorized.binomopt(
          american = F
          , putopt = is.put.opt
          , s = price.close #stock price
          , k = Strike #strike price
          , tt = time.to.exp.yrs #time to expiration
          , r = risk.free.rate #risk free rate
          , v = volatility #volatility
          , d = div.yield #dividend yield
          , nstep = 10
        )
        
        , opt.price.bs = ifelse(
          is.put.opt
          , yes = vectorized.bsput(
            s = price.close #stock price
            , k = Strike #strike price
            , tt = time.to.exp.yrs #time to expiration
            , r = risk.free.rate #risk free rate
            , v = volatility #volatility
            , d = div.yield #dividend yield
          )
          , no = vectorized.bscall(
            s = price.close #stock price
            , k = Strike #strike price
            , tt = time.to.exp.yrs #time to expiration
            , r = risk.free.rate #risk free rate
            , v = volatility #volatility
            , d = div.yield #dividend yield
          )
        )
      )
    
  }) -> list.final


lalala %>% 
  mutate(
    opt.price.binom.ame = vectorized.binomopt(
      american = T
      , putopt = is.put.opt
      , s = price.close #stock price
      , k = Strike #strike price
      , tt = time.to.exp.yrs #time to expiration
      , r = DGS3MO #risk free rate
      , v = volatility #volatility
      , d = div.yield #dividend yield
      , nstep = 10
    )
    
    , opt.price.binom.eur = vectorized.binomopt(
      american = F
      , putopt = is.put.opt
      , s = price.close #stock price
      , k = Strike #strike price
      , tt = time.to.exp.yrs #time to expiration
      , r = DGS3MO #risk free rate
      , v = volatility #volatility
      , d = div.yield #dividend yield
      , nstep = 10
    )
    
    , opt.price.bs = ifelse(
      is.put.opt
      , yes = vectorized.bsput(
        s = price.close #stock price
        , k = Strike #strike price
        , tt = time.to.exp.yrs #time to expiration
        , r = DGS3MO #risk free rate
        , v = volatility #volatility
        , d = div.yield #dividend yield
      )
      , no = vectorized.bscall(
        s = price.close #stock price
        , k = Strike #strike price
        , tt = time.to.exp.yrs #time to expiration
        , r = DGS3MO #risk free rate
        , v = volatility #volatility
        , d = div.yield #dividend yield
      )
    )
  ) -> lalala


# Upside e recomendação
lapply(
  list.final
  , function(options){
    
    options %>% 
      mutate(
        across(#Upside
          .cols = starts_with('opt.price')
          ,.fns = function(x){x - Ask} #Upside = o preço justo (valuation) - o quanto estão pedindo (Ask)
          ,.names = 'upside.{.col}'
        )
        , across(#Estratégia recomendada
          .cols = starts_with('upside')
          ,.fns = function(x){
            case_when(
              x > 0 ~ 'Buy'
              , x < 0 ~ 'Sell'
            )}
          ,.names = 'strategy.{.col}'
        )
      ) %>% 
      rename_with(#Ajuste nos nomes das variáveis
        .cols = starts_with('upside')
        , .fn = function(x){x %>% str_remove('opt.price.')}
      ) %>%
      rename_with(#Ajuste nos nomes das variáveis
        .cols = starts_with('strategy.')
        , .fn = function(x){
          x %>% 
            str_remove('opt.price.') %>%
            str_remove('upside.')
        }
      )
    
  }) -> list.final


lalala %>% 
  mutate(
    across(#Upside
      .cols = starts_with('opt.price')
      ,.fns = function(x){x - Ask} #Upside = o preço justo (valuation) - o quanto estão pedindo (Ask)
      ,.names = 'upside.{.col}'
    )
    , across(#Estratégia recomendada
      .cols = starts_with('upside')
      ,.fns = function(x){
        case_when(
          x > 0 ~ 'Buy'
          , x < 0 ~ 'Sell'
        )}
      ,.names = 'strategy.{.col}'
    )
  ) %>% 
  rename_with(#Ajuste nos nomes das variáveis
    .cols = starts_with('upside')
    , .fn = function(x){x %>% str_remove('opt.price.')}
  ) %>%
  rename_with(#Ajuste nos nomes das variáveis
    .cols = starts_with('strategy.')
    , .fn = function(x){
      x %>% 
        str_remove('opt.price.') %>%
        str_remove('upside.')
    }
  ) -> lalala


# Modelos não inclusos nesta avaliação
# Opções asiáticas
# derivmkts::geomasianmc()
# fOptions::MonteCarloOption()
# fOptions::NDF()
# Outras opções ainda, mas por simplicidade, optei apenas por binomial e Black-Scholes

# 3. ADENDO: GUIDE -----------------------------------------------------------
GUIDE()

# 4. VISUALIZAÇÃO ---------------------------------------------------------


# naniar::vis_miss()


# ANÁLISE SETORIAL?

# 3. PRECIFICAÇÃO DE OPÇÕES

# fOptions::BinomialTreeOption()
# fOptions::BinomialTreePlot

# derivmkts::binomopt()
# derivmkts::binomplot()


# 4. VISUALIZAÇÃO


