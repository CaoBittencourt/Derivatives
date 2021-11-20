# 1. PACOTES --------------------------------------------------------------
pkg <- c(
  'gghighlight', 'ggthemes', 'viridis', 'patchwork', 'naniar', 'scales' #Visualização
  , 'GUIDE', 'derivmkts', 'fOptions', 'quantmod', 'BatchGetSymbols', 'GetBCBData' #Finanças
  , 'qrmdata'
  # , 'simfinR' #, 'simfinapi' 
  # , 'lubridate' #Manipulação de Datas
  , 'plyr', 'glue', 'tidyverse') #Leitura e Manipulação de Dados

# Obs: testar depois o pacote 'micEcon' (tem aidsest() etc)
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# Conflito de pacotes
filter <- dplyr::filter 
rename <- dplyr::rename

# 2. DADOS ----------------------------------------------------------------
# Escopo (1 semestre)
DATA.inicial_1 <- Sys.Date() - months(6) - 22 #Para calcular a volatilidade anual é necessário ter 22 dias a mais de dados para não gerar NA's (i.e. para calcular a 12m rolling volatility também nos meses iniciais)  
DATA.inicial_2 <- Sys.Date() - months(6)
DATA.final <- Sys.Date()


# Tickers
BatchGetSymbols::GetIbovStocks() -> IBOV.stocks
BatchGetSymbols::GetSP500Stocks() -> SP500.stocks
BatchGetSymbols::GetFTSE100Stocks() -> FTSE100.stocks

list(
  'IBOV' = IBOV.stocks$tickers
  , 'SP500' = SP500.stocks$Tickers
  , 'FTSE100' = FTSE100.stocks$tickers
) -> list.tickers 

list(
  'IBOV' = '.SA'
  , 'SP500' = NULL
  , 'FTSE100' = '.L'
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
GetBCBData::gbcbd_get_series(
  id = c('SELIC_ANO' = 1178)
  # Código 1178 = SELIC anualizada (apenas 11, não anualizada). 
  # Para mais séries, ver "http://www.bcb.gov.br/?sgs".
  , first.date = DATA.inicial_2
  , last.date = DATA.final
  , format.data = 'wide'
) %>%  
  as_tibble(.) %>% 
  fill(
    SELIC_ANO, 
    .direction = 'downup'
  ) %>%
  mutate(
    SELIC_ANO = SELIC_ANO/100
  ) -> risk.free.BRA

# Taxa de juro livre de risco americana (US T-Bill 3M, ao ano)
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

# Taxa de juro livre de risco inglesa (LIBOR 3M, ao ano)
quantmod::getSymbols(Symbols = 'GBP3MTD156N', src = 'FRED')

GBP3MTD156N %>%
  as_tibble(rownames = 'ref.date') %>% 
  mutate(ref.date = as_date(ref.date)) %>% 
  filter(
    ref.date >= DATA.inicial_2
    , ref.date <= DATA.final
  ) %>% 
  fill(
    GBP3MTD156N, 
    .direction = 'downup'
  ) -> risk.free.GBR 

list(
  'IBOV' = risk.free.BRA
  , 'SP500' = risk.free.USA
  , 'FTSE100' = risk.free.GBR
) -> list.risk.free

# Volatilidade

xts(
  apply(list.prices$IBOV$df.tickers$ret.closing.prices, 2, runSD, n = 22)
  , index(index.ret)
) * sqrt(252)

list.prices %>% 
  lapply(names)

rollvol <- function(
  stock.returns
  , window = 22
  , anual = T
){
  
  rollapplyr(
    data = stock.returns
    , width = window
    , FUN = sd
    , na.rm = T
    , fill = NA
  ) -> vol
  
  if(anual){vol <- vol*sqrt(252)}  
  
  return(vol)
  
}


list.prices$IBOV %>% 
  arrange(ref.date) %>% 
  group_by(ticker) %>%
  mutate(volatility = rollvol(ret.closing.prices)) %>% 
  select(ticker, ref.date, volatility) -> teste

View(teste %>% filter(ticker == 'ITSA4.SA'))

lapply(
  list.prices
  , function(stocks){
    
    stocks %>% 
      arrange(ref.date) %>% 
      group_by(ticker) %>%
      mutate(volatility = rollvol(ret.closing.prices)) 
    
  }) -> lalala

lalala$IBOV %>% View(.)


# 3. MODELOS --------------------------------------------------------------
# fOptions::?BlackScholesOption()
# 
# fOptions::NDF()
# 
# fOptions::BinomialTreeOption()
# fOptions::BinomialTreePlot

# derivmkts::?binomopt()
# derivmkts::binomplot()

PerformanceAnalytics::apply.rolling(R = list.prices$IBOV$df.tickers %>% 
                                      filter(ticker == 'ABEV3.SA') %>%
                                      `rownames<-`(.$ref.date) %>%
                                      select(ret.adjusted.prices)
                                    , width = 22
                                    , FUN = 'sd.annualized'
                                    , scale = 252
)


# TypeFlag = “c” for call or “p” for put
# 
# [pronto] S = current or starting stock price
# 
# X = strike price. The function uses X instead of K.
# 
# Time = time to expiration in years
# 
# [pronto] r = risk free interest rate
# 
# sigma = volatility of the stock


lalala$calls %>% glimpse(.)
# Get Option Prices

# getOptionChain() -> lalala

# lapply(
#   list.tickers$SP500, 
#   function(stock){
#     
#     getOptionChain(stock)
#     
#   }
# ) %>% 
#   bind_rows(.)

# lalala$calls %>% glimpse(.)
# lalala$puts %>% glimpse(.)


# 
# 
# fOptions::MonteCarloOption()


# 3. ADENDO: GUIDE -----------------------------------------------------------
GUIDE()

# 4. VISUALIZAÇÃO ---------------------------------------------------------


# naniar::vis_miss()


# ANÁLISE SETORIAL?

# 3. PRECIFICAÇÃO DE OPÇÕES

# fOptions::BlackScholesOption()
# 
# fOptions::NDF()
# 
# fOptions::BinomialTreeOption()
# fOptions::BinomialTreePlot

# derivmkts::binomopt()
# derivmkts::binomplot()

# 
# 
# fOptions::MonteCarloOption()


# 4. VISUALIZAÇÃO


