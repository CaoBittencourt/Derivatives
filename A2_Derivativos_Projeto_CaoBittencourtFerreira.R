# 1. PACOTES --------------------------------------------------------------
pkg <- c(
  'gghighlight', 'ggthemes', 'viridis', 'patchwork', 'naniar', 'scales' #Visualização
  # , 'plotly' #Visualização
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


# 2. CURIOSIDADE: GUIDE -----------------------------------------------------------
# GUIDE()


# 3. DADOS ----------------------------------------------------------------
# Escopo dos preços = 1 ano (para permitir o cálculo do dividend yield 12m)
DATA.inicial_1 <- Sys.Date() - months(12) - 22 #Para calcular a volatilidade anual sem gerar NA's na primeira data é necessário ter 22 dias a mais de dados no início (i.e. para calcular a rolling volatility de 22 dias)  
DATA.inicial_2 <- Sys.Date() - months(12)
DATA.final <- Sys.Date()


# Tickers
# BatchGetSymbols::GetIbovStocks() -> IBOV.stocks
BatchGetSymbols::GetSP500Stocks() -> SP500.stocks
# BatchGetSymbols::GetFTSE100Stocks() -> FTSE100.stocks


# Lista de tickers
list(
  # 'IBOV' = IBOV.stocks$tickers
  # , 
  'SP500' = SP500.stocks$Tickers
  # , 'FTSE100' = FTSE100.stocks$tickers
) -> list.tickers 


# Sufixos do Yahoo Finance
list(
  # 'IBOV' = '.SA'
  # , 
  'SP500' = NULL
  # , 'FTSE100' = '.L'
) -> list.suffix

# Lista de tickers com sufixos do Yahoo Finance
Map(
  function(stocks, suffix){
    
    paste0(stocks, suffix)
    
  }
  , stocks = list.tickers
  , suffix = list.suffix
) -> list.tickers


# Sample ilustrativa de 5x tickers aleatórios (SP500 -> SP5)
lapply(
  list.tickers
  , function(tickers){
    
    sample(tickers, 5)
    
  }) -> list.tickers


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


# Lista de taxas de juro
list(
  # 'IBOV' = risk.free.BRA
  # , 
  'SP500' = risk.free.USA
  # , 'FTSE100' = risk.free.GBR
) -> list.risk.free


# Ajustes na lista de taxas de juro
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


# Volatilidade = rolling standard deviation (22 dias anualizada)
lapply(
  list.prices
  , function(stocks){
    
    stocks %>%
      arrange(ref.date) %>%
      group_by(ticker) %>%
      mutate(volatility = rollvol(ret.closing.prices))
    
  }) -> list.prices


# Dividend Yield
# getDividends vetorizada para incluir um vetor de tickers
Vectorize(
  quantmod::getDividends
  , vectorize.args = 'Symbol'
) -> vectorized.getDividends

# Dividendos no último ano (12m)
lapply(
  list.tickers
  , function(stocks){
    
    vectorized.getDividends(
      Symbol = stocks
      , from = DATA.inicial_2 - months(12)
      , to = DATA.inicial_2
    )  
    
  }) -> list.dividends


# Data frames de dividendos consolidados por ticker e data
# Obs: por concisão (minimizar o tamanho do data frame final), toma-se apenas os dividendos totais, ignorando as datas
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
    ) %>% 
      bind_rows(.) %>%
      group_by(ticker) %>%
      summarize(
        total.div.12m = unique(total.div.12m)
      )
  }) -> list.dividends


# Data frames de preços e dividendos 
Map(
  function(dividends, prices){
    
    dividends %>% 
      full_join(prices) %>%
      mutate(
        total.div.12m = replace_na(total.div.12m, 0) #NA dividendos = empresas que não pagaram dividendos => 0
        , div.yield = total.div.12m/price.close #dividend yield
      )
    
  }
  , dividends = list.dividends
  , prices = list.prices
) -> list.prices


# Data frame de preços, dividendos e taxa livre de risco
Map(
  function(risk.free, prices){
    
    prices %>%
      left_join(risk.free) 
    
  }
  , risk.free = list.risk.free
  , prices = list.prices
) -> list.prices


# Opções
# Get Option Chain vetorizada
Vectorize(
  quantmod::getOptionChain
  , vectorize.args = 'Symbols'
) -> vectorized.getOptionChain

# Dados de opções do Yahoo Finaças via getOptionChain (versão vetorizada)
# Escopo das opções = todas as opções disponíveis
lapply(
  list.tickers
  , function(stocks){
    
    vectorized.getOptionChain(
      Symbols = stocks
      , Exp = NULL #Todas as opções disponíveis
      # , Exp = glue('{year(DATA.inicial_2)}/{year(DATA.inicial_2) + 1}')
      , src = 'yahoo'
    )
    
  }) -> list.options

# Ajustes na lista de opções
lapply(
  list.options
  , function(options){
    
    options %>% 
      purrr::flatten(.) %>%
      purrr::flatten_df(.) %>%
      as_tibble(rownames = 'contract') %>%
      dplyr::mutate(
        
        exp.date = parse_number(contract)
        , exp.date = as.character(exp.date)
        , exp.date = ymd(exp.date)
        
        , ticker = str_remove_all(contract, '[:digit:]')
        , opt.type = str_sub(ticker, start = -1)
        , ticker = str_sub(ticker, end = -2)
        
        , is.put.opt = ifelse(opt.type == 'P', yes = T, no = F)
        
      ) %>% return(.)
    
  }) -> list.options

# Lista de data frames final: 
# Preços, dividend yield, taxas livre de risco, opções (call e put) e datas de vencimento das opções
Map(
  function(options, prices){
    
    options %>% 
      full_join(
        prices
        , by = 'ticker' #full_join apenas em ticker <=> para cada opção, todas as linhas de preços, dividendos e taxas de juro
      ) %>% 
      mutate(
        time.to.exp = time_length(exp.date - ref.date, unit = 'days')
        , time.to.exp.yrs = time.to.exp/252
      )
    
  }
  , options = list.options
  , prices = list.prices
  
) -> list.final


# 4. MODELOS --------------------------------------------------------------
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

# Modelo Black-Scholes (preço da call) vetorizado
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

# Modelo Black-Scholes (preço da put) vetorizado
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

# Precificação das opções pelo Modelo Binomial e Black-Scholes
lapply(
  list.final
  , function(options){
    
    options %>%
      mutate(
        # Modelo Black-Scholes
        opt.price.bs = ifelse(
          is.put.opt #Put ou Call
          , yes = vectorized.bsput(#Put
            s = price.close #stock price
            , k = Strike #strike price
            , tt = time.to.exp.yrs #time to expiration
            , r = risk.free.rate #risk free rate
            , v = volatility #volatility
            , d = div.yield #dividend yield
          )
          , no = vectorized.bscall(#Call
            s = price.close #stock price
            , k = Strike #strike price
            , tt = time.to.exp.yrs #time to expiration
            , r = risk.free.rate #risk free rate
            , v = volatility #volatility
            , d = div.yield #dividend yield
          )
        )
        
        # Modelo Binomial
        , opt.price.binom.ame = vectorized.binomopt(
          american = T #Opção americana
          , putopt = is.put.opt #Put ou Call
          , s = price.close #stock price
          , k = Strike #strike price
          , tt = time.to.exp.yrs #time to expiration
          , r = risk.free.rate #risk free rate
          , v = volatility #volatility
          , d = div.yield #dividend yield
          , nstep = 4 #apenas 4 steps para não demorar demais o cálculo
        )
        
        # Modelo Binomial
        , opt.price.binom.eur = vectorized.binomopt(
          american = F #Opção européia
          , putopt = is.put.opt #Put ou Call
          , s = price.close #stock price
          , k = Strike #strike price
          , tt = time.to.exp.yrs #time to expiration
          , r = risk.free.rate #risk free rate
          , v = volatility #volatility
          , d = div.yield #dividend yield
          , nstep = 4 #apenas 4 steps para não demorar demais o cálculo
        )
        
      )
    
  }) -> list.final

# Upside e estratégia recomendada
lapply(
  list.final
  , function(options){
    
    options %>% 
      mutate(
        across(#Upside
          .cols = starts_with('opt.price') #Modelo de apreçamento estimados
          ,.fns = function(x){(x/Ask) - 1} #Upside = 'preço justo' (valuation) vs o quanto estão pedindo (Ask)
          ,.names = 'upside.{.col}'
        )
        , across(#Estratégia recomendada
          .cols = starts_with('upside') 
          ,.fns = function(x){
            case_when(
              x > 0 ~ 'Buy' #Upside positivo: 'preço justo' da opção > preço de mercado => Opção subvalorizada => Compra
              , x < 0 ~ 'Sell' #Upside negativo: 'preço justo' da opção < preço de mercado => Opção sobrevalorizada => Venda
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


# Modelos excluídos desta avaliação:
# Opções asiáticas
# # derivmkts::geomasianmc()
# Opção de Monte Carlo
# # fOptions::MonteCarloOption()
# Non Deliverable Forward (Moeda)
# # fOptions::NDF()
# Outras opções ainda, mas por simplicidade e concisão, calcula-se apenas os modelos binomial e Black-Scholes


# 5. VISUALIZAÇÃO ---------------------------------------------------------
# Tema (The Economist)
theme_set(ggthemes::theme_economist(base_size = 14))

# Cores manuais
manual.pal <- c('Buy' = '#0f459b', 'Sell' = '#EF0707')

# Ilustração com um único ativo
list.tickers$SP500 %>% sample(1) -> ticker.sample

list.final$SP500 %>%
  filter(
    ticker == ticker.sample 
  ) %>%
  pull(contract) %>% 
  sample(1) -> opt.sample

list.final$SP500 %>%
  filter(
    ticker == ticker.sample 
    , contract == opt.sample
  ) %>% 
  drop_na(
    starts_with('opt.price')
  ) -> df.opt.sample


# Cotações das ações (preço de fechamento)
# Candlestick
df.opt.sample %>% 
  ggplot(
    aes(
      x = ref.date
      , y = price.close
    )) + 
  tidyquant::geom_candlestick(
    aes(
      open = price.open
      , high = price.high
      , low = price.low
      , close = price.close
    )) + 
  # tidyquant::geom_ma(color = '#0A0A0A') +
  scale_y_continuous(labels = scales::label_dollar(accuracy = .01)) + 
  labs(
    title = glue("{df.opt.sample$ticker}: Stock Price")
    , x = 'Date'
    , y = 'Price (USD)'
  ) -> plot.prices.candle

# Linhas
df.opt.sample %>%
  ggplot(
    aes(
      x = ref.date
      , y = price.close
    )) + 
  geom_line(
    color = '#0A0A0A'
    , size = .75
  ) + 
  scale_y_continuous(labels = scales::label_dollar(accuracy = .01)) + 
  labs(
    title = glue("{df.opt.sample$ticker}: Stock Price")
    , x = 'Date'
    , y = 'Price (USD)'
  ) -> plot.prices.line

# Dividend Yield das ações
df.opt.sample %>%
  ggplot(
    aes(
      x = ref.date
      , y = div.yield
    )) + 
  geom_line(
    color = '#0A0A0A'
    , size = .75
  ) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = .01)) + 
  labs(
    title = glue("{df.opt.sample$ticker}: Dividend Yield")
    , x = 'Date'
    , y = 'Dividend Yield (%)'
  ) -> plot.div.yield

# Volatilidade das ações
df.opt.sample %>%
  ggplot(
    aes(
      x = ref.date
      , y = volatility
    )) + 
  geom_line(
    color = '#0A0A0A'
    , size = .75
  ) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = .01)) + 
  labs(
    title = glue("{df.opt.sample$ticker}: Stock Volatility")
    , x = 'Date'
    , y = 'Volatility (%)'
  ) -> plot.volatility

# Taxa de juros livre de risco
df.opt.sample %>%
  ggplot(
    aes(
      x = ref.date
      , y = risk.free.rate
    )) + 
  geom_line(
    color = '#0A0A0A'
    , size = .75
  ) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = .01)) + 
  labs(
    title = glue("Risk free rate")
    , x = 'Date'
    , y = 'Risk free rate (%)'
  ) -> plot.risk.free.rate

# 'Preço justo' via Modelo Binomial e Black-Scholes
df.opt.sample  %>% 
  pivot_longer(
    cols = starts_with('opt.price')
    , names_to = 'opt.model'
    , values_to = 'opt.price'
  ) %>%
  filter(
    !is.na(opt.price)
  ) %>%
  ggplot(
    aes(
      x = ref.date
      , y = opt.price
    )) + 
  geom_line(size = .75) +
  facet_wrap(facets = vars(opt.model), labeller = as_labeller(
    c(
      'opt.price.bs' = 'Black-Scholes'
      , 'opt.price.binom.ame' = 'Binomial (Ame)'
      ,  'opt.price.binom.eur' = 'Binomial (Eur)'
    )
  )) +
  scale_y_continuous(labels = scales::label_dollar(accuracy = .01)) + 
  labs(
    title = glue("{df.opt.sample$ticker}: Option Fair Price ({opt.sample})")
    , x = 'Date'
    , y = 'Option Fair Price (USD)'
  ) -> plot.opt.price


# Upside e estratégia recomendada (Black-Scholes)
df.opt.sample %>% 
  ggplot(
    aes(
      x = ref.date
      , y = upside.bs
      , fill = strategy.bs
    )) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .01)) + 
  scale_fill_manual(values = manual.pal) +
  labs(
    title = glue("{df.opt.sample$ticker}: Upside and Recommended Strategy ({opt.sample})")
    , x = 'Date'
    , y = 'Option Upside (%)'
    , fill = 'Recommended Strategy'
  ) -> plot.upside


# Visualização
# Preços das ações, dividendos (yield), volatilidade e taxa livre de risco
plot.args <- plot.prices.candle + plot.div.yield + plot.volatility + plot.risk.free.rate
plot.args <- plot.args + plot_annotation(title = glue("{df.opt.sample$ticker}: {str_replace_all(DATA.inicial_2,'-','/')} - {str_replace_all(DATA.final,'-','/')}"))

# Preços das opções
plot.opt <- wrap_plots(
  plot.opt.price, plot.upside
  , design = 
  'a
  b'
  )  

plot.opt <- plot.opt + plot_annotation(title = glue("{df.opt.sample$ticker}: {str_replace_all(DATA.inicial_2,'-','/')} - {str_replace_all(DATA.final,'-','/')}"))

# Plots finais
plot.args
plot.opt
