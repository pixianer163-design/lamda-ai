{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}
module Exercise03 where

import Data.Maybe (fromMaybe, isJust, isNothing)

-- ============================================
-- Exercise 03: 类型系统与安全编程
-- ============================================
-- 目标：掌握 Maybe、Either 和自定义类型
-- 难度：★★★☆☆
-- 主题：用类型系统消除运行时错误

-- --------------------------------------------
-- 任务 1: 安全列表索引
-- --------------------------------------------
-- 对比 C 的数组越界风险
-- C: arr[i] 可能越界，返回垃圾值或崩溃
-- Haskell: 返回 Maybe，强制处理失败情况

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n = error "TODO: 实现安全索引"

-- --------------------------------------------
-- 任务 2: 解析整数（带错误处理）
-- --------------------------------------------
-- parseInt "123" = Right 123
-- parseInt "abc" = Left "Invalid number: abc"
-- parseInt "" = Left "Empty string"
-- 提示：使用 reads 函数，或手动检查每个字符

data ParseError = EmptyInput | InvalidChar Char | InvalidFormat
    deriving (Show, Eq)

parseInt :: String -> Either ParseError Int
parseInt [] = Left EmptyInput
parseInt s = error "TODO: 实现整数解析"

-- --------------------------------------------
-- 任务 3: 实现安全除法链
-- --------------------------------------------
-- 连续进行多次除法，任一失败都返回 Nothing
-- safeDivChain 1000 [2,5,10] = Just 10  (1000/2=500, 500/5=100, 100/10=10)
-- safeDivChain 100 [0,5] = Nothing  (除零错误)
-- 提示：使用 foldM 或递归

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

safeDivChain :: Int -> [Int] -> Maybe Int
safeDivChain initial divisors = error "TODO: 实现安全除法链"

-- --------------------------------------------
-- 任务 4: 银行账户系统
-- --------------------------------------------
-- 使用类型系统防止非法操作

data Currency = USD | EUR | CNY
    deriving (Show, Eq)

-- 金额总是与货币类型绑定
data Money = Money
    { amount :: Double
    , currency :: Currency
    } deriving (Show, Eq)

-- 账户可能有不同状态
data AccountStatus = Active | Frozen | Closed
    deriving (Show, Eq)

data Account = Account
    { accountId :: String
    , balance :: Money
    , status :: AccountStatus
    } deriving (Show)

data TransactionError = 
    InsufficientFunds
    | AccountFrozen
    | AccountClosed
    | CurrencyMismatch Currency Currency
    | InvalidAmount
    deriving (Show, Eq)

-- 实现存款（必须检查账户状态）
deposit :: Account -> Money -> Either TransactionError Account
deposit acc@Account{..} money = 
    case status of
        Closed -> Left AccountClosed
        Frozen -> Left AccountFrozen
        Active -> error "TODO: 实现存款逻辑"

-- 实现取款（必须检查余额和状态）
withdraw :: Account -> Money -> Either TransactionError Account
withdraw acc@Account{..} money =
    case status of
        Closed -> Left AccountClosed
        Frozen -> Left AccountFrozen
        Active -> error "TODO: 实现取款逻辑"

-- 实现转账（检查所有条件）
transfer :: Account -> Account -> Money -> Either TransactionError (Account, Account)
transfer from to money = 
    if currency (balance from) /= currency money
    then Left $ CurrencyMismatch (currency $ balance from) (currency money)
    else error "TODO: 实现转账逻辑"

-- --------------------------------------------
-- 任务 5: 使用自定义类型表示状态
-- --------------------------------------------
-- 温度转换，使用类型区分不同温标

data Celsius = Celsius Double
    deriving (Show)

data Fahrenheit = Fahrenheit Double
    deriving (Show)

data Kelvin = Kelvin Double
    deriving (Show)

-- 华氏度转摄氏度
toCelsius :: Fahrenheit -> Celsius
toCelsius (Fahrenheit f) = error "TODO: F -> C"

-- 摄氏度转华氏度
toFahrenheit :: Celsius -> Fahrenheit
toFahrenheit (Celsius c) = error "TODO: C -> F"

-- 摄氏度转开尔文
toKelvin :: Celsius -> Kelvin
toKelvin (Celsius c) = error "TODO: C -> K"

-- 开尔文转摄氏度（注意：K < 0 无效）
fromKelvin :: Kelvin -> Maybe Celsius
fromKelvin (Kelvin k) = error "TODO: K -> C，注意无效温度"

-- --------------------------------------------
-- 任务 6: 实现 lookup 函数
-- --------------------------------------------
-- 在关联列表中查找值
-- lookup "b" [("a",1), ("b",2), ("c",3)] = Just 2
-- lookup "z" [("a",1), ("b",2)] = Nothing

myLookup :: Eq k => k -> [(k, v)] -> Maybe v
myLookup key [] = Nothing
myLookup key ((k,v):rest) = error "TODO: 实现 lookup"

-- --------------------------------------------
-- 任务 7: 链式 Maybe 操作
-- --------------------------------------------
-- 从嵌套的结构中提取数据
-- 使用 >>= (bind) 或 do 语法

data Person = Person
    { name :: String
    , address :: Maybe Address
    } deriving (Show)

data Address = Address
    { street :: Maybe String
    , city :: String
    } deriving (Show)

-- 获取人的街道名，可能每一步都失败
getStreetName :: Person -> Maybe String
getStreetName person = error "TODO: 使用 >>= 或 do 语法实现"

-- ============================================
-- 进阶挑战
-- ============================================

-- 挑战：实现 Either 的链式操作
-- 如果任何一步失败，返回第一个错误
-- 如果都成功，返回最终结果

chainEither :: [a -> Either e a] -> a -> Either e a
chainEither fs initial = error "TODO: 链式 Either 操作"

-- ============================================
-- 测试数据
-- ============================================
-- :load exercises/Exercise03.hs
-- safeIndex [1,2,3] 1 应该返回 Just 2
-- safeIndex [1,2,3] 5 应该返回 Nothing
-- parseInt "42" 应该返回 Right 42
-- parseInt "abc" 应该返回 Left ...
-- safeDivChain 1000 [2,5] 应该返回 Just 100
-- safeDivChain 100 [0] 应该返回 Nothing
-- toCelsius (Fahrenheit 32) 应该返回 Celsius 0
