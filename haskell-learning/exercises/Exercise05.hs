{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NumericUnderscores #-}
module Exercise05 where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, replicateM_, when)

-- ============================================
-- Exercise 05: 并发编程实战
-- ============================================
-- 目标：掌握 Haskell 的轻量级线程和 STM
-- 难度：★★★★★
-- 对比：C 的 pthread vs Haskell 的 forkIO/STM

-- --------------------------------------------
-- 任务 1: 简单计数器（使用 MVar）
-- --------------------------------------------
-- 创建 N 个线程，每个线程增加计数器 M 次
-- 最后输出计数器值（应该是 N * M）

concurrentCounter :: Int -> Int -> IO Int
concurrentCounter numThreads incrementsPerThread = do
    counter <- newMVar 0
    -- 启动 numThreads 个线程
    -- 每个线程调用 incrementCounter counter incrementsPerThread
    -- 等待所有线程完成
    -- 读取最终值并返回
    error "TODO: 实现并发计数器"
  where
    incrementCounter :: MVar Int -> Int -> IO ()
    incrementCounter counter n = error "TODO: 实现 incrementCounter"

-- --------------------------------------------
-- 任务 2: 带超时的工作池
-- --------------------------------------------
-- 创建 N 个工作线程从共享队列取任务
-- 主线程提交任务，等待最多 T 微秒
-- 如果超时，返回已完成的任务数

data Task a = Task { taskId :: Int, action :: IO a }

workPool :: Int -> Int -> [IO a] -> IO Int
workPool numWorkers timeoutMicroseconds tasks = do
    -- 创建任务队列（Chan）
    -- 创建工作线程
    -- 提交任务到队列
    -- 等待任务完成或超时
    -- 返回完成的任务数
    error "TODO: 实现工作池"

-- --------------------------------------------
-- 任务 3: 读写锁（使用 STM）
-- --------------------------------------------
-- 实现读写锁：
-- - 多个读线程可以同时持有锁
-- - 写线程独占锁
-- - 写线程优先于读线程（防止饿死）

data RWLock = RWLock
    { readers :: TVar Int
    , writersWaiting :: TVar Int
    , writing :: TVar Bool
    }

newRWLock :: IO RWLock
newRWLock = do
    r <- newTVarIO 0
    w <- newTVarIO 0
    wr <- newTVarIO False
    return $ RWLock r w wr

acquireReadLock :: RWLock -> STM ()
acquireReadLock RWLock{..} = do
    -- 如果有写线程在等待或正在写，阻塞
    -- 否则增加读者计数
    error "TODO: 实现获取读锁"

releaseReadLock :: RWLock -> STM ()
releaseReadLock RWLock{..} = do
    -- 减少读者计数
    error "TODO: 实现释放读锁"

acquireWriteLock :: RWLock -> STM ()
acquireWriteLock RWLock{..} = do
    -- 增加等待写线程计数
    -- 等待没有读者且没有其他写线程
    -- 标记正在写
    error "TODO: 实现获取写锁"

releaseWriteLock :: RWLock -> STM ()
releaseWriteLock RWLock{..} = do
    -- 标记不在写
    -- 减少等待写线程计数
    error "TODO: 实现释放写锁"

-- 使用读写锁的辅助函数
withReadLock :: RWLock -> IO a -> IO a
withReadLock lock action = do
    atomically $ acquireReadLock lock
    result <- action
    atomically $ releaseReadLock lock
    return result

withWriteLock :: RWLock -> IO a -> IO a
withWriteLock lock action = do
    atomically $ acquireWriteLock lock
    result <- action
    atomically $ releaseWriteLock lock
    return result

-- --------------------------------------------
-- 任务 4: 限制并发的信号量
-- --------------------------------------------
-- 实现一个信号量，限制同时进行的操作数

data Semaphore = Semaphore
    { permits :: TVar Int
    , maxPermits :: Int
    }

newSemaphore :: Int -> IO Semaphore
newSemaphore n = do
    p <- newTVarIO n
    return $ Semaphore p n

acquire :: Semaphore -> STM ()
acquire Semaphore{..} = do
    -- 等待有可用的 permit
    -- 减少 permit 计数
    error "TODO: 实现 acquire"

release :: Semaphore -> STM ()
release Semaphore{..} = do
    -- 增加 permit 计数
    -- 注意：不能超过 maxPermits
    error "TODO: 实现 release"

withSemaphore :: Semaphore -> IO a -> IO a
withSemaphore sem action = do
    atomically $ acquire sem
    result <- action
    atomically $ release sem
    return result

-- 使用信号量限制并发
-- limitedConcurrentMap 4 (*2) [1..100]
-- 最多同时处理 4 个元素
limitedConcurrentMap :: Int -> (a -> IO b) -> [a] -> IO [b]
limitedConcurrentMap maxConcurrency f xs = do
    -- 创建信号量
    -- 使用 mapConcurrently 或手动管理线程
    -- 每个任务 acquire -> 执行 -> release
    error "TODO: 实现限制并发"

-- --------------------------------------------
-- 任务 5: 生产者-消费者队列（带容量限制）
-- --------------------------------------------
-- 实现有界队列：
-- - 队列有最大容量
-- - 生产者满了时阻塞
-- - 消费者空了时阻塞

data BoundedQueue a = BoundedQueue
    { queue :: TVar [a]
    , capacity :: Int
    , size :: TVar Int
    }

newBoundedQueue :: Int -> IO (BoundedQueue a)
newBoundedQueue cap = do
    q <- newTVarIO []
    s <- newTVarIO 0
    return $ BoundedQueue q cap s

writeQueue :: BoundedQueue a -> a -> STM ()
writeQueue BoundedQueue{..} x = do
    -- 如果队列满了，阻塞
    -- 否则添加元素
    error "TODO: 实现 writeQueue"

readQueue :: BoundedQueue a -> STM a
readQueue BoundedQueue{..} = do
    -- 如果队列空了，阻塞
    -- 否则取出元素
    error "TODO: 实现 readQueue"

-- 测试生产者-消费者
producerConsumerTest :: IO ()
producerConsumerTest = do
    queue <- newBoundedQueue 5 :: IO (BoundedQueue Int)
    
    -- 生产者：每秒生产 1 个，共 10 个
    forkIO $ replicateM_ 10 $ do
        threadDelay 1_000_000
        atomically $ writeQueue queue 1
        putStrLn "Produced"
    
    -- 消费者：每 2 秒消费 1 个
    forkIO $ forever $ do
        x <- atomically $ readQueue queue
        threadDelay 2_000_000
        putStrLn $ "Consumed: " ++ show x
    
    threadDelay 20_000_000  -- 运行 20 秒

-- --------------------------------------------
-- 任务 6: 超时操作
-- --------------------------------------------
-- 实现带超时的操作

timeout :: Int -> IO a -> IO (Maybe a)
timeout microseconds action = do
    -- 在新线程中执行 action
    -- 主线程等待 timeout
    -- 如果超时，返回 Nothing
    -- 如果完成，返回 Just result
    error "TODO: 实现 timeout"

-- 使用示例：
-- timeout 1000000 (threadDelay 500000 >> return "Done") = Just "Done"
-- timeout 1000000 (threadDelay 2000000 >> return "Too late") = Nothing

-- --------------------------------------------
-- 任务 7: 并行计算斐波那契
-- --------------------------------------------
-- 使用 forkIO 并行计算多个斐波那契数

parallelFibs :: [Int] -> IO [Int]
parallelFibs ns = do
    -- 为每个 n 创建一个线程计算 fib
    -- 收集所有结果
    -- 保持原始顺序
    error "TODO: 实现并行计算"
  where
    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

-- ============================================
-- 进阶挑战：实现 STM 的 retry/orElse
-- =============================================

-- 实现 tryReadChan：
-- 尝试从队列读取，如果空了立即返回 Nothing（不阻塞）
tryReadQueue :: BoundedQueue a -> STM (Maybe a)
tryReadQueue queue = error "TODO: 使用 orElse 实现非阻塞读取"

-- ============================================
-- 测试运行
-- =============================================
-- :load exercises/Exercise05.hs
-- concurrentCounter 10 1000 应该返回 10000
-- producerConsumerTest 应该展示生产者-消费者的协调
-- parallelFibs [35, 36, 37, 38] 应该并行计算（比顺序快）

main :: IO ()
main = do
    putStrLn "Running Exercise 05 tests..."
    
    -- 测试任务 1
    putStrLn "\n1. Testing concurrent counter..."
    result <- concurrentCounter 10 1000
    putStrLn $ "Result: " ++ show result ++ " (expected: 10000)"
    
    -- 测试任务 3
    putStrLn "\n2. Testing RWLock..."
    lock <- newRWLock
    -- 启动多个读写线程进行测试
    putStrLn "RWLock test completed"
    
    putStrLn "\nAll tests completed!"
