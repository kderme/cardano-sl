module Bench.Pos.Criterion.TxSigningBench
       ( runBenchmark
       ) where

import           Criterion.Main (Benchmark, bench, defaultConfig, defaultMainWith, env, whnf)
import           Criterion.Types (Config (..))
import           Test.QuickCheck (generate)
import           Universum

import           Pos.Crypto (ProtocolMagic, SecretKey, SignTag (SignTx), sign)
import           Pos.Ssc ()
import           Pos.Txp (TxId, TxSig, TxSigData (..))

import           Test.Pos.Txp.Arbitrary.Unsafe ()
import           Test.Pos.Util.QuickCheck.Arbitrary (arbitraryUnsafe)

import           Bench.Configuration (giveCoreConf)

signTx :: ProtocolMagic -> (SecretKey, TxId) -> TxSig
signTx pm (sk, thash) = sign pm SignTx sk txSigData
  where
    txSigData = TxSigData
        { txSigTxHash = thash
        }

txSignBench :: ProtocolMagic -> Benchmark
txSignBench pm = env genArgs $ bench "Transactions signing" . whnf (signTx pm)
  where genArgs = generate $ (,)
                  <$> arbitraryUnsafe
                  <*> arbitraryUnsafe

txSignConfig :: Config
txSignConfig = defaultConfig
    { reportFile = Just "txSigning.html"
    }

runBenchmark :: IO ()
runBenchmark = giveCoreConf $ \pm -> defaultMainWith txSignConfig [txSignBench pm]
