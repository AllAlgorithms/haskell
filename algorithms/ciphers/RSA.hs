{-
This module implements a textbook implementation of RSA.
-}
module RSA
  ( encrypt
  , decrypt
  , PubKey
  , PrivKey
  )
where

data PubKey = PubKey
  { pubN :: Integer
  , pubE :: Integer
  } deriving Show

data PrivKey = PrivKey
  { privN :: Integer
  , privD :: Integer
  } deriving Show

-- |RSA encryption.
encrypt :: PubKey -> Integer-> Integer
encrypt (PubKey n e) plainText = (plainText ^ e) `mod` n

-- |RSA decryption.
decrypt :: PrivKey -> Integer -> Integer
decrypt (PrivKey n d) cipherText = (cipherText ^ d) `mod` n

-- |Key generation.
--
-- Must:
-- 1. p and q must be distinct primes.
-- 2. 1 < e < (p-1)(q-1), and gcd(e, (p-1)(q-1)) = 1.
genKeyPair :: Integer -> Integer -> Integer -> (PubKey, PrivKey)
genKeyPair p q e = let phi = lcm (p-1) (q-1)
                       d   = modInv e phi
                       n   = p * q
                   in ((PubKey n e), (PrivKey n d))

-- |A slow primality test.
isPrime :: Integer -> Bool
isPrime n = all (==False) [ n `mod` x == 0 | x <- [2..n-1] ]

-- |Extended Eucl
exgcd :: Integer -> Integer -> (Integer, Integer, Integer)
exgcd a 0 = (a, 1, 0)
exgcd a b = (g, y, x - (a `div` b) * y)
  where (g, x, y) = exgcd b (a `mod` b)

-- |Modulo inverse.
--
-- b = a^(-1) mod n
-- b * a - k * n = 1
modInv :: Integer -> Integer -> Integer
modInv a n = let (_, b, _) = exgcd a n in b `mod` n

-- Example:
-- Choose e = 3, which is coprime to lcm(60,52)=780
(exPub, exPriv) = genKeyPair 61 53 17
plainText = 233
encrypted = encrypt exPub plainText
decrypted = decrypt exPriv encrypted
