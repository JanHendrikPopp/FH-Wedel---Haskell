module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0
fib     :: Integer -> Integer
fib x
  | x == 0  = 0
  | x == 1  = 1
  | x > 1   = fib (x-1) + fib (x-2)


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 n = fibHelper n 0 1

fibHelper :: Integer -> Integer -> Integer -> Integer
fibHelper n x0 x1
  | n == 0    = x0
  | otherwise = fibHelper (n-1) x1 (x0 + x1)



-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
c       :: Integer -> Integer
c 1 = 0
c n = 1 + if even n then c (div n 2) else c ((n * 3) + 1)



-- Definieren Sie ein endrekurive Variante von c

c1      :: Integer -> Integer
c1 n = cHelper n 0

cHelper :: Integer -> Integer -> Integer
cHelper n c
  | n == 1          = 0
  | (mod n 2) == 0  = 1 + (cHelper (div n 2) (c + 1))
  | otherwise       = 1 + (cHelper ((n * 3) + 1) (c + 1))


-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub = case (lb < 0 || lb > ub) of
              True  -> error "wrong input"
              False -> cm [lb..ub]
        where
          cm (x:xs)   = maximum ([c1 x] ++ [(cm xs)])
          cm [x]      = c1 x
          cm []       = 0

-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub = case (lb < 0 || lb > ub) of
              True  -> error "wrong input"
              False -> maxi f [lb..ub]
        where
          maxi f (x:xs)   = maximum ([f x] ++ [(maxi f xs)])
          maxi f [x]      = f x
          maxi f []       = 0


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = maxi2 f lb ub (lb, f lb)
  where maxi2 f lb ub (x,val)
              | lb <= ub         = let maxi = f lb in
                                   case (maxi < val) of
                                    True  -> maxi2 f (lb + 1) ub (x, val)
                                    False -> maxi2 f (lb + 1) ub (lb, maxi)
              | lb == (ub + 1)   = (x, val)
              | otherwise        = error "wrong input"


-- ----------------------------------------
