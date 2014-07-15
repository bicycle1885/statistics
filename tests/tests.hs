import Test.Framework (defaultMain)
import qualified Tests.Distribution as Distribution
import qualified Tests.Function as Function
import qualified Tests.KDE as KDE
import qualified Tests.Parametric as Parametric
import qualified Tests.NonParametric as NonParametric
import qualified Tests.Transform as Transform

main :: IO ()
main = defaultMain [ Distribution.tests
                   , Function.tests
                   , KDE.tests
                   , Parametric.tests
                   , NonParametric.tests
                   , Transform.tests
                   ]
