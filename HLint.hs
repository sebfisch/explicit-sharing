import HLint.Default
import HLint.Generalize
import HLint.Dollar

-- ignore hints w.r.t. eta-reduction that don't work due to
-- higher-rank polymorphism
ignore "Eta reduce" = Control.Monad.Sharing
