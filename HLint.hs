import HLint.Default
import HLint.Generalise
import HLint.Dollar

-- ignore some hints w.r.t. eta-reduction
ignore "Eta reduce" = Control.Monad.Sharing.Classes Data.Monadic.List
