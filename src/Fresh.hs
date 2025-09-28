module Fresh () where
import Syntax (VarId, Program (programDecls))

fresh :: (Program, VarId) -> (Program, VarId)
fresh (prog, var) = error "unimplemented"

freshVar :: VarId -> VarId
freshVar = (++ "'")
