package sroof.eval

import sroof.core.Context

/** Builds an NbE evaluation environment from a typing context.
 *
 *  Assumption entries are mapped to fresh semantic variables (neutral terms),
 *  and Def (let-binding) entries are reduced to semantic values via Eval.
 *
 *  This is the canonical shared implementation — previously duplicated in
 *  Bidirectional, Builtins, and Kernel.
 */
object EnvBuilder:
  def fromContext(ctx: Context): Env =
    ctx.entries.reverse.foldLeft(List.empty[Semantic]) { (partialEnv, entry) =>
      entry match
        case Context.Entry.Assum(_, _) =>
          Semantic.freshVar(partialEnv.size) :: partialEnv
        case Context.Entry.Def(_, _, defn) =>
          Eval.eval(partialEnv, defn) :: partialEnv
    }
