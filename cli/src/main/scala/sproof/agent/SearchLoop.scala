package sproof.agent

import sproof.core.{Term, Context, GlobalEnv}
import sproof.syntax.{SProof, STactic}
import sproof.Checker

/** Searches for a proof of a goal by trying generated tactic candidates. */
object SearchLoop:

  final case class SearchConfig(
    maxDepth: Int = 1,
    maxNodes: Int = 128,
  )

  final case class SearchStats(
    totalCandidates: Int,
    uniqueCandidates: Int,
    exploredNodes: Int,
    prunedDuplicates: Int,
    hitNodeLimit: Boolean,
  )

  final case class SearchResult(
    found: Option[STactic],
    stats: SearchStats,
  )

  /** Backward-compatible API: search with default configuration. */
  def search(ctx: Context, prop: Term)(using env: GlobalEnv): Option[STactic] =
    searchWithConfig(ctx, prop, SearchConfig()).found

  /** Search plus diagnostics about induction-variable strategy.
   *
   * Diagnostics are emitted only when no proof is found.
   */
  def searchWithDiagnostics(ctx: Context, prop: Term)(using env: GlobalEnv): (Option[STactic], List[String]) =
    val rankedVars = TacticGen.rankedInductionVars(ctx, prop).map(_._1)
    val result = searchWithConfig(ctx, prop, SearchConfig())
    val diagnostics =
      if result.found.nonEmpty then Nil
      else
        List(
          s"induction variable order: ${rankedVars.mkString(", ")}",
          s"fallback induction candidates tried: ${rankedVars.length}",
          s"proof search exhausted after ${result.stats.exploredNodes} candidate(s)",
        )
    (result.found, diagnostics)

  /** Search with configurable depth/node limits and deterministic pruning. */
  def searchWithConfig(
    ctx: Context,
    prop: Term,
    config: SearchConfig,
  )(using env: GlobalEnv): SearchResult =
    val raw = TacticGen.rawCandidates(ctx, prop, maxDepth = math.max(0, config.maxDepth))
    val ranked = TacticGen.dedupeCandidates(raw)
    val unique = ranked.map(_.tactic)
    val bounded = unique.take(math.max(1, config.maxNodes))

    var explored = 0
    var found: Option[STactic] = None
    val it = bounded.iterator
    while found.isEmpty && it.hasNext do
      val tac = it.next()
      explored += 1
      Checker.executeProof(ctx, prop, SProof.SBy(tac)) match
        case Right(_) => found = Some(tac)
        case Left(_)  => ()

    val stats = SearchStats(
      totalCandidates = raw.size,
      uniqueCandidates = unique.size,
      exploredNodes = explored,
      prunedDuplicates = raw.size - unique.size,
      hitNodeLimit = unique.size > math.max(1, config.maxNodes),
    )
    SearchResult(found, stats)
