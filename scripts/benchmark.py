#!/usr/bin/env python3
"""Run sproof benchmarks and compare medians against thresholds."""

from __future__ import annotations

import argparse
import json
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Tuple


@dataclass(frozen=True)
class Workload:
    name: str
    path: str


DEFAULT_WORKLOADS: List[Workload] = [
    Workload("nat", "examples/nat.sproof"),
    Workload("int", "examples/int.sproof"),
    Workload("list", "examples/list.sproof"),
    Workload("bool", "examples/bool.sproof"),
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", type=int, default=3, help="runs per workload (default: 3)")
    parser.add_argument("--sbt", default="sbt", help="sbt executable (default: sbt)")
    parser.add_argument(
        "--thresholds",
        default="benchmarks/thresholds.json",
        help="path to benchmark thresholds JSON",
    )
    parser.add_argument(
        "--output",
        default="benchmarks/results.json",
        help="output JSON report path",
    )
    parser.add_argument(
        "--fail-on-breach",
        action="store_true",
        help="exit non-zero when threshold breach is detected",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="print sbt output for each benchmark command",
    )
    return parser.parse_args()


def run_once(repo_root: Path, sbt_cmd: str, workload_path: str, verbose: bool) -> float:
    command = [sbt_cmd, f"cli/run check {workload_path}"]
    start = time.perf_counter()
    completed = subprocess.run(
        command,
        cwd=repo_root,
        text=True,
        capture_output=True,
        check=False,
    )
    elapsed_ms = (time.perf_counter() - start) * 1000.0
    if verbose:
        sys.stdout.write(completed.stdout)
        sys.stderr.write(completed.stderr)
    if completed.returncode != 0:
        print(f"[bench] command failed: {' '.join(command)}", file=sys.stderr)
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
        raise RuntimeError(f"benchmark command failed for {workload_path}")
    return elapsed_ms


def load_thresholds(path: Path) -> Dict[str, object]:
    if not path.exists():
        return {}
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, dict):
        raise ValueError(f"thresholds must be object: {path}")
    return data


def percentile_median(samples: List[float]) -> float:
    return float(statistics.median(samples))


def main() -> int:
    args = parse_args()
    if args.runs < 1:
        raise ValueError("--runs must be >= 1")

    repo_root = Path(__file__).resolve().parent.parent
    thresholds_path = (repo_root / args.thresholds).resolve()
    output_path = (repo_root / args.output).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    thresholds = load_thresholds(thresholds_path)
    workload_thresholds = thresholds.get("workloads", {}) if isinstance(thresholds.get("workloads", {}), dict) else {}
    total_threshold = thresholds.get("totalMedianMs")

    results = []
    breach_messages: List[str] = []

    for workload in DEFAULT_WORKLOADS:
        samples: List[float] = []
        for _ in range(args.runs):
            elapsed_ms = run_once(repo_root, args.sbt, workload.path, args.verbose)
            samples.append(elapsed_ms)
        median_ms = percentile_median(samples)
        threshold = workload_thresholds.get(workload.name)
        breached = isinstance(threshold, (int, float)) and median_ms > float(threshold)
        if breached:
            breach_messages.append(
                f"{workload.name}: median {median_ms:.2f}ms > threshold {float(threshold):.2f}ms"
            )
        results.append(
            {
                "name": workload.name,
                "path": workload.path,
                "samplesMs": [round(v, 2) for v in samples],
                "medianMs": round(median_ms, 2),
                "thresholdMs": float(threshold) if isinstance(threshold, (int, float)) else None,
                "breached": breached,
            }
        )

    total_median_ms = sum(float(item["medianMs"]) for item in results)
    total_breached = isinstance(total_threshold, (int, float)) and total_median_ms > float(total_threshold)
    if total_breached:
        breach_messages.append(
            f"totalMedianMs: {total_median_ms:.2f}ms > threshold {float(total_threshold):.2f}ms"
        )

    report = {
        "generatedAt": datetime.now(timezone.utc).isoformat(),
        "runs": args.runs,
        "sbt": args.sbt,
        "results": results,
        "summary": {
            "totalMedianMs": round(total_median_ms, 2),
            "totalThresholdMs": float(total_threshold) if isinstance(total_threshold, (int, float)) else None,
            "breached": len(breach_messages) > 0,
            "breaches": breach_messages,
        },
    }

    with output_path.open("w", encoding="utf-8") as f:
        json.dump(report, f, indent=2)

    print(f"[bench] report written: {output_path}")
    for item in results:
        threshold_text = (
            f"{item['thresholdMs']:.2f}ms"
            if isinstance(item["thresholdMs"], float)
            else "n/a"
        )
        marker = "FAIL" if item["breached"] else "OK"
        print(
            f"[bench] {item['name']:>4} median={item['medianMs']:>9.2f}ms "
            f"threshold={threshold_text:>9} [{marker}]"
        )
    print(
        f"[bench] total median={report['summary']['totalMedianMs']:.2f}ms "
        f"threshold={report['summary']['totalThresholdMs'] if report['summary']['totalThresholdMs'] is not None else 'n/a'}"
    )

    if breach_messages:
        print("[bench] threshold breaches detected:")
        for msg in breach_messages:
            print(f"  - {msg}")
        if args.fail_on_breach:
            return 2

    return 0


if __name__ == "__main__":
    sys.exit(main())
