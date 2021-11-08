#!/usr/bin/env python3

import os
import shutil
import subprocess
import statistics
from pathlib import Path
from datetime import datetime
from tabulate import tabulate

GAME_BIN = "./UU-INFOFP-Game-exe"

if __name__ == "__main__":
    if not os.path.isfile(GAME_BIN):
        print(f"Game binary '{GAME_BIN}' not found in the current working directory.")
        exit(1)

    temp_dir = Path("./tmp")
    results_dir = Path("./benchmark", datetime.today().strftime("%Y-%m-%d_%H:%M:%S"))

    # Run neofetch
    neofetch = subprocess.run(["neofetch", "--stdout"], stdout=subprocess.PIPE)
    neofetch.check_returncode()

    # Ensure tmp dir is empty
    if temp_dir.exists():
        shutil.rmtree(temp_dir)
    os.mkdir(temp_dir)

    # Run game in benchmark mode
    r = subprocess.run(
        [GAME_BIN, "--benchmark"],
        env=dict(
            os.environ, GALLIUM_HUD="frametime", GALLIUM_HUD_DUMP_DIR=f"{temp_dir}"
        ),
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )

    game_output = r.stdout.decode("utf-8")

    if r.returncode != 0 or "[Benchmark End]" not in game_output:
        print("Benchmark run not completed")
        exit(1)

    print("Completed benchmark run")

    # Collect output and move to benchmark results directory
    results_dir.mkdir(parents=True, exist_ok=True)

    with open(results_dir / "neofetch.txt", "w") as f:
        f.write(neofetch.stdout.decode("utf-8"))

    with open(results_dir / "game.log", "w") as f:
        f.write(game_output)

    os.rename(temp_dir / "frametime_(ms)", results_dir / "frametimes.log")

    # Parse frametimes and create some cool stats
    frametimes = []
    with open(results_dir / "frametimes.log", "r") as f:
        for line in f.readlines():
            try:
                frametimes.append(float(line))
            except:
                pass
    
    # Sort descending for easier selection of worst frametimes
    frametimes.sort(reverse=True)

    # Make a table of data
    file_contents = [
        ["mean frametime", statistics.mean(frametimes), "milliseconds"],
        ["lowest frametime", min(frametimes), "milliseconds"],
        ["highest frametime", max(frametimes), "milliseconds"],
        ["average framerate", 1000/statistics.mean(frametimes), "frames per second"],
        ["1% low", 1000/statistics.mean(frametimes[0:(len(frametimes)//100)]), "frames per second"],
        ["0.1% low", 1000/statistics.mean(frametimes[0:(len(frametimes)//1000)]), "frames per second"],
    ]

    headers = ["statistic","value","unit"]

    # Format table and write to file
    with open(results_dir / "report.txt", "w") as f:
        f.write(tabulate(file_contents, floatfmt=".3f", headers=headers))
