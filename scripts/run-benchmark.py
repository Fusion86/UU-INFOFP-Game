import os
import shutil
import subprocess
import statistics
from pathlib import Path
from datetime import datetime

GAME_BIN = "./UU-INFOFP-Game-exe"

if __name__ == "__main__":
    if not os.path.isfile(GAME_BIN):
        print(f"Game binary '{GAME_BIN}' not found in the current working directory.")
        exit(1)

    temp_dir = Path("./tmp")
    results_dir = Path("./benchmark", datetime.today().strftime("%Y-%m-%d %H:%M:%S"))

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

    with open(results_dir / "report.txt", "w") as f:
        f.write("mean frametime: {}\n".format(statistics.mean(frametimes)))
        f.write("lowest frametime: {}\n".format(min(frametimes)))
        f.write("highest frametime: {}\n".format(max(frametimes)))
