import os
import subprocess
import time
import argparse
import statistics
from tools import *


"""
Each benchmark file is executed 10

Return format expected: 
 - First output is the total time of execution
 - For each other output, they must be by group of 2:
    - A label
    - A time
"""


# CLI
parser = argparse.ArgumentParser()

parser.add_argument(
    "--msg",
    help="add a message at the top of benchmark results file",
)

args = parser.parse_args()


# Main
print(clr_str("\n\t\tLaunching benchmarks for Rizon language\n", YELLOW))

# Compilation
print("Compiling in release mode...")
result = subprocess.run(["zig", "build", "-Doptimize=ReleaseFast"], cwd="..", capture_output=True)
rizon_output = result.stderr.decode().strip()

if not rizon_output == "":
    print(rizon_output)
print("Compilation done\n")

# Result file
time = time.strftime("%Y%m%d-%H%M%S")
outfile = open(f"benchmark\\benchmark_results\\benchmark_{time}", "w")

outfile.write("     --- Running benchmarks for Rizon language ---\n\n")

if not args.msg is None:
    outfile.write(f"Benchmark message: {args.msg}\n\n")

dir = "benchmark"
nb_runs = 10

for file in os.listdir(dir):
    if file == "benchmark_results":
        continue

    path = f"{dir}\\{file}"
    print(f"running {file}...")
    outfile.write(f"running {file}...\n")

    times = []
    labels = []
    for i in range(nb_runs):
        print(f"Run n°{i}")

        result = subprocess.run(["..\\zig-out\\bin\\rizon.exe", path], capture_output=True)

        rev_output = result.stdout.decode().strip()
        rev_split = rev_output.split("\n")
        rev_split.insert(0, 'global')

        if i == 0:
            times = [[] for _ in range(int(len(rev_split) / 2))]

            for label in rev_split[0::2]:
                labels.append(label)

        for i, time in enumerate(rev_split[1::2]):
            times[i].append(float(time))
                

    means = [statistics.mean(x) for x in times]
    stdevs = [statistics.stdev(x) for x in times]

    for i in range(len(times)):
        outfile.write(f"\t{labels[i]}:  mean: {means[i]:.6f}, stdev: {stdevs[i]:.6f}\n")

    print()
    outfile.write("\n")

print()
outfile.close()
