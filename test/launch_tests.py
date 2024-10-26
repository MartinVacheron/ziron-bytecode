import os
import subprocess
import argparse
from tools import *


# CLI
parser = argparse.ArgumentParser()

parser.add_argument(
    "--print-err",
    action='store_true', # Acts like a flag
    help="only tests errors from tests",
    default=False
)

args = parser.parse_args()


# Main
print(clr_str("\t\tLaunching tests for Rizon language", YELLOW))

total_tests = 0
total_ok = 0
total_ko = 0

for dir in os.listdir():
    if dir in ["benchmark", "benchmark_results", "__pycache__", "limit"] or os.path.isfile(dir):
        continue

    print(f"\n\tTesting folder: {clr_str(dir, YELLOW)}\n")

    files = os.listdir(dir)
    nb_tests = len(files)
    print(f"running {nb_tests} tests")

    total_tests += nb_tests

    for file in files:
        path = f"{dir}\\{file}"

        # Error print mode
        if args.print_err:
            print(f"running {dir}::{file}...")
            
            result = subprocess.run(["..\\zig-out\\bin\\rizon.exe", path], capture_output=True)
            rizon_output = result.stdout.decode().strip()

            for line in rizon_output.split("\n"):
                if "error" in line or "-->" in line or " | " in line or "^" in line or line == "":
                    print(line)
                else:
                    continue

        else:
            result = subprocess.run(["..\\zig-out\\bin\\rizon.exe", path], capture_output=True)
            content = open(path, "r", encoding="utf-8")

            errors = []
            expects = []
            for line in content.readlines():
                if "error" in line:
                    err = line.split(": ")[1].strip()
                    errors.append(err)
                elif "expect" in line:
                    exp = line.split("expect: ")[1].strip()
                    expects.append(exp)

            rizon_output = result.stdout.decode().strip()
            rizon_res = []
            
            for line in rizon_output.split("\n"):
                if "-->" in line or " | " in line or "^" in line or line == "" or "] in " in line:
                    continue
                else:
                    rizon_res.append(line.strip())

            rizon_output = result.stderr.decode().strip()
            rizon_err = []
            for line in rizon_output.split("\n"):
                if "Error" in line:
                    rizon_err.append(line.split(": ")[1])

            ok = rizon_res == expects and rizon_err == errors

            if ok:
                res = clr_str("Ok", GREEN)
                total_ok += 1
            else:
                print(f"Got results: {rizon_res}")
                print(f"Expected results: {expects}")
                print(f"Got errors {rizon_err}")
                print(f"Expected erros: {errors}")
                res = clr_str("Ko", RED)
                total_ko += 1

            print(f"{clr_str('testing', YELLOW)} {dir}::{file}...  {res}")

            if not ok:
                if len(rizon_res) > 0:
                    print(f"Expected:\n{expects}")
                    print(f"Got:\n{rizon_res}")

                if len(rizon_err) > 0:
                    print(f"Expected errors:\n{errors}")
                    print(f"Got errros:\n{rizon_err}")

                print()


print(clr_str("\n\n\t\tStatistics\n", YELLOW))
print(f"Total tests: {total_tests}")
print("Total Ok: " + clr_str(f"{total_ok}", GREEN))
print("Total Ko: " + clr_str(f"{total_ko}", RED))

print("\n")
