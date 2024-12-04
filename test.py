#!/usr/bin/env python3

from pathlib import Path
import subprocess
import sys

EXAMPLES_DIR : str = "examples"
EXPECTED_DIR : str = "examples-expected"

def example_couple(path : Path) -> tuple[Path, Path]:
    return (path, Path(str(path).replace(EXAMPLES_DIR, EXPECTED_DIR).replace(".scm", ".txt")))

TESTS : list[str] = list(map(example_couple, list(Path(EXAMPLES_DIR).rglob("*.scm"))))
GLADOS : str = "./glados"

if len(TESTS) == 0:
    print(f"No .scm test file in {EXAMPLES_DIR}")
    quit()

def group_multiline_string(lines : list[str]) -> list[str]:
    new : list[str] = []
    is_in_str : bool = False

    for line in lines:
        if is_in_str:
            new[-1] += line
            if "'" in line:
                is_in_str = False
            continue
        new.append(line)
        is_in_str = line.count("'") % 2 == 1
    return new

def parse_expected(path : Path) -> dict[str, str]:
        config = { key : value for key, value in map(lambda line: map(str.strip, line.strip("\n").split(":", 1)), group_multiline_string(open(str(path), "r").readlines())) }
        if "RETCODE" not in config:
            config.update({ "RETCODE" : 0 })
        else:
            config["RETCODE"] = int(config["RETCODE"])
        for output in ("STDOUT", "STDERR"):
            if output in config:
                config[output] = config[output].strip("'")
        return config

n_passed = 0

for i, (test, expected) in enumerate(TESTS):
    if i > 0:
        print()
    config = parse_expected(expected)
    print(f"Test {i+1}: {GLADOS} {test}")
    passed = True
    run = subprocess.run([GLADOS, str(test)], stdout=subprocess.PIPE, stderr = subprocess.PIPE)
    if run.returncode != config["RETCODE"]:
        print(f"--> Got return code {run.returncode} but expected {config['RETCODE']}", file = sys.stderr)
        passed = False
    outputs = [("STDOUT", run.stdout), ("STDERR", run.stderr)]
    for output_name, output in outputs:
        output = output.decode("utf-8").strip("\n")
        if output_name in config and output != config[output_name]:
            print(f"--> Got {output_name} '{output}' but expected '{config[output_name].strip("'")}'")
            passed = False
    if passed:
        print("--> PASSED")
        n_passed += 1

print(f"\nSummary: passed {n_passed}/{len(TESTS)} test{'s' if len(TESTS) > 1 else ''}")

if n_passed != len(TESTS):
    sys.exit(1)