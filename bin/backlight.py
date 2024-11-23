#!/usr/bin/env python3
import sys
import i3ipc
import subprocess
import argparse
import tempfile


def main():
    parser = argparse.ArgumentParser()
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-i", type=int)
    group.add_argument("-d", type=int)
    group.add_argument("-s", type=int)
    args = parser.parse_args()

    sway = i3ipc.Connection()
    focused_output = next(o for o in sway.get_outputs() if o.focused)

    if focused_output.name == "eDP-1":
        fn = brightnessctl
    else:
        fn = ddcutil

    print(fn(args))


def brightnessctl(args: argparse.Namespace) -> str:
    cmd = ["brightnessctl", "s", "-m"]

    if args.i is not None:
        pct = str(args.i) + "%+"
    elif args.d is not None:
        pct = str(args.d) + "%-"
    elif args.s is not None:
        pct = str(args.s) + "%"

    cmd.append(pct)
    res = subprocess.run(cmd, capture_output=True, encoding="utf-8")
    pct = res.stdout.split(",")[3].removesuffix("%")

    return pct


def ddcutil(args: argparse.Namespace) -> str:
    cmd = ["ddcutil", "setvcp", "10"]
    cache_file = tempfile.gettempdir() + "/backlight"

    if args.s is not None:
        pct = args.s
    else:
        use_cache = True
        try:
            with open(cache_file) as f:
                prev_pct = int(f.read())
        except Exception:
            use_cache = False

        if not use_cache:
            try:
                res = subprocess.run(
                    ["ddcutil", "getvcp", "10", "-t"],
                    capture_output=True,
                    check=True,
                    encoding="utf-8",
                )
            except subprocess.CalledProcessError:
                sys.exit(1)
            prev_pct = int(res.stdout.split(" ")[3])

        if args.i is not None:
            pct = prev_pct + int(args.i)
        else:
            pct = prev_pct - int(args.d)

    pct = str(min(max(pct, 0), 100))
    cmd.append(pct)

    res = subprocess.run(cmd, capture_output=True)
    if len(res.stderr) > 0:
        sys.exit(1)

    with open(cache_file, "w") as f:
        f.write(pct)

    return pct


if __name__ == "__main__":
    main()
