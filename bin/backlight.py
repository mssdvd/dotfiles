#!/usr/bin/env python3
import argparse
import fcntl
import json
import os
import re
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser()
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-i", type=int)
    group.add_argument("-d", type=int)
    group.add_argument("-s", type=int)
    args = parser.parse_args()

    res = subprocess.run(
        ["swaymsg", "-t", "get_outputs", "-r"], capture_output=True, encoding="utf-8"
    )
    outputs = json.loads(res.stdout)
    focused_output = next(o for o in outputs if o["focused"])

    if focused_output["name"] == "eDP-1":
        fn = brightnessctl
    else:
        fn = ddcutil

    fn(args)


def brightnessctl(args: argparse.Namespace) -> None:
    lock_file = tempfile.gettempdir() + "/backlight-brightnessctl.lock"

    # brightnessctl's "s +/-" is itself a read-modify-write against sysfs.
    # It's fast, but not instant, and our own startup overhead is enough for
    # key repeat to overlap invocations, so guard it the same way as ddcutil.
    lock_fd = open(lock_file, "w")
    try:
        fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        # Don't read-and-print here: that read is unsynchronized with the
        # lock holder's write, so it can finish and flush *after* the lock
        # holder's fresher value, showing wob a stale value out of order.
        # Printing nothing keeps wob's last value until the real update.
        return

    try:
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
        print(pct, flush=True)
    finally:
        fcntl.flock(lock_fd, fcntl.LOCK_UN)
        lock_fd.close()


def resolve_bus(bus_cache_file: str) -> str:
    try:
        with open(bus_cache_file) as f:
            return f.read().strip()
    except FileNotFoundError:
        pass

    # "ddcutil detect" scans every I2C bus and is slow (~700ms); only pay
    # for it once and cache the result.
    res = subprocess.run(
        ["ddcutil", "detect", "--brief"], capture_output=True, encoding="utf-8"
    )
    for block in res.stdout.split("\n\n"):
        if block.startswith("Display "):
            m = re.search(r"/dev/i2c-(\d+)", block)
            if m:
                bus = m.group(1)
                with open(bus_cache_file, "w") as f:
                    f.write(bus)
                return bus
    sys.exit(1)


def ddcutil(args: argparse.Namespace) -> None:
    cache_file = tempfile.gettempdir() + "/backlight"
    bus_cache_file = tempfile.gettempdir() + "/backlight-bus"
    lock_file = cache_file + ".lock"

    # ddcutil's DDC/CI round-trip is slow (0.3-1s) and not safe to run
    # concurrently, so key repeat can pile up many overlapping processes.
    # If one is already in flight, drop this request instead of racing it.
    lock_fd = open(lock_file, "w")
    try:
        fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        # Don't read-and-print here: that read is unsynchronized with the
        # lock holder's write, so it can finish and flush *after* the lock
        # holder's fresher value, showing wob a stale value out of order.
        # Printing nothing keeps wob's last value until the real update.
        return

    try:
        bus = resolve_bus(bus_cache_file)

        if args.s is not None:
            pct = args.s
        else:
            use_cache = True
            try:
                with open(cache_file) as f:
                    prev_pct = int(f.read())
            except (FileNotFoundError, ValueError):
                use_cache = False

            if not use_cache:
                try:
                    res = subprocess.run(
                        ["ddcutil", "--bus", bus, "getvcp", "10", "-t"],
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
        cmd = ["ddcutil", "--bus", bus, "--noverify", "setvcp", "10", pct]

        res = subprocess.run(cmd, capture_output=True)
        if len(res.stderr) > 0:
            # The cached bus may be stale (monitor unplugged/remapped);
            # drop it so the next invocation re-detects instead of
            # failing forever against a dead bus number.
            try:
                os.remove(bus_cache_file)
            except FileNotFoundError:
                pass
            sys.exit(1)

        with open(cache_file, "w") as f:
            f.write(pct)
        print(pct, flush=True)
    finally:
        fcntl.flock(lock_fd, fcntl.LOCK_UN)
        lock_fd.close()


if __name__ == "__main__":
    main()
