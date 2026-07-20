#!/usr/bin/env python3
import sys
import json
import subprocess
import argparse
import tempfile
import fcntl


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


def ddcutil(args: argparse.Namespace) -> None:
    cmd = ["ddcutil", "setvcp", "10"]
    cache_file = tempfile.gettempdir() + "/backlight"
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
        print(pct, flush=True)
    finally:
        fcntl.flock(lock_fd, fcntl.LOCK_UN)
        lock_fd.close()


if __name__ == "__main__":
    main()
