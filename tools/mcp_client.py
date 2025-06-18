#!/usr/bin/env python3
import argparse
import socket
import sys


def parse_args():
    p = argparse.ArgumentParser(description="Mathex MCP client")
    p.add_argument("function", help="Mathex function to call")
    p.add_argument("args", nargs="*", help="Arguments to the function")
    p.add_argument("--host", default="localhost", help="Server host")
    p.add_argument("--port", type=int, default=4040, help="Server port")
    return p.parse_args()


def main():
    opts = parse_args()
    numbers = []
    for a in opts.args:
        try:
            if "." in a:
                numbers.append(float(a))
            else:
                numbers.append(int(a))
        except ValueError:
            sys.stderr.write(f"invalid number: {a}\n")
            return 1

    term = f"{{{opts.function},[{','.join(map(str, numbers))}]}}.\n"
    with socket.create_connection((opts.host, opts.port)) as s:
        s.sendall(term.encode())
        resp = s.recv(4096).decode().strip()
        print(resp)
    return 0


if __name__ == "__main__":
    sys.exit(main())
