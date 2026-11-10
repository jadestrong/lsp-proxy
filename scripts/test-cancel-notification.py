#!/usr/bin/env python3
"""Exercise cancellation and malformed notifications against the real proxy."""

import argparse
import json
import queue
import subprocess
import tempfile
import threading
from pathlib import Path


def read_responses(stream, responses):
    try:
        while True:
            headers = {}
            while True:
                line = stream.readline()
                if not line:
                    raise EOFError("proxy stdout closed")
                if line == b"\r\n":
                    break
                key, value = line.decode().split(":", 1)
                headers[key.lower()] = value.strip()
            body = stream.read(int(headers["content-length"]))
            responses.put(json.loads(body))
    except Exception as error:
        responses.put(error)


def send(process, message):
    body = json.dumps(dict(jsonrpc="2.0", **message)).encode()
    process.stdin.write(b"Content-Length: " + str(len(body)).encode() + b"\r\n\r\n" + body)
    process.stdin.flush()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, default=Path("target/release/emacs-lsp-proxy"))
    args = parser.parse_args()
    uri = "file:///tmp/lsp-proxy-cancel-test.java"
    cases = [
        ("$/cancelRequest", {"id": 8, "uri": uri}),
        ("$/cancelRequest", {"id": "symbol-8", "uri": uri}),
        ("$/cancelRequest", {"uri": uri, "params": {"id": 8, "uri": uri}}),
        ("$/cancelRequest", {"id": 9}),
        ("$/cancelRequest", None),
        ("$/cancelRequest", {"uri": uri, "params": {"id": []}}),
        ("textDocument/didChange", {"params": "invalid"}),
        ("textDocument/didFocus", {"params": {"textDocument": {"uri": uri}}}),
    ]
    with tempfile.TemporaryDirectory(prefix="lsp-proxy-cancel-") as directory:
        process = subprocess.Popen(
            [str(args.binary.resolve()), "--stdio", "--log", str(Path(directory) / "proxy.log")],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        responses = queue.Queue()
        threading.Thread(target=read_responses, args=(process.stdout, responses), daemon=True).start()
        try:
            for index, (method, params) in enumerate(cases, 1):
                send(process, {"method": method, "params": params})
                # A response proves both the controller and application survived.
                send(process, {"id": index, "method": "emacs/listWorkspaces", "params": {"params": None}})
                response = responses.get(timeout=10)
                if isinstance(response, Exception):
                    raise response
                assert response["id"] == index, response
                assert "error" not in response, response
            assert process.poll() is None, "proxy exited after notifications"
            send(process, {"id": 100, "method": "shutdown", "params": {"params": None}})
            response = responses.get(timeout=10)
            assert isinstance(response, dict) and response.get("id") == 100, response
            send(process, {"method": "exit", "params": {"params": None}})
            assert process.wait(timeout=10) == 0, "proxy failed to shut down normally"
            stderr = process.stderr.read().decode(errors="replace")
            assert "panicked" not in stderr, stderr
            print(f"PASS: {len(cases)} notification cases; subsequent requests succeeded; exit=0")
        finally:
            if process.poll() is None:
                process.terminate()
                process.wait(timeout=10)
            for stream in (process.stdin, process.stdout, process.stderr):
                stream.close()


if __name__ == "__main__":
    main()
