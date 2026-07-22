#!/usr/bin/env bash
set -euo pipefail

image="${1:?Usage: tests/container_smoke.sh <image>}"
container="2k1-container-smoke-$RANDOM"

cleanup() {
  docker rm --force "$container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --detach \
  --name "$container" \
  --platform linux/amd64 \
  --read-only \
  --tmpfs /tmp:rw,noexec,nosuid,size=128m \
  --publish 127.0.0.1::3838 \
  "$image" >/dev/null
host_port="$(docker port "$container" 3838/tcp | awk -F: '{print $NF}')"

for _ in $(seq 1 60); do
  if curl --fail --silent --show-error "http://127.0.0.1:${host_port}/" >/dev/null; then
    docker inspect --format '{{.State.Health.Status}}' "$container" | grep -Eq 'healthy|starting'
    exit 0
  fi
  if [ "$(docker inspect --format '{{.State.Running}}' "$container")" != true ]; then
    docker logs "$container" >&2
    exit 1
  fi
  sleep 2
done

docker logs "$container" >&2
echo "2k1-in-Silico did not become ready within 120 seconds." >&2
exit 1
