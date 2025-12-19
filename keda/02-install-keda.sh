#!/usr/bin/env bash
# Purpose: Enable (or disable) the AKS managed KEDA add-on on an existing cluster.
# The managed add-on replaces the prior manual manifest install.
# Usage:
#   Enable:  ./02-install-keda.sh -g <resource-group> -n <cluster-name>
#   Disable: ./02-install-keda.sh -g <resource-group> -n <cluster-name> --disable
# Notes:
# - AKS controls the KEDA version; you cannot pin it with this add-on.
# - If you previously installed KEDA manually (keda namespace present), remove it before enabling the add-on:
#     kubectl delete namespace keda (after ensuring no dependent CRs remain)
set -euo pipefail

usage(){ echo "Usage: $0 -g <rg> -n <cluster> [--disable] [--no-apply-scaledjob] [--scaledjob-file <path>]"; }

DISABLE=false
APPLY_SCALEDJOB=true
SCALEDJOB_FILE=""
while [[ $# -gt 0 ]]; do
  case $1 in
    -g) RG=$2; shift 2;;
    -n) CLUSTER=$2; shift 2;;
  --disable) DISABLE=true; shift;;
  --no-apply-scaledjob) APPLY_SCALEDJOB=false; shift;;
  --scaledjob-file) SCALEDJOB_FILE=$2; shift 2;;
    -h|--help) usage; exit 0;;
    *) echo "Unknown arg $1"; usage; exit 1;;
  esac
done
: "${RG:?rg required}"; : "${CLUSTER:?cluster required}"

if ! command -v az >/dev/null 2>&1; then
  echo "[ERROR] Azure CLI (az) not found in PATH" >&2
  exit 1
fi

if $DISABLE; then
  echo "[INFO] Disabling AKS KEDA add-on on $CLUSTER"
  az aks update -g "$RG" -n "$CLUSTER" --disable-keda >/dev/null
  echo "[INFO] Disabled. (Custom ScaledJob/ScaledObject CRDs will remain until you delete them)"
  exit 0
fi

# Detect existing manual install
if kubectl get ns keda >/dev/null 2>&1; then
  echo "[WARN] Namespace 'keda' exists. If this is from a previous manual install, delete it before enabling the managed add-on to avoid conflicts." >&2
fi

echo "[INFO] Enabling AKS KEDA add-on on cluster: $CLUSTER"
az aks update -g "$RG" -n "$CLUSTER" --enable-keda >/dev/null

echo "[INFO] Fetching credentials"
az aks get-credentials -g "$RG" -n "$CLUSTER" --overwrite-existing >/dev/null

echo "[INFO] Verifying KEDA operator deployment (kube-system namespace)"
ATTEMPTS=30
SLEEP=5
for ((i=1;i<=ATTEMPTS;i++)); do
  if kubectl get deploy keda-operator -n kube-system >/dev/null 2>&1; then
    kubectl rollout status deploy/keda-operator -n kube-system --timeout=60s && break || true
  fi
  echo "[INFO] Waiting for keda-operator (attempt $i/$ATTEMPTS)"
  sleep $SLEEP
done

if ! kubectl get deploy keda-operator -n kube-system >/dev/null 2>&1; then
  echo "[ERROR] keda-operator deployment not found after enabling add-on" >&2
  exit 1
fi

echo "[INFO] KEDA add-on enabled. Version (from labels):"
kubectl get deploy keda-operator -n kube-system -o jsonpath='{.metadata.labels.app\.kubernetes\.io/version}' || echo "<version label missing>"
echo
if $APPLY_SCALEDJOB; then
  # Determine ScaledJob manifest path
  if [[ -z "$SCALEDJOB_FILE" ]]; then
    SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
    SCALEDJOB_FILE="${SCRIPT_DIR}/scaledjob.yaml"
  fi
  if [[ ! -f "$SCALEDJOB_FILE" ]]; then
    echo "[WARN] ScaledJob file not found at $SCALEDJOB_FILE; skipping apply." >&2
  else
    echo "[INFO] Applying ScaledJob manifest: $SCALEDJOB_FILE"
    # Required substitution variables
    : "${RUNNER_IMAGE:?Set RUNNER_IMAGE env var (e.g. yourregistry/model-runner:tag)}"
    : "${QUEUE_NAME:?Set QUEUE_NAME env var}"; : "${STORAGE_QUEUE_CONNECTION_STRING:?Set STORAGE_QUEUE_CONNECTION_STRING env var}"; : "${STORAGE_ACCOUNT_NAME:?Set STORAGE_ACCOUNT_NAME env var}";
    if command -v envsubst >/dev/null 2>&1; then
      # Only substitute known placeholders
      envsubst '${RUNNER_IMAGE} ${QUEUE_NAME} ${STORAGE_QUEUE_CONNECTION_STRING} ${STORAGE_ACCOUNT_NAME}' < "$SCALEDJOB_FILE" | kubectl apply -f -
    else
      echo "[WARN] envsubst not found; performing naive sed substitution."
      tmpfile=$(mktemp)
      cp "$SCALEDJOB_FILE" "$tmpfile"
      sed -i "s#\${RUNNER_IMAGE}#${RUNNER_IMAGE}#g" "$tmpfile"
      sed -i "s#\${QUEUE_NAME}#${QUEUE_NAME}#g" "$tmpfile"
      sed -i "s#\${STORAGE_QUEUE_CONNECTION_STRING}#${STORAGE_QUEUE_CONNECTION_STRING}#g" "$tmpfile"
      sed -i "s#\${STORAGE_ACCOUNT_NAME}#${STORAGE_ACCOUNT_NAME}#g" "$tmpfile"
      kubectl apply -f "$tmpfile"
      rm -f "$tmpfile"
    fi
  fi
fi

echo "[INFO] Done."
