#!/usr/bin/env bash
set -euo pipefail

# Simple Azure Storage Queue consumer: single dequeue -> run model -> delete message immediately.
# Requires env vars: STORAGE_QUEUE_CONNECTION_STRING, QUEUE_NAME.

: "${STORAGE_QUEUE_CONNECTION_STRING:?Missing STORAGE_QUEUE_CONNECTION_STRING}"
: "${QUEUE_NAME:?Missing QUEUE_NAME}"
WORKDIR=/srv/rdmtool

# Dependencies: curl, jq, base64, Rscript
command -v jq >/dev/null 2>&1 || { echo "jq not found"; exit 1; }
command -v Rscript >/dev/null 2>&1 || { echo "Rscript not found"; exit 1; }

ACCOUNT_NAME=${STORAGE_ACCOUNT_NAME:-$(echo "$STORAGE_QUEUE_CONNECTION_STRING" | sed -n 's/.*AccountName=\([^;]*\).*/\1/p')}
[[ -z "$ACCOUNT_NAME" ]] && { echo "Could not determine storage account name" >&2; exit 1; }

iso_utc() { date -u '+%Y-%m-%dT%H:%M:%SZ'; }

get_message() {
  # Single dequeue attempt; message will become invisible briefly but we delete immediately.
  if command -v az >/dev/null 2>&1; then
    az storage message get --queue-name "$QUEUE_NAME" --connection-string "$STORAGE_QUEUE_CONNECTION_STRING" -o json 2>/dev/null || true
  else
    echo '[]'
  fi
}

DELETE_ID=""
DELETE_POP_RECEIPT=""
MSG_JSON=$(get_message)
COUNT=$(echo "$MSG_JSON" | jq 'length' 2>/dev/null || echo 0)
if [[ "$COUNT" -eq 0 ]]; then
  echo "No message available; exiting."; exit 0
fi
DELETE_ID=$(echo "$MSG_JSON" | jq -r '.[0].id // empty')
DELETE_POP_RECEIPT=$(echo "$MSG_JSON" | jq -r '.[0].popReceipt // empty')
RAW_TEXT=$(echo "$MSG_JSON" | jq -r '.[0].content // empty')
if [[ -z "$RAW_TEXT" ]]; then
  echo "Empty message content; exiting."; exit 0
fi
if DECODED=$(echo "$RAW_TEXT" | base64 -d 2>/dev/null); then
  :
else
  DECODED="$RAW_TEXT"
fi
RUN_NAME=$(echo "$DECODED" | jq -r '.runName // .run // empty') || true
SUBMISSION_ID=$(echo "$DECODED" | jq -r '.submissionId // .id // empty') || true
if [[ -z "$RUN_NAME" ]]; then
  RUN_NAME="run_$(date -u +%Y%m%d%H%M%S)"
fi

[[ -z "$DELETE_ID" ]] && { echo "No message id obtained; exiting."; exit 0; }

echo "STARTING $(iso_utc) submission=$SUBMISSION_ID run=$RUN_NAME"

# Delete immediately to ignore visibility timeout / retries as requested
if command -v az >/dev/null 2>&1; then
  if [[ -n "$DELETE_ID" && -n "$DELETE_POP_RECEIPT" ]]; then
    az storage message delete --queue-name "$QUEUE_NAME" --id "$DELETE_ID" --pop-receipt "$DELETE_POP_RECEIPT" --connection-string "$STORAGE_QUEUE_CONNECTION_STRING" || true
  fi
fi

set +e

# NEW: Show exactly what will run (and from where)
echo "Executing: Rscript \"$WORKDIR/Run_Model.R\" \"$RUN_NAME\" (cwd: $(pwd)); stderr is merged into stdout"

# Send both stdout and stderr to the console (merged)
Rscript "$WORKDIR/Run_Model.R" "$RUN_NAME" 2>&1
RC=$?

set -e

if [[ $RC -eq 0 ]]; then
  echo "COMPLETED $(iso_utc) submission=$SUBMISSION_ID run=$RUN_NAME"
else
  echo "FAILED $(iso_utc) code=$RC submission=$SUBMISSION_ID run=$RUN_NAME"
fi

echo "Done."
