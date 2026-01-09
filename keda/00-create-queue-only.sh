#!/usr/bin/env bash
# Purpose: Create (idempotently) an Azure Storage Queue for Groundfish (assumes storage account already exists)
# Usage:
#   Export environment variables first:
#     - GROUNDFISH_STORAGE_ACCOUNT_NAME
#     - GROUNDFISH_QUEUE_NAME
#     - GROUNDFISH_STORAGE_QUEUE_CONNECTION_STRING (optional, will be generated if not provided)
#     - GROUNDFISH_RUNNER_IMAGE (for documentation purposes)
#   Then run:
#     ./00-create-queue-only.sh
#
# Requirements: 
#   - az CLI logged in
#   - User has rights on the storage account
#   - Storage account already exists
set -euo pipefail

usage(){ grep '^#' "$0" | sed -e 's/^# \{0,1\}//'; }

# Check required environment variables
: "${GROUNDFISH_STORAGE_ACCOUNT_NAME:?Required environment variable GROUNDFISH_STORAGE_ACCOUNT_NAME is not set}"
: "${GROUNDFISH_QUEUE_NAME:?Required environment variable GROUNDFISH_QUEUE_NAME is not set}"

SA="$GROUNDFISH_STORAGE_ACCOUNT_NAME"
QNAME="$GROUNDFISH_QUEUE_NAME"

echo "[INFO] Using storage account: $SA"
echo "[INFO] Creating queue: $QNAME"

# Verify storage account exists
if ! az storage account show -n "$SA" --query id -o tsv >/dev/null 2>&1; then
  echo "[ERROR] Storage account '$SA' not found or not accessible"
  exit 1
fi

echo "[INFO] Storage account verified. Creating queue..."

# Create the queue (idempotent operation)
az storage queue create --name "$QNAME" --account-name "$SA" --only-show-errors >/dev/null

echo "[INFO] Queue '$QNAME' created successfully (or already exists)"

# Generate connection string if not provided
if [[ -z "${GROUNDFISH_STORAGE_QUEUE_CONNECTION_STRING:-}" ]]; then
  echo "[INFO] Generating connection string..."
  CONN_STR=$(az storage account show-connection-string -n "$SA" --query connectionString -o tsv)
  export GROUNDFISH_STORAGE_QUEUE_CONNECTION_STRING="$CONN_STR"
  echo "[INFO] Connection string generated and exported"
else
  echo "[INFO] Using provided connection string"
fi

# Output configuration
cat > groundfish-queue-connection.env <<EOF
# Groundfish Queue Configuration
# Generated: $(date)
GROUNDFISH_STORAGE_ACCOUNT_NAME=${SA}
GROUNDFISH_QUEUE_NAME=${QNAME}
GROUNDFISH_STORAGE_QUEUE_CONNECTION_STRING=${GROUNDFISH_STORAGE_QUEUE_CONNECTION_STRING}
GROUNDFISH_RUNNER_IMAGE=${GROUNDFISH_RUNNER_IMAGE:-}
EOF

echo "[INFO] Done. Configuration saved to groundfish-queue-connection.env"
echo ""
echo "To use this configuration, run:"
echo "  source groundfish-queue-connection.env"
