#!/usr/bin/env bash
# Purpose: Get Azure Storage Queue credentials and export environment variables
# Usage:
#   ./get-queue-credentials.sh -s <storage-account-name> -q <queue-name> [-g <resource-group>] [-i <runner-image>]
#
# This script retrieves the connection string for an Azure Storage Queue
# and exports the environment variables needed by scaledjobgroundfish.yaml
#
# Requirements: 
#   - az CLI logged in
#   - User has access to the storage account
set -euo pipefail

usage() {
  cat << EOF
Usage: $0 -s <storage-account> -q <queue-name> [options]

Required:
  -s, --storage-account    Azure Storage Account name
  -q, --queue-name         Azure Storage Queue name

Optional:
  -g, --resource-group     Resource group (auto-detected if not provided)
  -i, --runner-image       Container image for the runner (e.g., myregistry/groundfish:latest)
  -o, --output-file        Output .env file path (default: groundfish-queue.env)
  -h, --help               Show this help message

Examples:
  $0 -s mystorageaccount -q groundfish-queue
  $0 -s mystorageaccount -q groundfish-queue -g myResourceGroup -i myregistry/runner:v1.0
EOF
}

# Default values
RESOURCE_GROUP=""
RUNNER_IMAGE=""
OUTPUT_FILE="groundfish-queue.env"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -s|--storage-account)
      STORAGE_ACCOUNT="$2"
      shift 2
      ;;
    -q|--queue-name)
      QUEUE_NAME="$2"
      shift 2
      ;;
    -g|--resource-group)
      RESOURCE_GROUP="$2"
      shift 2
      ;;
    -i|--runner-image)
      RUNNER_IMAGE="$2"
      shift 2
      ;;
    -o|--output-file)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "[ERROR] Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

# Validate required parameters
if [[ -z "${STORAGE_ACCOUNT:-}" ]]; then
  echo "[ERROR] Storage account name is required (-s)" >&2
  usage
  exit 1
fi

if [[ -z "${QUEUE_NAME:-}" ]]; then
  echo "[ERROR] Queue name is required (-q)" >&2
  usage
  exit 1
fi

echo "[INFO] Storage Account: $STORAGE_ACCOUNT"
echo "[INFO] Queue Name: $QUEUE_NAME"

# Auto-detect resource group if not provided
if [[ -z "$RESOURCE_GROUP" ]]; then
  echo "[INFO] Resource group not provided, auto-detecting..."
  RESOURCE_GROUP=$(az storage account list --query "[?name=='$STORAGE_ACCOUNT'].resourceGroup" -o tsv)
  
  if [[ -z "$RESOURCE_GROUP" ]]; then
    echo "[ERROR] Could not find storage account '$STORAGE_ACCOUNT' or auto-detect resource group" >&2
    echo "[INFO] Please specify resource group with -g flag" >&2
    exit 1
  fi
  
  echo "[INFO] Detected Resource Group: $RESOURCE_GROUP"
else
  echo "[INFO] Resource Group: $RESOURCE_GROUP"
fi

# Verify storage account exists and is accessible
echo "[INFO] Verifying storage account access..."
if ! az storage account show -n "$STORAGE_ACCOUNT" -g "$RESOURCE_GROUP" &>/dev/null; then
  echo "[ERROR] Storage account '$STORAGE_ACCOUNT' not found in resource group '$RESOURCE_GROUP' or not accessible" >&2
  exit 1
fi

# Get connection string
echo "[INFO] Retrieving connection string..."
CONNECTION_STRING=$(az storage account show-connection-string \
  -n "$STORAGE_ACCOUNT" \
  -g "$RESOURCE_GROUP" \
  --query connectionString \
  -o tsv)

if [[ -z "$CONNECTION_STRING" ]]; then
  echo "[ERROR] Failed to retrieve connection string" >&2
  exit 1
fi

# Verify queue exists or create it
echo "[INFO] Checking if queue exists..."
if ! az storage queue exists \
  --name "$QUEUE_NAME" \
  --account-name "$STORAGE_ACCOUNT" \
  --connection-string "$CONNECTION_STRING" \
  --query exists -o tsv 2>/dev/null | grep -q "true"; then
  
  echo "[WARN] Queue '$QUEUE_NAME' does not exist. Creating it..."
  az storage queue create \
    --name "$QUEUE_NAME" \
    --account-name "$STORAGE_ACCOUNT" \
    --connection-string "$CONNECTION_STRING" \
    --only-show-errors >/dev/null
  echo "[INFO] Queue created successfully"
else
  echo "[INFO] Queue '$QUEUE_NAME' already exists"
fi

# Export environment variables
export GROUNDFISH_STORAGE_ACCOUNT_NAME="$STORAGE_ACCOUNT"
export GROUNDFISH_QUEUE_NAME="$QUEUE_NAME"
export GROUNDFISH_STORAGE_QUEUE_CONNECTION_STRING="$CONNECTION_STRING"

if [[ -n "$RUNNER_IMAGE" ]]; then
  export GROUNDFISH_RUNNER_IMAGE="$RUNNER_IMAGE"
fi

echo "[INFO] Environment variables exported"

# Write to .env file
cat > "$OUTPUT_FILE" << EOF
# Azure Storage Queue credentials for Groundfish KEDA ScaledJob
# Generated on $(date -u +"%Y-%m-%d %H:%M:%S UTC")
# Storage Account: $STORAGE_ACCOUNT
# Resource Group: $RESOURCE_GROUP

GROUNDFISH_STORAGE_ACCOUNT_NAME=$STORAGE_ACCOUNT
GROUNDFISH_QUEUE_NAME=$QUEUE_NAME
GROUNDFISH_STORAGE_QUEUE_CONNECTION_STRING=$CONNECTION_STRING
EOF

if [[ -n "$RUNNER_IMAGE" ]]; then
  echo "GROUNDFISH_RUNNER_IMAGE=$RUNNER_IMAGE" >> "$OUTPUT_FILE"
else
  echo "# GROUNDFISH_RUNNER_IMAGE=<set-your-container-image-here>" >> "$OUTPUT_FILE"
fi

echo ""
echo "[SUCCESS] Credentials saved to: $OUTPUT_FILE"
echo ""
echo "To use these credentials:"
echo "  1. Load the environment variables:"
echo "     source $OUTPUT_FILE"
echo ""
echo "  2. Apply to Kubernetes:"
echo "     envsubst < scaledjobgroundfish.yaml | kubectl apply -f -"
echo ""
