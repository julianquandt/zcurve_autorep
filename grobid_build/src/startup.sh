#!/bin/bash

# Start grobid-service in the background
/opt/grobid/grobid-service/bin/grobid-service > /dev/null 2>&1 &

# Function to check if Grobid service is running
check_grobid() {
    nc -z localhost 8070
}

# Wait for Grobid service to be up and running
echo "Waiting for Grobid service to start..."
while ! check_grobid; do
  sleep 1
done
echo "Grobid service is running."

# Run the Python script
time python3 /app/preprocess_pdf.py

/bin/bash