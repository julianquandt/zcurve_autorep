import os
import shutil
import sys
from grobid_client.grobid_client import GrobidClient

def process_folder(client, folder, n):
    failed_path = os.path.join(folder, "convert_failed")

    # Ensure the failed directory exists in the current folder
    os.makedirs(failed_path, exist_ok=True)

    print(f"Processing folder: {folder}")
    client.process("processFulltextDocument", folder, n=n)

    # Move .txt files to the convert_failed folder and check for failed conversions
    for file in os.listdir(folder):
        if file.endswith('.txt'):
            shutil.move(os.path.join(folder, file), os.path.join(failed_path, file))

    if any(file.endswith('.txt') for file in os.listdir(failed_path)):
        client.process("processFulltextDocument", folder, n=n)

def main():
    input_path = "./downloads/Psychological_Science"

    # Get the number of CPU cores and subtract 2
    default_n = max(1, os.cpu_count() - 2)
    n = input(f"Enter the number of parallel processing threads (default is {default_n}): ")
    n = default_n if n == '' else int(n)

    recursive = input("Do you want to process folders recursively? (y/n): ").lower() == 'y'

    client = GrobidClient(config_path="/home/julian/projects/grobid_client_python/config.json")

    if recursive:
        for root, dirs, files in os.walk(input_path):
            if any(file.endswith('.pdf') for file in files):
                process_folder(client, root, n)
    else:
        process_folder(client, input_path, n)

if __name__ == "__main__":
    main()
