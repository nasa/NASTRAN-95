import os
import sys


def is_binary(filename):
    """
    Check if the file is binary. A file is considered binary if it contains a null byte
    within the first 1024 bytes of its content.
    """
    try:
        with open(filename, "rb") as file:
            content = file.read(1024)
            if b"\0" in content:
                return True
    except Exception as e:
        print(f"Error reading {filename}: {e}")
    return False


def find_binary_files(directory):
    """
    Recursively find all binary files in the specified directory.
    """
    binary_files = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            path = os.path.join(root, file)
            if is_binary(path):
                binary_files.append(path)
    return binary_files


# Usage example:
directory_to_search = sys.argv[1]
binary_files = find_binary_files(directory_to_search)
for file in binary_files:
    print(file)
