import os
import re
import csv
from collections import Counter

# Function to read a file and return its content
def read_file(file_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            return file.read()
    except UnicodeDecodeError:
        # If UTF-8 decoding fails, try a different encoding
        with open(file_path, 'r', encoding='ISO-8859-1', errors='replace') as file:
            return file.read()

# Function to clean the text and remove all 'docno' tags
def remove_docno(text):
    # Remove all lines or parts containing 'docno'
    cleaned_text = re.sub(r'\bdocno\b.*?\bdocno\b', '', text, flags=re.IGNORECASE)
    return cleaned_text

# Function to split text into words
def clean_and_split_text(text):
    # Initial clean-up (removing non-alphanumeric characters except spaces)
    cleaned_text = re.sub(r'[^A-Za-z0-9\s]', '', text)
    # Split by spaces into words
    words_array = cleaned_text.split()
    return words_array

# Function to process words in the array: remove non-alphanumeric characters and convert to lowercase
def process_words(words):
    processed_words = []
    for word in words:
        # Remove non-alphanumeric characters (this may already be handled earlier, but it's here for safety)
        word = re.sub(r'[^A-Za-z0-9]', '', word)
        # Convert to lowercase
        word = word.lower()
        # Append the processed word to the new list
        processed_words.append(word)
    return processed_words

# Function to count the frequency of each word in the list
def count_word_frequency(words):
    return Counter(words)

# Function to process all files in a directory, count word frequencies, and append results to the CSV file
def process_files_in_directory(directory_path, csv_writer):
    # Loop through all files in the directory
    for root, dirs, files in os.walk(directory_path):
        for file in files:
            if file.endswith('.txt'):
                file_path = os.path.join(root, file)
                print(f"Processing file: {file_path}")
                # Read and process the file
                text = read_file(file_path)

                # Remove all 'docno' tags from the text
                text = remove_docno(text)

                words = clean_and_split_text(text)  # Split text into words
                cleaned_words = process_words(words)  # Further process words
                word_frequencies = count_word_frequency(cleaned_words)  # Count word frequencies

                # Save each word and its frequency in the CSV file
                for word, freq in word_frequencies.items():
                    if word:  # Make sure the word is not empty
                        csv_writer.writerow([file, word, freq])

# Main function to start the process
def main():
    directories = ['105-extracted-date', '106-extracted-date']  # Replace with actual paths
    output_csv = 'word_freq.csv'  # Output CSV file name

    # Open CSV file once for writing, so both directories append data to the same file
    with open(output_csv, 'w', newline='', encoding='utf-8') as csvfile:
        csv_writer = csv.writer(csvfile)
        # Write header row once
        csv_writer.writerow(['file_name', 'word', 'frequency'])

        # Process both directories
        for directory in directories:
            print(f"\nProcessing directory: {directory}")
            process_files_in_directory(directory, csv_writer)

if __name__ == "__main__":
    main()
