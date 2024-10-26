import re

# Function to remove non-alphanumeric characters and convert to lowercase
def clean_text(text):
    cleaned_text = re.sub(r'[^A-Za-z0-9]', '', text)  # Remove non-alphanumeric characters
    return cleaned_text.lower()  # Convert to lowercase

# Function to check if the cleaned text is a palindrome
def is_palindrome(cleaned_text):
    return cleaned_text == cleaned_text[::-1]  # Check if the string reads the same backward

# Function to get user input
def get_user_input():
    return input("Please enter a text to test if it's a palindrome: ")

# Function to display the result
def display_result(text, is_palindrome_flag):
    if is_palindrome_flag:
        print(f'"{text}" is a palindrome.')
    else:
        print(f'"{text}" is not a palindrome.')

# Main function to orchestrate the palindrome check
def main():
    text = get_user_input()  # Ask for user input
    cleaned_text = clean_text(text)  # Clean the text
    palindrome_flag = is_palindrome(cleaned_text)  # Check if it's a palindrome
    display_result(text, palindrome_flag)  # Display the result

# Entry point of the program
if __name__ == "__main__":
    main()
