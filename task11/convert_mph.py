# Function to convert miles per hour to kilometers per hour
def convert_mph(mph):
    return mph * 1.60934  # 1 mile = 1.60934 kilometers

# Function to get valid input from the user
def get_user_input():
    while True:
        try:
            mph = float(input("Please enter the speed in miles per hour (mph): "))
            if mph < 0:
                print("Speed cannot be negative. Please enter a valid number.")
            else:
                return mph
        except ValueError:
            print("Invalid input. Please enter a numeric value.")

# Function to display the output
def display_result(mph, kph):
    print(f"The speed {mph} mph is equal to {kph:.2f} km/h.")

# Main function to orchestrate the program flow
def main():
    print("Welcome to the Miles per Hour to Kilometers per Hour Converter!")
    mph = get_user_input()  # Input step
    kph = convert_mph(mph)  # Process step
    display_result(mph, kph)  # Output step

# Entry point of the program
if __name__ == "__main__":
    main()
