import csv
import random

# Initialize variables
num_groups = 24
participants_per_group = 4
total_participants = 96
conditions_AB = ['A', 'B']
conditions_AC = ['A', 'C']

# Define headers
headers = [
    'group', 'subject', 'subjectType', 'condition', 'task',
    'PS1', 'PS2', 'PS3', 'M1', 'M2', 'OE1', 'OE2',
    'PTDP1', 'PTDP2', 'PTDP3', 'PTDP4', 'PTDP5', 'PTDP6',
    'PDOQ1', 'PDOQ2', 'PDOQ3', 'PDOQ4', 'PDOQ5',
    'PA1', 'PA2', 'PA3', 'PA4',
    'NASA1', 'NASA2', 'NASA3', 'NASA4', 'NASA5', 'NASA6'
]

# Function to generate responses with randomness
def generate_responses(subjectType, condition):
    responses = []
    positive_items = [
        'PS1', 'PS2', 'PS3', 'OE1', 'OE2',
        'PTDP1', 'PTDP2', 'PTDP3', 'PTDP4', 'PTDP5', 'PTDP6',
        'PDOQ1', 'PDOQ2', 'PDOQ3', 'PDOQ4', 'PDOQ5',
        'PA1', 'PA2', 'PA3', 'PA4',
        'NASA4', 'NASA5', 'NASA6'
    ]
    negative_items = ['M1', 'M2', 'NASA1', 'NASA2', 'NASA3']

    # Determine score ranges
    if condition == 'A':
        if subjectType == 'majority':
            positive_range = (4, 5)
            negative_range = (3, 4)
        else:
            positive_range = (3, 4)
            negative_range = (4, 5)
    elif condition == 'B':
        if subjectType == 'majority':
            positive_range = (5, 6)
            negative_range = (2, 3)
        else:
            positive_range = (4, 5)
            negative_range = (3, 4)
    elif condition == 'C':
        if subjectType == 'majority':
            positive_range = (6, 7)
            negative_range = (1, 2)
        else:
            positive_range = (5, 6)
            negative_range = (2, 3)

    # Generate responses with randomness
    for item in headers[5:]:  # Start from index 5 since 'task' is at index 4
        if item in positive_items:
            score = random.randint(positive_range[0], positive_range[1])
        elif item in negative_items:
            score = random.randint(negative_range[0], negative_range[1])
        else:
            score = random.randint(3, 5)  # Neutral items
        responses.append(score)
    return responses

# Write CSV file
with open('./sampleDataGenerator/selfReported_results.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(headers)
    subject = 1
    for group in range(1, num_groups + 1):
        for participant in range(participants_per_group):
            subjectType = 'minority' if subject % 4 == 0 else 'majority'
            
            # Assign conditions and task orders
            if subject <= 48:
                conditions = conditions_AB
                if subject <= 24:
                    task_order = [1, 2]
                else:
                    task_order = [2, 1]
            else:
                conditions = conditions_AC
                if subject <= 72:
                    task_order = [1, 2]
                else:
                    task_order = [2, 1]
            
            # Loop over conditions and tasks
            for condition, task in zip(conditions, task_order):
                responses = generate_responses(subjectType, condition)
                row = [group, subject, subjectType, condition, task] + responses
                writer.writerow(row)
            subject += 1
