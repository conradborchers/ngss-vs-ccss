#!/usr/bin/env python3

"""
Perform sentiment analysis on tweet data stored in JSON files using VADER Sentiment Analysis.
Results are written to a CSV file 'vader-results.csv'.
"""

import glob
import json
import csv
from tqdm import tqdm
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

# Initialize the SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()

def get_compound(text: str) -> float:
    """
    Calculate sentiment compound score for the given text.
    
    Args:
    text (str): The text for sentiment analysis.
    
    Returns:
    float: The compound sentiment score.
    """
    vs = analyzer.polarity_scores(text)
    return vs['compound']

def main():
    """
    Process JSON files containing tweet data, perform sentiment analysis,
    and write results to a CSV file.
    """
    # Dictionary to store tweet IDs and their corresponding sentiment scores
    res = dict()

    # Process each JSON file in the 'json' directory
    for f in tqdm(glob.glob('json/*.json')):
        with open(f) as infile:
            tweets = json.load(infile)['data']
        # Extract sentiment scores for each tweet and store in the 'res' dictionary
        for tweet in tweets:
            res[tweet['id']] = get_compound(tweet['text'])

    # Write results to a CSV file
    with open('vader-results.csv', 'w', newline='') as f:
        w = csv.writer(f)
        # Write tweet IDs and their sentiment scores to the CSV file
        w.writerows(res.items())

if __name__ == "__main__":
    main()

