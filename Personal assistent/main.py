import pyttsx3
import speech_recognition as sr
import os
import webbrowser
import smtplib
from email.message import EmailMessage
import wikipedia
import requests
import json

# Initialize speech engine
engine = pyttsx3.init('sapi5')
engine.setProperty('rate', 190)
engine.setProperty('volume', 1.0)
voices = engine.getProperty('voices')
engine.setProperty('voice', voices[1].id)

# Set username and botname
USERNAME = 'Ashutosh'
BOTNAME = 'JARVIS'

def speak(text):
    engine.say(text)
    engine.runAndWait()

def take_user_input():
    r = sr.Recognizer()
    with sr.Microphone() as source:
        print('Listening....')
        r.pause_threshold = 1
        audio = r.listen(source)
    try:
        print('Recognizing...')
        query = r.recognize_google(audio, language="en-in")
        return query
    except Exception as e:
        print(e)
        return None

def send_email(receiver_address, subject, message):
    EMAIL = 'your_email@gmail.com'
    PASSWORD = 'your_password'
    email = EmailMessage()
    email['To'] = receiver_address
    email['From'] = EMAIL
    email.set_content(message)
    s = smtplib.SMTP("smtp.gmail.com", 587)
    s.starttls()
    s.login(EMAIL, PASSWORD)
    s.send_message(email)
    s.close()
    return True

def get_latest_news():
    NEWS_API_KEY = 'your_news_api_key'
    response = requests.get(f'http://newsapi.org/v2/top-headlines?country=in&apiKey={NEWS_API_KEY}').json()
    news_headlines = []
    for article in response['articles']:
        news_headlines.append(article['title'])
    return news_headlines

def search_on_wikipedia(query):
    results = wikipedia.summary(query, sentences=2)
    return results

def play_on_youtube(video):
    kit.playonyt(video)

def get_random_joke():
    headers = {'Accept': 'application/json'}
    res = requests.get("https://icanhazdadjoke.com/", headers=headers).json()
    return res["joke"]

def get_random_advice():
    res = requests.get("https://api.adviceslip.com/advice").json()
    return res["slip"]["advice"]

def main():
    speak(f"I am {BOTNAME}. How may I assist you?")
    while True:
        query = take_user_input().lower()
        if 'end email' in query:
            receiver_address = input("Enter receiver's email address: ")
            subject = input("Enter subject: ")
            message = input("Enter message: ")
            send_email(receiver_address, subject, message)
        elif 'latest news' in query:
            news_headlines = get_latest_news()
            speak("Here are the latest news headlines:")
            for headline in news_headlines:
                speak(headline)
        elif 'earch on wikipedia' in query:
            query = input("Enter your query: ")
            results = search_on_wikipedia(query)
            speak(results)
        elif 'play on youtube' in query:
            video = input("Enter video title: ")
            play_on_youtube(video)
        elif 'random joke' in query:
            joke = get_random_joke()
            speak(joke)
        elif 'random advice' in query:
            advice = get_random_advice()
            speak(advice)
        elif 'quit' in query:
            speak("Goodbye!")
            break
        else:
            speak("I didn't understand that. Please try again.")

if __name__ == "__main__":
    main()