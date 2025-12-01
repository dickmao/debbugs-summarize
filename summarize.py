#!/usr/bin/env python3
import sys
import os
from google import generativeai as genai

api_key = os.getenv('GEMINI_API_KEY')
if not api_key:
    print("ERROR: GEMINI_API_KEY not set", file=sys.stderr)
    sys.exit(1)

genai.configure(api_key=api_key)
model = genai.GenerativeModel('gemini-2.0-flash-exp')

prompt = sys.stdin.read()
response = model.generate_content(prompt)
print(response.text)
