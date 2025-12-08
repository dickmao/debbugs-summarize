#!/usr/bin/env python3
import sys
import os
from pathlib import Path
from google import generativeai as genai

api_key = os.getenv('GEMINI_API_KEY')
if not api_key:
    print("ERROR: GEMINI_API_KEY not set", file=sys.stderr)
    sys.exit(1)

genai.configure(api_key=api_key)
model_name = os.getenv('GEMINI_MODEL', 'gemini-2.5-flash')
model = genai.GenerativeModel(model_name)

script_dir = Path(__file__).parent
prompt = (script_dir / 'summarize-prompt.txt').read_text()
full_text = sys.stdin.buffer.read().decode('utf-8', errors='replace')
response = model.generate_content(prompt + '\n\n' + full_text)
print(response.text)
