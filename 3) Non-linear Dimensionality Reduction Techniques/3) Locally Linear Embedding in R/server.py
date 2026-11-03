from flask import Flask, render_template, send_from_directory
import os

app = Flask(__name__)

IMAGE_FOLDER = os.path.join('static', 'images')

if not os.path.exists(IMAGE_FOLDER):
    os.makedirs(IMAGE_FOLDER)

app.config['UPLOAD_FOLDER'] = IMAGE_FOLDER

@app.route('/')
def show_index():
    img_path = os.path.join(app.config['UPLOAD_FOLDER'], 'plot.png')
    if os.path.exists(img_path):
        return render_template('index.html', img_path=img_path)
    else:
        return render_template('index.html', img_path=None)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=3000)
