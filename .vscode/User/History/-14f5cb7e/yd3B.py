import log_error_classifier

model_path = '/mnt/artifacts/models/log_classification_20231019_205618'
classifier = log_error_classifier.HuggingFaceClassifier(path_or_pretrained=model_path)

tst_string = "This is a test"
text = [tst_string for i in range(0, 2)]
#text = ["This is a sentence. Does it have an error?", "There are many more to go"]

print("Did I make it here?")
#my_model = 

def my_model(text):
    predictions, probabilities = classifier.predict(text)
    return dict(a_random_number=random_number(start, stop))