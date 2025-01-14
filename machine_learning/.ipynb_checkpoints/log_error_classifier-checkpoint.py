import os
import random
import pandas as pd
from typing import Optional

from sklearn.model_selection import train_test_split
from datasets import Dataset, DatasetDict

import torch
from torch.utils.data import DataLoader, TensorDataset
from transformers import AdamW, AutoTokenizer, AutoModelForSequenceClassification, get_linear_schedule_with_warmup


class HuggingFaceClassifier:
    def __init__(self, path_or_pretrained: str, num_labels: Optional[int] = None):
        """
        The classifier allows models to either be freshly created or uploaded from a datapath
            - If num_labels is None, path_or_pretrained should be the path to saved model
            - Else, path should instead be the desired pretrained model from HF
            
        path_or_pretrained: path to precreated model OR name of pretrained model from HF
        num_labels: number of labels. This is not needed if loading a model that's already been fine-tuned 
        """
        
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.tokenizer = AutoTokenizer.from_pretrained(model_path)
        
        if num_labels is not None:
            self.model = AutoModelForSequenceClassification.from_pretrained(model_path, num_labels = num_labels)
        else:
            self.model = AutoModelForSequenceClassification.from_pretrained(model_path)
        
        self.model.to(self.device)
        self.model.eval()

    def train(self, df, epochs=3, batch_size=8, learning_rate=5e-5, warmup_steps=0):
        # Convert texts and labels to tensors
        # inputs = self.tokenizer(texts, truncation=True, padding=True, max_length=512)
        # input_ids = inputs["input_ids"]
        # attention_mask = inputs["attention_mask"]
        # labels = torch.tensor(labels).to(self.device)
        
        # Create a DataLoader
        dataset_logs = Dataset.from_pandas(df)
        tokenized_datasets_logs = dataset_logs.map(self.tokenizer(df['text'], padding="max_length", truncation=True))
        tokenized_datasets_logs = tokenized_datasets_logs.remove_columns(["text"])
        tokenized_datasets_logs = tokenized_datasets_logs.rename_column("label", "labels")
        tokenized_datasets_logs.set_format("torch")
        train_dataloader_logs = DataLoader(tokenized_datasets_logs, shuffle=True, batch_size=batch_size)
        
        
        # Define optimizer and scheduler
        optimizer = AdamW(self.model.parameters(), lr=learning_rate)
        num_training_steps = len(train_dataloader_logs) * epochs
        
        scheduler = get_linear_schedule_with_warmup(optimizer, num_warmup_steps=warmup_steps, num_training_steps=num_training_steps)
        
        # Training loop
        self.model.train()
        for epoch in range(epochs):
            for batch in train_dataloader_logs:
                batch = {k: v.to(device) for k, v in batch.items()}
                outputs = model(**batch)
                loss = outputs.loss
                loss.backward()

                optimizer.step()
                scheduler.step()
                optimizer.zero_grad()
#                 batch = tuple(t.to(self.device) for t in batch)
#                 b_input_ids, b_attention_mask, b_labels = batch

#                 # Forward pass
#                 outputs = self.model(b_input_ids, attention_mask=b_attention_mask, labels=b_labels)
#                 loss = outputs.loss

#                 # Backward pass
#                 optimizer.zero_grad()
#                 loss.backward()
#                 optimizer.step()
#                 scheduler.step()

        # Set model back to evaluation mode
        self.model.eval()

    def save_model(self, path: str):
        
        
    def predict(self, text: str):
        inputs = self.tokenizer(text, return_tensors="pt", truncation=True, max_length=512, padding="max_length")
        for key, value in inputs.items():
            inputs[key] = value.to(self.device)
        
        with torch.no_grad():
            outputs = self.model(**inputs)
            logits = outputs.logits
            probs = torch.nn.functional.softmax(logits, dim=-1)
            predicted_class = torch.argmax(probs, dim=-1).item()
        
        return predicted_class, probs[0].cpu().numpy()

# Usage example:
# model_name = "path_or_model_identifier"
# classifier = HuggingFaceClassifier(model_name)
# classifier.train(texts=["sample text 1", "sample text 2"], labels=[0, 1])
# predicted_class, class_probs = classifier.predict("Your text here")
# print(f"Predicted class: {predicted_class}, Class probabilities: {class_probs}")
if __name__ == "__main__":
    model_name = "bert-base-cased"
    classifier = HuggingFaceClassifier(model_name)
    
    project_name = 'allstate_log_github'
    data_directory = '/mnt/data/' + project_name + '/'

    dir_name = os.path.join(data_directory, 'classification_data')
    df_train = pd.read_csv(os.path.join(dir_name, "train_small.csv"))
    df_test = pd.read_csv(os.path.join(dir_name, "test_small.csv"))
    
    unique_labels = df_train['label'].unique()
    text_to_label = {'none':0, 'cluster':1, 'domino':2, 'user':3}
    label_to_text = {str(label):text for text, label in label_to_text.items()}
    
    df_train['label'] = df_train['label'].apply(lambda x: text_to_label[x])
    df_test['label'] = df_test['label'].apply(lambda x: text_to_label[x])
    
    classifier.train(df_train)
    