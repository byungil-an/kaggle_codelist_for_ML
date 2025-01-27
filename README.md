# kaggle_codelist_for_ML

This repository aggregates the source code and metadata of the best scorers on kaggle.

## Usage

Develop an app to compare your own source code with the code of kaggle excellence and get FB, and treat it like a database of the codes to be compared.

## File configuration
kaggle-metadata/  
├── competition1/  
│   ├── solution1.py  
│   ├── solution2.py  
│   ├── metadata1.json  
│   └── metadata2.json  
├── competition2/  
│   ├── solution1.py  
│   ├── solution2.py  
│   ├── metadata1.json  
│   └── metadata2.json  
...  

## Contents of JSON file
{  
    "title": "competition1",  
    "overview":  "This solution is designed to handle a regression problem.",  
    "type": "Classification",  
    "model": [  
      "Lightgbm",  
      "XGBoost",  
      ...  
    ]  
    "code": "37th-place-solution.ipynb",  
    "score": 0.576,  
    "evaluation": "Quadratic weghted kappa",  
    "min score": 0.555,  
    "medal": "Gold"  
}
