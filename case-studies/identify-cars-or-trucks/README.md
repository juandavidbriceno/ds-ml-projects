### Project: Identify cars or trucks

#### Notes:
- This project was developed and executed on a Windows operating system.
- This project create an environment and install python libraries with bash.
- Then, all code is run on a Jupyter notebook file.

#### Dataset
This project uses the [Car or Truck?](https://www.kaggle.com/datasets/ryanholbrook/car-or-truck) dataset from Kaggle.

- **Source:** Kaggle  
- **License:** Data files © Original Authors  
- **Description:** The dataset contains several images of cars and trucks in a homogeneous style.  
- **How to access:** Kaggle requires you to accept their terms, so users may need to download it manually.

#### packages versions:

Python 3.10  
TensorFlow 2.12.0  
NumPy 1.23.5  
Matplotlib 3.7.1  

#### 1. Create an environment in conda for this project

In bash (Anaconda prompt):

```bash
conda create -n myenv python=3.10
conda activate myenv
```

#### 2. Install libraries this project use
```bash
pip install tensorflow==2.12.0
pip install numpy==1.23.5
pip install matplotlib==3.7.1
pip install ipykernel
python -m ipykernel install --user --name=myenv --display-name "Python (myenv)"
```
#### 3. Open Jupyter Notebook in Anaconda Navigator
```bash
jupyter notebook
```
