README

Project:Identify_cars_or_trucks

Notes:
-This project was run on a Windows PC.
-This project create an environment and install python libraries with bash.
-Then, all code is run on a Jupyter notebook file.

packages versions:
Python 3.10
TensorFlow 2.12.0
NumPy 1.23.5
Matplotlib 3.7.1

1. Create an environment in conda for this project.

In bash(Anaconda prompt):

#
conda create -n myenv python=3.10
conda activate myenv

2. Install libraries this project use.
ip install tensorflow==2.12.0
pip install numpy==1.23.5
pip install matplotlib==3.7.1
pip install ipykernel
python -m ipykernel install --user --name=myenv --display-name "Python (myenv)"

3.Open Jupyter Notebook in Anaconda Navigator

