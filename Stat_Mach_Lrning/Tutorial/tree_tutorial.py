import sklearn.tree
import sklearn.model_selection
import sklearn.preprocessing
import sklearn.datasets
import sklearn.model_selection

import seaborn
import numpy as np
import numpy.random as npr
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from mpl_toolkits.mplot3d import Axes3D

# For Visualizing Decision Trees
import pydotplus
from sklearn.externals.six import StringIO  
from IPython.display import Image  
from sklearn.tree import export_graphviz
import pydotplus

def gen_observations(X, model='axis_aligned_step',
                     knots=np.array([[.4,.6],[.4,.6]]),
                     values=(np.arange(5,0,-1))*2):
    y = np.zeros(X.shape[0])
    
    if model == 'axis_aligned_step':
        y += np.logical_and(X[:,0] < knots[0,0], X[:,1] >= knots[1,0]) * values[0]
        y += np.logical_and(X[:,0] < knots[0,0], X[:,1] < knots[1,0]) * values[1]
        y += np.logical_and(X[:,0] >= knots[0,0], X[:,0] <= knots[0,1]) * values[2]
        y += np.logical_and(X[:,0] > knots[0,1], X[:,1] >= knots[1,1]) * values[3]
        y += np.logical_and(X[:,0] > knots[0,1], X[:,1] < knots[1,1]) * values[4]
    elif model == 'diagonal_step':
        y += np.logical_and(X[:,0] < X[:,1], X[:,1] > 1 - X[:,0]) * values[0]
        y += np.logical_and(X[:,0] < X[:,1], X[:,1] <= 1 - X[:,0]) * values[1]
        y += np.logical_and(X[:,0] >= X[:,1], X[:,1] > 1 - X[:,0]) * values[2]
        y += np.logical_and(X[:,0] >= X[:,1], X[:,1] <= 1 - X[:,0]) * values[3]
    else:
        raise Exception('{} is not a valid model'.format(model))
    return y


def plot_dataset(X, y_obs, y_true, model='axis_aligned_step'):
    # Generate Grid Of Points Covering Space and Truth
    grid = np.linspace(0,1,50)
    grid = np.transpose([np.tile(grid, len(grid)),
                         np.repeat(grid, len(grid))])
    truth = gen_observations(grid, model=model)
    
    # Generate Figure
    fig = plt.figure(figsize=(16,8))
    
    # Plot Ground Truth 
    ax = fig.add_subplot(121, projection='3d')
    for idx in [truth == value for value in np.unique(truth)]:
        ax.scatter(grid[idx, 0], grid[idx, 1], truth[idx])
        ax.set_xlabel('X1')
        ax.set_ylabel('X2')
        ax.set_zlabel('Generative Model')
    
    # Plot Observed Data
    ax = fig.add_subplot(122, projection='3d')
    for idx in [y_true == value for value in np.unique(y_true)]:
        ax.scatter(X[idx,0], X[idx,1], y_obs[idx])
        ax.set_xlabel('X1')
        ax.set_ylabel('X2')
        ax.set_zlabel('Observed Data')
    plt.show()


def generate_regression_dataset(n,
                                plot=False,
                                model='axis_aligned_step'):
    # Generate Data
    X = npr.uniform(low=0, high=1, size=[n,2])
    y_true = gen_observations(X, model=model)
    y_obs = y_true + np.random.randn(n)
    
    # Plot Ground Truth and Observations
    if plot:
        plot_dataset(X, y_obs, y_true, model=model)
        
    return X, y_obs, y_true

def generate_classification_dataset(n, 
                                    plot=False, 
                                    model='affine'):
    if model == 'affine':
        X, y = sklearn.datasets.make_classification(n_features=2,
                                                    n_redundant=0,
                                                    n_informative=2,
                                                    n_clusters_per_class=1)
        X += 2 * npr.uniform(size=X.shape)
    elif model == 'moons':
        X, y = sklearn.datasets.make_moons(noise=0.2)
    elif model == 'circles':
         X, y = sklearn.datasets.make_circles(noise=0.2, factor=0.5)
    else:
        raise Exception("{} is an invalid dataset model.")

    return X, y

def plot_tree(tree, X, y_obs, y_true, model='axis_aligned_step'):
    
    
    # Generate Grid Of Points Covering Space and Truth
    grid = np.linspace(0,1,50)
    grid = np.transpose([np.tile(grid, len(grid)),
                         np.repeat(grid, len(grid))])
    truth = gen_observations(grid, model=model)
    y_hat = tree.predict(grid)
    
    # Generate Figure
    fig = plt.figure(figsize=(16,8))
    
    # Plot Ground Truth 
    ax = fig.add_subplot(131, projection='3d')
    for idx in [truth == value for value in np.unique(truth)]:
        ax.scatter(grid[idx, 0], grid[idx, 1], truth[idx])
        ax.set_xlabel('X1')
        ax.set_ylabel('X2')
        ax.set_zlabel('Generative Model')
    
    # Plot Observed Data 
    ax = fig.add_subplot(132, projection='3d')
    for idx in [y_true == value for value in np.unique(y_true)]:
        ax.scatter(X[idx,0], X[idx,1], y_obs[idx])
        ax.set_xlabel('X1')
        ax.set_ylabel('X2')
        ax.set_zlabel('Observed Data')
        
    # Plot Learned Partition
    ax = fig.add_subplot(133, projection='3d')
    for idx in [truth == value for value in np.unique(truth)]:
        ax.scatter(grid[idx, 0], grid[idx, 1], y_hat[idx])
        ax.set_xlabel('X1')
        ax.set_ylabel('X2')
        ax.set_zlabel('Learned Model')

    plt.show()

def show_tree_graph(tree, features, labels):
    dot_data = StringIO()
    sklearn.tree.export_graphviz(tree, 
                                 out_file=dot_data,  
                                 filled=True,
                                 rounded=True,
                                 special_characters=True,
                                 feature_names=features,
                                 class_names=labels)
    return Image(pydotplus.graph_from_dot_data(dot_data.getvalue()).create_png())

def plot_classification_tree(clf, X, y, h = .02):
    
    # Generate Plotting Axis
    fig, axs = plt.subplots(1, 2, figsize=(14, 6))
    
    # preprocess dataset, split into training and test part
    #X = StandardScaler().fit_transform(X)
    X_train, X_test, y_train, y_test = sklearn.model_selection.train_test_split(X,
                                                        y,
                                                        test_size=.1)
    clf.fit(X_train, y_train)

    # Generate Grid To Make Decision Contour
    x_min, x_max = X[:, 0].min() - .5, X[:, 0].max() + .5
    y_min, y_max = X[:, 1].min() - .5, X[:, 1].max() + .5
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h),
                         np.arange(y_min, y_max, h))

    # Set Colors
    cm = plt.cm.RdBu
    cm_bright = colors.ListedColormap(['#FF0000', '#0000FF'])
    
    # Plot Generated Data
    ax=axs[0]
    ax.set_title("Generated Data")
    
    # Plot the training points
    ax.scatter(X_train[:, 0],
               X_train[:, 1],
               c=y_train,
               cmap=cm_bright,
               edgecolors='k')
    
    # and testing points
    ax.scatter(X_test[:, 0],
               X_test[:, 1],
               c=y_test,
               cmap=cm_bright,
               alpha=0.6,
               edgecolors='k')
    
    ax.set_xlim(xx.min(), xx.max())
    ax.set_ylim(yy.min(), yy.max())
    ax.set_xticks(())
    ax.set_yticks(())
    
     # Plot Decision Boundary
    ax = axs[1]
    # Plot the decision boundary
    Z = clf.predict_proba(np.c_[xx.ravel(), yy.ravel()])[:, 1].reshape(xx.shape)
    ax.contourf(xx, yy, Z, cmap=cm, alpha=.8)
    
    # Plot the Dataset
    ax.scatter(X_train[:, 0], X_train[:, 1], c=y_train, cmap=cm_bright, edgecolors='k')
    ax.scatter(X_test[:, 0], X_test[:, 1], c=y_test, cmap=cm_bright, edgecolors='k', alpha=0.6)
    ax.set_title('Learned Model')
    
    # Set Axis Limits
    ax.set_xlim(xx.min(), xx.max())
    ax.set_ylim(yy.min(), yy.max())
    ax.set_xticks(())
    ax.set_yticks(())
